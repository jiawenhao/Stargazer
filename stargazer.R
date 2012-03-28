# This file implements the core stepwise regression method of Stargazer.
# For more information, please visit [http://www.princeton.edu/~wjia/stargazer].
# Stargazer 1.0, Wenhao Jia [wjia@princeton.edu], Princeton University, 2012

library(splines)

# Summary
# --------
# The function below returns a spline-based linear regression model (as a
# string) for a give data frame, using a forward selection, adjusted-R^2-based
# stepwise method. The default options assume a column named "runtime" in the
# input file is to be modeled by all other columns in the input file.
#
# Default usage
# --------
# stargazer(data)
#
# Example in R
# --------
# > source("stepwise.R")
# > data = read.csv("matrix.csv")
# > model = stargazer(data)
# > print(model)
# [1] "runtime ~ + ns(blk, knots = c(2, 4, 8)) + ns(simd, knots = c(32)) + ..."
#
# Additional options
# --------
# data: The input data frame, with rows representing invididual designs.
# response: The column that represents the response (or dependent) variable.
# exclude: Exclude some columns from being used by the regression model.
# binary: Include binary parameters in the model.
# alpha: A small threshold for controlling the number of basic terms.
# beta: A small threshold for controlling the number of interaction terms.
# debug: Output extra information as the regression proceeds.
stargazer = function(data, response = "runtime", exclude = {}, binary = TRUE,
                     alpha = 0, beta = 0, debug = FALSE) {
  attach(data)
  params = names(data)

  # Remove the response column from the parameter list.
  rid = which(params == response)
  stopifnot(length(rid) == 1)
  params = params[-rid]
  # Exclude specified factors from the parameter list.
  params = setdiff(params, exclude)

  # Remove binary parameters if the option is off.
  if (!binary) {
    # Must start from the last parameter to avoid skipping items.
    for (i in length(params) : 1) {
      if (length(unique(get(params[i]))) < 3) {
        cat(params[i], " is not being used.\n", sep = "")
        params = params[-i]
      }
    }
  }

  nparam = length(params)
  stopifnot(nparam > 0)
  if (debug) {
    cat("Using ", nparam, " parameters: ", params, "\n", sep = "")
    cat("Number of samples: ", nrow(data), "\n", sep = "")
  }

  # Compute the regression curve of each parameter.
  curves = {}
  for (i in 1 : nparam) {
    curve = ""
    u = unique(get(params[i]))
    if (length(u) < 3) {
      # Use a straight line for binary parameters.
      curve = params[i]
    } else {
      # Use 1/2/3 knot(s) for a parameter with 3/4/5+ unique values.
      nknot = length(u) - 2
      if (nknot > 3)
        nknot = 3

      # Determine if the parameter varies linearly or multiplicatively.
      estimate = (max(u) + min(u)) / 2
      actual = mean(u)
      # Adopt linearity only if values spread *very* symmetrically.
      islinear = FALSE
      if (abs((estimate - actual) / estimate) < 0.01)
        islinear = TRUE

      # In either case, distribute knots evenly.
      curve = paste("ns(", params[i], ", knots = c(", sep = "")
      if (islinear) {
        base = min(u)
        step = (max(u) - min(u)) / (nknot + 1)
        knots = paste(base + step * 1 : nknot, collapse = ", ")
        curve = paste(curve, knots, sep = "")
      } else {
        base = log(min(u))
        step = (log(max(u)) - log(min(u))) / (nknot + 1)
        knots = paste(exp(base + step * 1 : nknot), collapse = ", ")
        curve = paste(curve, knots, sep = "")
      }
      curve = paste(curve, "))", sep = "")
    }

    if (debug)
      cat(params[i], ": ", curve, "\n", sep = "")
    curves[i] = curve
  }

  # Start the regression process.
  model = paste(response, " ~ ", sep = "")
  # As regression proceeds, parameters are moved from one set to the other.
  unused = params
  used = {}
  # R^2 of the currently-selected model.
  r2 = 0
  # The history of R^2 improvements for debugging.
  r2diff = {}

  converged = FALSE
  while (!converged) {
    # Make a list of enhanced models and their respective (adjusted) R^2.
    nunused = length(unused)
    adjr2s = rep(0, times = nunused)
    r2s = rep(0, times = nunused)
    models = paste(model, " + ", curves[match(unused, params)], sep = "")
    for (i in 1 : nunused) {
      t = lm(formula(models[i]))
      adjr2s[i] = summary(t)$adj.r.squared
      r2s[i] = summary(t)$r.squared
    }

    # In the first iteration, find the model with the largest R^2.
    nused = length(used)
    if (nused == 0) {
      maxv = max(r2s)
      stopifnot(maxv > 0)
      # When multiple parameters have the same R^2, use the first one.
      maxi = which(r2s == maxv)[1]
      stopifnot(maxi > 0)
      # Include the chosen parameter and there's no need to test interactions.
      model = models[maxi]
      r2diff = c(r2diff, maxv - r2)
      r2 = maxv
      used[nused + 1] = unused[maxi]
      unused = unused[-maxi]
      next
    }

    # In later iterations, use adjusted R^2 and test interactions.
    adjr2s = adjr2s[!is.na(adjr2s)]
    if (length(adjr2s) == 0) {
      # This happens when the number of samples is small compared to the
      # complexity of the model (i.e. degree of freedom is too low).
      converged = TRUE
      next
    }
    maxv = max(adjr2s)
    # In the unlikely event that multiple parameters have the same maximum
    # (adjusted) R^2 value, always use the first parameter.
    maxi = which(adjr2s == maxv)[1]

    # Include the term, if its adjusted R^2 is more than the current R^2 by
    # at least alpha.
    if (maxv - r2 < alpha * r2) {
      # No more beneficial basic term is found.
      converged = TRUE
    } else {
      model = models[maxi]
      r2diff = c(r2diff, r2s[maxi] - r2)
      r2 = r2s[maxi]
      chosen = unused[maxi]

      # Now test all the interaction terms.
      potential = used
      ended = FALSE
      while (!ended) {
        # Generate the models and their (adjusted) R^2.
        npotential = length(potential)
        adjr2s = rep(0, times = npotential)
        r2s = rep(0, times = npotential)
        models = paste(model, " + ", curves[which(params == chosen)],
                       " : ", curves[match(potential, params)], sep = "")
        for (j in 1 : npotential) {
          t = lm(formula(models[j]))
          adjr2s[j] = summary(t)$adj.r.squared
          r2s[j] = summary(t)$r.squared
        }
        
        # This happens when the number of samples is small compared to the
        # complexity of the model (i.e. degree of freedom is too low).
        adjr2s = adjr2s[!is.na(adjr2s)]
        if (length(adjr2s) == 0) {
          ended = TRUE
          next
        }
        maxv = max(adjr2s)
        maxj = which(adjr2s == maxv)[1]

        if (maxv - r2 < beta * r2) {
          ended = TRUE
        } else {
          model = models[maxj]
          r2diff = c(r2diff, r2s[maxj] - r2)
          r2 = r2s[maxj]
          potential = potential[-maxj]
          # If there are no termsleft, end loop.
          if (length(potential) == 0)
            ended = TRUE
        }
      }

      used[nused + 1] = unused[maxi]
      unused = unused[-maxi]
      if (length(unused) == 0)
        converged = TRUE
    }
  }

  if (debug) {
    cat("Final model: ", model, "\n", sep = "")
    cat("R^2: ", r2, "\n", sep = "")
  }
  detach(data)
  return(model)
}
