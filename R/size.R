# Stargazer 1.0 [http://www.princeton.edu/~wjia/stargazer]
# Wenhao Jia [wjia@princeton.edu], Princeton University, 2012

# This file shows how to obtain the prediction accuracy vs. sample size curve.

source("stargazer.R")

# The name (without extension) of the training and test files.
files = c("matrix")

# ---- The end of configurable options ----

# Number of points in the curve, from 0 to number of lines in the training set.
nstep = 10

# The number of times to repeat each experiment.
ntest = 5

for (file in files) {
  training = read.csv(paste(file, "-training.csv", sep = ""))
  test = read.csv(paste(file, "-test.csv", sep = ""))
  inc = nrow(training) / nstep
  for (step in seq(1 : nstep)) {
    errors = {}
    size = inc * step
    for (n in seq(1 : ntest)) {
      training.sample = training[sample(seq(1:nrow(training)), size), ]
      model = stargazer(training.sample)
      attach(training.sample)
      fit = lm(formula(model))
      prediction = predict(fit, test)
      real = test$runtime
      error = (abs(cbind(prediction) - real)) / real
      errors = c(errors, error)
      detach(training.sample)
    }
    cat(size, "samples\n")
    print(summary(errors))
  }
}
