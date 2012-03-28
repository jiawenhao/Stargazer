# Stargazer 1.0 [http://www.princeton.edu/~wjia/stargazer]
# Wenhao Jia [wjia@princeton.edu], Princeton University, 2012

# Measure model prediction accuracy using a training set and a test set.

source("stargazer.R")

# The list of input files (w/o extension) to test the prediction accuracy for.
files = c("matrix")

# ---- The end of configurable options ----

for (file in files) {
    training = read.csv(paste(file, "-training.csv", sep = ""))
    test = read.csv(paste(file, "-test.csv", sep = ""))
    model = stargazer(training)
    attach(training)
    fit = lm(formula(model))
    prediction = predict(fit, test)
    real = test$runtime
    error = (abs(prediction - real)) / real
    detach(training)
    cat(file, "\n")
    print(summary(error))
}
