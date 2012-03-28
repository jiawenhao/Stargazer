# Stargazer 1.0 [http://www.princeton.edu/~wjia/stargazer]
# Wenhao Jia [wjia@princeton.edu], Princeton University, 2012

# This file demonstrates how to time the regression process.

source("stargazer.R")

# The list of input files to time the regression for.
files = c("matrix.csv")

# ---- The end of configurable options ----

for (file in files) {
  data = read.csv(file)
  print(system.time(stargazer(data)))
}
