#!/usr/bin/python
# Stargazer 1.0 [http://www.princeton.edu/~wjia/stargazer]
# Wenhao Jia [wjia@princeton.edu], Princeton University, 2012

# This file randomly splits the input CSV file into a training set and a test set.
# Usage: python split.py filename ratio
import sys
import random

if len(sys.argv) != 3:
    print "Usage: " + sys.argv[0] + " file(.csv) ratio"
    sys.exit(1)
csv = sys.argv[1]
ratio = float(sys.argv[2])
input = open(csv + ".csv", "r")
lines = input.readlines()
input.close()
training = open(csv + "-training.csv", "w")
test = open(csv + "-test.csv", "w")

nline = len(lines)
nsample = int(nline * ratio)
random.seed()
test.write(lines[0])
for i in range(nsample):
    n = random.randint(1, nline - 1)
    test.write(lines[n])
    lines.remove(lines[n])
    nline = nline - 1
test.close()
for i in range(len(lines)):
    training.write(lines[i])
training.close()
print str(nsample) + " samples written"
