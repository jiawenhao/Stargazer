#!/usr/bin/python
# Stargazer 1.0 [http://www.princeton.edu/~wjia/stargazer]
# Wenhao Jia [wjia@princeton.edu], Princeton University, 2012

# This file filters the input CSV file such that only those records with field = value remain.
# Usage: python split.py filename field value
import sys

if len(sys.argv) != 4:
    print "Usage: " + sys.argv[0] + " file(.csv) field value"
    sys.exit(1)

file = sys.argv[1]
field = sys.argv[2]
value = int(sys.argv[3])

input = open(file + ".csv", "r")
lines = input.readlines()
input.close()
output = open(file + "-" + field + "-" + str(value) + ".csv", "w")
fields = lines[0].split(",")
for i in range(len(fields)):
    if fields[i] == field:
        pos = i
if pos == len(fields):
    print "Can't find field: " + field
    sys.exit(2)
output.write(lines[0])
n = 0
for i in range(1, len(lines)):
    line = lines[i]
    values = line.split(",")
    if int(values[pos]) == value:
        output.write(line)
	n = n + 1
print str(n) + " lines written"
output.close()
