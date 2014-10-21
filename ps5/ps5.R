setwd("~/Copy/Berkeley/stat243/ps5")

# Problem 1
load("ps5prob1.Rda")
p <- c(1/(1 + exp(-X %*% beta)))
likelihood <- prod(dbinom(y, n, p)) # Value of 0 obtained
likelihood
log_likelihood <- sum(log(dbinom(y, n, p)))
log_likelihood
# Problem 2
# (a)
options(digits = 20)
num <- 1.000000000001
num
# 12 dp of accuracy

# (b) and (d)
x <- c(1, rep(1e-16, 10000))
sum(x)
# No

sum_x <- 0
for (i in 1:(length(x)))
  sum_x <- sum_x + x[i]
identical(num, sum_x)
sum_x
# 0 dp of accuracy

y <- c(rep(1e-16, 10000), 1)
sum_y <- 0
for (i in 1:(length(y)))
  sum_y <- sum_y + y[i]
identical(num, sum_y)
sum_y
# 12 dp of accuracy, right answer

# (c) and (d)
import numpy as np
import decimal
num = 1.000000000001
x = np.array([1e-16]*(10001))
x[0] = 1
sum_x = 0
for i in range(0, len(x)):
  sum_x = sum_x + x[i]
np.equal(num, sum_x)
decimal.Decimal(sum_x)
# 0 dp of accuracy

y = np.array([1e-16]*(10001))
y[len(y)-1] = 1
sum_y = 0
for i in range(0, len(y)):
  sum_y = sum_y + y[i]
np.equal(num, sum_y)
decimal.Decimal(sum_y)
# 12 dp of accuracy, right answer

# (e)
# R's \texttt{sum()} function does not simply sum from left to right.

# (f) Extra credit: Figure out what R's sum() function is doing, 
# either by finding documentation or by looking at the actual code used.
# function (..., na.rm = FALSE)  .Primitive("sum")
library(pryr)
pryr::show_c_source(.Primitive("sum"))
# sorts in order of magnitude from most to least precise?

# This should suggest that the sum() function works in a smart way, 
# but that if you calculate the sum manually, you need to be careful in some situations.