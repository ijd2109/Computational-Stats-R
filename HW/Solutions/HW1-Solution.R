# HW 1
# Adopted from Rizzo's textbook:
# 
# p. 94, Q3.1
# 
# 1. Write R code to generate and return a random sample of size n from the two-parameter 
# exponential distribution Exp(lambda, eta) for any n, lambda, and eta. Draw the ecdf and 
# the theoretical cdf on the same chart to make sure the algorithm works properly.

n = 60
lambda = 2
eta = 5

x = rexp(n, lambda) + eta

plot(ecdf(x))

F = function (t) 1 - exp(-lambda*(t - eta))
curve(F,  col = "red", add = TRUE)

# 
# 
# Additional:
# 
# 2. Generate 10,000 sums of squares of 5 independent standard normal variables. 
# Compare the histogram of their distribution to the density of the appropriate 
# chi-square distribution. Estimate the mean and variance of the distribution 
# and compare to the true values.

m = matrix(rnorm(10000*5), 10000, 5)
x = apply(m^2, 1, sum)

hist(x, freq = FALSE)
f = function(t) dchisq(t, 5)
curve(f,col = "red", add = TRUE)

mean(x)
var(x)
# The empirical mean and variance are very close to the theoretical 5 and 10 values

# 3. Generate 10,000 Exp(1) random variables. Use density() to plot the empirical density 
# and compare it to the true exponential density.

plot(density(rexp(10000)))
curve(dexp, col = "red", add = TRUE)

# 4. Let Y = (U, V), where U and V are independent standard normal random variables. 
# Estimate the mean of the geometric length of the random vector Y. 
# (That is the average distance from the random point (U, V) to the origin of the coordinate system.)

R = 10000
mean(sqrt(rnorm(R)^2 + rnorm(R)^2))

# It is very close to the theoretical value of sqrt(pi/2) from Rayleigh distribution
