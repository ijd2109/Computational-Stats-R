# HW 2
# Investigate the bias of the regression coefficient estimators when the error variance
# sigma^2 varies with x (the heteroscedasticity problem) for different sample sizes and 
# different regression coefficients.
# The general model will be the simple linear regression model:
#  yi = β0 + β1xi + ei
# where ei is normally distributed with a mean of 0 and a variance equal to e^(γx_i )
#  Set up a Monte Carlo experiment and investigate what factors affect the bias of the 
# estimated coefficients. The factors to examine are:
#  Level of heterogeneity (gamma = 0, 0.5, 1, 2); Regression slope (β1 = 0, 0.5, 1, 2); 
# Sample sizes (n = 10, 25, 50, 100).
#  For each data set randomly sample the x-values from the N(0,1) distribution. 
# These should be the steps:
#   a) Simulate n values of x from N(0,1)
#   b) Obtain the variances of the errors from the formula e^(γx_i )
#   c) Simulate the ε_i from the corresponding normal distribution
#   d) Compute yi = β0 + β1xi + ei 
#   e) Estimate β ̂_0,β ̂_1 and store them in output matrix
#   f) Repeat steps b)-e) R times
#   g) Obtain the average β ̂_0,β ̂_1 and compare to the true values to assess the bias
#   h) Repeat from part a) with different gamma, b1 and n.
#   i) Summarize your findings.


n = 50
gamma = 1
b1 = 0
b0 = 46

# a)
x = rnorm(n)

# b)
s = exp(gamma*x)

# c)
e = rnorm(n, sd = sqrt(s))

# d)
y = b0 + b1*x + e

# Cross-check
plot(x,y)
# Increasing variability should be visible on the plot

# e) 

lm(y ~ x)$coef
# Reasonably close to b0 = 46 and b1

# f)
R = 1000
out = matrix(0, R, 2)

for (i in 1:R) 
{
  e = rnorm(n, sd = sqrt(s))
  y = b0 + b1*x + e
  out[i, ] = lm(y ~ x)$coef
}

# g) 
colMeans(out)
# Very close to true values!

# h)
n = 25
gamma = 2
b1 = 0.5
x = rnorm(n)
s = exp(gamma*x)
R = 10000
for (i in 1:R) 
{
  e = rnorm(n, sd = sqrt(s))
  y = b0 + b1*x + e
  out[i, ] = lm(y ~ x)$coef
}

colMeans(out)
# Similar to the other!

# i) Increasing variability did not introduce bias to the estimates

# 2. Consider the multiple regression model
# y_i=β_0 +β_1 x_1i +β_2 x_2i+ε_i,i=1,…,n

# a) Generate datasets. Use n = 20, x1 from U(0,1) and x2 from U(0, 2) distribution. 
# Generate the two sets of x’s only once – they will remain fixed, but next we will 
# generate many sets of y’s. Now set b0 = 1, b1 = 2, and b2 = 3, and generate random 
# errors from N(0, 1). Finally, generate the y’s using the above equation. 
# Repeat from the generation of the random errors to produce many sets of y’s. 
# Check your work with appropriate histograms.

n = 20
x1 = runif(n)
x2 = runif(n, max =2)
b0 = 1
b1 = 2
b2 = 3
R = 1000
y = matrix(0, R, n)

for (i in 1:R) y[i, ] = b0 + b1*x1 + b2*x2 + rnorm(n)

hist(y)
# Marginally y has normal distribution with mean 1 + 2*0.5 + 3*1 = 5 as it should have

hist(y[,1])
# Should have normal distribution with mean b0 + b1*x1[1] + b2*x2[1]

# b) Estimate the b’s and sigma in each dataset. Construct histograms of the distributions 
# of beta.hat0, beta.hat1, beta.hat2 and sigma.hat^2 and plot on the same screen splitting 2x2.

out = matrix(0, R, 4)
for (i in 1:R)
{
  reg = lm(y[i,] ~ x1 + x2)
  out[i, ] = c(coef(reg), sigma(reg))
}

par(mfrow = c(2,2))
hist(out[,1])
hist(out[,2])
hist(out[,3])
hist(out[,4]^2)

# c) Compute the means and standard deviations of your estimates. 
# Are they close to what they should be per the theoretical formulas?

apply(out, 2, mean)
# The numbers are very close to 1, 2, 3, and 1 as they should be

# Theoretical st. dev. of beta hats:
X = cbind(rep(1, n), x1, x2)
sqrt(diag(solve(t(X)%*%X)))

# Empirical st. dev. of beta hats:
apply(out, 2, sd)
# Standard deviations are very close to what the theory says!

# 3. Pr 3.11 on p. 95
# Adopted from Ex 3.20:

loc.mix <- function(n, p, mu1, mu2, Sigma) {
  #generate sample from BVN location mixture
  n1 <- rbinom(1, size = n, prob = p)
  n2 <- n - n1
  x1 <- rnorm(n1, mean = mu1, Sigma)
  x2 <- rnorm(n2, mean = mu2, Sigma)
  X <- c(x1, x2)            #combine the samples
  return(X[sample(1:n)])      #mix them
}

par(mfrow = c(2, 2))

p = 0.75
x <- loc.mix(1000, p, 0, 3, Sigma = 1)
r <- range(x) * 1.2
hist(x, xlim = r, ylim = c(0, .3), freq = FALSE,
       main = "p = 0.75", breaks = seq(-5, 10, .5))
lines(density(x), col = "red")

p = 0.5
x <- loc.mix(1000, p, 0, 3, Sigma = 1)
r <- range(x) * 1.2
hist(x, xlim = r, ylim = c(0, .3), freq = FALSE,
     main = "p = 0.5", breaks = seq(-5, 10, .5))
lines(density(x), col = "red")

p = 0.3
x <- loc.mix(1000, p, 0, 3, Sigma = 1)
r <- range(x) * 1.2
hist(x, xlim = r, ylim = c(0, .3), freq = FALSE,
     main = "p = 0.3", breaks = seq(-5, 10, .5))
lines(density(x), col = "red")

p = 0.1
x <- loc.mix(1000, p, 0, 3, Sigma = 1)
r <- range(x) * 1.2
hist(x, xlim = r, ylim = c(0, .3), freq = FALSE,
     main = "p = 0.1", breaks = seq(-5, 10, .5))
lines(density(x), col = "red")

# Conclusion: Empirical distribution appears bimodal for p between 0.3 and 0.7

par(mfrow = c(1, 1))