# User-defined function:

sumsq <- function(vec)
{
  mn <- mean(vec)
  d <- vec - mn
  d2 <- d^2
  ss <- sum(d2)
  ss
}

set.seed(123)
sumsq(rnorm(100))


### Simple simulation example

### Data generation function for standard normal
dg0 <- function(ss) {
  out <- rnorm(ss, mean = 0, sd = 1)
  #out
}

### Analysis function to estimate s^2 and sigma^2_hat
simFun0 <- function(dat) {
  nn <- length(dat)
  s2 <- var(dat)
  sigma2hat <- s2*((nn-1)/nn)
  out <- c(s2, sigma2hat)
  out
}

### Define number of replications and sample size
R <- 1000000
n <- 10

### Create a matrix to store output
out0 <- matrix(0, R, 2)

### Set the seed for reproducibility
set.seed(145389)

### Run the simulation within a for loop
for (i in 1:R) {
  out0[i,] <- simFun0( dg0(n) )
}

### Estimate bias, var, sd, mse
(means0 <- apply(out0, 2, mean))
(bias0 <- means0 - c(1,1))
(var0 <- apply(out0, 2, var))
(sd0 <- apply(out0, 2, sd))
(mse0 <- apply(out0, 2, function(vec) mean((vec - 1)^2)))
### Can also calculate the simulation standard error for the mean by
### dividing sd0 by the square root of R.
(se0 <- sd0/sqrt(R))


output <- rbind(means0, bias0, var0, mse0, se0)
rownames(output) <- c("Means", "Bias", "Var", "MSE", "SimSE")
colnames(output) <- c("s2", "sigma2hat")
output


# Theoretical values for comparison:
# For the N(0, 1), sigma = 1
# Therefore
# var(s^2) = 2*1/(10-1) = 0.2222
# var(sigmahat^2) = 2*1*(10-1)/10^2 = 0.18


### A simulation study to assess regression model accuracy
### Suppose the true model is Y = 2 + 3X + e, where e ~ N(0,1) and X is fixed.
set.seed(136344)
X <- runif(100, min = -2, max = 2)

### Generate Y using random e
Y <- 2 + 3*X + rnorm(100, mean = 0, sd = 1)
plot(X, Y)

### Plot the true regression line and the estimated one
abline(a = 2, b = 3, lwd = 3, col = 2)
abline(lm(Y ~ X), lwd = 2, col = "gray")

### Write a function to automate this process
genDat <- function(R)
{
  ### R is the number of replications
  ### Create a matrix for storing output
  out <- matrix(0, R, 2)
  ### Create regression model with given Betas
  for (i in 1:R) {
    Y <- 2 + 3*X + rnorm(100, mean = 0, sd = 1)
  ### Plot the estimated regression lines and ONLY 1 sample scatterplot
    if(i == 1) {plot(X, Y)}
    abline(lm(Y ~ X), lwd = 2, col = "gray")
    ### Record the intercepts and slopes for output
    out[i,] <- lm(Y ~ X)$coef
  }
  ### Plot the true regression line
  abline(a = 2, b = 3, lwd = 3, col = 2)
  ### Return the output
  out
}
R<-100000
### Run the function 50 times
out50 <- genDat(1000)

# Comparing to the true values:

colMeans(out50)
# Compare to the true a and b

apply(out50, 2, var)

# Home exercise: To compare to the theoretical variance
# change the code to also keep track of MSE along with the two betas
# then apply the formulae on slide 17


# Example of the inverse transform method
# Rizzo 3.4 (Rayleigh distribution)

# Define the Raylegih sigma
rs = 4

R = 10000
u = runif(R)
x = rs*sqrt(-2*log(1-u))

hist(x, freq = F)
lines(density(x), col = "red")
# Peak should be around the rs value

# Alternatively, there is rrayleigh function in the package VGAM

install.packages("VGAM")
library(VGAM)

lines(density(rrayleigh(R, rs)), col = "blue")


# In-class Exercise

# Generate 10,000 sums of two U(0,1) distributed random variables, 
# and estimate the mean and variance of the resulting distribution
# Remark: this is known as the triangular distribution


