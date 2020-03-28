### Exercise

# Generate 10,000 sums of two U(0,1) distributed random variables, and estimate the mean and variance of the resulting distribution

### Data generation function for U(0,1)
dg1 <- function(ss) {
  out <- runif(ss) # runif has default values 0 and 1 for the interval
}

### Analysis function to calculate the sum of two U(0,1) variables
simFun1 <- function(dat) 
{
  out <- sum(dat)
  out
}

### Define number of replications and sample size
R <- 10000
n <- 2

### Create a matrix to store output
# We need only one column, because we track only the sum of the variables
out1 <- matrix(0, R, 1)

### Set the seed for reproducibility
set.seed(462)

### Run the simulation within a for loop
for (i in 1:R) {
  out1[i,] <- simFun1( dg1(n) )
}

# Histogram:
hist (out1[,1], freq = F)
lines(density(out1[,1]),  col = "red")
# Should look like a triangle

### Estimate mean and variance
(mean1 <- apply(out1, 2, mean))
(var1 <- apply(out1, 2, var))

### The true values are:
### Mean = 1, Variance = 1/6
# Note: The distribution of a sum of two uniform r.v.?s is called triangular


