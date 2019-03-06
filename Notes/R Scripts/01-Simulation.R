# Plot of the standard normal density
x <- seq(-4, 4, by = 0.1)
y <- dnorm(x);
plot(x, y, type = 'l', col = 'blue', lwd = 2)


# Standard normal cdf
x <- seq(-4, 4, by = 0.1)
y <- pnorm(x);
plot(x, y, type = 'l', col = 'blue', lwd = 2)


# Standard normal quantiles (inverse of the cdf)
p <- seq(0.001, 0.999, by =0.001)
y <- qnorm(p)
plot(p, y, type = 'l', lwd = 6, col = 'blue')


# Note at 0 and 1 the corresponding quantiles are negative and positive infinity
p2 <- seq(0, 1, by=.001)
y2 <- qnorm(p2)
plot(p2, y2, type='l', col='red')
head(y2); tail(y2)


# Histogram of a sample of 200 standard normal variables
y <- rnorm(200)
hist(y)


# Check the effect of bins' width
y <- rnorm(200)
b <- seq(-4, 4, by = 0.2) # More narrow bins
hist(y, breaks = b, col = 'gray', border = 'white')


# Superimposing the density curve
y <- rnorm(2000); 
b = seq(-4.5, 4.5, by = 0.2)
hist(y, breaks = b, col = 'gray', border = 'black', freq = F)
lines(x,dnorm(x),col = 'blue', lwd = 2)
#Note when frequency is false and the units are in terms of density, this makes the 
# scale better to superimpse the density line.

# Note the result is slightly different every time!
# To prevent this we can set the seed
set.seed(123)

# Plot of the empirical cdf
n <- 20; 
y <- rnorm(n)
p <- ((1:n) - 0.5) / n # Empirical probabilities avoiding 0 and 1
plot(sort(y), p, type = 's', col = 'red')
plot(ecdf(y))
par(new=TRUE)

set.seed(123)

n <- 20; 
y <- rnorm(n)

p <- ((1:n)) / n # Empirical probabilities
plot(c(-3,sort(y),3), c(0,p,1), type = 's', col = 'red')


# Adding the theoretical cdf
set.seed(123)
par(new=TRUE)
n <- 40; y <- rnorm(n)
p <- ((1:n) - 0.5) / n
plot(sort(y), p, type = 's', col = 'red')
x <- seq(-3, 3, by = 0.1); 
lines(x, pnorm(x), col = 'blue', lwd = 2)


# Adding 0 and 1
set.seed(123)
n <- 40
y <- rnorm(n)
p <- ((1:n)) / n
plot(c(-3,sort(y),3), c(0,p,1), type = 's', col = 'red')
x <- seq(-3, 3, by = 0.1)
lines(x, pnorm(x), col = 'blue', lwd = 2)

# Similar idea: the Q-Q plot
n <- 40; 
y <- rnorm(n);
p <- ((1:n) - 0.5) / n

plot(qnorm(p), sort(y), col = 'red')
abline(0,1)


# Typical adjustment to probabilities to avoid 0 and 1
n <- 40; 
y <- rnorm(n);

p <- ((1:n) - 0.375) / (n+0.25)

plot(qnorm(p), sort(y), col = 'red')

abline(0,1)


# Same thing with the built-in function qqnorm:
n <- 400; 
y <- rnorm(n)
qqnorm(y)

qqline(y, col = 'blue', lwd = 2)


# Split the graph window
par(mfrow=c(1,2))
# mfrow arguments are matrix: row by column
# Histogram vs. density estimate
y <- rnorm(200); 
b = seq(-4, 4, by = 0.2)
hist(y, breaks = b, col = 'gray', border = 'white', freq = F)

lines(x,dnorm(x),col = 'blue', lwd = 2)

plot(density(y), col = 'red', lwd = 2)
lines(b, dnorm(b), col = 'blue', lwd = 2)


# Illustration of CLT for sum of uniform r.v.'s
# First switch back to normal graph display:
par(mfrow=c(1,1))
Y <- matrix(runif(10000 * 10), 10000, 10); 
y = apply(Y, 1, sum)
# this is applying to matrix 'Y', for each ROW (reprsented by "1"), 
# function = sum

plot(density(y), col = 'red', lwd = 2)
x <- seq(0, 10, by = 0.1); 
lines(x, dnorm(x, 10 * 0.5, sqrt(10 / 12)), col = 'blue', lwd = 2)

#HW1: Add theta to all observations of x after generating the rexp()
