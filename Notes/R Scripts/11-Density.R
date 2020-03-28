# Example 10.1 (Histogram density estimates using Sturges' Rule)

set.seed(12345) 
n <- 25
x <- rnorm(n)
# calc breaks according to Sturges' Rule
# Number of bins:
nclass <- ceiling(1 + log2(n))
nclass

# Class width
cwidth <- diff(range(x) / nclass)
cwidth

breaks <- min(x) + cwidth * 0:nclass
breaks
h.default <- hist(x, freq = FALSE, xlab = "default",
                  main = "hist: default")
z <- qnorm(ppoints(1000))
lines(z, dnorm(z))
# Note: the default is Sturges but selects "nice" breaks

h.sturges <- hist(x, breaks = breaks, freq = FALSE,
                  main = "hist: Sturges")
lines(z, dnorm(z))

print(h.default$breaks)
print(h.default$counts)
print(round(h.sturges$breaks, 1))
print(h.sturges$counts)
print(cwidth)

print(h.default$density[5])
print(h.sturges$density[4])


# Try again with larger sample:
n <- 1000
x <- rnorm(n)
# calc breaks according to Sturges' Rule
# Number of bins:
nclass <- ceiling(1 + log2(n))
nclass

# Class width
cwidth <- diff(range(x) / nclass)
cwidth

breaks <- min(x) + cwidth * 0:nclass
breaks
h.default <- hist(x, freq = FALSE, xlab = "default",
                  main = "hist: default")
z <- qnorm(ppoints(1000))
lines(z, dnorm(z))
# Note: the default is Sturges but selects "nice" breaks

h.sturges <- hist(x, breaks = breaks, freq = FALSE,
                  main = "hist: Sturges")
lines(z, dnorm(z))


# Values of the density estimate:
x0 <- .1
b <- which.min(h.default$breaks <= x0) - 1
print(c(b, h.default$density[b]))
b <- which.min(h.sturges$breaks <= x0) - 1
print(c(b, h.sturges$density[b]))

h.default$counts[7] / (n * 0.5)
h.sturges$counts[6] / (n * cwidth)

# True value is:
dnorm(0.1)

# Example 10.3 (Density estimation for Old Faithful)

library(MASS)  #for geyser and truehist
waiting <- geyser$waiting
n <- length(waiting)
# rounding the constant in Scott's rule
# and using sample standard deviation to estimate sigma
h <- 3.5 * sd(waiting) * n^(-1/3)
h

# number of classes is determined by the range and h
m <- min(waiting)
M <- max(waiting)
nclass <- ceiling((M - m) / h)
breaks <- m + h * 0:nclass

par(ask = TRUE)  #prompt to see next graph
h.scott <- hist(waiting, breaks = breaks, freq = FALSE,
                main = "")
truehist(waiting, nbins = "Scott", x0 = 0, prob=TRUE,
         col = 0)
hist(waiting, breaks = "scott", prob=TRUE, density=5,
     add=TRUE)

par(ask = FALSE)

# Example 10.4 (Frequency polygon density estimate)

waiting <- geyser$waiting   #in MASS
n <- length(waiting)
# freq poly bin width using normal ref rule
h <- 2.15 * sqrt(var(waiting)) * n^(-1/5)

# calculate the sequence of breaks and histogram
# using the pretty method for round cutoof points:
br <- pretty(waiting, diff(range(waiting)) / h)
brplus <- c(min(br)-h, max(br+h))
histg <- hist(waiting, breaks = br, freq = FALSE,
              main = "", xlim = brplus)

vx <- histg$mids     #density est at vertices of polygon
vy <- histg$density
delta <- diff(vx)[1] # h after pretty is applied
k <- length(vx)
vx <- vx + delta     # the bins on the ends
vx <- c(vx[1] - 2 * delta, vx[1] - delta, vx)
vy <- c(0, vy, 0)

# add the polygon to the histogram
polygon(vx, vy)

# check estimates by numerical integration to make sure the total area is 1
fpoly <- approxfun(vx, vy)
print(integrate(fpoly, lower=min(vx), upper=max(vx)))

# KDE manual implementation:
kde <- function(x,h)
{
  # Number of points in the grid
npt = 100
# Range of the grid
r = max(x) - min(x)
xmax <- max(x) + 0.1*r
xmin <- min(x) - 0.1*r
n <- length(x)
xgrid <- seq(from=xmin, to=xmax, length=npt)

# Vector to store the sum of the kernels at each point on the grid:
f = vector()
for (i in 1:npt)
  {
  # Vector of sum of kernels at the given point of the grid
  tmp=vector()
  for (ii in 1:n)
    {
    z=(xgrid[i] - x[ii])/h
    density=dnorm(z) # Change this for other kernels (this one is gaussian)
    tmp[ii]=density
  }
  f[i]=sum(tmp)
}
f=f/(n*h) #smoothing
lines(xgrid,f,col="grey")
} 

# Example from lecture slides with a small sample:
x = c(3, 4.5, 5.0, 8, 9)
# Default density estimate:
plot(density(x))
# Try different h here:
kde(x, 1.4)


# Example 10.7 (KDE of Old Faithful waiting time)

library(MASS)
waiting <- geyser$waiting
n <- length(waiting)

h1 <- 1.06 * sd(waiting) * n^(-1/5)
h1
h2 <- .9 * min(c(IQR(waiting)/1.34, sd(waiting))) * n^(-1/5)
h2
plot(density(waiting))

print(density(waiting))

# For comparison, with smaller bandwidth:
plot(density(waiting, bw = 2))

# Exercise 10.9 on p. 314:
# Simulate data from the normal location mixture:
# 0.5*N(0, 1) + 0.5*N(3, 1)
n = 1000
x = 1:n
for (i in 1:n) if (rbinom(1, 1, 0.5)) x[i] = rnorm(1) else x[i] = rnorm(1, m = 3)
hist(x, freq = F)

# Compare several choices of bandwidth, including the optimal and Silverman
# Plot the true density iver the density estimate for comparison
# Which choice appears to be the best?
silverman.h <- h2 <- .9 * min(c(IQR(x)/1.34, sd(x))) * n^(-1/5)

plot1<-plot()
plot1<-density(x, bw=silverman.h)
plot(plot1)

lines(density(x, silverman.h), col = 2)

# To add the true density:
lines(sort(x), 0.5*dnorm(sort(x)) + 0.5*dnorm(sort(x), m = 3), col = "blue")
