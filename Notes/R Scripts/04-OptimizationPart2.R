# Root-finiding in R
# uniroot function uses Brent's method (similar to bisection but faster)

# Example: Solve cos(x) = x
# There is no closed form answer!
f = function(x) cos(x) - x
curve(f)
abline(h =0)
uniroot(f, lower = -pi, upper = pi, tol = 1e-9)

# Try with our function from last lecture
# Note there was a typo!
bisection <- function(f_prime, int, precision = 1e-7)
{
  # ::: f_prime is the function for the first derivative
  # ::: of f, int is an interval such as c(0,1) which 
  # ::: denotes the domain
  
  N <- ceiling(log(precision/(diff(int)))/log(.5))
  f_prime_a <- f_prime(int[1] + diff(int)/2)
  for (i in 1:N)
  {
    if(f_prime_a > 0)
    {
      int[1] = int[1] + diff(int)/2
    } else
      if(f_prime_a < 0)
      {
        int[2] = int[2] - diff(int)/2
      } else
        if(f_prime_a == 0) return(int)
    
    f_prime_a <- f_prime(int[1] + diff(int)/2)
    # print(int)
  }
  int
}

# Apply the function:
bisection(f, c(0, 1))

# Another R function is polyroot for finding the roots of polynomials
# You need to specify the coefficients in increasing order
# Example: To solve 1 + 2x + x^2 = 0 we use
polyroot(c(1, 2, 1))
# Answer is x = -1

# Try with example from last lecture:
# f_prime = 4*x^3 - 42*x^2 + 120*x - 70
polyroot(c(-70, 120, -42, 4))
# The first root is the min of f(x) we already found

# Optimization with R
# The function optimize uses a method similar to golden section

### Example 11.11 on p. 338 (One-dimensional optimization with optimize)

x <- seq(2, 15, .001)
y <- log(x + log(x))/(log(1+x))
plot(x, y, type = "l")

f <- function(x)
  log(x + log(x))/log(1+x)

optimize(f, lower = 4, upper = 15, maximum = TRUE)

# Try with our function
golden <- function(f, int, precision = 1e-6)
{
  # ::: This function implements the golden section search for a 
  # ::: *minimum* for the function 'f' on the range [int]
  # ::: with precision no greater than 'precision'.
  # ::: Note: 'int' is an interval such as c(2,3).
  # ::: If you want to *maximize*, multiply your function by -1.
  
  rho <- (3-sqrt(5))/2 # ::: Golden ratio
  # ::: Work out first iteration here
  f_a <- f(int[1] + rho*(diff(int)))
  f_b <- f(int[2] - rho*(diff(int)))
  ### How many iterations will we need to reach the desired precision?
  N <- ceiling(log(precision/(diff(int)))/log(1-rho))
  for (i in 1:N)                    # index the number of iterations
  {
    f_a <- f(int[1] + rho*(diff(int)))
    f_b <- f(int[2] - rho*(diff(int)))
    if (f_a < f_b)  
    {
      int[2] = int[2] - rho*(diff(int))
      
    } else{
      if (f_a >= f_b)
      {
        int[1] = int[1] + rho*(diff(int))
        
        
      } }
  }
  int
}

g = function(x) -f(x)
# Apply the function:
golden(g, c(4, 15))

# Application to MLE problems
### Example 11.12 (MLE: Gamma distribution)

m <- 20000
est <- matrix(0, m, 2)
n <- 200
r <- 5
lambda <- 2

obj <- function(lambda, xbar, logx.bar) {
  digamma(lambda * xbar) - logx.bar - log(lambda)
}

for (i in 1:m) {
  x <- rgamma(n, shape=r, rate=lambda)
  xbar <- mean(x)
  u <- uniroot(obj, lower = .001, upper = 10e5,
               xbar = xbar, logx.bar = mean(log(x)))
  lambda.hat <- u$root
  r.hat <- xbar * lambda.hat
  est[i, ] <- c(r.hat, lambda.hat)
}

ML <- colMeans(est)
ML
hist(est[, 1], breaks="scott", freq=FALSE,
     xlab="r", main="")
points(ML[1], 0, cex=1.5, pch=20)
hist(est[, 2], breaks="scott", freq=FALSE,
     xlab=bquote(lambda), main="")
points(ML[2], 0, cex=1.5, pch=20)

# Two-dimensional optimization
# The function optim uses quasi-Newton method
### Example 11.13 (Two-dimensional optimization with optim)

LL <- function(theta, sx, slogx, n) {
  r <- theta[1]
  lambda <- theta[2]
  loglik <- n * r * log(lambda) + (r - 1) * slogx -
    lambda * sx - n * log(gamma(r))
  - loglik
}

n <- 200
r <- 5;    
lambda <- 2
x <- rgamma(n, shape=r, rate=lambda)

optim(c(1,1), LL, sx=sum(x), slogx=sum(log(x)), n=n)

# Repeat the simulation:
mlests <- replicate(20000, expr = {
  x <- rgamma(200, shape = 5, rate = 2)
  optim(c(1,1), LL, sx=sum(x), slogx=sum(log(x)), n=n)$par
})
colMeans(t(mlests))


# Exercise: Find the MLE of location parameter of t distribution
install.packages("metRology")
library(metRology)

nu =3
x= rt.scaled(5, df=nu, mean = 5)

lt = function(m)
{
  prod((gamma(nu+1)/(sqrt(nu*pi)*gamma(nu/2)))*((nu+(x-m)^2)/nu)^(-(nu+1)/2))
}

m = seq(4,6, len=100)
ltm = numeric()
for (i in 1:100) ltm[i] = lt(m[i])
plot(m, ltm, type="l")

optimize(lt, lower = 4, upper = 5, maximum = TRUE)



# Gradient descent

# Number of iterations
n = 10
alpha = 0.1

# Target function
f = function(x, y) x^2*y

x <- seq(-10, 10, length= 30)
y <- x

# Outer product of the vectors x and y with function f applied to each combination
z <- outer(x, y, f)

# Cleaning the missing values when 0/0
z[is.na(z)] <- 1
# Changing background to white
op <- par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")


f_gradient = function(x, y) c(2*x*y, x^2)

m = matrix(0, n, 2)
# Initial values
m[1,1] = 3
m[1,2] = 2

for (i in 2:n)
{
  m[i,] = m[i-1,] + alpha*f_gradient(m[i-1,1],m[i-1,2])
}

fval = numeric()
for (i in 1:n) fval[i] = f(m[i,1], m[i,2])
cbind(fval,m)
