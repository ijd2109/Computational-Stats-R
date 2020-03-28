# HW 3

# 1. The following data are from a Cauchy(theta) distribution with location parameter theta, 
# which has a pdf:
  
#  f(x | theta)=1/(π[1+〖(x-θ)〗^2])  
  
# 1.77, -0.23, 2.76, 3.80, 3.47, 56.75, -1.34, 4.24, -2.44, 3.29, 3.71, -2.40, 4.53, 
# -0.07, -1.05, -13.87, -2.53, -1.75, 0.27, 43.21

# a) Graph the log likelihood function.
  
#   Find the MLE of theta using any method. Try all the following starting points 
# if your method requires a starting point:
#    -11, -1, 0, 1.5, 4, 4.7, 7, 8, 38
# Discuss your results. 
  
# If your method requires a starting interval, try the points -1 and 1, -2 and -1, 
# and finally -3 and 3.

# a)
x = c(1.77, -0.23, 2.76, 3.80, 3.47, 56.75, -1.34, 4.24, -2.44, 3.29, 3.71, -2.40, 4.53, 
-0.07, -1.05, -13.87, -2.53, -1.75, 0.27, 43.21)

LL = function(theta) -sum(log(pi*(1 + (x - theta)^2)))

theta = seq(-10, 10, len = 100)
LLvec = 1:100

for (i in 1:100) LLvec[i] = LL(theta[i])
plot(theta, LLvec, type = "l")
# The MLE appears to be a bit smaller than 0

# b)

golden <- function(f, int, precision = 1e-6)
{
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
      
    } else
      {
      if (f_a >= f_b) int[1] = int[1] + rho*(diff(int))
      }
  }
  int
}

LLmin = function(theta) -LL(theta)

golden(LLmin, c(-1,1))
# Converges to the correct max
# MLE of theta = -0.19229

#Cross-check with optimize
optimize(LL, lower = -1, upper = 1, maximum = TRUE)

golden(LLmin, c(-2,-1))
# Converges to the right endpoint because LL is increasing in (-2, -1)

golden(LLmin, c(-3,3))
# Converges to the correct value.

# Conclusion: it is important the search interval contains the target max or min

# 2. 
# Use any multivariate method to find the maximum of 

# f(x_1,x_2 )=-(x_1-2)^4-(x_1-2x_2 )^2
# with starting points 0 and 3.

f = function(x) (x[1]-2)^4 + (x[1]-2*x[2] )^2
optim(c(0,3), f)

# Maximum is at x1 = 1.9783572, x2 = 0.9890109

# Cross-check with gradient method:

n = 10000
alpha = 0.09

# Target function
f = function(x) -(x[1]-2)^4 - (x[1]-2*x[2] )^2
f_gradient = function(x, y) c(-4*(x-2)^3 -2*(x-2*y), -2*(x-2*y)*(-2))

m = matrix(0, n, 2)
# Initial values
m[1,1] = 0
m[1,2] = 3

for (i in 2:n)
{
  m[i,] = m[i-1,] + alpha*f_gradient(m[i-1,1],m[i-1,2])
}

fval = numeric()
for (i in 1:n) fval[i] = f(m[i,])

m[n,]
# Close enough