# An example of a unimodal function (on [0,2])
f <- function(x)
{
  1*x^4 - 14*x^3 + 60*x^2 - 70*x
}

### plot the function f from -.5 to 7
curve(f, from = -.5, to = 7, n = 1001, lwd = 5)
# We see it has 3 local extrema

### plot the horizontal line
abline(h = 0, lty = 5, lwd = 3)

### The derivative of f (computed manually)
f_prime <- function(x)
{
  4*x^3 - 42*x^2 + 120*x - 70
}


### plot f_prime from -.5 to 7
curve(f_prime, from = -.5, to = 7, n = 1001, lwd = 2, lty = 5, col = "blue1", add = T)
# The extrema are where the red curve crosses the horizontal line

### The 2nd derivative of f
f_dbl <- function(x)
{
  12*x^2 - 84*x + 120
}

### plot f_dbl from -.5 to 7
curve(f_dbl, from = -.5, to = 7, n = 1001, lwd = 2, lty = 5, col = "brown1", add = T)
# We know the extrema is a min if blue curve is positive and max if it is negative




# FUNCTION TO DO THE GOLDEN SECTION SEARCH #########
##########################################################
# provide an interval in which only one maximum/minimum exists
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

# Apply the function:
golden(f, c(0, 2.5))


# PLOT FOR GOLDEN SECTION from class notes
# Not needed for HW or quiz!

### plot the function from -.5 to 2.5
curve(f, from = -.5, to = 2.5, n = 1001, lwd = 3, ylim = c(-30, 45)) # plot the function from -.5 to 2.5
### Create diagram to depict starting points and first picks
a0 <- 0; b0 <- 2;
### plot dashed vertical lines from a0 and b0 to curve
text(a0 - .03,-30, "a0")
segments(a0, -100, a0, f(a0), lty = 2, lwd = 2)
text(b0 - .03,-30, "b0")
segments(b0, -100, b0, f(b0), lty = 2, lwd = 2)

# First iteration:
a1 <- .5; b1 <- 1.5
text(a1 - .03,-30, "a1")
segments(a1, -100, a1, f(a1), lty = 2, lwd = 2)
text(b1 - .03,-30, "b1")
segments(b1, -100, b1, f(b1), lty = 2, lwd = 2)

### Plot f(a1) and f(b1) on the y-axis
text(-0.03, f(a1) + 1, "f(a1)")
segments(a1, f(a1), -100, f(a1), lty = 2, lwd = 2)
text(-0.03, f(b1) + 1, "f(b1)")
segments(b1, f(b1), -100, f(b1), lty = 2, lwd = 2)

### Mark the true minimum with 'M'
M <- .77
text(M - .03,-30, "M")
segments(M, -31, M, f(M), lty = 2, lwd = 2)


# Using package NLRoot

install.packages("NLRoot")
library(NLRoot)

# Note the function SMfzero is designed to find the root, so we provide f_prime as the argument
SMfzero(f_prime, 0, 2)

####### FUNCTION FOR BISECTION METHOD ####################
##########################################################
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
       int[2] = int[1] + diff(int)/2
    } else
      if(f_prime_a < 0)
      {
       int[1] = int[1] + diff(int)/2
      } else
        if(f_prime_a == 0) return(int)
        
    f_prime_a <- f_prime(int[1] + diff(int)/2)
  }
  int
}

# Apply the function:
bisection(f_prime, c(0, 2.5))



####### FUNCTION FOR NEWTON'S METHOD #####################
##########################################################

newton.1 <- function(f_prime, f_dbl, precision = 1e-6, start)
{
  # ::: f_prime is first derivative function
  # ::: f_dbl is second derivitive function
  # ::: start is starting 'guess'
  
  x_old <- start
  x_new <- x_old-f_prime(x_old)/f_dbl(x_old)
  
  i <- 1 # ::: use 'i' to print iteration number
  print(paste0("Iteration ", i, "; Estimate = ", x_new) )
  # while loop (below) indicates that the loop will continue until the
  # criteria below is NOT met any longer.
  while (abs(x_new-x_old) > precision)
  {
    x_old <- x_new
    x_new <- x_old-f_prime(x_old)/f_dbl(x_old) 
    
    # ::: keep track of iteration history
    print(paste0("Iteration ", i+1, "; Estimate = ", x_new) )
    i <- i + 1
  }
  x_new
}

# Apply it:

newton.1(f_prime, f_dbl, start = 1.5)
# Try different starting points to see what happens if you start too far away!





#####################################
###### PLOT NEWTON'S METHOD #########
#####################################
curve(f, from = -.5, to = 7, n = 1001, lwd = 3) # plot the function
### plot inflection points
abline(v = 2, col = 'black', lty = 5)
abline(v = 5, col = 'black', lty = 5)

q_newton <- function(x, xk)
{
  f(xk) + f_prime(xk)*(x - xk) + .5*(f_dbl(xk))*(x - xk)^2
}

q_2 <- function(x) q_newton(x, 2)
curve(q_2, from = -1, to = 3, add = T, col = 'green1', lwd = 3)
x_2 <- 2 - f_prime(2)/f_dbl(2)
abline(v = 2, col = 'green1', lty = 5)
abline(v = x_2, col = 'green1', lty = 5)
points(2, f(2), col = 'green1', pch = 20, bg = 'green1', cex = 2)

q_25 <- function(x) q_newton(x, 2.5)
curve(q_25, from = -1, to = 7, add = T, col = 'brown1', lwd = 3)
x_2 <- 2.5 - f_prime(2.5)/f_dbl(2.5)
abline(v = 2.5, col = 'brown1', lty = 5)
abline(v = x_2, col = 'brown1', lty = 5)
points(2.5, f(2.5), col = 'brown1', pch = 20, bg = 'brown1', cex = 2)

q_0 <- function(x) q_newton(x, 0)
curve(q_0, from = -.5, to = 2, add = T, col = 'blue1', lwd = 3)
x_0 <- 0 - f_prime(0)/f_dbl(0)
abline(v = 0, col = 'blue1', lty = 5)
abline(v = x_0, col = 'blue1', lty = 5)
points(0, f(0), col = 'blue1', pch = 20, bg = 'blue1', cex = 2)




# Plot Newton's method for zero-finding
curve(f, from = -.5, to = 7, n = 1001, lwd = 3) # plot the function
abline(h = 0, col = "black", lty = 5, lwd = 2)

x0 <- 3
x1 <- x0 - f(x0)/f_prime(x0)
abline(v = x0, col = "black", lty = 5, lwd = 2)
abline(v = x1, col = "blue1", lty = 5, lwd = 2)
points(x0, f(x0), col = 'blue1', pch = 20, bg = 'blue1', cex = 2)
abline(a = -27, b = 20, col = "blue1", lwd = 3)

x0 <- 1.35
x1 <- x0 - f(x0)/f_prime(x0)
points(x0, f(x0), col = 'brown1', pch = 20, bg = 'blue1', cex = 2)
abline(a = -50.42402, b = 25.29650, col = "brown1", lwd = 3)
abline(v = x1, col = "brown1", lty = 5, lwd = 2)



####### FUNCTION FOR SECANT METHOD #####################
########################################################

secant <- function(f_prime, start1, start2, precision = 1e-6)
{
  # f_prime is first derivative function
  # start1 and star2 are starting guesses
  
  x_old1 = start1
  x_old2 = start2
  x_new = x_old2-f_prime(x_old2)*((x_old2-x_old1)/(f_prime(x_old2)-f_prime(x_old1))) 
  
  i <- 1 # i is iteration number
  print(paste0("Iteration ", i, "; Estimate = ", x_new) )
  while (abs(x_new-x_old2) > precision)
  {
    x_old1 = x_old2
    x_old2 = x_new
    x_new = x_old2-f_prime(x_old2)*((x_old2-x_old1)/(f_prime(x_old2)-f_prime(x_old1)))     
    # ::: keep track of iteration history
    print(paste0("Iteration ", i+1, "; Estimate = ", x_new) )
    i <- i + 1
  }
  x_new

}

# Try it:

secant(f_prime, 0, 1.5)



######### PLOT SECANT METHOD ##################################
###############################################################
curve(f, from = -.5, to = 7, n = 1001, lwd = 3) # plot the function from -.5 to 7
abline(h = 0, col = "black", lty = 5, lwd = 2)

points(.9, f(.9), col = 'blue1', pch = 20, bg = 'blue1', cex = 2)
points(1.2, f(1.2), col = 'blue1', pch = 20, bg = 'blue1', cex = 2)
x_old <- .9; x_oldest <- 1.2
x_n <- x_old - f(x_old)*( (x_old - x_oldest)/(f(x_old) - f(x_oldest)) )
segments(x0 = .9, y0 = f(.9), x1 = x_n, y1 = 0, col = "blue1", lty = 5, lwd = 2)

abline(v = x_n, col = "blue1", lty = 5, lwd = 2)
points(x_n, f(x_n), col = "brown1", pch = 20, bg = 'brown1', cex = 2)

x_oldest <- x_old
x_old <- x_n
x_n <- x_old - f(x_old)*( (x_old - x_oldest)/(f(x_old) - f(x_oldest)) )
segments(x0 = x_n, y0 = 0, x1 = x_old, y1 = f(x_old), col = "brown1", lty = 5, lwd = 2)

abline(v = x_n, col = "brown1", lty = 5, lwd = 2)
points(x_n, f(x_n), col = 'green1', pch = 20, bg = 'green1', cex = 2)
