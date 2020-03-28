# Exercise: Consider the function sin(x) between -3pi/4 and 3pi/4
# a) Plot sin(x) and its first and second derivatives 
# Set accuracy parameter to 10e-5

# b) Find min of sin(x). Use staring interval (-2, -1).
# c) Find max of sin(x). This time use starting interval (0.5, 2.5).
# d) Find the root of sin(x) in the given interval
# e) Compare the methods in terms of number of iterations


# a)
# Plot sin(x) between -3pi/4 and 3pi/4
curve(sin, from = -3*pi/4, to = 3*pi/4, n = 100, lwd = 4)

### plot the horizontal line
abline(h = 0, lty = 5, lwd = 3)

### The derivative of sin(x)
f_prime = cos

# Plot derivative of sin(x) between -3pi/4 and 3pi/4
curve(f_prime, from = -3*pi/4, to = 3*pi/4, n = 100, lwd = 2, lty = 5, col = "blue1", add = T)


# The 2nd derivative of sin(x)
f_dbl <- function(x)
{
  -sin(x)
}

# Plot f_dbl from -3pi/4 and 3pi/4
curve(f_dbl, from = -3*pi/4, to = 3*pi/4, n = 100, lwd = 2, lty = 5, col = "brown1", add = T)



# Functions from class

##########################################################
####### FUNCTION TO DO THE GOLDEN SECTION SEARCH #########
##########################################################
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
  print(N) 
 for (i in 1:(N))                    # index the number of iterations
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

golden(sin, c(-2,-1), 10e-5)


##########################################################
####### FUNCTION FOR BISECTION METHOD ####################
##########################################################
bisection <- function(f_prime, int, precision = 1e-7)
{
  # ::: f_prime is the function for the first derivative
  # ::: of f, int is an interval such as c(0,1)
  
  N <- ceiling(log(precision/(diff(int)))/log(.5))
  print(N)
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
        if(f_prime_a == 0)
        {

          return(int)
        }
    f_prime_a <- f_prime(int[1] + diff(int)/2)
  }
  int
}

bisection(cos,c(-2,-1),10e-5)



##########################################################
####### FUNCTION FOR NEWTON'S METHOD #####################
##########################################################

newton <- function(f_prime, f_dbl, precision = 1e-6, start)
{
  # ::: f_prime is first derivative function
  # ::: f_dbl is second derivitive function
  # ::: start is starting 'guess'
  
  x_old <- start
  x_new <- x_old-f_prime(x_old)/f_dbl(x_old)
  
  i <- 1 # ::: use 'i' to print iteration number
  print(paste0("Iteration ", i, "; Estimate = ", x_new) )
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

newton(cos, f_dbl, 10e-5,-2)



########################################################
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

secant(cos,-2,-1,10e-5)

# c) To find max, we can use either method:

f = function(x) ?sin(x)
golden(f, c(0.5,2.5), 10e-5)
# Note this time it took 21 iteratoins, because the interval is wider


f_prime = function(x) -cos(x)
bisection(f_prime,c(0.5,2.5),10e-5)
# Again one extra iteration compared to before, but still faster than Golden

newton(cos, f_dbl, 10e-5, 2.5)
# Newton?s method is actually faster now!

secant(cos, 0.5, 2.5,10e-5)


# d) To find the zero we can use f and f_prime in Newton?s method

newton(sin, cos, 10e-5, 1)

