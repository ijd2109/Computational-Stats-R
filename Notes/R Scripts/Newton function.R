newton <- function(f_prime, f_dbl, start, precision = 1e-5) {
  #Compute the first iteration to obtain x and x_i, and their difference
  x_old <- as.numeric(start)
  x_new <- x_old - f_prime(x_old)/f_dbl(x_old)
  diff <- x_new - x_old
  #if the difference is above the desired precision, repeat
  while (abs(diff) > precision) {
    x_old <- x_new #switch the new value into the position of the old
    x_new <- x_old - f_prime(x_old)/f_dbl(x_old) #compute a new, new value
    diff <- x_new - x_old #compute a new difference, the while-loop will now restart
  }
  return(list(x = x_new))
}
# #Example
# newton(f_prime = function(x) (4*x^3-42*x^2+120*x-70),
#        f_dbl = function(x) (12*x^2-84*x+120),
#        start = 1)