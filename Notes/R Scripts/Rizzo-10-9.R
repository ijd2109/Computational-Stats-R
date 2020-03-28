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

(b1 =  1.06 * sd(x) * n^(-1/5))
plot(density(x, bw = b1))
(b2 = .9 * min(c(IQR(x)/1.34, sd(x))) * n^(-1/5))
lines(density(x, bw = b2), lty = 2)
lines(density(x, bw = 0.2), lty = 3)


# To add the true density:
lines(sort(x), 0.5*dnorm(sort(x)) + 0.5*dnorm(sort(x), m = 3), col = "red")
# Silverman looks to be the best