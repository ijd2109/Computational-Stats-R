# Rizzo 
# Example 8.1 (Permutation distribution of a statistic)

# Plot the data
attach(chickwts)
boxplot(formula(chickwts))
# We note that linseed and soybean groups are similar
# So we will test if they are from the same population

# obtain the x and y samples
x <- as.vector(weight[feed == "soybean"])
y <- as.vector(weight[feed == "linseed"])
detach(chickwts)
x
y
#The theory is that if these two are actually distributed
#in the same manner (from the same underlying population)
#then this mixing shuffling sampling won't matter
#in the final results, how many actually came from group
#1 and which from group 2 which will then prove they are
#from the indistinguishable populations (the same), or not!

# We first compare the two groups with a t-test:
#assuming they are normally dist from the same population.
R <- 1000              #number of replicates
z <- c(x, y)          #pooled sample
n = length(x)
K <- 1:length(z)      # Total pooled sample size
reps <- numeric(R)   #storage for replicates
# Value of t-stat for original sample:
t0 <- t.test(x, y)$statistic
t0

# Generate the permutations and compute the t-stat on each of them
set.seed(235)
for (i in 1:R) {
  #generate indices k for the x sample
  k <- sample(K, size = n, replace = FALSE)
  x1 <- z[k]
  y1 <- z[-k]      #complement of x1
  reps[i] <- t.test(x1, y1)$statistic
}

# p-value
p <- mean(c(t0, abs(reps) >= t0))
p #the p-val was just the proportion of t-observed were larger than t-crit
#[1] 0.2121124
# Note the use of absolute value because it is a two-tail test!
# Conclusion: since p-value > 0.05 we don't reject Ho: mu_linseed = mu_soybean

# For comparison, the p-value under the normal distribution assumption:
t.test(x, y)$p.value
#since it was the same as the t-test, we show that they two samples are normally
#distributed (the permutation test is not really needed)

# Histogram of the t stat replicates and the observed value
hist(reps, main = "", freq = FALSE, xlab = "T replicates")
points(t0, 0, cex = 1, pch = 16)      #observed T


# Example 8.2 K-S method
#comparing two CDF to see if they are exactly the same
#cmpare a density of a sample to a known CDF
# continues Example 8.1

# Permutation distribution of the Kolmogorov-Smirnov statistic
# Ho: F = G vs. Ha: F not equal G

D <- numeric(R)      #storage for replicates

# To supress the p-value warning:
options(warn = -1)

# Value of the K-s stat from original sample:
D0 <- ks.test(x, y, exact = FALSE)$statistic

set.seed(234)
for (i in 1:R) {
  k <- sample(K, size = n, replace = FALSE)
  x1 <- z[k]
  y1 <- z[-k]      #complement of x1
  D[i] <- ks.test(x1, y1, exact = FALSE)$statistic
}

# p-value:
p <- mean(c(D0, D) >= D0)
p
# Conclusion: Since p-value > 0.05 we don't reject Ho

# Restore warnings:
options(warn = 0)


hist(D, main = "", freq = FALSE, xlab = "K-S replicates")
points(D0, 0, cex = 1, pch = 16)      #observed D


# Permutation test of independence with correlation

# Define the test statistic
cor1 = function(z, ix)
{
  n = nrow(z)
  x = z[,1]    # leave x as is
  y = z[ix, 2] # permute the rows of y
  return(cor(x,y))
}

# Obtaining the permutations wiht the boot function
library(boot)
#Idea: sample so that there distributions in the bootstrap sample
#are normal and independent. Then compare what the p-value would have been
#of the correlation from that bootstrap population to the 
#one actual correlation p value.
# Dataset
z = as.matrix(iris[1:50, 1:2])
plot(z)
#note below, the permutation argument means the sample 
#will be without replacement 
boot.obj = boot(data = z, statistic = cor1, R = 1000, sim = "permutation")
tb = c(boot.obj$t0, boot.obj$t)
hist(tb, freq = F)
points(boot.obj$t0, 0, cex = 1, pch = 16)

# p-value
mean(tb >= boot.obj$t0)

# Compare to cor.test p-value
cor.test(z[,1], z[,2])

# Both reject Ho: Sepal length and width are independent