# Generate data with 10 covariates and an outcome that
### is related to only five of them linearly

install.packages("mvtnorm")
install.packages("clusterGeneration")
library(mvtnorm)
library(clusterGeneration)

### Generate a random covariance matrix with package clusterGeneration
set.seed(1790)
cov1 <- genPositiveDefMat(dim = 10, covMethod = "eigen")
### Generate a random vector of 10 means from norm(0, 10)
mns1 <- rnorm(10, 0, 10)
### Generate coefficients for the output for Y from norm(0, 1)
coef1 <- rnorm(11, 0, 1/2) # First one is the intercept
### Set five of them equal to zero
coef1[sample(2:11, 5, replace = FALSE)] <- 0

datGen <- function(N) {
  ### Generate the X matrix 
  X <- rmvnorm(n = N, mean = mns1, sigma = cov1$Sigma)
  ### Add column of 1s for the intercept
  X_aug <- cbind(1, X)
  ### Create the output Y 
  Y <- X_aug %*% coef1 + rnorm(N, 0, 1)
  dfOut <- data.frame(cbind(X, Y))
  names(dfOut) <- c(paste0("X", 1:10), "Y")
  dfOut
}


set.seed(5671)
ss100 <- datGen(100)

lm1 <- lm(Y ~ ., data = ss100)
summary(lm1)

### Use bestglm package for best subset selection
install.packages("bestglm")
library(bestglm)
# Using regsubsets from leaps package which performs exhaustive search:
rsOut <- regsubsets(Y ~ ., data = ss100)
summary(rsOut)
#BIC is better for the "explanation" or insights purpose
#AIC is better if you want to make prediction.
# Note that bestglm requires you to specify the data as predictors and then response
# The default family is gaussian so this parameter is optional
bs1 <- bestglm(Xy = ss100, family = gaussian, IC = "BIC")
summary(bs1)

# What is in the output of bestglm?
names(bs1)
bs1$BestModel
bs1$Subsets

# Now with AIC
bs2 <- bestglm(Xy = ss100, IC = "AIC")
summary(bs2)
bs2$BestModel


# ECLS-K Example ############################################################
###############################################################################
### Load the eclsk_c data by opening a browser window
load(file.choose())

### Or call the path directly (fill in your path)
load(file = "d:/Work/Columbia/HUDM6026/eclsk_c.Rdata")
head(eclsk_c)
dim(eclsk_c)

# Cleaning the data
eclsk1 <- eclsk_c[, !names(eclsk_c) %in% c("S1_ID", "CHILDID", "C5R4RSCL", "C6R4RSCL", "C5R4MSCL", "C6R4MSCL", "F5SPECS")]
eclsk1 <- cbind(eclsk1, F5SPECS = eclsk_c$F5SPECS, C6R4MSCL = eclsk_c$C6R4MSCL)
head(eclsk1)

# This is how you can time how long it takes to run the function
st <- Sys.time()
bs3 <- bestglm(Xy = eclsk1, 
               family = gaussian,
               IC = "BIC")
en <- Sys.time()
(tm <- en - st)


bs4 <- bestglm(Xy = eclsk1, 
               family = gaussian,
               IC = "AIC")

plot(0:35, bs3$Subsets$BIC, type = "b", ylab = "BIC", ylim = c(40700, 41500),
     xlab = "Number of Covariates", lwd = 3, pch = 19, main = "BIC", cex.main = 2)

plot(0:35, bs4$Subsets$AIC, type = "b", ylab = "AIC", ylim = c(40525, 41300),
     xlab = "Number of Covariates", lwd = 3, pch = 19, main = "AIC", cex.main = 2)

summary(bs3$BestModel)
summary(bs4$BestModel)


# Forward selection with MASS
library(MASS)
# Smallest model with just intercept:
min.model <- lm(C6R4MSCL ~ 1, data = eclsk1)
# Largest model with all predictors:
max.model <- lm(C6R4MSCL ~ ., data = eclsk1)
# Scope of the search for forward selection
scp <- list(lower = min.model, upper = max.model)
# Forward selection
fwd <- stepAIC(min.model, direction = 'forward', scope = scp)
fwd$coefficients

### Do the results match up with best subset selection?
# Variables in forward selection (minus the intercept)
d1 <- names(fwd$coefficients)[-1]
# Variables in best subset selection:
d2 <- names(bs4$Subsets[27, bs4$Subsets[27,] == TRUE])[-1]
# Check if d1 is a subset of d2 and if d2 is a subset of d1
d1 %in% d2
d2 %in% d1
# Perfet match! (Not guaranteed in general)

### Forward selection with BIC (k = log(N))
fwd2 <- stepAIC(min.model,
                direction = 'forward', 
                scope = scp,
                k = log(nrow(eclsk1)))
fwd2$coefficients

### Do the results match up with best subset selection?
d1 <- names(fwd2$coefficients)[-1]
d2 <- names(bs3$Subsets[16, bs4$Subsets[16,] == TRUE])[-1]
d1 %in% d2
d2 %in% d1

### Backward selection with MASS
library(MASS)
min.model <- lm(C6R4MSCL ~ 1, data = eclsk1)
max.model <- lm(C6R4MSCL ~ ., data = eclsk1)
scp <- list(lower = min.model, upper = max.model)
bwd <- stepAIC(max.model, 
               direction = 'backward', 
               scope = scp)
bwd$coefficients

### Do the results match up with best subset selection?
d1 <- names(bwd$coefficients)[-1]
d2 <- names(bs4$Subsets[27, bs4$Subsets[27,] == TRUE])[-1]
d1 %in% d2
d2 %in% d1

### Backward selection with BIC (k = log(N))
bwd2 <- stepAIC(max.model,
                direction = 'backward', 
                scope = scp,
                k = log(nrow(eclsk1)))
bwd2$coefficients

### Do the results match up with best subset selection?
d1 <- names(fwd2$coefficients)[-1]
d2 <- names(bs3$Subsets[16, bs4$Subsets[16,] == TRUE])[-1]
d1 %in% d2
d2 %in% d1

###############################################################################
### Simulated example to investigate detection of quadratic term ##############
###############################################################################
N <- 200
set.seed(7915)
Xd2 <- rmvnorm(N, sigma = diag(8))
colnames(Xd2) <- paste0("X", 1:8)
head(Xd2)
Xd2 <- data.frame(Xd2)
Y <- .4*Xd2$X1 + .2*Xd2$X2 + -.6*Xd2$X3 + -.1*Xd2$X4 + Xd2$X5^2 + rnorm(N)

set.seed(1650)
lm2 <- lm(Y ~ ., data = Xd2)
summary(lm2)

lm3 <- lm(Y ~ . + I(X5^2), data = Xd2)
summary(lm3)

### Forward selection with AIC
min.mod <- lm(Y ~ 1, data = Xd2)
max.mod <- lm(Y ~ ., data = Xd2)
scp2 = list(lower = min.mod, upper = max.mod)
fwd3 <- stepAIC(min.mod,
                direction = 'forward', 
                scope = scp2)
fwd3$coefficients

### Forward selection with AIC and quadratics
max.mod2 <- lm(Y ~ . + I(X1^2) + I(X2^2) + I(X3^2) + I(X4^2) + 
                       I(X5^2) + I(X6^2) + I(X7^2) + I(X7^2), data = Xd2)
scp3 = list(lower = min.mod, upper = max.mod2)
fwd4 <- stepAIC(min.mod,
                direction = 'forward', 
                scope = scp3)
fwd4$coefficients


