# Example from ISLR:
library(ISLR)
#note: ridge regression pacakge does not 
#automatically do casewise deletion so you have to do it yourself.

# Baseball data from the book package
head(Hitters)
# First cleand the data and remove NA values as otherwise the glmnet package will not run:
Hitters=na.omit(Hitters)
# Check there are no missing:
with(Hitters,sum(is.na(Salary)))

#  As the package glmnet does not use the model formula language, we are then require to give it:
# a matrix x of predictors
# and a response vector
# the "~.-1" reflects that we don't want to include the intercept, we could also say
# -"var name"
x = model.matrix(Salary~.-1,data=Hitters) 
# model.matrix function is very convenient to convert data frames to matrices and creates dummy variables
y = Hitters$Salary

# The ridge-regression model is fitted by calling the glmnet function with `alpha=0` 
# (When alpha equals 1 you fit a lasso model).
# For alphas in between 0 and 1, you get what's called elastic net models, which are in between ridge and lasso.

library(glmnet)
#alpha =0 is ridge, =1 is lasso and .5 is something in between, you can do 0 to 1
fit.ridge=glmnet(x,y,alpha=0)
plot(fit.ridge,xvar="lambda",label=TRUE)

# It makes a plot as a function of log of lambda, and is plotting the coefficients. 
# glmnet develops a whole part of models on a grid of values of lambda (about 100 values of lambda). 

# Ridge regression gives a whole path of model and we need to pick one. 
# glmnet's got a built-in function, called CV.glmnet that will do cross validation 

cv.ridge=cv.glmnet(x,y,alpha=0)
plot(cv.ridge)

# For large lambda, the mean squared error is very high, and the coefficients are restricted to be too small
# and then at some point, it kind of levels off. This seems to indicate that the full model is doing a good job. 

# There's two vertical lines.
# One is at the minimum,
# and the other vertical line is within one standard error of the minimum. 
# The second line is a slightly more restricted model that does almost as well as the minimum

# At the top of the plot, you actually see how many non-zero variables coefficients are in the model. 
# There's all 20 variables in the model (19 variables plus the intercept) and no coefficient is zero. 
#We choose the model that minimized the mse

# Lasso:
fit.lasso=glmnet(x,y,alpha=1)
plot(fit.lasso,xvar="lambda",label=TRUE)

# The plot has various choices (see help file). The deviance shows the percentage of deviance explained
# (equivalent to r squared in case of regression) 

plot(fit.lasso,xvar="dev",label=TRUE)

# A lot of the r squared was explained for quite heavily shrunk coefficients. 
# And towards the end, with a relatively small increase in r squared from between 0.4 and 0.5, 
# coefficients grow very large. This may be an indication that the end of the path is overfitting. 

cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)

# To obtain the lambda with the smalles MSPE
cv.lasso$lambda.min
# cross-check with the plot
log(cv.lasso$lambda.min)

# Coefficients:
# There's a coefficient function extractor that works on a cross validation object 
# and pick the coefficient vector corresponding to the best model, 
coef(cv.lasso)

# ECLS-K Example from last time:
### Load the eclsk_c data by opening a browser window
load(file.choose())

### Or call the path directly (fill in your path)
load(file = file.choose())
dim(eclsk_c)

# Or just double-click on file name

### Remove outcomes and selection
eclsk1 <- eclsk_c[, !names(eclsk_c) %in% c("S1_ID", "CHILDID", "C5R4RSCL",
                            "C6R4RSCL", "C5R4MSCL", "C6R4MSCL", "F5SPECS")]

### Standardize the predictors by dividing each by its standard deviation.
sds <- apply(eclsk1, 2, sd)
matsds <- matrix(rep(sds, times = nrow(eclsk1)), nrow(eclsk1), byrow = TRUE)
eclsk1 <- eclsk1/matsds

### Put selection and math outcome at end of data frame
eclsk1 <- cbind(eclsk1, F5SPECS = eclsk_c$F5SPECS, 
                C6R4MSCL = eclsk_c$C6R4MSCL)
head(eclsk1)

# OLS regression of outcome on other variables
lm1 <- lm(C6R4MSCL ~ ., data = eclsk1)
summary(lm1)

# If you know what lambda to use the MASS package has a function to fit ridge regression
library(MASS)

# User0defined function to get coefficients from ridge regression with given lambda
# This function is used only for the plots
fridge <- function(lam) 
  {
    lmr <- lm.ridge(C6R4MSCL ~ ., data = eclsk1, lambda = lam)
    out <- coef(lmr)
    return(out) 
  }

# Plot of betas for different lambda values
# Range of lambda values
dom <- c(10^c(-1:6))
outs <- sapply(dom, fridge)[-1,]
range(outs)
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(log(dom), outs[1,], type = "l", lty = 1, col = 1, ylim = range(outs), 
     xlab = "log(lambda)", ylab = "Ridge Betas", main = "Ridge regression 
     coefficients for ECLSK data\nexample for different values of lambda")
for (i in 1:dim(outs)[1]) {points(log(dom), outs[i,], type = "l", 
                                  col = rainbow(dim(outs)[1])[i])}

# Plot of L2 norm
l2 <- function(vec) {vec%*%vec}
plot(x = log(dom), apply(outs, 2, l2), type = "l", xlab = "log(lambda)", 
     ylab = "l2 Norm of Ridge Betas")



### Ridge example with ECLSK
summary(lm1)

# We will use the glmnet package in order to perform ridge regression and the lasso.
library(glmnet)

set.seed(2911)
# cv.glment does k-fold cross-validation for glmnet to find optimal lambda
cvridge <- cv.glmnet(x = as.matrix(eclsk1[,1:35]), y = eclsk1[,36], family = "gaussian", alpha = 0)
# x is the input matrix, of dimension nobs by nvars
# y is the  response variable
# alpha = 1 is the lasso penalty, and alpha=0 the ridge penalty (1 is the default)
coef(cvridge)
cvridge$lambda
plot(cvridge)

# Compare coefficients
coefs1 <- cbind(coef(lm1), as.numeric(coef(cvridge)))
colnames(coefs1) <- c("OLS", "Ridge")
coefs1


# Scale equivariance of OLS.
summary(lm(C6R4MSCL ~ MIRT + RIRT, data = eclsk1))
MIRT2 <- eclsk1$MIRT/100
summary(lm(C6R4MSCL ~ MIRT2 + RIRT, data = eclsk1))


# We now repeat ECLSK data with lasso
# Function to get coefficients from lasso regression with given lambda
library(glmnet)
flasso <- function(lam) {
  lml <- glmnet(x = as.matrix(eclsk1[,1:35]), y = eclsk1[,36], 
                family = "gaussian", alpha = 1, lambda = lam)
  out <- coef(lml)
  out }
dom2 <- c(10^seq(-2, 1.5, .5))
outs <- sapply(dom2, flasso)
outs2 <- cbind(as.numeric(outs[[1]][-1]), as.numeric(outs[[2]][-1]), 
               as.numeric(outs[[3]][-1]), as.numeric(outs[[4]][-1]),
               as.numeric(outs[[5]][-1]), as.numeric(outs[[6]][-1]), 
               as.numeric(outs[[7]][-1]), as.numeric(outs[[8]][-1]))
range(outs2)
par(mar = c(6.1, 4.1, 4.1, 2.1))
plot(log(dom2), outs2[1,], type = "l", lty = 1, col = 1, ylim = range(outs2), 
     xlab = "log(lambda)", ylab = "Lasso Betas", main =  
     "Lasso regression coefficients for ECLSK data\nexample for different values of lambda")
for (i in 1:dim(outs2)[1]) {points(log(dom2), outs2[i,], type = "l", 
                            lwd = 2, col = rainbow(dim(outs2)[1])[i])}

# L1 norm
l1 <- function(vec) {sum(abs(vec))}
plot(x = log(dom2), apply(outs2, 2, l1), type = "l", xlab = "log(lambda)", 
     ylab = "l1 Norm of Lasso Betas", lwd = 3)


# Lasso example with ECLSK
summary(lm1)
# Use alpha = 1 for lasso
set.seed(1080)
cvlasso <- cv.glmnet(x = as.matrix(eclsk1[,1:35]), y = eclsk1[,36], 
                 family = "gaussian", alpha = 1)
coef(cvlasso)

coefs2 <- cbind(coef(lm1), as.numeric(coef(cvridge)), as.numeric(coef(cvlasso)))
colnames(coefs2) <- c("OLS", "Ridge", "Lasso")
coefs2
round(coefs2, 2)


### Principal components example with ECLSK data
### Raw plot
plot(eclsk1$RIRT, eclsk1$MIRT, xlab = "Reading Pretest Score (RIRT)",
     ylab = "Math Pretest Score (MIRT)", main = "Raw Data Values")
### Centered plot
plot(eclsk1$RIRT - mean(eclsk1$RIRT), eclsk1$MIRT - mean(eclsk1$MIRT), 
     xlab = "Mean-Centered Reading Pretest Score (RIRT)",
     ylab = "Mean-Centered Math Pretest Score (MIRT)", main = "Mean-Centered Data Values")

### Centered and scaled
plot(MIRT ~ RIRT, data = scale(eclsk1),#[sample(1:7362, size = 1000, replace = FALSE),],
     xlab = "Mean-Centered and Scaled Reading Pretest Score (RIRT)",
     ylab = "Mean-Centered and Scaled Math Pretest Score (MIRT)", 
     main = "Mean-Centered and Scaled Data Values")
### Principal components
ec.pca <- prcomp( ~ MIRT + RIRT, data = eclsk1,
                  center = TRUE, scale. = TRUE)
ec.pca

### With PCA fit
plot(MIRT ~ RIRT, data = scale(eclsk1),#[sample(1:7362, size = 1000, replace = FALSE),],
     xlab = "Mean-Centered and Scaled Reading Pretest Score (RIRT)",
     ylab = "Mean-Centered and Scaled Math Pretest Score (MIRT)", 
     main = "Principal Components")
abline(a = 0, b = 1, col = 3, lwd = 3)
abline(a = 0, b = -1, col = 4, lwd = 3, lty = 2)


### Run PCR with package pls.
library(pls)
pcr.fit <- pcr(C6R4MSCL ~ ., data = eclsk1, scale = TRUE, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
