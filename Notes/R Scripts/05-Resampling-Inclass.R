### Generate data from a cubic model with normal errors
set.seed(3249)
x <- runif(100, -3, 3)
y <- 2 + 2*x - 1*x^2 + .5*x^3 + rnorm(100, mean = 0, sd = 4)
dat <- data.frame(cbind(x, y))
plot(dat)
curve(expr = 2 + 2*x - 1*x^2 + .5*x^3, 
      from = -3, to = 3, n = 501, add = TRUE,
      lwd = 3, col = 4)

### Randomly select 1/2 of cases for a test set
set.seed(3487)
ind <- sample(1:100, 50, replace = FALSE)

train <- dat[ind,]
str(train)
test <- dat[-ind,]
str(test)


plot(train, pch = 19)
### Finer plotting
prx <- seq(-3, 3, by = .01)
newd <- data.frame(cbind(x = prx))

### Fit polynomial models on training data and evaluate error prediction error
### on both training and test sets
tr1 <- lm(y ~ poly(x, degree = 1, raw = TRUE), data = train)
# lines(x = train$x[xTrOrd], y = tr1$fitted.values[xTrOrd], col = 2, lwd = 3)
lines(x = prx, y = predict(tr1, newd), col = 2, lwd = 3)
(trError1 <- mean((tr1$fitted.values - train$y)^2))
(tstError1 <- mean((predict(tr1, newdata = test) - test$y)^2))

tr2 <- lm(y ~ poly(x, degree = 2, raw = TRUE), data = train)
# lines(x = train$x[xTrOrd], y = tr2$fitted.values[xTrOrd], col = 3, lwd = 3)
lines(x = prx, y = predict(tr2, newd), col = 3, lwd = 3)
(trError2 <- mean((tr2$fitted.values - train$y)^2))
(tstError2 <- mean((predict(tr2, newdata = test) - test$y)^2))

tr3 <- lm(y ~ poly(x, degree = 3, raw = TRUE), data = train)
# lines(x = train$x[xTrOrd], y = tr3$fitted.values[xTrOrd], col = 4, lwd = 3)
lines(x = prx, y = predict(tr3, newd), col = 4, lwd = 3)
(trError3 <- mean((tr3$fitted.values - train$y)^2))
(tstError3 <- mean((predict(tr3, newdata = test) - test$y)^2))

tr4 <- lm(y ~ poly(x, degree = 4, raw = TRUE), data = train)
# lines(x = train$x[xTrOrd], y = tr4$fitted.values[xTrOrd], col = 5, lwd = 3)
lines(x = prx, y = predict(tr4, newd), col = 5, lwd = 3)
(trError4 <- mean((tr4$fitted.values - train$y)^2))
(tstError4 <- mean((predict(tr4, newdata = test) - test$y)^2))

tr5 <- lm(y ~ poly(x, degree = 5, raw = TRUE), data = train)
# lines(x = train$x[xTrOrd], y = tr5$fitted.values[xTrOrd], col = 6, lwd = 3)
lines(x = prx, y = predict(tr5, newd), col = 6, lwd = 3)
(trError5 <- mean((tr5$fitted.values - train$y)^2))
(tstError5 <- mean((predict(tr5, newdata = test) - test$y)^2))

tr10 <- lm(y ~ poly(x, degree = 10, raw = TRUE), data = train)
# lines(x = train$x[xTrOrd], y = tr10$fitted.values[xTrOrd], col = "darkgray", lwd = 3)
lines(x = prx, y = predict(tr10, newd), col = "darkgray", lwd = 3)
(trError10 <- mean((tr10$fitted.values - train$y)^2))
(tstError10 <- mean((predict(tr10, newdata = test) - test$y)^2))

tr20 <- lm(y ~ poly(x, degree = 20, raw = TRUE), data = train)
# lines(x = train$x[xTrOrd], y = tr20$fitted.values[xTrOrd], col = 9, lwd = 3)
lines(x = prx, y = predict(tr20, newd), col = 9, lwd = 3)
(trError20 <- mean((tr20$fitted.values - train$y)^2))
(tstError20 <- mean((predict(tr20, newdata = test) - test$y)^2))

legend(x = "bottomright", legend = c(paste0("Polynomial Degree ", c(1, 2, 3, 4, 5, 10, 20))),
       col = c(2:6, "darkgray", 9), lwd = 3)

xTrOrd <- order(train$x)

### Training set prediction error
plot(train, pch = 19, ylim = c(-30, 18))
lines(x = prx, y = predict(tr20, newd), col = 9, lwd = 3)
for (i in 1:length(train$x[xTrOrd])) {
  segments(x0 = train$x[xTrOrd][i], y0 = tr20$fitted.values[xTrOrd][i],
           x1 = train$x[xTrOrd][i], y1 = train$y[xTrOrd][i],
           col = 2, lty = 1, lwd = 3)
}
points(train$x[xTrOrd], tr20$fitted.values[xTrOrd], pch = 19, col = 2, cex = .5)

### Add in the test set points with open circles
points(test)
xTstOrd <- order(test$x)
for (i in 1:length(train$x[xTstOrd])) {
  segments(x0 = test$x[xTstOrd][i], y0 = predict(tr20, newdata = test)[xTstOrd][i],
           x1 = test$x[xTstOrd][i], y1 = test$y[xTstOrd][i],
           col = "purple", lty = 1, lwd = 3)
}

### Function for mean squared prediction error
plotMSEs <- function(degrees = 1:10, trn = train, tst = test) {
  lmOut <- matrix(0, nrow = length(degrees), ncol = 2)
  for (i in degrees) {
    linmod <- lm(y ~ poly(x, degree = i, raw = TRUE), data = trn)
    lmOut[i,1] <- mean((linmod$fitted.values - trn$y)^2)
    lmOut[i,2] <- mean((predict(linmod, newdata = tst) - tst$y)^2)
  }
  colnames(lmOut) <- c("train", "test")
  lmOut
}

out <- plotMSEs(1:20)
plot(1:20, out[,1], type = "b", lwd = 3, lty = 2, ylim = c(0, 51), pch = 19,
     xlab = "Polynomial Degree", ylab = "Mean Squared Prediction Error")
points(1:20, out[,2], type = "b", lwd = 3, lty = 3, pch = 24)
axis(side = 1, at = 1:20)
legend(x = "topleft", legend = c("test set error", "training set error"), 
       lwd = 3, lty = 3:2, pch = c(24, 19))

### Generate 5 random train/test partitions
set.seed(3948) 
ind1 <- sample(rep(0:1, each = 50), replace = FALSE)
ind2 <- sample(rep(0:1, each = 50), replace = FALSE)
ind3 <- sample(rep(0:1, each = 50), replace = FALSE)
ind4 <- sample(rep(0:1, each = 50), replace = FALSE)
ind5 <- sample(rep(0:1, each = 50), replace = FALSE)

pm1 <- plotMSEs(1:20, trn = dat[ind1 == 1,], tst = dat[ind1 == 0,])
pm2 <- plotMSEs(1:20, trn = dat[ind2 == 1,], tst = dat[ind2 == 0,])
pm3 <- plotMSEs(1:20, trn = dat[ind3 == 1,], tst = dat[ind3 == 0,])
pm4 <- plotMSEs(1:20, trn = dat[ind4 == 1,], tst = dat[ind4 == 0,])
pm5 <- plotMSEs(1:20, trn = dat[ind5 == 1,], tst = dat[ind5 == 0,])

plot(1:20, pm1[,2], type = "b", col = rainbow(11)[2], lwd = 3,
     ylim = c(12, 40), xlab = "Polynomial Degree", 
     ylab = "Mean Squared Prediction Error")
axis(1, at = 1:20)
points(1:20, pm2[,2], type = "b", col = rainbow(11)[4], lwd = 3)
points(1:20, pm3[,2], type = "b", col = rainbow(11)[6], lwd = 3)
points(1:20, pm4[,2], type = "b", col = rainbow(11)[8], lwd = 3)
points(1:20, pm5[,2], type = "b", col = rainbow(11)[10], lwd = 3)


### LOOCV with package boot
install.packages("boot")
library(boot)

### LOOCV function for polynomial fits
looPoly <- function(deg, dat) {
  lm1 <- glm(y ~ poly(x, degree = deg, raw = TRUE), 
             family = "gaussian", data = dat)
  #note that k = n means we will do LOO by doing n regressions, one
  #for each row.
  #delta[1] is the cv error estimate.
  # and we are going to run this for all the different polynomial
  # degrees we want to try and n regressions for each one.
  out <- cv.glm(data = dat, glmfit = lm1, K = nrow(dat))$delta[1]
  out
}

looOut <- sapply(1:20, FUN = looPoly, dat = dat)
looOut

ts.plot(looOut, type = "b")


### k-fold CV
#note if you have a binomial regression to do, just change family
KFoldPoly <- function(deg, dat) {
  lm1 <- glm(y ~ poly(x, degree = deg, raw = TRUE), 
             family = "gaussian", data = dat)
  out <- cv.glm(data = dat, glmfit = lm1, K = 5)$delta[1]
  out
}

looOut <- sapply(1:20, FUN = KFoldPoly, dat = dat)
looOut
# time series plot:
ts.plot(looOut, type = "b")


# Exercise:
# Repeat the cross-validation using 9 -20*x +16*x^2 as the true polynomial
# Use sd = 10 in the normal error
# Obtain fit plots for degrees 1, 2, 3, 4, 5, and 10
# Obtain the MSE for polynomials with degrees 1, ..., 20
# Which one is the best fitting on the test dataset?
# Repeat with 80% training and 20% test datasets
set.seed(8321)
x <- runif(100, -3, 3)
y <- 9 -20*x +16*x^2 + rnorm(100, mean = 0, sd = 10)
dat <- data.frame(cbind(x, y))
plot(dat)
curve(expr = 9 -20*x +16*x^2, 
      from = -3, to = 3, n = 501, add = TRUE,
      lwd = 3, col = 4)

set.seed(1212)
ind <- sample(1:100, 80, replace = FALSE)

train <- dat[ind,]
str(train)
test <- dat[-ind,]
#str(test)

plotMSEs <- function(degrees = 1:10, trn = train, tst = test) {
  lmOut <- matrix(0, nrow = length(degrees), ncol = 2)
  for (i in degrees) {
    linmod <- lm(y ~ poly(x, degree = i, raw = TRUE), data = trn)
    lmOut[i,1] <- mean((linmod$fitted.values - trn$y)^2)
    lmOut[i,2] <- mean((predict(linmod, newdata = tst) - tst$y)^2)
  }
  colnames(lmOut) <- c("train", "test")
  lmOut
}

out <- plotMSEs(1:20)
plot(1:20, out[,1], type = "b", lwd = 3, lty = 2, pch = 19,
     xlab = "Polynomial Degree", ylab = "Mean Squared Prediction Error")
points(1:20, out[,2], type = "b", lwd = 3, lty = 3, pch = 24)
#above showed cubic
#now LOOCV:
looPoly <- function(deg, dat) {
  lm1 <- glm(y ~ poly(x, degree = deg, raw = TRUE), 
             family = "gaussian", data = dat)
  #note that k = n means we will do LOO by doing n regressions, one
  #for each row.
  #delta[1] is the cv error estimate.
  # and we are going to run this for all the different polynomial
  # degrees we want to try and n regressions for each one.
  out <- cv.glm(data = dat, glmfit = lm1, K = nrow(dat))$delta[1]
  out
}

looOut <- sapply(1:20, FUN = looPoly, dat = dat)
looOut

ts.plot(looOut, type = "b")
#now the result shows parabola

# Exercise:
# Repeat the above using sin(x) as the true curve
# Use sd = 0.3 in the normal error
# Obtain fit plots for degrees 1, 2, 3, 4, 5, and 10
# Obtain the MSE for polynomials with degrees 1, ..., 20
# Which one is the best fitting on the test dataset?
# Repeat with 80% trainin and 20% test datasets


