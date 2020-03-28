# We use the Boston data set from the MASS package
# Goal: predict the median house value (mdev) based on lstat (% of lower status of the population).

# Load the data
data("Boston", package = "MASS")
head(Boston)
dim(Boston)

# Plot the variables of interest:
plot(medv ~ lstat, data = Boston,  xlab = "Percent of lower status of the population", 
                                   ylab = "Median house value ($1000s)")
# The relationship is clearly non-linear

# We randomly split the data into training set (80%) and test set (20%).
n = nrow(Boston)
set.seed(577)
training = sample(1:n, 0.8*n)
train.data = Boston[training, ]
test.data = Boston[-training, ]

# Linear regression
model1 = lm(medv ~ lstat, data = train.data)
# Make predictions
predictions = predict(model1, test.data)
# Model performance
(RMSE.1 = sqrt(mean((predictions-test.data$medv)^2)))

# Visualize the fit
plot(medv ~ lstat, data = train.data, xlab = "Percent of lower status of the population", 
                                     ylab = "Median house value ($1000s)")
abline(model1, col = "red")

# Polynomial regression
summary(model2 <- lm(medv ~ poly(lstat, 2, raw = TRUE), data = train.data))
summary(model3 <- lm(medv ~ poly(lstat, 3, raw = TRUE), data = train.data))
summary(model4 <- lm(medv ~ poly(lstat, 4, raw = TRUE), data = train.data))
summary(model5 <- lm(medv ~ poly(lstat, 5, raw = TRUE), data = train.data))
summary(model6 <- lm(medv ~ poly(lstat, 6, raw = TRUE), data = train.data))

# Compare models
AIC(model1, model2, model3, model4, model5, model6)
BIC(model1, model2, model3, model4, model5, model6)
#Note anova is sequential, so we are only testing sig of model6 really.
anova(model1, model2, model3, model4, model5, model6)
# We can conclude that polynomial terms beyond the fifth order are not needed

# Make predictions
predictions = predict(model5, test.data)
# Model performance
(RMSE.5 = sqrt(mean((predictions-test.data$medv)^2)))
# Compare to linear prediction error:
RMSE.1
# We achieved much better predictions!

# Plot polynomial fits
plot(medv ~ lstat, data = train.data, xlab = "Percent of lower status of the population", 
                                      ylab = "Median house value ($1000s)")
lines(sort(train.data$lstat), predict(model1, train.data[order(train.data$lstat),]), col = rainbow(5)[1])
lines(sort(train.data$lstat), predict(model2, train.data[order(train.data$lstat),]), col = rainbow(5)[2])
lines(sort(train.data$lstat), predict(model3, train.data[order(train.data$lstat),]), col = rainbow(5)[3])
lines(sort(train.data$lstat), predict(model4, train.data[order(train.data$lstat),]), col = rainbow(5)[4])
lines(sort(train.data$lstat), predict(model5, train.data[order(train.data$lstat),]), col = rainbow(5)[5])
legend(x = "topright", col = rainbow(5), lwd = 2, legend = c("Linear", "Quadratic", "Cubic", "Degree 4", "Degree 5"))


# Step functions
# Creating the new dummy variable
table(cut(train.data$lstat, breaks=c(0,10,20,30,40)))
lstat.step = cut(train.data$lstat, breaks=c(0,10,20,30,40))
train.data = cbind(train.data, lstat.step)
lstat.step = cut(test.data$lstat, breaks=c(0,10,20,30,40))
test.data = cbind(test.data, lstat.step)

fit_step = lm(medv ~ lstat.step, data = train.data)
coef(summary(fit_step))

preds = predict(fit_step, newdata = test.data)
(RMSE.step = sqrt(mean((preds-test.data$medv)^2)))
# Not a very good fit!

# Plot step function
plot(medv ~ lstat, data = train.data, xlab = "Percent of lower status of the population", 
     ylab = "Median house value ($1000s)")
segments(0, coef(fit_step)[1], 10, coef(fit_step)[1], col = "red")
segments(10, coef(fit_step)[1]+coef(fit_step)[2], 20, coef(fit_step)[1]+coef(fit_step)[2], col = "red")
segments(20, coef(fit_step)[1]+coef(fit_step)[3], 30, coef(fit_step)[1]+coef(fit_step)[3], col = "red")
segments(30, coef(fit_step)[1]+coef(fit_step)[4], 40, coef(fit_step)[1]+coef(fit_step)[4], col = "red")

# Piecewise polynomial
# Let's fit different cubic polynomials for lstat < 10 and > 10
# Note the cutoff point was chosen very arbitrary

# How many observations in each region:
table(cut(train.data$lstat, breaks=c(0,10, 40)))

# Create new variables for prediction and plotting
lstat.10 = cut(train.data$lstat, breaks= c(0,10, 40))
train.data = cbind(train.data, lstat.10)
lstat.10 = cut(test.data$lstat, breaks=c(0,10, 40))
test.data = cbind(test.data, lstat.10)

# We use a model with interaction effects which results in fitting different polynomials in different regions:
summary(model.10 <- lm(medv ~ poly(lstat, 3, raw = TRUE)*lstat.10, data = train.data))
preds = predict(model.10, newdata = test.data)
(RMSE.10 = sqrt(mean((preds-test.data$medv)^2)))
# Not so good :(

# Plot the piecewise cubic fit
plot(medv ~ lstat, data = train.data, xlab = "Percent of lower status of the population", 
     ylab = "Median house value ($1000s)")
lines(sort(train.data$lstat)[1:166], predict(model.10, train.data[order(train.data$lstat)[1:166],]), col = "red")
lines(sort(train.data$lstat)[167:404], predict(model.10, train.data[order(train.data$lstat)[167:404],]), col = "red")
abline(v = 10, lty = 2)


# Spline regression
# Defining the knots
knots <- quantile(train.data$lstat, p = c(0.25, 0.5, 0.75))

# We use the package splines
# the function bs generates b-spline basis for polynomial splines
install.packages("splines")

library(splines)
model.spline <- lm (medv ~ bs(lstat, knots = knots), data = train.data)

# Make predictions
predictions <- predict(model.spline, test.data)
# Model performance
(RMSE.spline = sqrt(mean((predictions -test.data$medv)^2)))

# Plot
plot(medv ~ lstat, data = train.data, xlab = "Percent of lower status of the population", 
     ylab = "Median house value ($1000s)")
lines(sort(train.data$lstat), predict(model.spline, train.data[order(train.data$lstat),]), col = "red")



# Smoothing splines
fit.smooth = smooth.spline(train.data$lstat, train.data$medv)
plot(medv ~ lstat, data = train.data, xlab = "Percent of lower status of the population", 
     ylab = "Median house value ($1000s)")
lines(fit.smooth, col = "blue")


# Local regression:

loessMod10 = loess(medv ~ lstat, data = train.data, span=0.10)
loessMod25 = loess(medv ~ lstat, data = train.data, span=0.25)
loessMod50 = loess(medv ~ lstat, data = train.data, span=0.50)
smoothed10 <- predict(loessMod10) 
smoothed25 <- predict(loessMod25) 
smoothed50 <- predict(loessMod50) 

plot(medv ~ lstat, data = train.data, xlab = "Percent of lower status of the population", 
     ylab = "Median house value ($1000s)")
lines(sort(train.data$lstat), smoothed10[order(train.data$lstat)], col="red")
lines(sort(train.data$lstat), smoothed25[order(train.data$lstat)], col="green")
lines(sort(train.data$lstat), smoothed50[order(train.data$lstat)], col="blue")

# Prediction errors
smoothed10 <- predict(loessMod10, test.data) 
smoothed25 <- predict(loessMod25, test.data) 
smoothed50 <- predict(loessMod50, test.data) 

(RMSE.s10 = sqrt(mean((smoothed10 - test.data$medv)^2)))
(RMSE.s25 = sqrt(mean((smoothed25 - test.data$medv)^2)))
(RMSE.s50 = sqrt(mean((smoothed50 - test.data$medv)^2)))

# Generalized additive models:
#for generalizing to multiple X variables.
install.packages("mgcv")
library(mgcv)
model.gam <- gam(medv ~ bs(lstat), data = train.data)
# Make predictions
predictions <- predict(model.gam,test.data)

(RMSE.gam = sqrt(mean((predictions - test.data$medv)^2)))
full.model.gam <- gam(medv ~ ., data = train.data)
