# We use the Boston data set from the MASS package
# Goal: predict the median house value (mdev) based on lstat (% of lower status of the population).

# Load the data
data("Boston", package = "MASS")
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


summary(model5 <- lm(medv ~ poly(lstat, 5, raw = TRUE), data = train.data))


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
lines(sort(train.data$lstat), predict(model5, train.data[order(train.data$lstat),]), col = rainbow(5)[5])


# Step functions
# Creating the new variable
table(cut(train.data$lstat, breaks=c(0,10,20,30,40)))
lstat.step = cut(train.data$lstat, breaks=c(0,10,20,30,40))
train.data = cbind(train.data, lstat.step)
lstat.step = cut(test.data$lstat, breaks=c(0,10,20,30,40))
test.data = cbind(test.data, lstat.step)

fit_step = lm(medv ~ lstat.step, data = train.data)
coef(summary(fit_step))

preds = predict(fit_step, newdata = test.data)
(RMSE.step = sqrt(mean((preds-test.data$medv)^2)))
RMSE.1

# Not a very good fit!

# Plot step function
plot(medv ~ lstat, data = train.data, xlab = "Percent of lower status of the population", 
     ylab = "Median house value ($1000s)")
segments(0, coef(fit_step)[1], 10, coef(fit_step)[1], col = "red")
segments(10, coef(fit_step)[1]+coef(fit_step)[2], 20, coef(fit_step)[1]+coef(fit_step)[2], col = "red")
segments(20, coef(fit_step)[1]+coef(fit_step)[3], 30, coef(fit_step)[1]+coef(fit_step)[3], col = "red")
segments(30, coef(fit_step)[1]+coef(fit_step)[4], 40, coef(fit_step)[1]+coef(fit_step)[4], col = "red")

library(rpart)
tree <- rpart(medv ~ lstat, data= train.data)
summary(tree)
s <- seq(0, 40, by =.1)

lines(s, predict(tree, data.frame(lstat = s)), col = "blue")
(RMSE.tree = sqrt(mean((predict(tree, test.data)-test.data$medv)^2)))
RMSE.5

tree.7 <- rpart(medv ~ lstat, data= train.data, control=rpart.control(minsplit = 30, cp = 0.00001))
plot(medv ~ lstat, data = train.data, xlab = "Percent of lower status of the population", 
     ylab = "Median house value ($1000s)")
lines(s, predict(tree.7, data.frame(lstat = s)), col = "green")
(RMSE.tree.7 = sqrt(mean((predict(tree.7, test.data)-test.data$medv)^2)))
RMSE.5


# Classification tree with rpart
library(rpart)

# We use the data frame kyphosis to predict a type of deformation (kyphosis) after surgery, 
# from age in months (Age), number of vertebrae involved (Number), and the highest vertebrae operated (Start).

# grow tree
fit <- rpart(Kyphosis ~ Age + Number + Start,
             method="class", data=kyphosis)

fit # display the results 
plot(fit, uniform=TRUE,
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)

# optional parameters for controlling tree growth: control=rpart.control(minsplit=30, cp=0.001) 
# requires that the minimum number of observations in a node be 30 before attempting a split 
# and that a split must decrease the overall lack of fit by a factor of 0.001 (cost complexity factor) 
# before being attempted. 


# Regression tree example:
# grow tree
fit <- rpart(Mileage~Price + Country + Reliability + Type,
             method="anova", data=cu.summary)

printcp(fit) # display the results
plot(fit) # visualize results
summary(fit) # detailed summary of splits



# plot tree
plot(fit, uniform=TRUE,
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.6)
