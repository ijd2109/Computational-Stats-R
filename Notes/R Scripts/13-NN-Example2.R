# Data of housing values in Boston suburbs
library(MASS)
install.packages("mlbench")
library(mlbench)
data <- Boston
head(data)
dim(data)

# We want to predict the median value medv by the other variables
# First try good old regression model, but first splitting 
# the dataset into training and testing to avoid overfitting

# 75% training and 25% test 
# Select randomly 75% of the indeces to be training:
index <- sample(1:nrow(data),round(0.75*nrow(data)))
# Extract the training dataset from the main dataset
train <- data[index,]
# The rest are test data
test <- data[-index,]

# Predict with linear model using all predictors:
lm.fit <- lm(medv~., data=train)
summary(lm.fit)

# How well did we predict it?
# Try the results on the test data:
pr.lm <- predict(lm.fit,test)
cbind(test$medv, pr.lm)

# Mean squared prediction error based on the test dataset
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
MSE.lm

# Now neural network!
# For the neural network we will scale the data first
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

head(train_)

install.packages("neuralnet")
library(neuralnet)

# A fancy way to write 
medv~crim+zn+indus+chas+nox+rm + age + dis + rad + tax + ptratio + black + lstat
# because y ~ . is not accepted in neuralnet package

n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))


# We will use 2 hidden layers with configuration 13:5:3:1, meaning the input layer has 13 inputs (the 13 x variables), the two hidden layers have 5 and 3 neurons and the output layer has a single output

nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

# hidden defines a vector with number of neurons in each layer
# linear.output = TRUE corresponds to regression
# linear.output = FALSE corresponds to classification

# Fancy plot!
plot(nn)

# Predicting on the test dataset
pr.nn <- compute(nn,test_[,1:13])

# Prediction with neural network
pr.nn <- compute(nn,test_[,1:13])

# Taking only final output and rescaling it
pr.nn_ <- pr.nn$net.result*(max(data$medv)-min(data$medv))+min(data$medv)

# MS prediction error for neural network fit
MSE.nn <- sum((test$medv - pr.nn_)^2)/nrow(test_)

cbind(test$medv, pr.lm, pr.nn_)


print(paste(MSE.lm,MSE.nn))

# Almost half of the regression!!!!


# Visally examining the fit
plot(test$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)

points(test$medv,pr.lm,col='blue',pch=18,cex=0.7)

abline(0,1,lwd=2)

legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))
