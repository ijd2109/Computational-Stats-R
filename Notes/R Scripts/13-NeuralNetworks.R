# Artificial Neural Networks

# Example 1: Create an ANN to perform square rooting
install.packages("neuralnet")
library(neuralnet)  # For Neural Network

# Generate 50 random numbers uniformly distributed between 0 and 100
# and store them as a dataframe
traininginput <-  as.data.frame(runif(50, min = 0, max = 100))
trainingoutput <- sqrt(traininginput)

# Column bind the data into one variable
trainingdata <- cbind(traininginput,trainingoutput)
colnames(trainingdata) <- c("Input","Output")
trainingdata

# Train the neural network
# Going to have 10 hidden layers
# Threshold is a numeric value specifying the threshold for the partial
# derivatives of the error function as stopping criteria.
net.sqrt <- neuralnet(Output ~ Input, trainingdata, hidden = 10, threshold = 0.01)
print(net.sqrt)

# Plot the neural network
plot(net.sqrt)

# Test the neural network on some new data
testdata <- as.data.frame((1:15)^2) # generate some squared numbers
net.results <- compute(net.sqrt, testdata) # run them through the neural network

# Let's see what properties net.sqrt has
ls(net.results)

# Let's see the results
print(net.results$net.result)
# Notice the algorithm is very sensitive to the range of data!

# Let's display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata), as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)


# Example 2: Create a ANN for prediction

# We give a brief example of regression with neural networks and comparison with
# multivariate linear regression. The data set is housing data for 506 census tracts of
# Boston from the 1970 census. The goal is to predict median value of owner-occupied homes.

# Load the data and inspect the range (which is 1 - 50)
install.packages("mlbench")
library(mlbench)
data(BostonHousing)
head(BostonHousing)
summary(BostonHousing$medv)

# Build the multiple linear regression model
lm.fit <- lm(medv ~ ., data = BostonHousing)
lm.predict <- predict(lm.fit)

# Calculate the MSE and plot
mean((lm.predict - BostonHousing$medv)^2) # MSE = 21.89483
par(mfrow = c(2,1))
plot(BostonHousing$medv, lm.predict, main = "Linear Regression Predictions vs Actual (MSE = 21.9)", 
     xlab = "Actual", ylab = "Predictions", pch = 19, col = "brown")

# Build the feed-forward ANN (w/ one hidden layer)
library(nnet)        # For Neural Network
nnet.fit <- nnet(medv/50 ~ ., data = BostonHousing, size = 2) # scale inputs: divide by 50 to get 0-1 range
plot(nnet.fit)
nnet.predict <- predict(nnet.fit)*50 # multiply 50 to restore original scale
# Try the above with neuralnet
# 
# library(neuralnet)
# n <- names(BostonHousing)
# f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
# nn <- neuralnet(f,data=BostonHousing,hidden=c(5,3),linear.output=T)

# hidden defines a vector with number of neurons in each layer
# linear.output = TRUE corresponds to regression
# linear.output = FALSE corresponds to classification

# Fancy plot!
plot(nn)

# Calculate the MSE and plot 
mean((nnet.predict - BostonHousing$medv)^2) 
plot(BostonHousing$medv, nnet.predict, main = "Artificial Neural Network Predictions vs Actual",
     xlab = "Actual", ylab = "Predictions", pch = 19, col = "blue")

# Next, we use the function train() from the package caret to optimize the ANN 
# hyperparameters decay and size, Also, caret performs resampling to give a better 
# estimate of the error. We scale linear regression by the same value, so the error 
# statistics are directly comparable.

library(mlbench)
data(BostonHousing)

install.packages("caret")
library(caret)

# Optimize the ANN hyperpameters and print the results
mygrid <- expand.grid(.decay = c(0.5, 0.1), .size = c(4, 5, 6))
nnet.fit2 <- train(medv/50 ~ ., data = BostonHousing, method = "nnet", maxit = 1000, 
                 tuneGrid = mygrid, trace = FALSE)
print(nnet.fit2)

# Scale the linear regression and print the results
lm.fit2 <- train(medv/50 ~ ., data = BostonHousing, method = "lm") 
print(lm.fit2)


# Example 3: Create a ANN for classification
# Predict the identification of Iris plant Species on the basis of plant 
# attribute measurements: Sepal.Length, Sepal.Width, Petal.Length, Petal.Width

# Package for Neural Networks using the Stuttgart Neural Network Simulator
install.packages("RSNNS")
library(RSNNS)

# Load and store the 'iris' data
data(iris)

# Generate a sample from the 'iris' data set
irisSample <- iris[sample(1:nrow(iris), length(1:nrow(iris))), 1:ncol(iris)]
irisSample
# Predictors:
irisValues <- irisSample[, 1:4] #seperate from the y variable.
head(irisValues)
# Response
irisTargets <- irisSample[, 5] #partition out the y variable.
head(irisTargets)

# Generate a binary matrix from an integer-valued input vector representing class labels
# That is, the design matrix with dummy variables
irisDecTargets <- decodeClassLabels(irisTargets)
head(irisDecTargets)

# Split the data into the training and testing set, and then normalize
# Function splitForTrainingAndTest is from RSNNS package
# 15% test and 85% training
irisSample <- splitForTrainingAndTest(irisValues, irisDecTargets, ratio = 0.15)
irisSample <- normTrainingAndTestSet(irisSample)

# Train the Neural Network (Multi-Layer Perceptron)
# size is the number of units in the hidden layer
# learnFuncParams is usually a value between 0.1 and 1. It specifies the gradient descent step width
nn3 <- mlp(irisSample$inputsTrain, irisSample$targetsTrain, size = 2, learnFuncParams = 0.1, maxit = 100, 
	inputsTest = irisSample$inputsTest, targetsTest = irisSample$targetsTest)
print(nn3)

# Predict using the testing data
testPred6 <- predict(nn3, irisSample$inputsTest)
testPred6
# Note these are the probabilities of each classification

# Calculate the Confusion Matrices for the Training and Testing Sets
confusionMatrix(irisSample$targetsTrain, fitted.values(nn3))
confusionMatrix(irisSample$targetsTest, testPred6)

# Calculate the Weights of the Newly Trained Network
weightMatrix(nn3)

# Plot the Iterative Error of both training (black) and test (red) error
# This shows how the Number of Iterations Affects the Weighted SSE
plotIterativeError(nn3, main = "# of Iterations vs. Weighted SSE")
legend(80, 80, legend = c("Test Set", "Training Set"), col = c("red", "black"), pch = 17)

# See how changing the Learning Rate Affects the Average Test Error
err <- vector(mode = "numeric", length = 10)
learnRate = seq(0.1, 1, length.out = 10)
for (i in 1:10){
	fit <- mlp(irisSample$inputsTrain, irisSample$targetsTrain, size = 2, learnFuncParams = learnRate[i], maxit = 50, 
	  inputsTest = irisSample$inputsTest, targetsTest = irisSample$targetsTest)
	err[i] <- mean(fit$IterativeTestError)
}

# Plot the Effect of Learning Rate vs. Average Iterative Test Error
plot(learnRate, err, xlab = "Learning Rate", ylab = "Average Iterative Test Error",
	main = "Learning Rate vs. Average Test Error", type = "l", col = "brown")