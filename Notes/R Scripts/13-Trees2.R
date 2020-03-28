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

tabulate(kyphosis$Kyphosis)
tabulate(kyphosis[kyphosis$Start >= 8.5,]$Kyphosis)
tabulate(kyphosis[kyphosis$Start < 8.5,]$Kyphosis)

# optional parameters for controlling tree growth: control=rpart.control(minsplit=30, cp=0.001) 
# requires that the minimum number of observations in a node be 30 before attempting a split 
# and that a split must decrease the overall lack of fit by a factor of 0.001 (cost complexity factor) 
# before being attempted. 

# Missclassification error
conf.matrix <- table(kyphosis$Kyphosis, predict(fit,type="class"))
conf.matrix
1-sum(diag(conf.matrix))/nrow(kyphosis)

# Alternatively:
printcp(fit)
# Multiply root node error by leaves rel error:
0.20988*0.76471

# Pruning
fitp <- prune(fit, cp = 0.019608)
printcp(fitp)
plot(fitp) #plot smaller rpart object
text(fitp, use.n=TRUE, all=TRUE, cex=.8)
# Error rate:
0.20988*0.82353

# Regression tree example:
# grow tree
fit <- rpart(Mileage~Price + Country + Reliability + Type,
             method="anova", data=cu.summary)

printcp(fit) # display the results
plot(fit) # visualize results
text(fit, pretty = 0) #to get better text output
summary(fit) # detailed summary of splits



# plot tree
plot(fit, uniform=TRUE,
     main="Regression Tree for Mileage ")
text(fit, use.n=TRUE, all=TRUE, cex=.6, pretty =0)


# Examples from the textbook:
# Fitting Classification Trees
install.packages("tree")
library(tree) #only vailabel on 3.6 on WINDOWS
library(ISLR)

# Data:
attach(Carseats)
head(Carseats)

# Create dichotomous variable for prediction:
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)

# Growing the tree:
tree.carseats=tree(High~.-Sales,Carseats)
summary(tree.carseats)

# Plot the tree
plot(tree.carseats)
text(tree.carseats,pretty=0, cex = 0.7)
# pretty = 0 includes names of variables rather than just letter

tree.carseats
# prints output corresponding to each branch of the tree. 
# displays the split criterion, the number of observations in that branch, 
# the deviance, the overall prediction # for the branch (Yes or No), 
# and the fraction of observations in that branch that take on values of Yes and No.

# Cross-validation:
set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
# Error rate:
1-(86+57)/200

# Pruning:
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
# We ask for misclassification error to decide the pruning (default is deviance)
names(cv.carseats)
# The cv.tree() function reports the number of terminal nodes of each tree considered (size) 
# as well as the corresponding error rate and the value of the
# cost-complexity parameter used (k, which corresponds to alpha from lecture notes).

cv.carseats
# Note that, despite the name, dev corresponds to the cross-validation error rate in this instance. 
# The tree with 8 terminal nodes results in the lowest cross-validation error rate, with 50 cross-validation errors

# Plot the error rate
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")

# We now apply the prune.misclass() function in order to prune the tree to obtain the nine-node tree.

prune.carseats=prune.misclass(tree.carseats,best=8)
plot(prune.carseats)
text(prune.carseats,pretty=0)

# How well does it perform on the test data set?
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
1-(94+60)/200

# Larger tree
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
1-(86+62)/200
# Has lower accuracy!


# Regression tree:
library(MASS)
set.seed(1)

# Boston dataset split into training and test
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train) #automatically does regression b/c continuous response variable

summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)

# Pruning:
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
# Most complex tree is selected

# We can prune nevertheless:
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1) #add a 45 degree line
# RMSPE
sqrt(mean((yhat-boston.test)^2))
# Recall with nonlinear regression we achieved only around 5.5 RMSE!

# Bagging and Random Forests
install.packages("randomForest")
library(randomForest)

# Bagging:
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston, subset = train, mtry = 13, importance=TRUE)
#  The argument mtry=13 indicates that all 13 predictors should be considered
# for each split of the tree-in other words, that bagging should be done.
bag.boston

# Predictions
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
sqrt(mean((yhat.bag-boston.test)^2))
# RMSE is much smaller now!!!

# ntree controls the number of trees
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
sqrt(mean((yhat.bag-boston.test)^2))

# Random forest:
# Growing a random forest proceeds in exactly the same way, except that
# we use a smaller value of the mtry argument. By default, randomForest()
# uses p/3 variables when building a random forest of regression trees, and
# sqrt(p) variables when building a random forest of classification trees. 
# Here we use mtry = 6.

set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
sqrt(mean((yhat.rf-boston.test)^2))
# Even smaller RMSE!

importance(rf.boston)
varImpPlot(rf.boston)
