# LOAD PACKAGES
library(MASS) 
library(caTools) # for the split train.test
library(corrplot) # for correlation plot
library(boot)
library(dplyr)
library(class)
library(tree)
library(randomForest)
library(gbm)
library(Metrics)
attach(Boston) 


# TRAIN-TEST
set.seed(43)
r<-nrow(Boston)
target <- Boston[sample(r),]
train <- target[(1:round(0.5*r)), ]
test <- target[((round(0.5*r) + 1):r), ]

##########################################################################

# DECISION TREE
# Recursive Splitting
tree = tree(formula = medv ~ ., data = train)
# plot it
plot(tree)
text(tree ,pretty=0)
# summarise it 
summary(tree)
# Tree pruning
cv.boston=cv.tree(tree)
plot(cv.boston$size ,cv.boston$dev ,type='b') # visualise the best amount of nodes

prune.boston=prune.tree(tree.boston ,best=5) # after 5 the values don't improve
prune.boston
plot(prune.boston)
text(prune.boston ,pretty=0) # Final tree


yhat=predict(tree.boston ,newdata=test)
boston.test=test$medv
plot(yhat,boston.test)
abline(0 ,1)
(rmse_decisionTree = sqrt(mean((yhat-boston.test)^2)))

##########################################################################

# RANDOM_FOREST
random_for=randomForest(medv~., train, mtry=5, importance =TRUE )  

# Verify the predictions
y_pred = predict(random_for ,newdata = test)
plot(y_pred, test$medv)
abline(0 ,1)
(rmse_random = sqrt(mean((y_pred - test$medv)^2)))


##########################################################################

# BOOSTING 

boost_medv=gbm(medv~. - zn - chas - rad - indus ,data= train, distribution= "gaussian", 
               n.trees=10000, interaction.depth=2, 
               shrinkage = 0.02, verbose = F)
summary(boost_medv)

# Predict medv on the test set 
yhat.boost=predict(boost_medv,newdata=test, n.trees=5000)
sqrt(mean((yhat.boost - test$medv)^2))

