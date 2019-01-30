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
library(pROC)


# Create dummy variable 
c_median = median(Boston$crim)
Bost = Boston
Bost['cr01'] = ifelse(crim > c_median, 1, 0)
Bost$crim <- NULL
attach(Bost)


# Train/Test
set.seed(43)
r<-nrow(Boston)
target <- Boston[sample(r),]
train <- target[(1:round(0.5*r)), ]
test <- target[((round(0.5*r) + 1):r), ]

#######################################
# KNN
set.seed(43)
# Randomise
Boston_random = Bost[sample(nrow(Boston)),]

# Standardise
standardized.X= scale(Boston_random[,-14])
test = 1:250
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Boston_random$cr01[-test]
test.Y = Boston_random$cr01[test]

# k = 1
knn.pred=knn(train.X,test.X,train.Y,k=1) 
mean(test.Y==knn.pred)
table(knn.pred,test.Y)

ROC = roc(knn.pred, test.Y)
plot(ROC, col = 'red')
auc(ROC)


################################################

# CLASSIFICATION TREES

Crime =ifelse(cr01 == 1,"Above","Below")
B = data.frame(Bost, Crime)
B$cr01 <- NULL


set.seed(2)
train = sample(1:(nrow(B)/2))
test =B[-train , -14]
crime.test = B$Crime[-train]
cr_tree =tree(Crime ~. , B, subset=train)

# Recursive Tree
summary(cr_tree)
plot(cr_tree )
text(cr_tree ,pretty = 0)

# verification on the test data
tree.pred = predict(cr_tree, test, type="class")
table( tree.pred , crime.test)
mean(crime.test == tree.pred)


# determination of the best value of tree complexity ( cv in function of alpha ) in order to reduce the variance and prevent overfitting

set.seed(3)
cv_tree =cv.tree(cr_tree , FUN = prune.misclass ) 
# classification error rate to guide the cross-validation and pruning process, 
# rather than the default for the cv.tree() function, which is deviance
plot(cv_tree)

# prune.misclass() function in order to prune the tree to obtain the six-node tree
cr_prune = prune.misclass(cr_tree,best=6)
plot(cr_prune)
text(cr_prune,pretty=0)

# application of the model to the test data to evaluate the performance

tree_pred=predict(cr_prune, test,type="class")
table(tree_pred ,crime.test)
mean(crime.test == tree_pred)

ROC = roc(crime.test, tree_pred)
plot(ROC, col = 'red')
auc(ROC)

################################################

# RANDOM FOREST

random_for = randomForest(Crime ~ . , B, subset=train, 
                          mtry=13, importance =TRUE )  

# Verify the predictions
y_pred = predict(random_for ,newdata = test)
table(y_pred, crime.test)
(rmse_random = mean(y_pred == crime.test))
