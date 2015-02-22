# Practical Machine Learning Final Project
# Set working directory first

library(randomForest)
library(caret)
library(ggplot2)
set.seed(12345)



# 1. load csv and delete columns which are mostly NAs
all_train <- read.csv("pml-training.csv", stringsAsFactors=F, na.strings=c(NA,"NA","#DIV/0!"))
all_train <- all_train[,colSums(!is.na(all_train)) > 19000]

# delete columns with numbering, names and non-numeric values
all_train <- all_train[c(-1,-2,-5,-6)]


# set cross validation set apart
inTrain = createDataPartition(all_train$classe, p = 3/4)[[1]]
rfTraining = all_train[ inTrain,]; rfCross_val = all_train[-inTrain,]
pcaTraining = all_train[ inTrain,]; pcaCross_val = all_train[-inTrain,]



# 2. build a random forest model and validate with cross_val set
rfFit <- randomForest(as.factor(classe) ~ ., data=rfTraining)
rfPred <- predict(rfFit, rfCross_val)

# draw table and calculate accuracy
rfCross_val$predRight <- rfPred==rfCross_val$classe
table(rfPred, rfCross_val$classe)
sum(rfCross_val$predRight)/length(rfCross_val$predRight)



# 3. predict the test set
testing <- read.csv("pml-testing.csv", stringsAsFactors=F, na.strings=c(NA,"NA","#DIV/0!"))
testPred <- predict(rfFit, testing)
View(testPred)
