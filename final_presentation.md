Practical Machine Learning Course Project
========================================================
author: Younghak Jang
date: 150222

Explanation
========================================================

Since the outcome('classe') was a categorical variable, not binomial variable, I tried the random forest model.
My code consists of three parts.
- Loading and Cleansing the Data: Load the csv file and delete columns that are mostly NAs. Then split the set into the training and cross validation set
- Building the Random Forest Model: Build random forest model with the training set, and then validate with the cross validation set
- Actual Testing: Predict the testing set for submission

Slide with Code
========================================================


```r
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
```

Slide with Code
========================================================


```r
# Set working directory first

library(randomForest); library(caret); library(ggplot2)
set.seed(12345)

# 1. load csv and delete columns which are mostly NAs
all_train <- read.csv("pml-training.csv", stringsAsFactors=F, na.strings=c(NA,"NA","#DIV/0!"))
all_train <- all_train[,colSums(!is.na(all_train)) > 19000]

# delete columns with numbering, names and non-numeric values
all_train <- all_train[c(-1,-2,-5,-6)]
```

Slide with Code
========================================================


```r
# set cross validation set apart
inTrain = createDataPartition(all_train$classe, p = 3/4)[[1]]
rfTraining = all_train[ inTrain,]; rfCross_val = all_train[-inTrain,]

# 2. build a random forest model and validate with cross_val set
rfFit <- randomForest(as.factor(classe) ~ ., data=rfTraining)
rfPred <- predict(rfFit, rfCross_val)

# draw table and calculate accuracy
rfCross_val$predRight <- rfPred==rfCross_val$classe
table(rfPred, rfCross_val$classe)
sum(rfCross_val$predRight)/length(rfCross_val$predRight)
```

Slide with Code
========================================================


```r
# 3. predict the test set
testing <- read.csv("pml-testing.csv", stringsAsFactors=F, na.strings=c(NA,"NA","#DIV/0!"))
testPred <- predict(rfFit, testing)
View(testPred)
```


Cross Validation Results
========================================================


```r
> table(rfPred, rfCross_val$classe)
      
rfPred    A    B    C    D    E
     A 1395    4    0    0    0
     B    0  945    2    0    0
     C    0    0  852    1    0
     D    0    0    1  803    1
     E    0    0    0    0  900

> sum(rfCross_val$predRight)/length(rfCross_val$predRight)
[1] 0.9981648
```
