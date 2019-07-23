---
title: "Practical Machine Learning Project_CDopazo"
author: "Carlos Dopazo"
date: "22 de julio de 2019"
output: html_document
---

#Introduction

####Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project.The goal of this project is to predict the manner in which they did the exercise. 
####Six participants were asked to perform one set of 10 repetitions of the Unilateral Dumbbell Biceps Curl in five different fashions: exactly according to the specification(Class A), throwing the elbows to the front (Class B), lifting the dumbbell only halfway (Class C), lowering the dumbbell only halfway (Class D) and throwing the hips to the front (Class E).
####The Random Forest model developed got 99.57% accuracy,  less than 0.43% out of sample error, and was able to predict the 20 test cases.

####The data for this project come from this source: http://web.archive.org/web/20161224072740/http:/groupware.les.inf.puc-rio.br/har. is a courtesy of "Ugulino, W.; Cardador, D.; Vega, K.; Velloso, E.; Milidiu, R.; Fuks, H. Wearable Computing: Accelerometers' Data Classification of Body Postures and Movements"


###Libraries
```{r}
library(caret)
library(rpart)
library(rpart.plot)
library(rattle)
library(randomForest)
```
###Data
```{r}
TrainD <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),header=TRUE)
TestD <- read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),header=TRUE)
```
###Exploring
```{r}
dim(TrainD)
dim(TestD)
str(TrainD)
```
####We got 19622 observations and 160 variables in the Training dataset.
Also we can notice that some of the variables have a lot of NAs, is better to remove this variables to avoid messing up the model with missing values.

###Data Cleaning
####We are going to remove all the columns(variables) that had 80% or more missing values(NAs) from the train and test set.
```{r}
TrainNa <- which(colSums(is.na(TrainD) |TrainD=="")>0.9*dim(TrainD)[1])
TrainDClear <- TrainD[,-TrainNa]
TrainDClear <- TrainDClear[,-c(1:7)]
TestNa <- which(colSums(is.na(TestD) |TestD=="")>0.9*dim(TestD)[1]) 
TestDClear <- TestD[,-TestNa]
TestDClear <- TestDClear[,-1]
dim(TrainDClear)
dim(TestDClear)
```
#### After removing the variables with a lot of missing values (more than 80%) we now have a data set with 53 columns (variables) instead of 160.
###Partitioning for modeling
```{r}
set.seed(666)
partition <- createDataPartition(TrainDClear$classe, p=0.75, list=FALSE)
Train <- TrainDClear[partition,]
Test <- TrainDClear[-partition,]
dim(Train)
dim(Test)
```
###Modeling
####We will try to build a model that predicts the outcome, we will start with a Decision Tree model.
###Decision Tree
```{r}
#model
Treemodel <- rpart(classe ~ ., data=Train, method="class")
fancyRpartPlot(Treemodel)
#predicting/cross-validation
predict_tree <- predict(Treemodel, Test, type = "class")
summarytree <- confusionMatrix(predict_tree, Test$classe)
summarytree
#predictionplot
plot(summarytree$table, col = summarytree$byClass, 
     main = "Decision Tree ")
```
####The accuracy for the Decision Tree model is 0.7486, its good but maybe we can find a model that fits better our data. We will try now the Random Forest model.

###Random Forest
```{r}
#model
controlForest <- trainControl(method="cv", number=3, verboseIter=FALSE)
Forestmodel <- train(classe ~ ., data=Train, method="rf", trControl=controlForest)
Forestmodel$finalModel
#predicting/cross-validation
predict_forest <- predict(Forestmodel, newdata=Test)
summaryforest <- confusionMatrix(predict_forest, Test$classe)
summaryforest

```
####The accuracy for the Random Forest model is 0.9957 almost 100% of right guessing,less than 0.43% out of sample error, we dont need to look for any other model, this one seems to be perfect.

###Conclusion
####Now in this last section we will use the best model we got (Random Forest model) and try to predict the outcome for the 20 observations in the TestData set (this will answer the final course quiz).

```{r}
TestData_Quiz <- predict(Forestmodel, newdata=TestDClear)
TestData_Quiz
```

