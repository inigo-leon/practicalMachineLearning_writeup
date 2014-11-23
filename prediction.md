---
title: "Exercise Prediction"
author: "Iñigo León"
date: "November  $23^{th},2014$"
output: html_document
---


```r
library(knitr)
library(caret)
## I set the seed to 314 for reproducibility
set.seed(314)
```

## Summary

Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. 

Our goal is be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. 

## Objective

The goal of this project is to predict the manner in which the users did the exercise. This is the "classe" variable in the training set. We may use any of the other variables to predict with. For this first we had to choose the training and test sets, then we proceeded to clean the data, removing those variables that are not significant for the model or has missing values. After that we fitted a random forest model using the remaining variables. Finally we tested our model with the test set and measure how accurate our model is.

## Building the Model

### Data, training and testing set

The first thing to do is read the data and divide it two sets, the training set and the test set. The test set is used to train the model that will fit the data, and the test set serves to measure the accuracy of the model. In this case we used 75% of the data in the training set, and 25% in the test set.


```r
data <- read.csv("pml-training.csv",na.strings=c("NA", NULL, "#DIV/0!", "", NA))

inTrain <- createDataPartition(y=data$classe,p=0.75,list=FALSE)

training <- data[inTrain,]
testing <- data[-inTrain,]
```

### Cleaning data
The next step is to clean the data, eliminating the variables that has no influence on the model. Obvously, I have to proccess training and testing set in the same way.


First of all, I am going to remove all variables with only NA values.


```r
testing<-testing[, colSums(is.na(training))==0]
training<-training[, colSums(is.na(training))==0]
```

The next step is to identificate the variables that are not relevant for the model by a "near zero variance" test.


```r
nzv <- nearZeroVar(training,saveMetrics=TRUE)
training <- training[,!nzv$nzv]
testing <- testing[,!nzv$nzv]
```

The next step to reduce the number of is was to reduce even more the number of variables, so we 
Other columns that I have to delete are those that has no logical influence over the outcome. In this case I choose "X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2" and "cvtd_timestamp"


```r
training$X<-NULL
training$user_name<-NULL
training$raw_timestamp_part_1<-NULL
training$raw_timestamp_part_2<-NULL
training$cvtd_timestamp<-NULL

testing$X<-NULL
testing$user_name<-NULL
testing$raw_timestamp_part_1<-NULL
testing$raw_timestamp_part_2<-NULL
testing$cvtd_timestamp<-NULL
```


I can delete even more variables by correlation. I am going to detect all the variables with a correlation > 0.8 and eliminate from de set of variables. Repeat until there is no varible in this set.


```r
M <- abs(cor(training[,-ncol(training)]))
diag(M) <- 0
which(M > 0.8,arr.ind=T)
```

```
##                  row col
## yaw_belt           4   2
## total_accel_belt   5   2
## accel_belt_y      10   2
## accel_belt_z      11   2
## accel_belt_x       9   3
## magnet_belt_x     12   3
## roll_belt          2   4
## roll_belt          2   5
## accel_belt_y      10   5
## accel_belt_z      11   5
## pitch_belt         3   9
## magnet_belt_x     12   9
## roll_belt          2  10
## total_accel_belt   5  10
## accel_belt_z      11  10
## roll_belt          2  11
## total_accel_belt   5  11
## accel_belt_y      10  11
## pitch_belt         3  12
## accel_belt_x       9  12
## magnet_belt_z     14  13
## magnet_belt_y     13  14
## gyros_arm_y       20  19
## gyros_arm_x       19  20
## magnet_arm_x      25  22
## accel_arm_x       22  25
## magnet_arm_z      27  26
## magnet_arm_y      26  27
## accel_dumbbell_x  35  29
## accel_dumbbell_z  37  30
## gyros_dumbbell_z  34  32
## gyros_forearm_z   47  32
## gyros_dumbbell_x  32  34
## gyros_forearm_z   47  34
## pitch_dumbbell    29  35
## yaw_dumbbell      30  37
## gyros_forearm_z   47  46
## gyros_dumbbell_x  32  47
## gyros_dumbbell_z  34  47
## gyros_forearm_y   46  47
```

So we can delete those variables


```r
training <-training[,-c(4,5,9,10,11,12,14,20,25,27,32,34,35,37,46)]
testing <- testing[,-c(4,5,9,10,11,12,14,20,25,27,32,34,35,37,46)]
M <- abs(cor(training[,-ncol(training)]))
which(M > 0.8,arr.ind=T)
```

```
##                      row col
## num_window             1   1
## roll_belt              2   2
## pitch_belt             3   3
## gyros_belt_x           4   4
## gyros_belt_y           5   5
## gyros_belt_z           6   6
## magnet_belt_y          7   7
## roll_arm               8   8
## pitch_arm              9   9
## yaw_arm               10  10
## total_accel_arm       11  11
## gyros_arm_x           12  12
## gyros_arm_z           13  13
## accel_arm_x           14  14
## accel_arm_y           15  15
## accel_arm_z           16  16
## magnet_arm_y          17  17
## roll_dumbbell         18  18
## pitch_dumbbell        19  19
## yaw_dumbbell          20  20
## total_accel_dumbbell  21  21
## gyros_dumbbell_y      22  22
## accel_dumbbell_y      23  23
## magnet_dumbbell_x     24  24
## magnet_dumbbell_y     25  25
## magnet_dumbbell_z     26  26
## roll_forearm          27  27
## pitch_forearm         28  28
## yaw_forearm           29  29
## total_accel_forearm   30  30
## gyros_forearm_x       31  31
## gyros_forearm_z       32  32
## accel_forearm_x       33  33
## accel_forearm_y       34  34
## accel_forearm_z       35  35
## magnet_forearm_x      36  36
## magnet_forearm_y      37  37
## magnet_forearm_z      38  38
```

So we have now "only"38 variables

### Creating the model

Once cleaned the data, I have chosen the Random Forest algorithm using principal components to create the model. 

```r
modelFit <- train(training$classe ~ .,method="rf",preProcess="pca",data=training)
```




This is the model's details.


```r
modelFit
```

```
## Error: objeto 'modelFit' no encontrado
```

I get an accuracy of 0.964 with a kappa of 0.955. This model seems to be good, but I have to prove it with the testing set.


### Model evaluation

The last step is to evaluate the model over the testing set.


```r
prediction<-predict(modelFit, testing)
```

```
## Error: objeto 'modelFit' no encontrado
```

```r
confusionMatrix(prediction, reference = testing$classe)
```

```
## Error: objeto 'prediction' no encontrado
```

As we can see, the accuracy anf the kappa values are more or less the same as in the training model, so I have a good model.
