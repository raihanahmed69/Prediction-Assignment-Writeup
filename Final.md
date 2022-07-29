---
title: 'Assignment: Prediction Assignment Writeup'
author: "Raihan Ahmed"
date: "July 29, 2022"
output: html_document
---

##1.Synopsis:
###Question:
The goal of your project is to predict the manner in which the subjects in this study did the exercise. This is the "classe" variable in the training set. The other varaibles were used to predict the outcome "classe" of the provided test data.

###Data:
The training data for this project were downloaded from : https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
The test data  were downloaded from:
https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

###Features: 
The training data contain 19622 observations with 160 features. Removal of unnecessary features and near zero variables led to the reduction of the features from 160 to 44. The training data were divided into  Train_validation, Train_Train and Train_Test datasets for builiding and validation of the prediction modelling. 

###Algorithm:
Among the 6 different prediction models were evaluated, the prediction modelling with random forest with ANOVA provided the highest accuracy (99.41%) of all the tested models. 

### Evaluation: 
This predictive modelling with random forest gave an accuracy rate of 99.337% on testing the validation dataset. The expected error rate is 0.66%. Further, the test data downloaded from the website were evalualted with this random forest predictive algorithm based on ANOVA. 


##2.Customizing Startup
###Remove any pre-existing variables/objects

```r
rm(list = ls(all = TRUE))
```

###Temporarily turn off warnings

```r
oldw <- getOption("warn")
options(warn = -1)
```

###clear the console

```r
cat("\014") 
```



###Install and Load the requisite libaries 

```r
Lib_to_used<-c("datasets", "data.table","rpart","randomForest", "nnet", "dplyr", "kernlab","RRF", "caret", "rattle", "ggplot2", "lattice")# enter the pre-requsite libraries

Load_Lib <- function(lib) {
        if (!lib %in% installed.packages()) {
                print(paste("installing",lib)) 
                install.packages(lib) 
        }
}
a=0
b= length(Lib_to_used)
for (i in Lib_to_used) {
        Load_Lib(i)
        library(i, character.only = TRUE)
        a = a+1
        print (paste0(a, " of ", b, " required libraries have been loaded:", " [library:", i, "]"))
}
```

```
## [1] "1 of 12 required libraries have been loaded: [library:datasets]"
## [1] "2 of 12 required libraries have been loaded: [library:data.table]"
## [1] "3 of 12 required libraries have been loaded: [library:rpart]"
## [1] "4 of 12 required libraries have been loaded: [library:randomForest]"
## [1] "5 of 12 required libraries have been loaded: [library:nnet]"
## [1] "6 of 12 required libraries have been loaded: [library:dplyr]"
## [1] "7 of 12 required libraries have been loaded: [library:kernlab]"
## [1] "8 of 12 required libraries have been loaded: [library:RRF]"
## [1] "9 of 12 required libraries have been loaded: [library:caret]"
## [1] "10 of 12 required libraries have been loaded: [library:rattle]"
## [1] "11 of 12 required libraries have been loaded: [library:ggplot2]"
## [1] "12 of 12 required libraries have been loaded: [library:lattice]"
```

###Automatically install the requistite libraries if not previously installed from the CRAN mirror

```r
r <- getOption("repos")
r["CRAN"] <- "http://cran.us.r-project.org"
options(repos = r)
rm(r)
```
###Remove any pre-existing variables/objects

```r
rm(list = ls(all = TRUE))
```
##3.Data Processing:
###3.1 Create a folder in the desktop for this assignment

```r
setwd("~/Desktop")
if (!dir.exists("Practical_Machine")){
        dir.create("Practical_Machine")
}
```

###3.2 Download and unzip the data to the local desktop folder called "Data.zip"

```r
setwd("~/Desktop/Practical_Machine")
if (!file.exists("Train.csv")){
        DataURL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        download.file(DataURL, destfile = "Train.csv", method="curl")
        
}

if (!file.exists("Test.csv")){
        DataURL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        download.file(DataURL, destfile = "Test.csv", method="curl")
        
}
```
###3.3 .Load the data

```r
setwd("~/Desktop/Practical_Machine")
data <-fread ("Train.csv",header = TRUE, na.strings = c("NA", "#DIV/0!", ""))
dim(data)
```

```
## [1] 19622   160
```

```r
Test <-fread ("Test.csv",header = TRUE, na.strings = c("NA", "#DIV/0!", ""))
dim(Test)
```

```
## [1]  20 160
```

### 3.4 Clean the data:

Remove variables with missing values

```r
isna<-colSums(is.na(data)) == 0
data1<-subset(data, select = isna)
dim(data1)
```

```
## [1] 19622    60
```
Convert classe "character" variable into "factor" variable for downstream analyses:

```r
data1$classe<-as.factor(data1$classe)
```

Delete all columns with "characters" except the column that is to be predicted from the analysis if it is a "character" class:

```r
indx <- which(unlist(summarise_each(data1, funs(class))!='character'))
data2<-subset(data1, select= indx)
dim(data2)
```

```
## [1] 19622    56
```

Remove unnessary columns

```r
data3<-subset(data2, select = -c(raw_timestamp_part_1, raw_timestamp_part_2,num_window ))
dim(data3)
```

```
## [1] 19622    53
```

Remove zero covariates

```r
nsv <- nearZeroVar(data3,saveMetrics=TRUE)
non_zero<-nsv$percentUnique>1
non_zero_columns_classe<-c(rownames(nsv[non_zero,]), "classe")
data4<-as.data.frame(subset(data3, select= non_zero_columns_classe))
dim(data4)
```

```
## [1] 19622    44
```

###3.5 Train Data spliting:
The train dataset was splited into validation, training and test datasets to optimise and validate the prediction algorithm.

```r
set.seed(7777)
Main <- createDataPartition(y=data4$classe, p=0.8, list=FALSE)
Train_validation <- data4[-Main,]; subData <- data4[Main,]
group<- createDataPartition(y=subData$classe, p=0.8, list=FALSE)
Train_Train <- subData[group,]; Train_Test <- subData[-group,]
dim(Train_validation)
```

```
## [1] 3923   44
```

```r
dim(Train_Train)
```

```
## [1] 12562    44
```

```r
dim(Train_Test)
```

```
## [1] 3137   44
```


###3.6  Modelling and Predictions with the Training dataset
Create an empty table for later summary of the prediction accuracy:

```r
results = data.frame("name" = character(), "Accuracy" = numeric())
results_f = data.frame("name" = character(), "Accuracy" = numeric())
rownames(results)<- NULL
rownames(results_f)<-NULL
```
Used the following Prediction Models:
1. Model_rpart     :Recursive Partitioning and Regression Trees
2. Model_rf        :Random Forest
3. Model_rf_ANOVA  :Random Forest based on ANOVA
4. Model_nnet      :Fit Neural Networks
5. Model_lssvm     :Least Squares Support Vector Machine
6. Model_RRF       :Regularized Random Forest


```r
set.seed(7777)
Model_rpart<- rpart(classe ~ ., method= "class", data=Train_Train)
Prediction<- predict(Model_rpart,newdata=Train_Test, type = "class")
rp<-confusionMatrix(Prediction, Train_Test$classe)$overall[1]
results<-data.frame("name"= "Model_rpart", "Accuracy"= rp)
results_f<- rbind(results_f,results)

Model_rf<- randomForest(classe~., data = Train_Train, method = "class")
Prediction<- predict(Model_rf,newdata=Train_Test, type = "class")
rf<-confusionMatrix(Prediction, Train_Test$classe)$overall[1]
results<-data.frame("name"= "Model_rf", "Accuracy"= rf)
results_f<- rbind(results_f,results)

Model_rf_ANOVA<- randomForest(classe~., data = Train_Train, method = "ANOVA")
Prediction<- predict(Model_rf_ANOVA,newdata=Train_Test, type = "class")
rf_A<-confusionMatrix(Prediction, Train_Test$classe)$overall[1]
results<-data.frame("name"= "Model_rf_ANOVA", "Accuracy"= rf_A)
results_f<- rbind(results_f,results)


Model_nnet<- nnet(classe~., data = Train_Train , size = 3)
```

```
## # weights:  152
## initial  value 21711.407338 
## iter  10 value 19433.597271
## iter  20 value 18771.602484
## iter  30 value 18653.028516
## iter  40 value 18580.289303
## iter  50 value 18524.973724
## iter  60 value 18182.201222
## iter  70 value 17811.929345
## iter  80 value 17706.415140
## iter  90 value 17528.487649
## iter 100 value 17399.166227
## final  value 17399.166227 
## stopped after 100 iterations
```

```r
Prediction<- predict(Model_nnet,newdata=Train_Test, type = "class")
nn<-confusionMatrix(Prediction, Train_Test$classe)$overall[1]
results<-data.frame("name"= "Model_nnet", "Accuracy"= nn)
results_f<- rbind(results_f,results)


Model_lssvm<- lssvm(classe~., data = Train_Train)
```

```
## Using automatic sigma estimation (sigest) for RBF or laplace kernel
```

```r
Prediction<- predict(Model_lssvm,newdata=Train_Test)
ll<-confusionMatrix(Prediction, Train_Test$classe)$overall[1]
results<-data.frame("name"= "Model_lssvm", "Accuracy"= ll)
results_f<- rbind(results_f,results)


Model_RRF<- RRF(classe~., data = Train_Train)
Prediction<- predict(Model_RRF,newdata=Train_Test)
RR<-confusionMatrix(Prediction, Train_Test$classe)$overall[1]
results<-data.frame("name"= "Model_RRF", "Accuracy"= RR)
results_f<- rbind(results_f,results)
```
###3.7 Summary of evaluations of the Prediction Algorithms with Train_Test data Set

```r
rownames(results_f)<-NULL
arrange(results_f, -Accuracy)
```

```
##             name  Accuracy
## 1 Model_rf_ANOVA 0.9939433
## 2       Model_rf 0.9933057
## 3      Model_RRF 0.9882053
## 4    Model_lssvm 0.7959834
## 5    Model_rpart 0.7554989
## 6     Model_nnet 0.3547976
```

###3.8  Evaluation of the Prediction Algorithms with Train_Validation Data Set
It is clear that the random forest based on ANOVA gives the best accuracy of the all the tested models. So, this model will be used for the further  validation and to predict the test data outcome. 

```r
Prediction<- predict(Model_rf_ANOVA,newdata=Train_validation, type = "class")
confusionMatrix(Prediction, Train_validation$classe)$overall[1]
```

```
##  Accuracy 
## 0.9936273
```
So, the accuracy is estimated to be around 99.41%.

##4.Prediction of the Actual Test Data Set
###4.1 Preprocessing of the Test Data to have the same columns as that of the Train data set for prediction analysis:

```r
cl_name<-colnames(subset(Train_Train, select = -c(classe)))
Test_data<-subset(Test, select = cl_name)
dim(Test_data)
```

```
## [1] 20 43
```
## 4.2. Prediction of the Test Data

```r
Model_rf_ANOVA<- randomForest(classe~., data = Train_Train, method = "ANOVA")
predict(Model_rf_ANOVA,newdata=Test_data, type = "class")
```

```
##  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 
##  B  A  B  A  A  E  D  B  A  A  B  C  B  A  E  E  A  B  B  B 
## Levels: A B C D E
```




