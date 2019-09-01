# Load packages
library('ggplot2') 
library('ggthemes') 
library('scales') 
library('dplyr') 
library('mice') 
library('randomForest') 
library('plyr')
library(corrplot)
library('tm')

##########################################################################################
## 1 Data Description
##########################################################################################

## Original Features
#
# Survived   : int   ---> Class Label
# PassengerId: int  
# Pclass     : int  
# Name       : chr  
# Sex        : chr  
# Age        : num  
# SibSp      : int  
# Parch      : int  
# Ticket     : chr  
# Fare       : num  
# Cabin      : chr  
# Embarked   : chr

## Load both Train and Test data
Train <- read.csv("train.csv", header=TRUE,stringsAsFactors = FALSE, na.strings=c("", "NA"))
Test <- read.csv("test.csv", header=TRUE,stringsAsFactors = FALSE, na.strings=c("", "NA"))

## Append new column in order to separate Train from Test
Train$TableName <- 'TRAIN'
Test$TableName <- 'TEST'
Test$Survived <- NA

## Backing up TestData's PassengerId for Kaggle Submission
TestPassengerId <- Test$PassengerId

## Bind Train & Test, and get ready for Data Exploration
All <- rbind(Train, Test)
summary(All)

##########################################################################################
## 2 Data Exploration and Missing Data
##########################################################################################

## Get the Missing Data information EXCEPT for the class label Survived
AllExceptSurvived <- subset(All, select = c(-Survived))
summary(is.na(AllExceptSurvived))


## Missing Data Summary
# Age 263/1309
# Fare 1/1309
# Cabin 1014/1309
# Embarked 2/1309


## Handling Missing Fare, we are going to fill it with the median value of Pclass 3
TempPclass3Fare <- All[All$Pclass==3, ]$Fare
All$Fare[is.na(All$Fare)] <- median(TempPclass3Fare, na.rm = TRUE)


## Handling Missing Embarked, we are going to fill them with the most frequent value
TableEmbarkedFreq = count(All$Embarked)
MostFreqEmbarkedValue = TableEmbarkedFreq$x[which(TableEmbarkedFreq$freq == max(TableEmbarkedFreq$freq))]
All$Embarked[is.na(All$Embarked)] <- MostFreqEmbarkedValue


## Handling Missing Cabin by converting into categorical, is.na = UNASSIGNED, otherwise = ASSIGNED
All$Cabin[!is.na(All$Cabin)] <- 'ASSIGNED'
All$Cabin[is.na(All$Cabin)] <- 'UNASSIGNED'


## Up to this step, only Missing Age data is not handled yet
## we will deal with Missing Age Handling later
summary(is.na(All))


##########################################################################################
## 3 Feature Engineering
##########################################################################################

##########################################################################################
## 3.1 Generating new features
##########################################################################################

## Generating the following New Features
# FamilySize
# PerPassengerFare
# Title
# SocialStatus
# PayNoFare 
# AgeGroup (Covered in section below when we handle missing Age data)
# Cabin (we've changed the missing of Cabin feature from it's original entirely, it's practically a new feature)


## Generating FamilySize
All$FamilySize = All$SibSp + All$Parch + 1


## Generating PerPassengerFare
All$PerPassengerFare = All$Fare / (All$SibSp + All$Parch + 1)
All$PerPassengerFare <- round(All$PerPassengerFare)


## Generating Title from Name
All$Title <- gsub('(.*, )|(\\..*)', '', All$Name) 
commoner_title <- c('Dona', 'Don', 'Miss','Mlle','Mme','Mr','Mrs','Ms')
royalty_title <- c('Lady', 'the Countess', 'Sir', 'Jonkheer', 'Master')
rank_title <- c('Capt', 'Col', 'Dr', 'Major', 'Rev')


## Generating SocialClass based on Title
All$SocialClass[All$Title %in% commoner_title] <- 'commoner' 
All$SocialClass[All$Title %in% royalty_title] <- 'royal'
All$SocialClass[All$Title %in% rank_title] <- 'rank'


## Generating NoFare, those with Fare value of zero, probably Crews or VIPs
All$PayNoFare <- (All$PerPassengerFare < 0.0001)


##########################################################################################
## 3.2 Dropping Apparent Redundant and Irrelevant Features
##########################################################################################

## The following features are dropped
# Fare   as we have the PerPassengerFare
# SibSp  as we have the FamilySize
# Parch  as we have the FamilySize
# Name   
# Passenger ID
# Ticket
All <- subset(All, select = -c(Fare, SibSp, Parch, Name, PassengerId, Ticket))


##########################################################################################
## 3.3 Age Missing Data Handling & New AgeGroup Feature Generation
##########################################################################################

## Handling missing Age with MICE
AgeMiceInit = mice(All, maxit=0) 
AgePredMatrix = AgeMiceInit$predictorMatrix
AgeImputation<-mice(All, m=5, predictorMatrix = AgePredMatrix)
All <- complete(AgeImputation)


# Confirm all missing data are handled
summary(is.na(All))


## Generating AgeGroup based on Age
# Age<12       YoungChildren
# 12<=Age<21   ChildrenAndYoungAdult
# 21<=Age<30   Adult
# 30<=Age<50   MiddleAgedAdult
# Age>=50      SeniorAdult
All$AgeGroup[All$Age <12] <-                  'YoungChildren'
All$AgeGroup[All$Age >= 12 & All$Age < 21] <- 'ChildrenAndYoungAdult'
All$AgeGroup[All$Age >=21  & All$Age < 30] <- 'Adult'
All$AgeGroup[All$Age >=30  & All$Age < 50] <- 'MiddleAgedAdult'
All$AgeGroup[All$Age >=50] <-                 'SeniorAdult'


## Dropping Age as we have AgeGroup now
All <- subset(All, select = -c(Age))


##########################################################################################
## 3.4 Feature importance with Boruta
##########################################################################################

## Install and Load library packages
requiredPackages <- c("Boruta", "mlbench")
if (length(setdiff(requiredPackages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(requiredPackages, rownames(installed.packages())))  
}

library(Boruta)
library(mlbench)
library(caret)
library(randomForest)


## Converting to factor 
All$Pclass <- as.factor(All$Pclass)
All$Sex <- as.factor(All$Sex)
All$Cabin <- as.factor(All$Cabin)
All$Embarked <- as.factor(All$Embarked)
All$FamilySize <- as.factor(All$FamilySize)
All$Title <- as.factor(All$Title)
All$SocialClass <- as.factor(All$SocialClass)
All$PayNoFare <- as.factor(All$PayNoFare)
All$AgeGroup <- as.factor(All$AgeGroup)
All$Survived <- as.factor(All$Survived)


## TableName is the column we added to separate Train from Test data. Hence removing it to run feature importance
BorutaAllExceptTableName <- subset(All, select = c(-TableName))

## Running Boruta
set.seed(111)
boruta <- Boruta(Survived ~ ., data = BorutaAllExceptTableName, doTrace = 2, maxRuns = 300)

## Plotting feature importance
plot(boruta, las = 2, cex.axis = 0.7)

##########################################################################################
## 3.5 Feature importance with Random Forest
##########################################################################################

## TableName is the column we added to separate Train from Test data. Hence removing it to run feature importance
RandomForestAllExceptTableName <- subset(All, select = c(-TableName))

## Running RandomForest
RandomForestImportance <- randomForest(Survived ~ ., data = RandomForestAllExceptTableName, importance = TRUE)

## Exploring feature importance
RandomForestImportance$confusion
varImpPlot(RandomForestImportance, sort = T, n.var = 10, main = "Top 10 - Variable Importance")


##########################################################################################
## 3.6 Feature Dropping after Boruta and RandomForest
##########################################################################################

## Both Boruta and RandomForest shows that the new feature PayNoFare we created is NOT important
## that's probably because there are so few records with PayNoFare=TRUE, and I don't think Titianic only
## has 17 Crews and VIPs if it's the real situation. Anyway, we respect the feature importance results, 
## hence, PayNoFare is dropped
All <- subset(All, select = -c(PayNoFare))


##########################################################################################
## 3.7 Feature Correlation with Pearson Correlation Matrix
##########################################################################################

## Removing class label Survived and our irrelevant TableName
CorMatrixAllExceptTableNameAndSurvived <- subset(All, select = c(-Survived, -TableName))


## Running Pearson Correlation among remaining features
CorMatrixAllExceptTableNameAndSurvived <- lapply(CorMatrixAllExceptTableNameAndSurvived, as.integer)
CorMatrixAllExceptTableNameAndSurvived <- as.data.frame(CorMatrixAllExceptTableNameAndSurvived)
CorMatrix <- round(abs(cor(CorMatrixAllExceptTableNameAndSurvived, method = c("pearson"))), 2)
CorMatrix


## Plot the Correlation Matrix Among Attributes
corrplot(CorMatrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

##########################################################################################
## 3.8 Feature Dropping after Pearson Correlation Matrix
##########################################################################################

## Observe that the following Pairs demonstrate strong correlations >0.5
# FamilySize & SocialClass
# Pclass & Cabin
# PerPassengerFare & PClass
# Title & SocialClass

## At this stage, since both Title and SocialClass are new features created by us and
## SocialClass is created on top of Title, hence the expected correlation. We are confident to
## drop Title here. 
All <- subset(All, select = -c(Title))


##########################################################################################
## 4 Training & Prediction & Evaluation
##########################################################################################

##########################################################################################
## 4.1 Ready the data before running any Model
##########################################################################################

## Getting back the Train data with the TableName we created
Train_Data <- All[All$TableName=='TRAIN', ]


## Getting back the Test data with the TableName we created
Test_Data <- All[All$TableName=='TEST', ]


## Retiring TableName as it's served its purpose
Train_Data <- subset(Train_Data, select = c(-TableName))
Test_Data <- subset(Test_Data, select = c(-TableName))

##########################################################################################
## 4.2 This BLOCK is reserved for Jiang Lei's Modeling, please don't trespass
##########################################################################################












##########################################################################################
## 4.3 This BLOCK is reserved for Ming Xiu's Modeling, please don't trespass
##########################################################################################












##########################################################################################
## 4.4 This BLOCK is reserved for Yew Wing's Modeling, please don't trespass
##########################################################################################















##########################################################################################
## 4.5 Fengzhi: Naive Bayes Classification
##########################################################################################

## Quick simple split data into 2/3 and 1/3
Train_Train <- Train_Data[1:599,]
Train_Test <- Train_Data[600:891,]

# check on the split proportion, they are roughly the same
prop.table(table(Train_Train$Survived))
prop.table(table(Train_Test$Survived))

## build Naive Bayes model by Laplace smoothing
library(e1071)
survival_classifier <- naiveBayes(Train_Train, Train_Train$Survived, laplace = 1)

test_pred <- predict(survival_classifier, Train_Test)

library(gmodels)
CrossTable(test_pred, Train_Test$Survived,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))

# try the real thing
survival_classifier <- naiveBayes(Train_Data, Train_Data$Survived, laplace = 1) 

summary(Test_Data)

real_test_pred <- predict(survival_classifier, Test_Data)
TestSurvivedLabel <- Test_Data$Survived

PassengerId <- TestPassengerId
Survived <- TestSurvivedLabel

submission_naivebayes <- data.frame(PassengerId, Survived)
submission_naivebayes

write.csv(submission_naivebayes, file="gender_submission1.csv")

##########################################################################################
## 4.2 NN Model
##########################################################################################

requiredPackages <- c("keras", "mlbench", "dplyr", "magrittr", "neuralnet")
if (length(setdiff(requiredPackages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(requiredPackages, rownames(installed.packages())))  
}

library(keras)
library(mlbench) 
library(dplyr)
library(magrittr)
library(neuralnet)

#back it up first
Train_Train2 <- Train_Train
Train_Test2 <- Train_Test

Train_Train$Pclass <- as.integer(Train_Train$Pclass)
Train_Train$Sex <- as.integer(Train_Train$Sex)
Train_Train$Cabin <- as.integer(Train_Train$Cabin)
Train_Train$Embarked <- as.integer(Train_Train$Embarked)
Train_Train$FamilySize <- as.integer(Train_Train$FamilySize)
Train_Train$Title <- as.integer(Train_Train$Title)
Train_Train$SocialClass <- as.integer(Train_Train$SocialClass)
Train_Train$PayNoFare <- as.integer(Train_Train$PayNoFare)
Train_Train$AgeGroup <- as.integer(Train_Train$AgeGroup)
Train_Train$PerPassengerFare <- as.integer(Train_Train$PerPassengerFare)

Train_Train$Survived <- as.integer(Train_Train$Survived)

Train_Test$Pclass <- as.integer(Train_Test$Pclass)
Train_Test$Sex <- as.integer(Train_Test$Sex)
Train_Test$Cabin <- as.integer(Train_Test$Cabin)
Train_Test$Embarked <- as.integer(Train_Test$Embarked)
Train_Test$FamilySize <- as.integer(Train_Test$FamilySize)
Train_Test$Title <- as.integer(Train_Test$Title)
Train_Test$SocialClass <- as.integer(Train_Test$SocialClass)
Train_Test$PayNoFare <- as.integer(Train_Test$PayNoFare)
Train_Test$AgeGroup <- as.integer(Train_Test$AgeGroup)
Train_Test$PerPassengerFare <- as.integer(Train_Test$PerPassengerFare)

Train_Test$Survived <- as.integer(Train_Test$Survived)

Train_Train$Pclass
Train_Train$Sex
Train_Train$Cabin
Train_Train$Embarked
Train_Train$FamilySize
Train_Train$Title
Train_Train$SocialClass
Train_Train$PayNoFare
Train_Train$AgeGroup
Train_Train$PerPassengerFare
Train_Train$Survived

Train_Test$Pclass
Train_Test$Sex
Train_Test$Cabin
Train_Test$Embarked
Train_Test$FamilySize
Train_Test$Title
Train_Test$SocialClass
Train_Test$PayNoFare
Train_Test$AgeGroup
Train_Test$PerPassengerFare
Train_Test$Survived

  
# Neural Network Model Building
survival_NNet <- neuralnet(Survived ~ ., # 1 vs 10
                          data = Train_Train)


plot(survival_NNet,
     col.hidden = 'red',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

Train_Test_Except_Survived <- subset(Train_Test, select=c(-Survived))
NNet_Results <- compute(survival_NNet, Train_Test_Except_Survived)

Results <- data.frame(Actual = Train_Test$Survived, Prediction = NNet_Results$net.result)

RoundedResults<-sapply(Results, round, digits=0)
RoundedResultSdf=data.frame(RoundedResults)
attach(RoundedResultSdf)

table(Prediction, Actual)

### Improving NN by adding more hidden layers
survival_NNet1 <- neuralnet(Survived ~ ., # 1 vs 10
                            data = Train_Train,
                            hidden=5,
                            stepmax=1e6)

plot(survival_NNet1,
     col.hidden = 'red',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

NNet_Results1 <- compute(survival_NNet1, Train_Test_Except_Survived)
NNet_Pred1 <- NNet_Results1$net.result

Results1 <- data.frame(Actual_1 = Train_Test$Survived, Prediction_1 = NNet_Results1$net.result)

RoundedResults1<-sapply(Results1, round, digits=0)
RoundedResultSdf1=data.frame(RoundedResults1)
attach(RoundedResultSdf1)
table(Prediction_1, Actual_1)



###### Code TODO (Fengzhi to Share tonight):
#1. Bring MX's AgeGroup
#2. Drop PayNoFare
#3. Drop SocialClass
#4. Run's AGE imp after feature engineering
  ## Final selected Feature set except Age itself

## boosting of models
  # cross validation
  # Caret & XGBoost

####### Slides:
  #MX: Problem statement, Data Description, Data Exploration, Data Imputation 2.5min
  #JL: Feature Generation & Feature Dropping & Feature Selection 2.5 min
  #YW: Feature Importance + RandomForest, NN, Logistic Regression 2.5 min
  #FZ: NB, DesicionTree, KNN, Kaggle Summary(score summary of different models) 2.5

###### Reports
  # MX: create Google Doc
  # All: for each model, attach the report to the same Google Doc (chart, statistics, results and discussion)
  # Problem Stat, Data Des, Data Exp => MX
  # Feature selection & dropping ==> JL
  # Summary of Feature Importance(rf, boruta) ==> FZ
  # Kaggle Submission Summary & Possible/future improvement
