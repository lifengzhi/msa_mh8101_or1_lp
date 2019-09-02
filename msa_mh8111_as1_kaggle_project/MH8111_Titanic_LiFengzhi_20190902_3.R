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
TempPclass3Fare <- All[All$Pclass==3 && All$Embarked=='S', ]$Fare
All$Fare[is.na(All$Fare)] <- median(TempPclass3Fare, na.rm = TRUE)

## Handling Missing Embarked, we are going to fill them with the most frequent value
TableEmbarkedFreq = count(All$Embarked)
MostFreqEmbarkedValue = as.character(TableEmbarkedFreq$x[which(TableEmbarkedFreq$freq == max(TableEmbarkedFreq$freq))])
All$Embarked[is.na(All$Embarked)] = MostFreqEmbarkedValue


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


## Generating FamilySize and Discretize it into 3 categories
All$FamilySize = All$SibSp + All$Parch + 1
All$FamilySizeCat[All$FamilySize == 1] <- 'single'
All$FamilySizeCat[All$FamilySize < 5 & All$FamilySize > 1] <- 'small'
All$FamilySizeCat[All$FamilySize > 4] <- 'large'
All$FamilySize <- All$FamilySizeCat
All <- subset(All, select=c(-FamilySizeCat))

## Generating PerPassengerFare
All$PerPassengerFare = All$Fare / (All$SibSp + All$Parch + 1)
All$PerPassengerFare <- round(All$PerPassengerFare)


## Generating Title from Name, and group them under categories
All$Title <- gsub('(.*, )|(\\..*)', '', All$Name)

## We are using RawTitle to generate another SocialClass Feature later, hence back it up here
All$RawTitle <- All$Title

table(All$Sex, All$Title)

## Replace the French Title with English
All$Title[All$Title == 'Mlle']        <- 'Miss' 
All$Title[All$Title == 'Ms']          <- 'Miss'
All$Title[All$Title == 'Mme']         <- 'Mrs' 
table(All$Sex, All$Title)

## Rare titles, maybe elite class
RareTitle <- c("Capt", "Col", "Don", "Dona", "Dr","Jonkheer", "Lady", "Major","Rev","Sir", "the Countess")
All$Title[All$Title %in% RareTitle]  <- 'RareTitle'
table(All$Sex, All$Title)

## Generating the SocialClass feature based on RawTitle
commoner_title <- c('Dona', 'Don', 'Miss','Mlle','Mme','Mr','Mrs','Ms')
royalty_title <- c('Lady', 'the Countess', 'Sir', 'Jonkheer', 'Master')
rank_title <- c('Capt', 'Col', 'Dr', 'Major', 'Rev')
All$SocialClass[All$RawTitle %in% commoner_title] <- 'commoner' 
All$SocialClass[All$RawTitle %in% royalty_title] <- 'royal'
All$SocialClass[All$RawTitle %in% rank_title] <- 'rank'

## Drop the RawTitle, since it's only a temporary duplication for Title
All <- subset(All, select=c(-RawTitle))


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
AllExceptSurvived <- subset(All, select=c(-Survived))
AgeMiceInit = mice(AllExceptSurvived, maxit=0) 
AgePredMatrix = AgeMiceInit$predictorMatrix
AgeImputation<-mice(AllExceptSurvived, m=5, predictorMatrix = AgePredMatrix)
AllExceptSurvived <- complete(AgeImputation)
All$Age <- AllExceptSurvived$Age

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
BorutaTrain <- All[All$TableName=="TRAIN",]
BorutaTrain <- subset(BorutaTrain, select=c(-TableName))

## Running Boruta
set.seed(111)
boruta <- Boruta(Survived ~ ., data = BorutaTrain, doTrace = 2, maxRuns = 300)

## Plotting feature importance
plot(boruta, las = 2, cex.axis = 0.7)

##########################################################################################
## 3.5 Feature importance with Random Forest
##########################################################################################

## TableName is the column we added to separate Train from Test data. Hence removing it to run feature importance
RandomForestTrain <- All[All$TableName=="TRAIN",]
RandomForestTrain <- subset(RandomForestTrain, select=c(-TableName))

## Running RandomForest
RandomForestImportance <- randomForest(Survived ~ ., data = RandomForestTrain, importance = TRUE)

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
## drop SocialClass here. 
All <- subset(All, select = -c(SocialClass))


summary(All)


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
## 4.5 Naive Bayes and Neural Network Modeling by Li Fengzhi
##########################################################################################

##########################################################################################
## 4.5.1 Naive Bayes
##########################################################################################

## Quick simple split data into 2/3 and 1/3
Train_Train <- Train_Data[1:599,]
Train_Test <- Train_Data[600:891,]

## check on the split proportion, they are roughly the same
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

## Apply the model to REAL Test Data
real_test_pred <- predict(survival_classifier, Test_Data)

## Group the two columns into Kaggle Submission required format
PassengerId <- TestPassengerId
Survived <- real_test_pred

submission_naivebayes <- data.frame(PassengerId, Survived)

## Write into submission file
write.csv(submission_naivebayes, file="gender_submission_naivebayes.csv")
###########################################
## Kaggle Score ===> 0.75598  NaiveBayes ##
###########################################


##########################################################################################
## 4.5.2 Neural Network Model
##########################################################################################

## Install and Load libraries
requiredPackages <- c("keras", "mlbench", "dplyr", "magrittr", "neuralnet")
if (length(setdiff(requiredPackages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(requiredPackages, rownames(installed.packages())))  
}

library(keras)
library(mlbench) 
library(dplyr)
library(magrittr)
library(neuralnet)

## Convert to integer values
Train_Train <-lapply(Train_Train, as.integer)
Train_Test <-lapply(Train_Test, as.integer)
Train_Train <- as.data.frame(Train_Train)
Train_Test <- as.data.frame(Train_Test)


## Neural Network Model Building
## We explored configuring the model with different parameters. 
survival_NNet <- neuralnet(Survived ~ ., 
                            data = Train_Train,
                            hidden=5, act.fct = "logistic", #smoothing
                            stepmax=1e6)

## Plot it
plot(survival_NNet,
     col.hidden = 'red',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')

## Prediction
Train_Test_Except_Survived <- subset(Train_Test, select=c(-Survived))
NNet_Results <- compute(survival_NNet, Train_Test_Except_Survived)
NNet_Pred <- NNet_Results$net.result

## Round the NN results, assign the illegal values to its near legal neighbor
PredResult <- sapply(NNet_Pred, round, digits=0)
PredResult[PredResult > 2] <- 2
PredResult[PredResult < 1] <- 1

## Evaluate
CrossTable(PredResult, Train_Test$Survived,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))


## Ready the REAL Test_Data
Test_Data_ExceptSurvived <- subset(Test_Data, select=c(-Survived))
Test_Data_ExceptSurvived <-lapply(Test_Data_ExceptSurvived, as.integer)
Test_Data_ExceptSurvived <- as.data.frame(Test_Data_ExceptSurvived)

## Prediction on the REAL Test_Data
Real_NNet_Results <- compute(survival_NNet, Test_Data_ExceptSurvived)
Real_NNet_Pred <- Real_NNet_Results$net.result
RealPredResult <- sapply(Real_NNet_Pred, round, digits=0)
RealPredResult[RealPredResult > 2] <- 2
RealPredResult[RealPredResult < 1] <- 1
RealPredResult[RealPredResult == 1] <- 0
RealPredResult[RealPredResult == 2] <- 1

## Group the two columns into Kaggle Submission required format
PassengerId <- TestPassengerId
Survived <- RealPredResult

submission_neuralnet <- data.frame(PassengerId, Survived)

## Write into submission file
write.csv(submission_neuralnet, file="gender_submission_neuralnet.csv")
###########################################
## Kaggle Score ===> 0.77511   NeuralNet ##
###########################################

##########################################################################################
## End of Modeling Section
##########################################################################################


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
