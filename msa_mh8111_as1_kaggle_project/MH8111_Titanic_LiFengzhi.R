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

Train <- read.csv("train.csv", header=TRUE,stringsAsFactors = FALSE, na.strings=c("", "NA"))
Test <- read.csv("test.csv", header=TRUE,stringsAsFactors = FALSE, na.strings=c("", "NA"))

str(Train)
str(Test)

Train$TableName <- 'TRAIN'
Test$TableName <- 'TEST'
Test$Survived <- NA

TestPassengerId <- Test$PassengerId
TestPassengerId

All <- rbind(Train, Test)


## Original features

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

summary(All)

##########################################################################################
## 2 Data Exploration and Missing Data
##########################################################################################

## get the missing data info except class label column
AllExceptSurvived <- subset(All, select = c(-Survived))
summary(is.na(AllExceptSurvived))

## the following features are with missing data
# Age 263/1309
# Fare 1/1309
# Cabin 1014/1309
# Embarked 2/1309

## handling missing Fare
TempFare <- All$Fare
All$Fare[is.na(All$Fare)] <- median(TempFare, na.rm = TRUE)

## confirm missing data handling of Fare
summary(is.na(All$Fare))

## handling missing Embarked, we are going to fill the missing Embarked with the most frequent value
## as there are only 2 records missing, doesn't worth the effort digging into it
TableEmbarkedFreq = count(All$Embarked)
MostFreqEmbarkedValue = TableEmbarkedFreq$x[which(TableEmbarkedFreq$freq == max(TableEmbarkedFreq$freq))]
All$Embarked[is.na(All$Embarked)] <- MostFreqEmbarkedValue

## confirm missing data handling of Embarked
summary(is.na(All$Embarked))

## handling missing Cabin by converting into categorical based on assigned or not
summary(is.na(All$Cabin))
All$Cabin[!is.na(All$Cabin)] <- 'ASSIGNED'
All$Cabin[is.na(All$Cabin)] <- 'UNASSIGNED'

All$Cabin

## confirm missing data handling of Cabin
summary(is.na(All$Cabin))

## Handling missing Age with MICE
AgeMiceInit = mice(Train, maxit=0) 
AgePredMatrix = AgeMiceInit$predictorMatrix

# remove PassengerId, Ticket column from the predicate
AgePredMatrix[, c("PassengerId", "Ticket")]=0    
AgeImputation<-mice(All, m=5, predictorMatrix = AgePredMatrix)

All <- complete(AgeImputation)

# confirm missing data handling of Age
summary(is.na(All$Age))

## confirm that all missing data are handled
summary(is.na(All))

##########################################################################################
## 3 Feature Engineering
##########################################################################################

##########################################################################################
## 3.1 Generating new features
##########################################################################################

## feature generation of the following
#FamilySize
#PerPassengerFare
#Title
#SocialStatus
#PayNoFare
#AgeGroup
#CabinAssignment (already handled in Step 2 when handling missing Cabin data)

## generating FamilySize
All$FamilySize = All$SibSp + All$Parch + 1

## generating PerPassengerFare
All$PerPassengerFare = All$Fare / (All$SibSp + All$Parch + 1)
All$PerPassengerFare <- round(All$PerPassengerFare)

## generating Title based on Name
All$Title <- gsub('(.*, )|(\\..*)', '', All$Name) 
commoner_title <- c('Dona', 'Don', 'Miss','Mlle','Mme','Mr','Mrs','Ms')
royalty_title <- c('Lady', 'the Countess', 'Sir', 'Jonkheer', 'Master')
rank_title <- c('Capt', 'Col', 'Dr', 'Major', 'Rev')

## generating Social Class based on Title
All$SocialClass[All$Title %in% commoner_title] <- 'commoner' 
All$SocialClass[All$Title %in% royalty_title] <- 'royal'
All$SocialClass[All$Title %in% rank_title] <- 'rank'

## generating NoFare, those with zero Fare, probably suggesting Crew or VIP
All$PayNoFare <- (All$PerPassengerFare < 0.01)

## generating AgeGroup based on Age

table(All$Age)
#Age<=5       Toddler
#5<Age<=16    Child
#16<Age<=36   YoungAdult
#36<Age<=55   Adult
#Age>55       Senior

All$AgeGroup <- NA

for (i in 1:nrow(All))
{
  if (All[i,]$Age <= 5)
  {
    All[i,]$AgeGroup = "Toddler"
  }
  else if (All[i,]$Age > 5 && All[i,]$Age <=16)
  {
    All[i,]$AgeGroup = "Child"
  }
  else if (All[i,]$Age > 16 && All[i,]$Age <=36)
  {
    All[i,]$AgeGroup = "YoungAdult"
  }
  else if (All[i,]$Age > 36 && All[i,]$Age <=55)
  {
    All[i,]$AgeGroup = "Adult"
  }
  else
  {
    All[i,]$AgeGroup = "Senior"
  }
}

summary(as.factor(All$AgeGroup))

## generating new Cabin feature
# already done in Step 2

##########################################################################################
## 3.2 Dropping features
##########################################################################################

## the following features are dropped
#Fare   as we have the PerPassengerFare
#SibSp  as we have the FamilySize
#Parch  as we have the FamilySize
#Age    as we have the AgeGroup
#Name   
#Passenger ID
#Ticket

All <- subset(All, select = -c(Fare, SibSp, Parch, Age, Name, PassengerId, Ticket))


##########################################################################################
## 3.2 Feature importance
##########################################################################################

AllExceptTableNameAndSurvived <- subset(All, select = c(-Survived, -TableName))
summary(AllExceptTableNameAndSurvived)

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

##########################################################################################
## 3.2.1 Boruta
##########################################################################################
requiredPackages <- c("Boruta", "mlbench")
if (length(setdiff(requiredPackages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(requiredPackages, rownames(installed.packages())))  
}

# Libraries
library(Boruta)  # Mythological God of forest, it based on random Forest
library(mlbench)
library(caret)
library(randomForest)

AllExceptTableName <- subset(All, select = c(-TableName))

set.seed(111)
boruta <- Boruta(Survived ~ ., data = AllExceptTableName, doTrace = 2, maxRuns = 300)
print(boruta)

plot(boruta, las = 2, cex.axis = 0.7)
attStats(boruta)   # attribute statistics

##########################################################################################
## 3.2.2 Random Forest 
##########################################################################################

AllExceptTableName <- subset(All, select = c(-TableName))

RandomForestImportance <- randomForest(Survived ~ .,
                         data = AllExceptTableName,
                         importance = TRUE)

RandomForestImportance$confusion
FeatureImportance <- varImpPlot(RandomForestImportance, sort = T, n.var = 10, main = "Top 10 - Variable Importance")


##########################################################################################
## 4 Training & Prediction & Evaluation
##########################################################################################

## Ready the data for modeling

table(All$TableName)

# please use Train_To_Use for the training and validation
Train_Data <- All[All$TableName=='TRAIN', ]

# Used for Kaggle submission
Test_Data <- All[All$TableName=='TEST', ]

Train_Data <- subset(Train_Data, select = c(-TableName))
Test_Data <- subset(Test_Data, select = c(-TableName))

# Split the Train into Train and Test set
Train_Train <- Train_Data[1:599,]
Train_Test <- Train_Data[600:891,]

# check on the split proportion, they are roughly the same
prop.table(table(Train_Train$Survived))
prop.table(table(Train_Test$Survived))

##########################################################################################
## 4.1 Naive Bayes Classification
##########################################################################################

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


###### TODO:
#1. Bring MX's AgeGroup
#2. Drop PayNoFare
#3. Drop SocialClass
#4. Run's AGE imp after feature engineering
  ## Final selected Feature set except Age itself
