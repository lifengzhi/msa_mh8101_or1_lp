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
All$Cabin[is.na(All$Cabin)] <- 'UNASSIGNED'
All$Cabin[!is.na(All$Cabin)] <- 'ASSIGNED'

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

set.seed(111)
boruta <- Boruta(Survived ~ ., data = All, doTrace = 2, maxRuns = 500)
print(boruta)

plot(boruta)
# We cannot see each feature clear

plot(boruta, las = 2, cex.axis = 0.7)
attStats(boruta)   # attribute statistics

##########################################################################################
## 3.2.2 Random Forest 
##########################################################################################

AllExceptTableName <- subset(All, select = c(-TableName))

rf_model <- randomForest(Survived ~ .,
                         data = AllExceptTableName,
                         importance = TRUE)


rf_model$confusion
featureImportance <- varImpPlot(rf_model, sort = T, n.var = 10, main = "Top 10 - Variable Importance")
plot(featureImportance)
attributes(rf_model)
##????
##importance(rf_model)

##########################################################################################
## 3.2.3 Correlation Matrix 
##########################################################################################






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

# Split the Train into Train and Test set
Train_Train <- Train_Data[1:599,]
Train_Test <- Train_Data[600:891,]

# check on the split proportion, they are roughly the same
prop.table(table(Train_Train$Survived))
prop.table(table(Train_Test$Survived))

##########################################################################################
## 4.1 Naive Bayes Classification
##########################################################################################

## build Naive Bayes model
library(e1071)
survival_classifier <- naiveBayes(Train_Train, Train_Train$Survived, laplace = 1)

test_pred <- predict(survival_classifier, Train_Test)

library(gmodels)
CrossTable(test_pred, Train_Test$Survived,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))


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



################################
Train_Train1 <- Train_Train

summary(is.factor(Train_Train1$Sex))

Train_Train1$Pclass <- as.numeric(Train_Train1$Pclass)
Train_Train1$Sex <- as.numeric(levels(Train_Train1$Sex))[Train_Train1$Sex] 

Train_Train1$Cabin <- as.numeric(Train_Train1$Cabin)
Train_Train1$Embarked <- as.numeric(Train_Train1$Embarked)
Train_Train1$FamilySize <- as.numeric(Train_Train1$FamilySize)
Train_Train1$Title <- as.numeric(Train_Train1$Title)
Train_Train1$SocialClass <- as.numeric(Train_Train1$SocialClass)
Train_Train1$PayNoFare <- as.numeric(Train_Train1$PayNoFare)
Train_Train1$AgeGroup <- as.numeric(Train_Train1$AgeGroup)
Train_Train1$Survived <- as.numeric(Train_Train1$Survived)

is.factor(Train_Train)
# Neural Network Model Building
# 4 layers: 1 input, 1 output, 2 hidden
survival_NNet <- neuralnet(Survived ~ ., # 1 vs 13
                     data = Train_Train,     # we use all the mydata to build this model
                     hidden = c(10,5),  # two hidden layers, with 10 neuros in first while 5 neuros in second hidden layer
                     linear.output = F,
                     lifesign = 'full',
                     rep=1)

# Visualization
plot(survival_NNet)

plot(survival_NNet,
     col.hidden = 'red',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'lightblue')  #node filling color



plot(NNmodel,
     col.hidden = 'red',
     col.hidden.synapse = 'darkgreen',
     show.weights = F,
     information = F,
     fill = 'burlywood') #node filling color






requiredPackages <- c("Boruta", "mlbench")
if (length(setdiff(requiredPackages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(requiredPackages, rownames(installed.packages())))  
}

# Libraries
library(Boruta)  # Mythological God of forest, it based on random Forest
# It main idea
# For each feature, it generates a shadow feature which has all the values 
# shuffled across to introduce randomness. Then, it creates classification
# models using orginal features and shadow feature. Then access the importance
# of each feature. The basic idea is that if a feature is not doing better
# than shadow attribute, then it is not important and should remove the 
# feature from the model. 

library(mlbench)
library(caret)
library(randomForest)


##################################################
# Feature Selection with the Boruta algorithm
##################################################
set.seed(111)
boruta <- Boruta(Survived ~ ., data = Train_To_Use, doTrace = 2, maxRuns = 500)
#"." means all the 60 variables
# doTrace: verbosity level. 0 means no tracing, 
# 1 means reporting decision about each attribute as soon as it is justified, 
# 2 means the same as 1, plus reporting each importance source run, 
# 3 means the same as 2, plus reporting of hits assigned to yet undecided attributes.


print(boruta)

plot(boruta)
# We cannot see each feature clear

plot(boruta, las = 2, cex.axis = 0.7)
Makedecision <- TentativeRoughFix(boruta)  # help us to make quick decision
print(Makedecision)
attStats(boruta)   # attribute statistics