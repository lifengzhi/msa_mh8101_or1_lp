# Load packages
library('ggplot2') 
library('ggthemes') 
library('scales') 
library('dplyr') 
library('mice') 
library('randomForest') 
library('plyr')
library(corrplot)

Train <- read.csv("train.csv", header=TRUE,stringsAsFactors = FALSE, na.strings=c("", "NA"))
Test <- read.csv("test.csv", header=TRUE,stringsAsFactors = FALSE, na.strings=c("", "NA"))

str(Train)
str(Test)

Train$TableName <- 'TRAIN'
Test$TableName <- 'TEST'
Test$Survived <- NA

All <- rbind(Train, Test)
summary(All)

#handling Cabin
summary(is.na(All$Cabin))
All$Cabin[is.na(All$Cabin)] <- 'UNASSIGNED'
All$Cabin[!is.na(All$Cabin)] <- 'ASSIGNED'


#Handling missing data in Age Approach 1:
init = mice(Train, maxit=0) 
predM = init$predictorMatrix

#remove PassengerId, Ticket column from the predicate
predM[, c("PassengerId", "Ticket")]=0    
imp<-mice(All, m=5, predictorMatrix = predM)
print(imp)
summary(imp)
All <- complete(imp)
summary(All)
summary(is.na(All))

#handling missing Embarked
All$Embarked[is.na(All$Embarked)] <- 'S'


#handling Missing Fare, revisit, auto handled by MICE

#new feature Per Passenger Fare
All$PerPassengerFare = All$Fare / (All$SibSp + All$Parch + 1)
All <- subset(All, select = -c(Fare))
summary(is.na(All))


#new feature Social Status by Ming Xiu [TODO: to copy over]
All$Title <- gsub('(.*, )|(\\..*)', '', All$Name) 
commoner_title <- c('Dona', 'Don', 'Miss','Mlle','Mme','Mr','Mrs','Ms')
royalty_title <- c('Lady', 'the Countess', 'Sir', 'Jonkheer', 'Master')
rank_title <- c('Capt', 'Col', 'Dr', 'Major', 'Rev')

All$SocialClass[All$Title %in% commoner_title] <- 'commoner' 
All$SocialClass[All$Title %in% royalty_title] <- 'royal'
All$SocialClass[All$Title %in% rank_title] <- 'rank'


#new feature FamilySize
All$FamilySize = All$SibSp + All$Parch + 1
All <- subset(All, select = -c(SibSp, Parch))
summary(All)

#adding new NoFare
#new feature NoFare

All$NoFare <- (All$PerPassengerFare < 0.01)
print(sum(All$NoFare == TRUE))

summary(All)

table(All$TableName)

#please use Train_To_Use for the training and validation
Train_To_Use <- All[All$TableName=='TRAIN', ]

#please use Test_To_Use for the final score and submision to kaggle
Test_To_Use <- All[All$TableName=='TEST', ]

summary(Train_To_Use)
