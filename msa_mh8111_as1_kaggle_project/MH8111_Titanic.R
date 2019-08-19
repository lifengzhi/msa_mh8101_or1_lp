# Load packages
library('ggplot2') 
library('ggthemes') 
library('scales') 
library('dplyr') 
library('mice') 
library('randomForest') 
library('plyr')
#install.packages("corrplot")
library(corrplot)

#setwd("~/Desktop/MSA/msa_mh8111_as1_kaggle_project/")
Train <- read.csv("train.csv", header=TRUE,stringsAsFactors = FALSE, na.strings=c("", "NA"))
Test <- read.csv("test.csv", header=TRUE,stringsAsFactors = FALSE, na.strings=c("", "NA"))

#raw data exploration for missing data
summary(is.na(Train))
summary(is.na(Test))

myfunction <- function(my_data)
{
  feature_names = colnames(my_data)
  for (col_i in feature_names)
  {
    is_na = is.na(my_data[col_i])
    if (sum(is_na)) 
    {
      print(summary(is_na))
    }
  }
}

myfunction(Train)
myfunction(Test)

#new feature Per Passenger Fare
Train$PerPassengerFare = Train$Fare / (Train$SibSp + Train$Parch + 1)
Train <- subset(Train, select = -c(Fare))
summary(Train)

#new feature NoFare
print(subset(Test, Test$Fare == 0))
print(subset(Test, Test$Ticket == "LINE"))

print(subset(Train, Train$PerPassengerFare == 0))
print(subset(Train, Train$Ticket == "LINE"))

Train$NoFare <- (Train$PerPassengerFare < 0.01)
print(sum(Train$NoFare == TRUE))

summary(Train)

#new feature FamilySize
Train$FamilySize = Train$SibSp + Train$Parch + 1
Train <- subset(Train, select = -c(SibSp, Parch))
summary(Train)

#new feature Social Status by Ming Xiu [TODO: to copy over]
Train$Title <- gsub('(.*, )|(\\..*)', '', Train$Name) 
commoner_title <- c('Dona', 'Don', 'Miss','Mlle','Mme','Mr','Mrs','Ms')
royalty_title <- c('Lady', 'the Countess', 'Sir', 'Jonkheer', 'Master')
rank_title <- c('Capt', 'Col', 'Dr', 'Major', 'Rev')

Train$SocialClass[Train$Title %in% commoner_title] <- 'commoner' 
Train$SocialClass[Train$Title %in% royalty_title] <- 'royal'
Train$SocialClass[Train$Title %in% rank_title] <- 'rank'

#Handling Cabin missing data
#####dropping feature completely, see arguments in slides

Train <- subset(Train, select = -c(Cabin))
summary(Train)

#Per Passenger Fare grouped by Pclass
p1AvgFare = mean(subset(Train$PerPassengerFare, Train$Pclass == 1))
p1AvgFare

p2AvgFare = mean(subset(Train$PerPassengerFare, Train$Pclass == 2))
p2AvgFare

p3AvgFare = mean(subset(Train$PerPassengerFare, Train$Pclass == 3))
p3AvgFare

#Handling missing data Fare in Test
#filling the missing Fare in Test, [TODO] need compute avgFare by class combining both Train and Test later
print(subset(Test, is.na(Test$Fare)))
#argument: age > 60, class = 3, highly unlikely to be Crew Labor, assigning mean of p3AvgFare to him


#Handling missing data in Embarked with the most frequent value 
##argument, only 2 are missing, there is no great value digging into it.
tableEmbarkedReq = count(Train$Embarked)
mostFreqValue = tableEmbarkedReq$x[which(tableEmbarkedReq$freq == max(tableEmbarkedReq$freq))]
Train$Embarked[is.na(Train$Embarked)] <- mostFreqValue

summary(is.na(Train))

#partial optimization
Train$Embarked <- as.factor(Train$Embarked)
Train$Sex <- as.factor(Train$Sex)
Train$Pclass <- as.factor(Train$Pclass)
Train$Title <- as.factor(Train$Title)
Train$SocialClass <- as.factor(Train$SocialClass)

summary(is.na(Train))

#Handling missing data in Age Approach 1:
init = mice(Train, maxit=0) 
predM = init$predictorMatrix

#remove PassengerId, Ticket column from the predicate
predM[, c("PassengerId", "Ticket")]=0    
imp<-mice(Train, m=5, predictorMatrix = predM)
print(imp)
summary(imp)
Train1 <- complete(imp)
summary(Train1)
summary(is.na(Train1))

summary(Train1)
summary(Train1$Age)
dim(Train1)

#Handling missing data in Age Approach 2: 
#linear regression Age as function of most correlated features
summary(is.na(Train))

#split Train into NA and non-NA set
AgeTrainToPredict <-Train[is.na(Train$Age), ]
AgeTrainLRSet <- Train[!is.na(Train$Age), ]

#compute the correlation matrix to get top 5 features that most correlated to Age
AgeTrain_LR <- AgeTrainLRSet[, c("Pclass","Sex","Age","FamilySize","PerPassengerFare","Embarked", "NoFare", "Title", "SocialClass")]
summary(AgeTrain_LR)
AgeTrain_LR <- AgeTrain_LR %>% mutate_if(is.factor, as.numeric)
corVal <- round(abs(cor(AgeTrain_LR, method = c("pearson"))), 2)
print(corVal)

#find the top 5 features correlated with Age in the non-NA set
top_5_cor_features <- sort(corVal["Age",], decreasing = TRUE)[2:6]
print(top_5_cor_features)

#finding a multi linear regression model with the five features
#note the strong correlation between SocialClass and Title, so we drop SocialClass in the LM 
AgeTrainLRSet$Title
ageFit <- lm(Age ~ Pclass + Title + FamilySize + PerPassengerFare, data = AgeTrainLRSet)
coeffs = coefficients(ageFit)
print(coeffs)

#check out this page for the metric explaination
#http://r-statistics.co/Linear-Regression.html
summary(ageFit)

predRes <- predict(ageFit, AgeTrainToPredict)
summary(predRes)
print(predRes)
#[Comments on MultiVariable linear regression] 
###the age value we get from this model has negative values. 
###he R-squared & Ajusted R-squared value is about 40%, the prediction accuracy is not promising, thus dropped. 



#[TODO] based on predicted age, categrize it into age groups.. seems a valid feature. 

#non-intuitive feature important
# 1. feature important using random forest
set.seed(100)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + FamilySize + 
                           PerPassengerFare + Embarked + NoFare + Title + SocialClass,
                         data = Train1,
                         importance = TRUE)

importance(rf_model)
rf_model$confusion
featureImportance <- varImpPlot(rf_model, sort = T, n.var = 9, main = "Top 9 - Variable Importance")
plot(featureImportance)


# 2. feature importance using other approaches
#Train1_1 <- Train1[, c("Pclass","Sex","Age","FamilySize","PerPassengerFare","Embarked", "NoFare", "Title", "SocialClass")]
Train1_1 <- Train1[, c("Pclass","Sex","Age","FamilySize","PerPassengerFare", "NoFare", "Title", "SocialClass")]
Train1_1 <- Train1_1 %>% mutate_if(is.factor, as.numeric)
summary(is.na(Train1_1))
print(Train1_1$Embarked)
summary(is.na(Train1))
print(as.factor(Train1$Embarked))

corVal <- round(cor(Train1_1, method = c("pearson", "kendall", "spearman")), 2)
print(corVal)
corrplot(corVal, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#
#[TODO] Why this one has individual categories from each Feature?????
set.seed(7)
library(mlbench)
library(caret)
Train1_2 <- Train1[, c("Survived", "Pclass","Sex","Age","FamilySize","PerPassengerFare", "NoFare", "Title", "SocialClass")]
#control <- trainControl(method="repeatedcv", number=10, repeats=3)
control <- trainControl(method="cv", number=5)
model <- train(Survived~., data=Train1_2, trControl=control, importance=TRUE)
featureImportance <- varImp(model)
print(featureImportance)
plot(featureImportance)
