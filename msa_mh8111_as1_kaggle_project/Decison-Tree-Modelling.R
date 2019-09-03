##########################################################################################
## 4.2 This BLOCK is reserved for Jiang Lei's Modeling, please don't trespass
##########################################################################################
## packages for decision tree modelling
install.packages('rpart')
install.packages('rpart.plot')
library(rpart)
library(rpart.plot)
library(gmodels)

## split the train data to two parts
Train_Train <- Train_Data[1:599,]
Train_Test <- Train_Data[600:891,]

## check on the split proportion, they are roughly the same
prop.table(table(Train_Train$Survived))
prop.table(table(Train_Test$Survived))

## building decision tree model
set.seed(1234)
Model_DT<-rpart(Survived~.,Train_Train,method="class")
rpart.plot(Model_DT)
PRE_TDT<-predict(Model_DT,Train_Train,type="class")

## check with cross table and confusion matrix
CrossTable(PRE_TDT, Train_Train$Survived,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))
confusionMatrix(PRE_TDT,Train_Train$Survived)

## use cross validation for overfitted check 
cv.10 <- createMultiFolds(Train_Train$Survived, k = 10, times = 10)
## control
ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10,index = cv.10)
Train_Train <- as.data.frame(Train_Train)
str(Train_Train)
##Train the data
Train_Train_Except_Survival<-subset(Train_Train, select=c(-Survived))
Model_CDT <- train(x = Train_Train_Except_Survival, y = Train_Train$Survived, method = "rpart", tuneLength = 30,
                   trControl = ctrl)

##Check the accurcay
##Accurcay using 10 fold cross validation of Single tree is 0.809 
##Seems Overfitted earlier using Single tree, there our accurcay rate is 0.83
rpart.plot(Model_CDT$finalModel)

# predict the test data
PRE_VDTS=predict(Model_CDT$finalModel,Train_Test,type="class")

# check with cross table 
CrossTable(PRE_VDTS, Train_Test$Survived,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))
# check with confusion table , the accuracy is 0.8322
confusionMatrix(PRE_VDTS,Train_Test$Survived)

# test the real data and submit on kaggle 
real_test_pred <- predict(Model_CDT$finalModel, Test_Data,type="class")
Test_Survived_Label <- real_test_pred
PassengerId <- TestPassengerId
Survived <- Test_Survived_Label
submission_decision_tree <- data.frame(PassengerId, Survived)
write.csv(submission_decision_tree, file="gender_submission.csv")
