##### logistic regression - all variables have to be numeric #####

#### back the Train_Data and Test_Data for different models 
MX_Test_Data <-Test_Data
MX_Train_Data <-Train_Data

### convert factors to numeric
## Test data
for(i in c(2,3,4,5,6,8,9))
{MX_Test_Data[, i] <- as.numeric(MX_Test_Data[, i])}
## Train data
for(i in c(2,3,4,5,6,8,9))
{MX_Train_Data[, i] <- as.numeric(MX_Train_Data[, i])}

### split train data into train_train and train_test to predict model accuracy
library(caTools)
set.seed(123) 
split = sample.split(MX_Train_Data, SplitRatio = 0.7) 
train_train_set = subset(MX_Train_Data, split == TRUE)
train_test_set = subset(MX_Train_Data, split == FALSE)

### train logistic regression
log_reg <- glm(Survived ~.,family="binomial", data = train_train_set)
log_reg  # Null deviance = 527.7; AIC= 545.7; residual deviance = 527.7
prediction <- predict(log_reg, train_test_set)

### create cross table ###
library(gmodels)
CrossTable(prediction, train_test_set$Survived,  # 56.9% and 43.1% 
           dnn = c('Predicted', 'Actual'))

library(caret)
### variable importance
varImp(log_reg) # Sex, Pclass, FamilySize, Title (top 4)

#### 5-fold cross validation ### 
control <- trainControl(method ="repeatedcv",number=10,repeats =10,savePredictions = TRUE)
log_CV <- train(Survived ~.,family="binomial", data = train_train_set, method= "glm", trControl=control, tuneLength =5)
pred_CV <- predict(log_CV,train_test_set)
confusionMatrix(pred_CV,train_test_set$Survived) # accuracy improved with cross validation! ## accuracy = 82.49%

### Use prediction from cross validation to do the real test
final_pred <- predict(log_CV, MX_Test_Data)
solution <- data.frame(PassengerID = TestPassengerId, Survived = final_pred)
write.csv(solution, file = 'log_regmod_Solution.csv', row.names = F) ## 77.0% score on kaggle

################## KNN - all independent variables have to be normalised ################
##the normalization function is created
nor <-function(x) { (x -min(x))/(max(x)-min(x))   }

### Perform nomalization on test and train set
Test_nor <- as.data.frame(lapply(MX_Test_Data[,c(2,3,4,5,6,7,8,9)], nor))
Train_nor <- as.data.frame(lapply(MX_Train_Data[,c(2,3,4,5,6,7,8,9)], nor))

## add Survived into Train and Test dataset
Train_nor$Survived <- MX_Train_Data$Survived
Test_nor$Survived <- MX_Test_Data$Survived

### split train data into train_train and train_test to predict model accuracy
library(caTools)
set.seed(111) 
split = sample.split(Train_nor, SplitRatio = 0.7) 
train_train_set2 = subset(Train_nor, split == TRUE)
train_test_set2 = subset(Train_nor, split == FALSE)

### train KNN 
library(class)
nrow(train_train_set2) #594 
knn_mod <- knn(train= train_train_set2, test=train_test_set2, cl = train_train_set2$Survived, k=24) # k = sqrt of observations (=24)
knn_mod
CrossTable(x = train_test_set2$Survived, y = knn_mod) # 60.3% are correct

#### 5-fold cross validation ### 
control_KNN <- trainControl(method ="repeatedcv",number=5,savePredictions = TRUE)
knn_CV <- train(Survived~.,method= "knn", tuneGrid =expand.grid(k=1:24),trControl=control_KNN, metric = "Accuracy",
                data = MX_Train_Data)
knn_CV  ## k=4 is optimal 

#### use k=4 (optimal) to see if accuracy improved
knn_mod2 <- knn(train= train_train_set2, test=train_test_set2, cl = train_train_set2$Survived, k=4) 
knn_mod2
CrossTable(x = train_test_set2$Survived, y = knn_mod) # remains the same as 60.3%

### Use prediction from cross validation to do the real test
knn_predictions <- class::knn(Train_nor[,-9], Test_nor[,-9], MX_Train_Data$Survived, k=4)
knn_predictions
knn_predictions <- ifelse(knn_predictions==1,0,1)
solution <- data.frame(PassengerID = TestPassengerId, Survived = knn_predictions)
write.csv(solution, file = 'knn_Solution.csv', row.names = F)  # 23.4% on kaggle
