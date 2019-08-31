########################## Classification using Naive Bayes ##########################
###################### Classification using UCI Adult dataset ########################
################### https://archive.ics.uci.edu/ml/datasets/Adult ####################

install.packages('tm')

######################################################################################
## Step 1: Data Description
######################################################################################

# class label: income: >50K, <=50K.
# 1. age: continuous.
# 2. workclass: Private, Self-emp-not-inc, Self-emp-inc, Federal-gov, Local-gov, State-gov, 
               #Without-pay, Never-worked.
# 3. fnlwgt: continuous.
# 4. education: Bachelors, Some-college, 11th, HS-grad, Prof-school, Assoc-acdm, Assoc-voc, 
               #9th, 7th-8th, 12th, Masters, 1st-4th, 10th, Doctorate, 5th-6th, Preschool.
# 5. education-num: continuous.
# 6. marital-status: Married-civ-spouse, Divorced, Never-married, Separated, 
                    #Widowed, Married-spouse-absent, Married-AF-spouse.
# 7. occupation: Tech-support, Craft-repair, Other-service, Sales, Exec-managerial, Prof-specialty, 
               #Handlers-cleaners, Machine-op-inspct, Adm-clerical, Farming-fishing, 
               #Transport-moving, Priv-house-serv, Protective-serv, Armed-Forces.
# 8. relationship: Wife, Own-child, Husband, Not-in-family, Other-relative, Unmarried.
# 9. race: White, Asian-Pac-Islander, Amer-Indian-Eskimo, Other, Black.
# 10. sex: Female, Male.
# 11. capital-gain: continuous.
# 12. capital-loss: continuous.
# 13. hours-per-week: continuous.
# 14. native-country: United-States, Cambodia, England, Puerto-Rico, Canada, 
                     #Germany, Outlying-US(Guam-USVI-etc), India, Japan, Greece, South, China, 
                     #Cuba, Iran, Honduras, Philippines, Italy, Poland, Jamaica, Vietnam, Mexico, 
                     #Portugal, Ireland, France, Dominican-Republic, Laos, Ecuador, Taiwan, Haiti, 
                     #Columbia, Hungary, Guatemala, Nicaragua, Scotland, Thailand, Yugoslavia, El-Salvador, 
                     #Trinadad&Tobago, Peru, Hong, Holand-Netherlands.


######################################################################################
## Step 2: Data Exploration
######################################################################################

# load the data
adult_data = read.table("adult.data",
                  sep=",",
                  header=FALSE,
                  col.names=c("age", "work_class", "fnlwgt", "education", "education_num",
                              "marital_status", "occupation", "relationship", "race","sex",
                              "capital_gain", "capital_loss", "hours_per_week","country", 
                              "income"),
                  fill=FALSE,
                  strip.white=T,
                  na.strings=c("?", "NA"))

# overview of the data
dim(adult_data)
head(adult_data)
str(adult_data)

# find out the missing data info
summary(is.na(adult_data))

# find out the missing data percentage
sum(is.na(adult_data))/prod(dim(adult_data))

# since the dataset size is considerably large comparing to the number of missing records
# I just omit the missing data
adult_data_full <- na.omit(adult_data)

# check to make sure no missing data
summary(is.na(adult_data_full))
dim(adult_data_full)

######################################################################################
## Step 3: Feature Engineering
######################################################################################

######################################################################################
## 3.1 Age: Convert age from numerical to categorical thru Binning
######################################################################################
summary(adult_data_full$age)

# age<18=Youth,  probably haven't even started working
# 18<age<25=YoungAdult,  limited years of working, probably just started working
# 25<age<60=Adult, main workforce
# age>60=SeniorAdult, retiree

# Take note that below Binning is going to take a while, 
# maybe because my laptop is slow though
for (i in 1:nrow(adult_data_full))
{
  if (adult_data_full[i,]$age <= 18)
  {
    adult_data_full[i,]$age = "Youth"
  }
  else if (adult_data_full[i,]$age > 18 && adult_data_full[i,]$age <=25)
  {
    adult_data_full[i,]$age = "YoungAdult"
  }
  else if (adult_data_full[i,]$age > 25 && adult_data_full[i,]$age <=60)
  {
    adult_data_full[i,]$age = "Adult"
  }
  else
  {
    adult_data_full[i,]$age = "SeniorAdult"
  }
}

# make age into factor
adult_data_full$age <- as.factor(adult_data_full$age)

######################################################################################
## 3.2 Education: Reduce education to fewer categories
######################################################################################

summary(adult_data_full$education)

# PreHighSchool
# HighSchool
# Bachelor
# Master
# Doctor
# Other

adult_data_full$education = gsub("^10th","PreHighSchool",adult_data_full$education)
adult_data_full$education = gsub("^11th","PreHighSchool",adult_data_full$education)
adult_data_full$education = gsub("^12th","PreHighSchool",adult_data_full$education)
adult_data_full$education = gsub("^1st-4th","PreHighSchool",adult_data_full$education)
adult_data_full$education = gsub("^5th-6th","PreHighSchool",adult_data_full$education)
adult_data_full$education = gsub("^7th-8th","PreHighSchool",adult_data_full$education)
adult_data_full$education = gsub("^9th","PreHighSchool",adult_data_full$education)
adult_data_full$education = gsub("^Preschool","PreHighSchool",adult_data_full$education)

adult_data_full$education = gsub("^Some-college","HighSchool",adult_data_full$education)
adult_data_full$education = gsub("^HS-Grad","HighSchool",adult_data_full$education)

adult_data_full$education = gsub("^Bachelors","Bachelor",adult_data_full$education)
adult_data_full$education = gsub("^Masters","Master",adult_data_full$education)
adult_data_full$education = gsub("^Doctorate","Doctor",adult_data_full$education)

adult_data_full$education = gsub("^Assoc-acdm","Other",adult_data_full$education)
adult_data_full$education = gsub("^Assoc-voc","Other",adult_data_full$education)
adult_data_full$education = gsub("^Prof-school","Other",adult_data_full$education)

# make education into factor
adult_data_full$education <- as.factor(adult_data_full$education)

######################################################################################
## 3.3 Marital_Status: Reduce marital_status to fewer categories
######################################################################################

summary(adult_data_full$marital_status)

# Single (never married before)
# Married (remain married)
# MarriedBefore (previously married, now single)
# Widowed

# convert to charactor from factor for comparison
adult_data_full$marital_status <- as.character(adult_data_full$marital_status)

adult_data_full$marital_status[adult_data_full$marital_status=="Never-married"] = "Single"
adult_data_full$marital_status[adult_data_full$marital_status=="Married-AF-spouse"] = "Married"
adult_data_full$marital_status[adult_data_full$marital_status=="Married-civ-spouse"] = "Married"
adult_data_full$marital_status[adult_data_full$marital_status=="Married-spouse-absent"] = "MarriedBefore"
adult_data_full$marital_status[adult_data_full$marital_status=="Separated"] = "MarriedBefore"
adult_data_full$marital_status[adult_data_full$marital_status=="Divorced"] = "MarriedBefore"
adult_data_full$marital_status[adult_data_full$marital_status=="Widowed"] = "Widowed"

# convert back to factor
adult_data_full$marital_status <- as.factor(adult_data_full$marital_status)


######################################################################################
## 3.4 Country: split country into groups based on location and level of development
######################################################################################

# clearly different countries, the income levels are quite different
# thus, I'm categories the countries based on location and also level of development

summary(adult_data_full$country)

# America_North (US, Canada)
# America_South (Columbia, Ecuador)
# America_Latin 
# Europe_West (more developed Europe)
# Europe_East (less developed Europe)
# Asia_SouthEast (Thailand etc)
# Asia_NorthEast (Japan)
# Asia_GreaterChina (China, Taiwan)
# Other (all the rest)

adult_data_full$country <- as.character(adult_data_full$country)

adult_data_full$country[adult_data_full$country=="Cambodia"] = "Asia_SouthEast"
adult_data_full$country[adult_data_full$country=="Canada"] = "America_North"    
adult_data_full$country[adult_data_full$country=="China"] = "Asia_GreaterChina"       
adult_data_full$country[adult_data_full$country=="Columbia"] = "America_South"    
adult_data_full$country[adult_data_full$country=="Cuba"] = "America_South"        
adult_data_full$country[adult_data_full$country=="Dominican-Republic"] = "America_Latin"
adult_data_full$country[adult_data_full$country=="Ecuador"] = "America_South"     
adult_data_full$country[adult_data_full$country=="El-Salvador"] = "America_South" 
adult_data_full$country[adult_data_full$country=="England"] = "Europe_West"
adult_data_full$country[adult_data_full$country=="France"] = "Europe_West"
adult_data_full$country[adult_data_full$country=="Germany"] = "Europe_West"
adult_data_full$country[adult_data_full$country=="Greece"] = "Europe_East"
adult_data_full$country[adult_data_full$country=="Guatemala"] = "America_Latin"
adult_data_full$country[adult_data_full$country=="Haiti"] = "America_Latin"
adult_data_full$country[adult_data_full$country=="Holand-Netherlands"] = "Europe_West"
adult_data_full$country[adult_data_full$country=="Honduras"] = "America_Latin"
adult_data_full$country[adult_data_full$country=="Hong"] = "Asia_GreaterChina"
adult_data_full$country[adult_data_full$country=="Hungary"] = "Europe_East"
adult_data_full$country[adult_data_full$country=="India"] = "Asia_SouthEast"
adult_data_full$country[adult_data_full$country=="Iran"] = "Asia_SouthEast"
adult_data_full$country[adult_data_full$country=="Ireland"] = "Europe_West"
adult_data_full$country[adult_data_full$country=="Italy"] = "Europe_West"
adult_data_full$country[adult_data_full$country=="Jamaica"] = "America_Latin"
adult_data_full$country[adult_data_full$country=="Japan"] = "Asia_NorthEast"
adult_data_full$country[adult_data_full$country=="Laos"] = "Asia_SouthEast"
adult_data_full$country[adult_data_full$country=="Mexico"] = "America_Latin"
adult_data_full$country[adult_data_full$country=="Nicaragua"] = "America_Latin"
adult_data_full$country[adult_data_full$country=="Outlying-US(Guam-USVI-etc)"] = "America_Latin"
adult_data_full$country[adult_data_full$country=="Peru"] = "America_South"
adult_data_full$country[adult_data_full$country=="Philippines"] = "Asia_SouthEast"
adult_data_full$country[adult_data_full$country=="Poland"] = "Europe_East"
adult_data_full$country[adult_data_full$country=="Portugal"] = "Europe_East"
adult_data_full$country[adult_data_full$country=="Puerto-Rico"] = "America_Latin"
adult_data_full$country[adult_data_full$country=="Scotland"] = "Europe_West"
adult_data_full$country[adult_data_full$country=="South"] = "Europe_East"
adult_data_full$country[adult_data_full$country=="Taiwan"] = "Asia_GreaterChina"
adult_data_full$country[adult_data_full$country=="Thailand"] = "Asia_SouthEast"
adult_data_full$country[adult_data_full$country=="Trinadad&Tobago"] = "America_Latin"
adult_data_full$country[adult_data_full$country=="United-States"] = "America_North"
adult_data_full$country[adult_data_full$country=="Vietnam"] = "Asia_SouthEast"
adult_data_full$country[adult_data_full$country=="Yugoslavia"] = "Europe_East"

adult_data_full$country <- as.factor(adult_data_full$country)


######################################################################################
## 3.5 Work_class: reduce the number of categories for work_class
######################################################################################

summary(adult_data_full$work_class)

# PublicSector
# PrivateSector
# SelfEmployed
# Unemployed

adult_data_full$work_class = gsub("^Federal-gov","PublicSector",adult_data_full$work_class)
adult_data_full$work_class = gsub("^Local-gov","PublicSector",adult_data_full$work_class)
adult_data_full$work_class = gsub("^State-gov","PublicSector",adult_data_full$work_class)
adult_data_full$work_class = gsub("^Private","PrivateSector",adult_data_full$work_class)
adult_data_full$work_class = gsub("^Self-emp-inc","SelfEmployed",adult_data_full$work_class)
adult_data_full$work_class = gsub("^Self-emp-not-inc","SelfEmployed",adult_data_full$work_class)
adult_data_full$work_class = gsub("^Without-pay","Unemployed",adult_data_full$work_class)
adult_data_full$work_class = gsub("^Never-worked","Unemployed",adult_data_full$work_class)

# convert work_class to factor
adult_data_full$work_class <- as.factor(adult_data_full$work_class)


######################################################################################
## 3.6 Capital gain/loss: Generating new feature: investment tri-state feature
######################################################################################

# since capital gain have a positive effects on income, captal loss have negative
# effect on income, no captical gain & loss could also tell that probably there is no investment

# new feature investment
# Gain  : when capital gain > 0
# Loss  : when capital loss > 0
# None  : when both capital gain and loss == 0

summary(adult_data_full$capital_gain)
summary(adult_data_full$capital_loss)

adult_data_full$investment = NA

for (i in 1:nrow(adult_data_full))
{
  if (adult_data_full[i,]$capital_gain > 0)
  {
    adult_data_full[i,]$investment = "Gain"
  }
  else if (adult_data_full[i,]$capital_loss > 0)
  {
    adult_data_full[i,]$investment = "Loss"
  }
  else
  {
    adult_data_full[i,]$investment = "None"
  }
}

adult_data_full$investment <- as.factor(adult_data_full$investment)

summary(adult_data_full$investment)

######################################################################################
## 3.7 Hours_per_week: converting from continuous to categorical with Binning
######################################################################################

# clearly work hours per week is related to income, positively or negatively

summary(adult_data_full$hours_per_week)

# LOOSE <20
# NORMAL (20, 40], the normal range 
# OVERTIME (40, 60)
# INSANE >60

for (i in 1:nrow(adult_data_full))
  {
    if(adult_data_full[i,]$hours_per_week<=20)
    {
      adult_data_full[i,]$hours_per_week="LOOSE"
    }
    else if (adult_data_full[i,]$hours_per_week>20 && adult_data_full[i,]$hours_per_week<=40)
    {
      adult_data_full[i,]$hours_per_week="NORMAL"
    }
    else if (adult_data_full[i,]$hours_per_week>40 && adult_data_full[i,]$hours_per_week<=60)
    {
      adult_data_full[i,]$hours_per_week="OVERTIME"
    }
    else
    {
      adult_data_full[i,]$hours_per_week="INSANE"
    }
}

adult_data_full$hours_per_week <- as.factor(adult_data_full$hours_per_week)

summary(adult_data_full$hours_per_week)

######################################################################################
## 3.8 Feature dropping
######################################################################################

# since we are using Naive Bayes, which works well with categorical data. 
# the following columns are dropped 
# -education_num, it's high correlated to education and it's continous numerical
# -fnlwgt, this is sampling weight, I don't see a point of using it

adult_data_full <- subset(adult_data_full, select = -c(fnlwgt, education_num))

# in section 3.6, we have created new feature investment based on capital gain and loss
# hence, we are dropping capital and capital loss 

adult_data_full1 <- adult_data_full
adult_data_full <- subset(adult_data_full, select = -c(capital_gain, capital_loss))


######################################################################################
## 3.9 Split data into Train and Test
######################################################################################
summary(adult_data_full)

train_data <- adult_data_full[1:20000,]
test_data <- adult_data_full[20001:30162,]

# check on the split proportion, they are roughly the same
prop.table(table(train_data$income))
prop.table(table(test_data$income))

##########################################################
## Step 4: Train the model
##########################################################

library(e1071)
income_classifier <- naiveBayes(train_data, train_data$income)


##########################################################
## Step 5: Predict with the Model and Evaluate performance
##########################################################

test_pred <- predict(income_classifier, test_data)

test_pred

test_data$income

library(gmodels)
CrossTable(test_pred, test_data$income,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))

## the Result is as followed, notice that there are 8 records that is classified incorrectly

# Total Observations in Table:  10162 
#               | Actual 
#  Predicted    |     <=50K |      >50K | Row Total | 
#  -------------|-----------|-----------|-----------|
#  <=50K        |      7590 |         8 |      7598 | 
#               |     0.747 |     0.001 |           | 
#  -------------|-----------|-----------|-----------|
#  >50K         |         0 |      2564 |      2564 | 
#               |     0.000 |     0.252 |           | 
#  -------------|-----------|-----------|-----------|
#  Column Total |      7590 |      2572 |     10162 | 
#  -------------|-----------|-----------|-----------|


##########################################################
## Step 6: Model Improvement
##########################################################

# introducing Laplace smoothing parameter to the Naive Bayes classifier
income_classifier2 <- naiveBayes(train_data, train_data$income, laplace = 1)

test_pred2 <- predict(income_classifier2, test_data)

CrossTable(test_pred2, test_data$income,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))


# After adding laplace smoothing, the model is definitely improved
# the number of error prediction is reduced from 8 to 2

# Total Observations in Table:  10162 
#               | Actual 
#  Predicted    |     <=50K |      >50K | Row Total | 
#  -------------|-----------|-----------|-----------|
#  <=50K        |      7590 |         2 |      7592 | 
#  |     0.747  |     0.000 |           | 
#  -------------|-----------|-----------|-----------|
#  >50K         |         0 |      2570 |      2570 | 
#  |     0.000  |     0.253 |           | 
#  -------------|-----------|-----------|-----------|
#  Column Total |      7590 |      2572 |     10162 | 
#  -------------|-----------|-----------|-----------|


######################################################################################
###################### End of Classification using Naive Bayes #######################
######################################################################################
