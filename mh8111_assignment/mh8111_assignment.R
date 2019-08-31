########################## Classification using Naive Bayes ##########################
############################ Filtering Spam SMS messages #############################
####################### From Machine Learning with R Chapter  ########################

##########################################################
## Step 2: Data Exploration and Preparation
##########################################################

# load sms data into data frame
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE)

# overview of raw data
str(sms_raw)

# convert class label (spam/ham) into factor.
sms_raw$type <- factor(sms_raw$type)

# summary of class label (type) after conversion
str(sms_raw$type)
table(sms_raw$type)

#install and load the tm package
install.packages("tm")
library(tm)

# create the corpus
sms_corpus <- Corpus(VectorSource(sms_raw$text))

# view the sms_corpus info
print(sms_corpus)

# take a look at the first 3 SMS
inspect(sms_corpus[1:3])

# clean up the corpus, 
# to lower case, remove numbers, remove stopwords, remove punctuation, strip white spaces
sms_corpus_clean <- tm_map(sms_corpus, tolower)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

# create document term matrix
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

# split the document term matrix
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

# split the raw to save the labels
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test  <- sms_raw[4170:5559, ]

# check the proportion after split
prop.table(table(sms_raw_train))
prop.table(table(sms_raw_test))

# load wordcloud library
library(wordcloud)

# view the word cloud of sms_corpus_train
wordcloud(sms_corpus_train, min.freq = 40, random.order = FALSE)

# view the SPAM and HAM wordcloud
spam <- subset(sms_raw_train, type == "spam")
ham  <- subset(sms_raw_test, type == "ham")
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

# find and save frequent words
freqeuent_words <- findFreqTerms(sms_dtm_train, 5)
str(freqeuent_words)

# keep only the frequent terms
sms_dtm_train <- sms_dtm_train[ , freqeuent_words]
sms_dtm_test <- sms_dtm_test[ , freqeuent_words]

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

# apply convert_counts() to train/test data
sms_train <- apply(sms_dtm_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_dtm_test, MARGIN = 2, convert_counts)

##########################################################
## Step 3: Train the model
##########################################################

library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

##########################################################
## Step 4: Predict with the Model and Evaluate performance
##########################################################

sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))

##########################################################
## Step 5: Model Improvement
##########################################################

sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)

sms_test_pred2 <- predict(sms_classifier2, sms_test)

CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, 
           prop.c = FALSE, 
           prop.r = FALSE,
           dnn = c('Predicted', 'Actual'))
