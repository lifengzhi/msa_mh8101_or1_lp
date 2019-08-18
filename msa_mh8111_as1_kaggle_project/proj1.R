# Load packages
library('ggplot2') 
library('ggthemes') 
library('scales') 
library('dplyr') 
library('mice') 
library('randomForest') 

setwd("~/Desktop/MSA/msa_mh8111_as1_kaggle_project/")
Train <- read.csv("train.csv", header=TRUE,stringsAsFactors = FALSE, na.strings=c("", "NA"))
summary(is.na(Train$Cabin))
?read.csv


