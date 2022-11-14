#load all required package
library(dplyr)
library(ggplot2)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)
library(e1071)
library(xgboost)
library(stringr)
library(lubridate)
library(tm)
library(rms)
library(glmnet)
library(pROC)
library(doMC)
library(kernlab)
#make the analysis reproducible
set.seed(100)


#Importing the dataset
data<- read.csv("C:/Users/adm/Documents/Datasets/Lending Club Dataset/loan.csv")
head(data,5)

#We take a look at the number of each loan_status so far

data %>% group_by(loan_status) %>% summarise(count = n())


data$loan_status = ifelse(str_detect(data$loan_status,"Paid"),data$loan_status,"Default")

tmp = data %>% group_by(loan_status) %>% summarise(ncount = n())
tmp$ncount = 100 * tmp$ncount/nrow(data)
tmp$ncount_p = str_c(round(tmp$ncount,2),"%")
ggplot(tmp,aes(x=loan_status,y=ncount,fill=loan_status)) + geom_bar(stat="identity") +
  geom_text(aes(label=ncount_p),vjust = 2)