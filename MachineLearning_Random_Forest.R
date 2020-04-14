library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(caret)
library(rpart.plot)
library(rpart)
library(e1071)




########################################################################


########################################################################
#Importing Orignal Datasets

Acc_Veh_cat_cols <- read.csv("Acc_Veh_cat_cols.csv")


########################################################################


########################################################################
#Train test splitting
intrain <- createDataPartition(y = Acc_Veh_cat_cols$Accident_Severity, p = 0.6, list = FALSE)
training <- Acc_Veh_cat_cols[intrain,]
testing <- Acc_Veh_cat_cols[-intrain,]

train1<-data.frame(training)
train1$Accident_Severity == "Slight"

#####################################################################


#############################################################################
#Class Imbalance percentages
val<- data.frame(table(training$Accident_Severity))
val["percentage"] <- (val$Freq/sum(val$Freq))*100

table(testing$Accident_Severity)
val_test<-data.frame(table(testing$Accident_Severity))
val_test["percentage"]<-(val_test$Freq/sum(val_test$Freq))*100

#############################################################################


#####################################################################

##***************Random Forest***************##

install.packages("randomForest")
library(randomForest)
costMatrix <- matrix(c(0,10,1,0), nrow=2)
mod_rf <- randomForest(Accident_Severity ~ ., data = training, ntree = 50, mtry = 7, importance = TRUE, strata =training$Accident_Severity, sampsize=c(Fatal = 500, Serious = 1000, Slight = 3000))
predictions <- predict(mod_rf, testing, type = "class")


#####################################################################


#############################################################################
# Accuracy and other metrics

cm <- confusionMatrix(predictions, testing$Accident_Severity)
cm <- as.matrix(cm[["table"]])
cm

diag <- diag(cm)                #number of correctly classified instances
rowsums <- apply(cm,1,sum)      #number of instances per class
colsums <- apply(cm,2,sum)      #number of predictions per class

precision = diag/colsums
recall = diag/rowsums
f1 = 2* precision *recall /(precision+recall)

data.frame(precision, recall, f1)

macroPrecision = mean(precision)
macroRecall = mean(recall)
macroF1 = mean(f1)

data.frame(macroPrecision, macroRecall, macroF1)

#############################################################################


#####################################################################
