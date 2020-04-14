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
glimpse(Acc_Veh_cat_cols)
Acc_Veh_cat_cols[c(1,2,5,6)]<-lapply(Acc_Veh_cat_cols[c(1,2,5,6)], as.factor)         
         
select(Acc_Veh_cat_cols, -c(1,2,3,5,6,8))->Acc_Veh_cat_cols
table(Acc_Veh_cat_cols$Sex_of_Driver)
########################################################################


########################################################################

Acc_Veh_df=as.data.frame(Acc_Veh_cat_cols)
#Creating data from table
repeating_sequence=rep.int(seq_len(nrow(Acc_Veh_df)), Acc_Veh_df$Sex_of_Driver) #This will repeat each combination equal to the frequency of each combination

#Create the dataset by row repetition created
Acc_Veh_dataset=Acc_Veh_df[repeating_sequence,]
#We no longer need the frequency, drop the feature
intrain <- createDataPartition(y = Acc_Veh_df$Sex_of_Driver, p = 0.6, list = FALSE)
training <- Acc_Veh_df[intrain,]
testing <- Acc_Veh_df[-intrain,]

train1<-data.frame(training)

#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(Sex_of_Driver ~., data=training)
#What does the model say? Print the model summary
Naive_Bayes_Model

#Prediction on the dataset
NB_Predictions=predict(Naive_Bayes_Model,testing$Sex_of_Driver, type = c("class"), subset(testing$Sex_of_Driver), threshold = 0.5, eps = 0.5)
#Confusion matrix to check accuracy
table(NB_Predictions,testing$Sex_of_Driver)

#########################
#Another way of implementing Naive Bayes
#########################

library(mlr)

#Create a classification task for learning on Acc_Veh_cat_col Dataset and specify the target feature
task = makeClassifTask(data = training, target = "Accident_Severity")

#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")

#Train the model
NB_mlr = train(selected_model, task)

#Read the model learned  
NB_mlr$learner.model

#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = testing[,1:3]))

##Confusion matrix to check accuracy
table(predictions_mlr[,1],testing$Accident_Severity)
