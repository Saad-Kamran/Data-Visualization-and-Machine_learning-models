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
library(nnet)


########################################################################


########################################################################
#Importing Orignal Datasets

Acc_Veh_cat_cols <- read.csv("Acc_Veh_cat_cols.csv")
summary(Acc_Veh_cat_cols)
str(Acc_Veh_cat_cols)
########################################################################


########################################################################
#Train test splitting
train <- sample_frac(Acc_Veh_cat_cols, 0.6)
sample_id <- as.numeric(rownames(train)) # rownames() returns character so as.numeric
test <- Acc_Veh_cat_cols[-sample_id,]
str(train)


# Setting the basline 
train$Accident_Severity <- relevel(train$Accident_Severity , ref = )



# Loading the nnet package
require(nnet)
# Training the multinomial model
multinom.fit <- multinom(Accident_Severity ~ Age_Band_of_Driver + Sex_of_Driver -1, subset = Accident_Severity ,data = train)

# Checking the model
summary(multinom.fit)

## extracting coefficients from the model and exponentiate
exp(coef(multinom.fit))

# Predicting the values for train dataset
train$precticed <- predict(multinom.fit, newdata = train, "class")

# Building classification table
ctable <- table(train$Accident_Severity, train$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)

# Predicting the values for train dataset
test$precticed <- predict(multinom.fit, newdata = test, "class")

# Building classification table
ctable <- table(test$Accident_Severity, test$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)

