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

Accident_Information <- read.csv("Accident_Information.csv")
View(Accident_Information)
Vehicle_Information <- read.csv("Vehicle_Information.csv")
View(Vehicle_Information)

########################################################################


########################################################################
#Handling Time

#Splitting Time into hours and minutes
Accident_Information<-separate(data = Accident_Information, col = "Time", into = c('hour', 'minutes'), sep = ':')

#Check for na values
sum(is.na(Accident_Information$hour))
sum(is.na(Accident_Information$minutes))

#Convert Values to integer
typeof(Accident_Information$hour)
typeof(Accident_Information$minutes)
sapply(Accident_Information$hour, class)
sapply(Accident_Information$minutes, class)

Accident_Information[c(30,31)]<-lapply(Accident_Information[c(30,31)], as.integer)         
sapply(Accident_Information$hour,class)         
sapply(Accident_Information$minutes, class)

#Scoring Time Values
Accident_Information$hour[Accident_Information$hour < 6  | Accident_Information$hour >= 23] = 5
Accident_Information$hour[Accident_Information$hour >= 6  & Accident_Information$hour < 10] = 1
Accident_Information$hour[Accident_Information$hour >= 10 & Accident_Information$hour < 15] = 2
Accident_Information$hour[Accident_Information$hour >= 15 & Accident_Information$hour < 19] = 3
Accident_Information$hour[Accident_Information$hour >= 19 & Accident_Information$hour < 23] = 4
unique(Accident_Information$hour)

########################################################################


########################################################################
#Keeping only columns needed for algorithm

#Accident Information Dataframe
select(Accident_Information, -c(2,3,4,5,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,31,,33,34,35))->Accident_Information

#Vehicle Information Dataframe
select(Vehicle_Information, -c(4,5,7,8,9,10,11,12,13,15,16,17,18,,20,21,22,23,24))->Vehicle_Information

#Merging both dataframes
Acc_Veh_Info<-merge(x=Accident_Information, y=Vehicle_Information, by="Accident_Index", all = TRUE)

########################################################################


########################################################################
#Checking and ommitting na values
sum(is.na(Acc_Veh_Info))
na.omit(Acc_Veh_Info)->Acc_Veh_Info
sum(is.na(Acc_Veh_Info))


########################################################################


########################################################################
#Preparing data types
glimpse(Acc_Veh_Info)

#CATEGORICAL VARIABLES
#Converting categorical variables to factors
Acc_Veh_Info[c(1,3,4,5,6,9,10)]<-lapply(Acc_Veh_Info[c(1,3,4,5,6,9,10)], as.factor) 
glimpse(Acc_Veh_Info)


#NUMERIC VARIABLES

#Treating Outliers
#Engine Capacity
boxplot(Acc_Veh_Info$Engine_Capacity_.CC.,main = "Engine Capacity",
        xlab = "Index",
        ylab = "Engine Capacity",
        col = "black",
        border = "blue",
        horizontal = TRUE)

condition <- Acc_Veh_Info$Engine_Capacity_.CC.< 20000
Acc_Veh_Info <- Acc_Veh_Info[condition,]

#Age of Vehicle
boxplot(Acc_Veh_Info$Age_of_Vehicle, main = "Age of Vehicle",
        xlab = "Index",
        ylab = "Age of Vehicle",
        col = "orange",
        border = "brown",
        horizontal = TRUE)

condition <- Acc_Veh_Info$Age_of_Vehicle < 20
Acc_Veh_Info<- Acc_Veh_Info[condition,]

#Binning values of age of vehicle
Acc_Veh_Info$Age_of_Vehicle[Acc_Veh_Info$Age_of_Vehicle >= 1  & Acc_Veh_Info$Age_of_Vehicle < 2] = 1
Acc_Veh_Info$Age_of_Vehicle[Acc_Veh_Info$Age_of_Vehicle >= 2   & Acc_Veh_Info$Age_of_Vehicle < 3] = 2
Acc_Veh_Info$Age_of_Vehicle[Acc_Veh_Info$Age_of_Vehicle >= 3 & Acc_Veh_Info$Age_of_Vehicle < 7] = 3
Acc_Veh_Info$Age_of_Vehicle[Acc_Veh_Info$Age_of_Vehicle >= 7 & Acc_Veh_Info$Age_of_Vehicle < 10] = 4
Acc_Veh_Info$Age_of_Vehicle[Acc_Veh_Info$Age_of_Vehicle >= 10] = 5
unique(Acc_Veh_Info$Age_of_Vehicle)

glimpse(Acc_Veh_Info)
########################################################################


########################################################################
select(Acc_Veh_Info, -c(1))->Acc_Veh_Info
Acc_Veh_cat_cols<- Acc_Veh_Info[,c(3,2,4,5,6,7,8,9,1)]
View(Acc_Veh_cat_cols)                               


typeof(Acc_Veh_cat_cols$Age_of_Vehicle)
Acc_Veh_cat_cols[c(5)]<-lapply(Acc_Veh_cat_cols[c(5)], as.factor)
typeof(Acc_Veh_cat_cols$Age_of_Vehicle)
plot(Acc_Veh_cat_cols$Accident_Severity)

#####################################################################

write.csv(Acc_Veh_cat_cols, "C:\\Users\\Dell\\Desktop\\Acc_Veh_cat_cols.csv", row.names = FALSE)

#############################################################################
#Train test splitting
intrain <- createDataPartition(y = Acc_Veh_cat_cols$Accident_Severity, p = 0.8, list = FALSE)
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

