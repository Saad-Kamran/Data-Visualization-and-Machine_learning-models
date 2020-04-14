#####################################
#Importing both the dataset

library(readr)
Accident_Information <- read.csv("Accident_Information.csv")
View(Accident_Information)
Vehicle_Information <- read.csv("Vehicle_Information.csv")
View(Vehicle_Information)
#############################################

############################################
#Taking a brief overview on the datset
library(dplyr)
library(tidyverse)
glimpse(Accident_Information)


############################################
#Checking unique values in desired columns of the dataset
unique(Accident_Information$Junction_Control)
unique(Accident_Information$Number_of_Vehicles)
unique(Accident_Information$Road_Surface_Conditions)
unique(Accident_Information$Weather_Conditions)
unique(Accident_Information$Speed_limit)

#Checking the number of values in desired columns table wise
table(Accident_Information$Speed_limit)
table(Accident_Information$InScotland)
table(Accident_Information$Weather_Conditions)
table(Accident_Information$Carriageway_Hazards)

#####################################
#Dropping the columns that are not needed
Acc_Info=select(Accident_Information, -c(3,5,13,17,18,19,20,25,33))
colnames(Acc_Info)


###################################
#Making a csv file for the new dataframe
Acc_Info <- data.frame(Acc_Info)
write.csv(Acc_Info, "C:\\Users\\Dell\\Desktop\\Acc_Info.csv", row.names = FALSE)
