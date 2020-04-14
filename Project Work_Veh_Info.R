#####################################
#Importing both the dataset
library(readr)
Vehicle_Information <- read.csv("Vehicle_Information.csv")
View(Vehicle_Information)

####################################
#Taking a brief overview on the datset
library(dplyr)
library(tidyverse)
glimpse(Vehicle_Information)

####################################
#Checking unique values in the desired columns of the dataset
unique(Vehicle_Information$Sex_of_Driver)
unique(Vehicle_Information$Age_Band_of_Driver)
unique(Vehicle_Information$Engine_Capacity_.CC.)
unique(Vehicle_Information$model)
unique(Vehicle_Information$Vehicle_Location.Restricted_Lane)
#Checking the number of values in desired columns table wise
table(Vehicle_Information$Sex_of_Driver)
table(Vehicle_Information$Hit_Object_in_Carriageway)
table(Vehicle_Information$Hit_Object_off_Carriageway)
table(Vehicle_Information$Vehicle_Leaving_Carriageway)
table(Vehicle_Information$Skidding_and_Overturning)


####################################
#Dropping the columns that are not needed
Veh_Info=select(Vehicle_Information, -c(3,4,5,6,9,10,11,12,13,16,17,18,20))
colnames(Veh_Info)

###################################
#Making csv file for the new dataframe
Veh_Info <- data.frame(Veh_Info)
write.csv(Veh_Info, "C:\\Users\\Dell\\Desktop\\Veh_Info.csv", row.names = FALSE)
