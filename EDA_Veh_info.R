library(readr)
library(dplyr)
library(ggplot2)
library(plotrix)
library(treemap)
library(tidyverse)

##############################
#Importing the dataset
Veh_Info<-read.csv("Veh_Info.csv")
table(Veh_Info$Age_Band_of_Driver)


#################################
#Age band and Sex of Driver
age_sex <- data.frame(Veh_Info$Age_Band_of_Driver, Veh_Info$Sex_of_Driver)
age_sex[-grep("Data missing or out of range", age_sex$Veh_Info.Age_Band_of_Driver, age_sex$Veh_Info.Sex_of_Driver),]-> age_sex
age_sex[-grep("Data missing or out of range", age_sex$Veh_Info.Sex_of_Driver),]->age_sex
age_sex[-grep("Not known",age_sex$Veh_Info.Sex_of_Driver),]->age_sex
age_sex %>% group_by(Veh_Info.Age_Band_of_Driver,Veh_Info.Sex_of_Driver) %>% summarise(count=n())  -> age_sex
table(age_sex$Veh_Info.Sex_of_Driver)


###############################
#Plotting barplot of the dataframe age_sex
ggplot(age_sex, aes(x=age_sex$Veh_Info.Age_Band_of_Driver, y=count, fill = Veh_Info.Age_Band_of_Driver))  + geom_bar(stat="identity", position = position_dodge(width = 0.2)) +
labs(x="Age Band", y="Male/Female") + coord_flip()

###############################
#Plotting treemap
table(Veh_Info$Vehicle_Manoeuvre)
veh_manoe <- data.frame(Veh_Info$Vehicle_Manoeuvre)
veh_manoe %>% group_by(Veh_Info.Vehicle_Manoeuvre) %>% summarise(count=n())  -> veh_manoe
veh_manoe<-data.frame(veh_manoe)
treemap(veh_manoe, index = "Veh_Info.Vehicle_Manoeuvre", vSize = "count",  
        type="index")




