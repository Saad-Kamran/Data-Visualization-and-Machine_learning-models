library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(tidyverse)

#############################
#Importing the dataset
Acc_Info <-read.csv("Acc_Info.csv")
View(Acc_Info)



#Month Year Analysis of Accident trend
#MonthYear extarction
typeof(Acc_Info$Date)
Acc_Info$monthyear <- Acc_Info$year <- format((as.Date(Acc_Info$Date)), "%Y-%m")

#Month Year Analysis of Accident trend
#MonthYear extarction
typeof(Acc_Info$Date)
Acc_Info$monthyear <- Acc_Info$year <- format((as.Date(Acc_Info$Date)), "%Y-%m")

monthly_acc<-data.frame(Acc_Info$monthyear)
monthyear_acc <- data.frame(table(Acc_Info$monthyear))
plot(monthyear_acc)
lines(monthyear_acc$Freq)


#Yearly trend
#Year Extraction
Acc_Info$year <- format((as.Date(Acc_Info$Date)), "%Y")
yearly_acc <- data.frame(table(Acc_Info$year))
plot(yearly_acc)
lines(yearly_acc)
weekly_avg <- data.frame(table(Acc_Info$Day_of_Week))
plot(weekly_avg)
lines(weekly_avg)

#Accident Severity
table(Acc_Info$Accident_Severity)
acc_severity<-data.frame(Acc_Info$Accident_Severity)
table(acc_severity$Acc_Info.Accident_Severity)->acc_severity
acc_severity<-as.data.frame(acc_severity)
acc_severity["percentage"] <- (acc_severity$Freq/sum(acc_severity$Freq))*100
acc_severity = acc_severity$Freq / sum(acc_severity$Freq)*100

x <- c(0.012, 0.139, 0.847)
labels <- c("Fatal", "Serious", "Slight")
pct <- (x*100)
lbls <- paste(labels, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels
pie(x,labels = lbls, col=rainbow(length(lbls)),main="acc_severity_pie_chart")


#fatalities per year
fatal_monthyear <- data.frame(Acc_Info$Number_of_Casualties, Acc_Info$monthyear)
fatal_monthyear %>% group_by(Acc_Info.monthyear) %>% summarise(sum(Acc_Info.Number_of_Casualties))  -> fatal_monthyear
plot(fatal_monthyear)
lines(fatal_monthyear)

#Checking the proportion of fatalities
share_fatal<-data.frame(Acc_Info$Date, Acc_Info$Accident_Index, Acc_Info$Accident_Severity)
share_fatal$Acc_Info.Date <- format((as.Date(share_fatal$Acc_Info.Date)), "%Y")
table(share_fatal$Acc_Info.Date, share_fatal$Acc_Info.Accident_Severity)->share_fatal
plot(share_fatal)





