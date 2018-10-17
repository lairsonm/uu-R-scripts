
#Clean console
rm(list = ls())


#Use libraries
library(sqldf)
library(ggplot2)
library(tidyverse)
library(lubridate)

#disable cientific notation
options(scipen=999)

#load RData
load("C:/Users/lairs/Downloads/testDataMatthieu.RData", fullData <- new.env())

#move the data from the env to the dataFrame
dataFrame <- fullData$dat

#using a new table to play the data
workingData <- dataFrame

#Creating the days
workingData$datetimenew <- as.POSIXlt(as.numeric(workingData[,5]),origin = "2017-01-09 01:13:37",tz = "GMT")

#Creating the time grouping(dawn, morning, afternoon, evening, night)
workingData$createddatetime <- as.POSIXct(as.numeric(workingData[,5]),origin = "2017-01-09 01:13:37",tz = "GMT")
workingData$timegroup = cut(hour(workingData$createddatetime),c(-1,7,12,17,21,24))
levels(workingData$timegroup) = c("dawn (0-7)","morning (7-12)","afternoon (12-17)","evening (17-21)","night (21-24)")

#isolate the days
workingData$createddate <- as.POSIXct(strptime(workingData$createddatetime, format="%Y-%m-%d"))

#load Holidays
holidaysData = read.csv("C:/Users/lairs/Downloads/holidays weekend merged - holidays weekend merged.csv")

#Change the date format
holidaysData$createddate <- as.POSIXct(holidaysData$createddate)

#left join the data
labelledData <- merge(workingData, holidaysData, by="createddate", all.x=TRUE)

