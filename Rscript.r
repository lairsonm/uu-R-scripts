
#Clean console
rm(list = ls())


#Use libraries
library(sqldf)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(scales)

#disable cientific notation
options(scipen=999)

#load RData
load("C:/Users/lairs/Downloads/testDataMatthieu.RData", fullData <- new.env())

#move the data from the env to the dataFrame
dataFrame <- fullData$dat

#using a new table to play the data
workingData <- dataFrame

#Creating the days
workingData$datetimenew <- as.POSIXct(as.numeric(workingData[,5]),origin = "2017-01-09 01:13:37",tz = "GMT")

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

#Group by user
group_by_user <- group_by(labelledData, user_id)

#Summarise the grouping
count_right_answers <- summarise(group_by_user,
                                 number_of_excercises = n(),
                                 correct_answers = sum(correct_answered==1),
                                 ratio_right_answers = (correct_answers / number_of_excercises) * 100
)

#creating the graph with table, X and Y axis
graph = ggplot(count_right_answers, aes(x=number_of_excercises, y=correct_answers))

#Printing the graph with points and color ratio 
graph+geom_point(aes(col=ratio_right_answers))


#Group by day
group_by_date <- group_by(labelledData, createddate)

#Summarise the grouping
count_right_answers_date <- summarise(group_by_date,
                                 holiday = mean(sure),
                                 number_of_excercises = n(),
                                 correct_answers = sum(correct_answered==1),
                                 ratio_right_answers = (correct_answers / number_of_excercises) * 100
)

#creating the graph with table, X and Y axis
graph = ggplot(count_right_answers_date, aes(x=createddate, y=ratio_right_answers))

graph+geom_point(aes(col=ifelse(holiday==1, 'Holiday', 'Not Holiday')))

#get number of excercises per person per day
teste <- labelledData %>%
  group_by(user_id, createddate) %>%
  summarise(total_excercises = n(),
            mean_excercises = mean(n()))

#getting the may holidays
mayHolidays <- filter(labelledData, createddate > "2017-04-05" & createddate < "2017-05-15")

#group by date and user
may_group_by_date_user <- mayHolidays %>%
  group_by(user_id, createddate) %>%
  summarise(holiday = mean(sure),
            number_of_excercises = n(),
            correct_answers = sum(correct_answered==1),
            ratio_right_answers = (correct_answers / number_of_excercises) * 100
  )

#group by date
may_group_by_date <- mayHolidays %>%
  group_by(createddate) %>%
  summarise(holiday = mean(sure),
            number_of_excercises = n(),
            correct_answers = sum(correct_answered==1),
            ratio_right_answers = (correct_answers / number_of_excercises) * 100
  )

#creating the graph with table, X and Y axis
graph2 = ggplot(may_group_by_date, aes(x=createddate, y=ratio_right_answers, fill=holiday))

graph2+geom_bar(stat = "identity")+ scale_y_continuous(limits=c(70,77),oob = rescale_none)


############################YULIE IS HERE LALALA########################################

#after we decide the real each task
#take students who practice in vacation

#isolate christmas
christmasData <- labelledData[labelledData[, "holiday"] =="christmas holiday",]
tempchristmas <- ddply(christmasData,~user_id,summarise,number_of_response=length(unique(created)))
tempchristmas1 <- filter(tempchristmas, number_of_response >= 20)

#isolate summer holiday
summerData <- labelledData[labelledData[, "holiday"] =="summer holiday",]
tempsummer <- ddply(summerData,~user_id,summarise,number_of_response=length(unique(created)))
tempsummer1 <- filter(tempsummer, number_of_response >= 20)

#isolate may holiday
mayhData <- labelledData[labelledData[, "holiday"] =="may holidays",]
tempmayh <- ddply(mayhData,~user_id,summarise,number_of_response=length(unique(created)))
tempmayh1 <- filter(tempmayh, number_of_response >= 20)

#inner join the data
#inner join the data
vacationpracticingstudents <- merge(tempsummer1, tempchristmas1,  by="user_id")
vacationpracticingstudents <- merge(vacationpracticingstudents, tempmayh1,  by="user_id")
vacationpracticingstudents[is.na(vacationpracticingstudents)] <- 0

colnames(vacationpracticingstudents) <- c("user_id", "nor_summer","nor_christmas","nor_mayh")

#share the file to the group as sample
write.csv(vacationpracticingstudents, file="samplevalid.csv")

#call all the vacation practicing student data 
 vacstudent <- merge(x=labelledData[,c("user_id","createddate","created","correct_answered", "response_in_milliseconds","timegroup","holiday","sure","user_domain_rating")],y=vacationpracticingstudents, by = "user_id")

#make plot to show summarized number of response in a day when sure-holiday 	 
sumvac <- ddply(vacstudent,~createddate,summarise,number_of_response=length(created),isholiday=unique(sure))	 

sumvac$createddate <- date(sumvac$createddate)
mygraph7 <- ggplot(sumvac, aes(x=createddate, y=number_of_response, fill=factor(isholiday))) + 
    geom_bar(stat="identity") +
    theme(axis.text.x = element_text(angle=90, vjust=0.2)) +
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b %Y")
mygraph7


#####################################END OF YULIE################################################################

