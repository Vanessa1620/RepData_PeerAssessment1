unzip("activity.zip")

## Read data
data<-read.csv("activity.csv")
datadf<-data.frame(data)

## Load libraries
library(dplyr)
library(ggplot2)

## Histogram for total number of steps taken each day
stepsperday<-summarize(group_by(datadf,date),sum(steps))
colnames(stepsperday)<-c("Date","Steps")

hist(stepsperday$Steps,xlab="Total steps taken each day",main="Histogram for total steps taken each day",breaks = 30,col="pink")

## Mean and Median of total number of steps taken each day
mean(stepsperday$Steps,na.rm = TRUE)
median(stepsperday$Steps,na.rm = TRUE)

## Time series plot for average number of steps taken each day
averageintervalsteps<-summarize(group_by(datadf,interval),mean(steps,na.rm = TRUE))
colnames(averageintervalsteps)<-c("Interval","AverageSteps")

ggplot(averageintervalsteps,aes(x=Interval,y=AverageSteps))+geom_line()+labs(x="Interval",y="Average Steps",title="Time series plot for average number of steps taken")

## The 5 minute interval with maximum average for steps taken
averageintervalsteps[averageintervalsteps$AverageSteps==max(averageintervalsteps$AverageSteps),]

## Imputing the missing values 
missingvalues<-sum(is.na(datadf))
replacewithmean<-function(x) replace(x,is.na(x),mean(x,na.rm = TRUE))
newdatadf<-datadf %>% group_by(interval) %>% mutate(steps=replacewithmean(steps))
head(newdatadf)

## Histogram for total number of steps taken each day after missing values have been imputed
newstepsperday<-summarize(group_by(newdatadf,date),sum(steps))
colnames(newstepsperday)<-c("Date","Steps")

hist(newstepsperday$Steps,xlab="Total steps taken each day",main="Histogram for total steps taken each day",breaks = 30,col="pink")

## New Mean and Median
mean(newstepsperday$Steps)
median(newstepsperday$Steps)

## Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
newdatadf$date<-as.Date(newdatadf$date)
newdatadf$weekday<-weekdays(newdatadf$date)
newdatadf$weekend<-ifelse(newdatadf$weekday=="Saturday" | newdatadf$weekday=="Sunday", "Weekend", "Weekday")

weekendweekday<-aggregate(newdatadf$steps,by=list(newdatadf$weekend,newdatadf$interval),mean)
names(weekendweekday)<-c("Weekend","Interval","Steps")

ggplot(weekendweekday,aes(x=Interval,y=Steps,color=Weekend))+geom_line()+facet_grid(Weekend ~.)+xlab("Interval")+ylab("Mean of Steps")+ggtitle("Comparison of average number of steps on Weekdays and Weekends")
