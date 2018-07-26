#**********************************************************************************
# Title: Reproducible Research: Peer Assessment 1
# Name : Nikhil Prakash
#**********************************************************************************

library(magrittr)
library(dplyr)
library(ggplot2)

## Loading and preprocessing the data
actData <- read.csv("activity.csv", header = TRUE)
head(actData)

## What is mean total number of steps taken per day?
steps <- aggregate(actData$steps, by = list(Date = actData$date), FUN = sum)
library(ggplot2)
names(steps)[names(steps) == "x"] <- "Total"
temp <- as.Date(steps$Date, "%Y-%m-%d")
steps$Date <- format(temp, format = "%m-%d")
head(steps)
ggplot(data = na.omit(steps), aes(Total)) + geom_histogram(binwidth = 1500, colour = "white") +
    xlab("Total Number of Steps Taken Each Day") + ylab("Count") +
    ggtitle("Histogram of the Total Number of Steps Taken Each Day")

## Mean & Median
mean(na.omit(steps$Total))
median(na.omit(steps$Total))

## What is the average daily activity pattern?
pattern <- aggregate(steps ~ interval, data = actData, FUN =mean)
ggplot(data = pattern, aes(x = interval, y = steps)) + geom_line() +
    xlab("Time Intervals (5 Minutes interval)") + ylab("Total Number of Steps") +
    ggtitle("Average Number of Steps Taken in 5-Minute Interval")

head(pattern)
pattern[which(pattern$steps == max(pattern$steps)),]

## Imputing missing values
sapply(X = actData, FUN = function(x) sum(is.na(x)))

replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meandata <- actData%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(meandata)
newdata <- as.data.frame(meandata)
head(newdata)
summary(newdata)

stepsByDay <- tapply(meandata$steps, meandata$date, sum)
qplot(stepsByDay, xlab='Total steps per day', ylab='Frequency', binwidth=500)

## Compare Mean and Median 

mean(na.omit(steps$Total))
median(na.omit(steps$Total))

## Are there differences in activity patterns between weekdays and weekends?

meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )
meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")
ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
    ggtitle("Comparison of Average Number of Steps in Each Interval")
