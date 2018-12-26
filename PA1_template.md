---
title: "RR_Projec1"
author: "Yuntian Liu"
date: "2018年12月25日"
output: html_document
---
##1. get and clean the data
###1. prepare and load the data
```{r prepare packages and load dat, echo = TRUE}
library('ggplot2')
library('plyr')
library('timeDate')
library('reshape2')
zipFile <- "repdata-data-activity.zip"
if (!file.exists("Data/activity.csv")) {
    dataURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(dataURL, zipFile, mode = "wb")
    unzip(zipFile, files = NULL, exdir = "Data",  unzip = "internal")
    file.remove(zipFile)
}
dirName <- 'Data'
fileName = "activity.csv"
fileNameActivity <- file.path(dirName, fileName)
data <- read.csv(file = fileNameActivity, header = TRUE, colClasses = c("numeric", "Date", "numeric"))
head(data)
```  

###2. Preprocessing the data
```{r processing the data, echo = TRUE}
# add the weekday to the dataset
data$weekday <- weekdays(data$date)
# create a copy of data set with NA rows
data.WithNA <- data
# remove all the rows with 'NA'
data.withoutNA <- data[complete.cases(data),]
head(data.WithNA)
```  

##2 What is mean toal number of steps taken per day?  

###1. Calculate the daily steps
```{r Calculate total number of steps per day, echo = TRUE}
sum.steps.day <- ddply(data.withoutNA, .(date), summarise, steps = sum(steps,na.rm = TRUE))
head(sum.steps.day)
```  

###2. Plotting histogram of the daily steps
```{r plotting histogram, echo = TRUE}
plot(sum.steps.day$date, sum.steps.day$steps, type = "h", main = "Histogram of daily steps", xlab = "Date", ylab = "dailysteps", col = "grey",lwd=5)
abline(h = mean(sum.steps.day$steps, na.rm = TRUE), col = "red", lwd = 2)
```  

###3.Calculate the mean and median of the daily steps
```{r calculate mean and median, echo = TRUE}
paste("Mean steps per Day =", round(mean(sum.steps.day$steps, na.rm = TRUE), 0))
paste("Median steps per Day =", round(median(sum.steps.day$steps, na.rm = TRUE), 0))
```  

##3. What is the average daily activity pattern?
###1. Calculate the number of steps per interval
```{r, calculate the number of steps per interval, echo = TRUE}
mean.steps.interval <- ddply(data.withoutNA, .(interval), summarise, steps = mean(steps, na.rm = TRUE))
head(mean.steps.interval)
```  

###2. Plotting the average daily activity pattern by interval
```{r, plotting the average daily pattern by interval, echo = TRUE}
ggplot(mean.steps.interval, aes(interval, steps)) + geom_line() + xlab("Interval") + ylab("Average steps per interval") + ggtitle("Average Daily Activity Pattern")
```  

##4. Imputing missing values
###1. Calculate the total number of missing values(NA)
```{r, calculate the number of NA, echo = TRUE}
sum(is.na(data.WithNA$steps))
```  
###2. Devise a strategy for filling in all of the missing values
Ideally, we calculate the mean number of steps per 5-minute interval
```{r,filling in the NAs with mean, echo = TRUE}
# Calculate the mean value per day and interval.
mean.weekday <- ddply(data.WithNA, .(interval, weekday), summarise, steps = round(mean(steps, na.rm = TRUE), 2))
# Get list of indices where steps value = NA
naIndex = which(is.na(data.WithNA$steps))
# Merge dataset 'data.WithNA' with dataset mean.steps.interval 
merged.NA = merge(data.WithNA, mean.steps.interval, by = "interval", suffixes = c(".actual", ".stepsInt"))
data.Complete <- data.WithNA
# Replace NA values with value by steps
data.Complete[naIndex, "steps"] <- merged.NA[naIndex, 'steps.stepsInt']
# Ascertain if all the NA values have been replaced
paste("Missing values in new dataset =", sum(is.na(data.Complete)))
# Calculate total number of steps per day  
steps.day <- ddply(data.Complete, .(date), summarise, steps = round(sum(steps, na.rm = TRUE),0))
head(steps.day)
```  

###3 create a new dataset equals to the original dataset but with the missing data filled in.  
```{r,plotting histogram for new dataset,echo = TRUE}
plot(steps.day$date, steps.day$steps, type = "h", main = "Histogram of daily steps (NAs replaced)", xlab = "Date", ylab = "Steps per day", col = "grey", lwd = 6)
abline(h = mean(steps.day$steps, na.rm = TRUE), col = "red", lwd = 2)
```

###4. Calculate the mean and the median of new dataset  
```{r,calculating mean and median, echo = TRUE}
# Calculate total number of steps per day  
sum.steps.day <- ddply(data.Complete, .(date), summarise, steps = sum(steps))
paste("Mean steps per Day =", round(mean(sum.steps.day$steps), 0)) 
paste("Median steps per Day =", round(median(sum.steps.day$steps), 0))
```  

##5. Differences in activity patterns between weekdays and weekends
###1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```{r, echo = TRUE}
# Evaluate wether date is weekday or weekend
data.Complete$daytype <- lapply(data.Complete$date, function(x) ifelse(isWeekday(x, wday = 1:5), 'weekday', 'weekend'))
# flatten list to vector
data.Complete$daytype <- unlist(data.Complete$daytype, use.names = TRUE)
# Create Factor variable
data.Complete$daytype <- as.factor(data.Complete$daytype)
str(data.Complete)
head(data.Complete)
```  
###2. Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days
```{r, echo = TRUE}
# Calculate the 5 - minute interval and the average number of steps taken on weekdays and weekends
day.interval.Steps <- ddply(data.Complete, .(interval, daytype), summarise, steps = mean(steps, na.rm = TRUE))
# plotting the timw series panel plot
ggplot(day.interval.Steps, aes(x = interval, y = steps)) +
    geom_line(aes(col=daytype))+
    ylab('Number of steps') + xlab("Interval") +
    ggtitle("Number of Steps per Interval by daytype (weekend/weekend)") +
    facet_grid(daytype ~ .)
```




