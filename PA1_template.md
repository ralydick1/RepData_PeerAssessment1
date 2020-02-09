---
title: "Reproducible Research Project 1"
author: "Alex Lydick"
date: "2/7/2020"
output:
  html_document: 
    keep_md: yes
    toc: yes
---


```r
knitr::opts_chunk$set(echo = TRUE)
```

## Loading and preprocessing the data

```r
data <- read.csv("./data/activity.csv")
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## What is mean total number of steps taken per day?
 1. Calculate the total number of steps taken per day

```r
steps_day <- aggregate(steps ~ date, data, sum, na.rm = TRUE)
```
 2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.

```r
hist(steps_day$steps, xlab = "Total Steps", ylab = "Frequency",
     main = "Total Number of Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
 3) Calculate and report the mean and median of the total number of steps taken per day

```r
MeanStepsDay <- mean(steps_day$steps)
MedStepsDay <- median(steps_day$steps)
cat("Mean:", MeanStepsDay,"\n")
```

```
## Mean: 10766.19
```

```r
cat("Median:", MedStepsDay)
```

```
## Median: 10765
```

## What is the average daily activity pattern?
 1. Make a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps_interval <- aggregate(steps~interval, data=data, mean, na.rm=TRUE)
plot(steps~interval, data=steps_interval, type = "l",
     xlab = "Intervals", ylab= "Average Steps", 
     main="Avg Steps by Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max_interval <- steps_interval[which.max(steps_interval$steps),1]
cat("Max Interval:", max_interval)
```

```
## Max Interval: 835
```

## Inputing missing values
 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)

```r
cat("Number of NA's:", sum(is.na(data$steps)))
```

```
## Number of NA's: 2304
```

 2-3. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# New Dataset for replacing NA steps values by average steps for matching interval
data_NoNA <- data
for(i in 1:nrow(data_NoNA)){
        if(is.na(data_NoNA[i,]$steps)){
                data_NoNA[i,]$steps <- steps_interval[steps_interval$interval==data_NoNA[i,]$interval,]$steps
        }
}
```

 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
## Aggregate by date and make histogram plot
totSteps_day <- aggregate(steps ~ date, data=data_NoNA,sum)
hist(totSteps_day$steps,xlab="Total Steps", ylab = "Frequency",
     main = "Total Number of Steps by Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


```r
MeanStepsDay_2 <- mean(totSteps_day$steps)
MedStepsDay_2 <- median(totSteps_day$steps)
cat("Mean No NA's:", MeanStepsDay_2,"\n")
```

```
## Mean No NA's: 10766.19
```

```r
cat("Median No NA's:", MedStepsDay_2)
```

```
## Median No NA's: 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
data_NoNA$date <- as.Date(strptime(data_NoNA$date, format="%Y-%m-%d"))
data_NoNA$day <- weekdays(data_NoNA$date)
#For loop to categorize Weekend versus Weekday
for(i in 1:nrow(data_NoNA)){
  if(data_NoNA[i,]$day %in% c("Saturday","Sunday")){
    data_NoNA[i,]$day <- "Weekend"
  }
  else{
    data_NoNA[i,]$day <- "Weekday"
  }
}
## Aggregate data for plot & Tidy Up
steps_Weekday <- aggregate(data_NoNA$steps ~ data_NoNA$interval + data_NoNA$day,data_NoNA,mean)
names(steps_Weekday) <- c("Interval","Day","Steps")
```

 2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
library(lattice)
```

```
## Warning: package 'lattice' was built under R version 3.4.4
```

```r
xyplot(Steps~Interval|Day, steps_Weekday,type ="l", 
       layout = c(1,2),xlab="Intervals",ylab="Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
