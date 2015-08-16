---
title: "Peer Assessment 1"
author: "Jenner Franco"
date: "16 August, 2015"
output: html_document
---

# Peer Assessment 1
## Peer Assessment 1

**Loading and preprocessing the data**

*1. Download the data from internet*

```r
setwd("C:\\Users\\Jenner\\Desktop\\Reproducible\\RepData_PeerAssessment1")

file <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(url = file, destfile = "data.zip")
unzip("data.zip")
```

*2. Load the data*

```r
activity <- read.csv("activity.csv")
tail(activity)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

**What is mean total number of steps taken per day?**


```r
suppressMessages(library(dplyr))
DATE <- group_by(activity, date)
STEPS <- summarize(DATE, totsteps = sum(steps))
STEPS <- as.data.frame(STEPS)
```
*1. Calculate the total number of steps taken per day*


```r
STEPS
```

```
##          date totsteps
## 1  2012-10-01       NA
## 2  2012-10-02      126
## 3  2012-10-03    11352
## 4  2012-10-04    12116
## 5  2012-10-05    13294
## 6  2012-10-06    15420
## 7  2012-10-07    11015
## 8  2012-10-08       NA
## 9  2012-10-09    12811
## 10 2012-10-10     9900
## 11 2012-10-11    10304
## 12 2012-10-12    17382
## 13 2012-10-13    12426
## 14 2012-10-14    15098
## 15 2012-10-15    10139
## 16 2012-10-16    15084
## 17 2012-10-17    13452
## 18 2012-10-18    10056
## 19 2012-10-19    11829
## 20 2012-10-20    10395
## 21 2012-10-21     8821
## 22 2012-10-22    13460
## 23 2012-10-23     8918
## 24 2012-10-24     8355
## 25 2012-10-25     2492
## 26 2012-10-26     6778
## 27 2012-10-27    10119
## 28 2012-10-28    11458
## 29 2012-10-29     5018
## 30 2012-10-30     9819
## 31 2012-10-31    15414
## 32 2012-11-01       NA
## 33 2012-11-02    10600
## 34 2012-11-03    10571
## 35 2012-11-04       NA
## 36 2012-11-05    10439
## 37 2012-11-06     8334
## 38 2012-11-07    12883
## 39 2012-11-08     3219
## 40 2012-11-09       NA
## 41 2012-11-10       NA
## 42 2012-11-11    12608
## 43 2012-11-12    10765
## 44 2012-11-13     7336
## 45 2012-11-14       NA
## 46 2012-11-15       41
## 47 2012-11-16     5441
## 48 2012-11-17    14339
## 49 2012-11-18    15110
## 50 2012-11-19     8841
## 51 2012-11-20     4472
## 52 2012-11-21    12787
## 53 2012-11-22    20427
## 54 2012-11-23    21194
## 55 2012-11-24    14478
## 56 2012-11-25    11834
## 57 2012-11-26    11162
## 58 2012-11-27    13646
## 59 2012-11-28    10183
## 60 2012-11-29     7047
## 61 2012-11-30       NA
```

*2. Make a histogram of the total number of steps taken each day*


```r
hist(STEPS$totsteps, main = "Total number of steps taken each day", 
xlab = "Total number of steps", col = "green")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

*3. Calculate and report the mean and median of the total number of steps taken per day*


```r
mean(STEPS$totsteps, na.rm=T)
```

```
## [1] 10766.19
```

```r
median(STEPS$totsteps, na.rm=T)
```

```
## [1] 10765
```

**What is the average daily activity pattern?**


```r
INTERVAL <- group_by(activity, interval)
AVSTEPS <- summarize(INTERVAL, avsteps = mean(steps, na.rm=T))
```

*1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)*


```r
plot(AVSTEPS$interval, AVSTEPS$avsteps, type="l",
     xlab= "interval", ylab="average number of steps")
title(main="Time Series Plot")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

*2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?*


```r
AVSTEPSA <- arrange(AVSTEPS, desc(avsteps))
AVSTEPSA[1,]
```

```
## Source: local data frame [1 x 2]
## 
##   interval  avsteps
## 1      835 206.1698
```

**Imputing missing values**

*1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)*


```r
good <- complete.cases(activity)
dim(activity[!good,])[1]
```

```
## [1] 2304
```

*2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.*

*3. Create a new dataset that is equal to the original dataset but with the missing data filled in.*


```r
mergeDATA <- merge(activity, AVSTEPS, by.x="interval")
mergeDATAA <- arrange(mergeDATA, date)
naDATA <- filter(mergeDATAA, is.na(steps))
activity$steps[is.na(activity$steps)] <- naDATA$avsteps
```

*4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*


```r
DATE <- group_by(activity, date)
STEPS <- summarize(DATE, totsteps = sum(steps))
STEPS <- as.data.frame(STEPS)
```

```r
hist(STEPS$totsteps, main = "Total number of steps taken each day", 
xlab = "Total number of steps", col = "blue")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 

```r
mean(STEPS$totsteps)
```

```
## [1] 10766.19
```

```r
median(STEPS$totsteps)
```

```
## [1] 10766.19
```

*Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?*

*Only the median is changed*

**Are there differences in activity patterns between weekdays and weekends?**

*1. Create a new factor variable in the dataset with two levels weekday and weekend indicating whether a given date is a weekday or weekend day.*


```r
activity <- mutate(activity, day=weekdays(as.Date(activity$date)))
activity <- mutate(activity, daytype = factor(1*(day=="Saturday" | day=="Sunday"), labels = c("weekday", "weekend")))
activity <- select(activity, steps, date, interval, daytype)
```

*2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.*


```r
TDI <- group_by(activity, daytype, interval)
TDIAS <- summarize(TDI, avsteps = mean(steps, na.rm=T))
```


```r
library(lattice)
pl <- xyplot(avsteps ~ interval | daytype, xlab = "interval", ylab="Number of steps", data=TDIAS, 
               type = "l", main = "Average Steps by Interval")
print(pl, position = c(0, .3, 1, .9), more = TRUE)
```

![plot of chunk unnamed-chunk-17](figure/unnamed-chunk-17-1.png) 

