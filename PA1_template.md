---
title: "Course 5 project 1"
author: "Aline S Ferreira"
date: "2020/08/20"
output:
  html_document: 
    keep_md: true
---



## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as Fitbilt, Nike Fuelband or Jawbone Up. These types of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behaviour, or because they are tech geeks. But these data remain under-utilizes both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site: 
https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip

The variables included in this dataset are:

steps: number of steps taking in a 5-minute interval (missing values are coded as NA)
date: the date on which the measurement was taken in YYYY-MM-DD format
interval: indentifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separeted-value (CSV) file and there are a total of 17,568 observations in this dataset.


## Loading and prepreprocessing the data

1. Load the data (i.e. read.csv())


```r
setwd ("C:/Users/Aline Ferreira/Documents/AprendendoR/Course5/W2")
path <- getwd ()
library ("data.table")
```

```
## Warning: package 'data.table' was built under R version 3.6.3
```

```r
library ("ggplot2")
```

```
## Warning: package 'ggplot2' was built under R version 3.6.3
```

```r
fileURL <-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileURL, destfile = paste0(getwd (),"/repdata%2Fdata%2Factivity.zip"), method = "auto")
unzip ("/repdata%2Fdata%2Factivity.zip", exdir = "data")
```

```
## Warning in unzip("/repdata%2Fdata%2Factivity.zip", exdir = "data"): erro 1 na
## extração a partir de arquivo zip
```

2. Process/transform the data (if necessary into a format suitable for your analysis)


```r
activityDT <-data.table::fread(input="data/activity.csv")
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day


```r
Total_Steps <-activityDT [,c(lapply (.SD, sum, na.rm=FALSE)), .SDcols = c("steps"), by = .(date)]

head (Total_Steps)
```

```
##          date steps
## 1: 2012-10-01    NA
## 2: 2012-10-02   126
## 3: 2012-10-03 11352
## 4: 2012-10-04 12116
## 5: 2012-10-05 13294
## 6: 2012-10-06 15420
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day.


```r
ggplot(Total_Steps, aes (x = steps)) +
      geom_histogram(fill = "blue", binwidth = 1000) +
      labs (title= "Daily steps", x = "steps", y = "frequency")
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day


```r
Total_Steps [,. (Mean_Steps = mean (steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]
```

```
##    Mean_Steps Median_Steps
## 1:   10766.19        10765
```
## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
IntervalDT <-activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by= .(interval)]

ggplot (IntervalDT, aes (x= interval, y = steps)) + geom_line(color = "blue", size = 1) + labs (title = "Average daily steps", x = "interval", y = "average steps/day")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
IntervalDT[steps == max(steps), . (max_interval = interval)]
```

```
##    max_interval
## 1:          835
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
activityDT[is.na(steps), .N]
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for taht 5-minute interval, etc.


```r
activityDT[is.na(steps), "steps"] <- activityDT [, c(lapply (.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
data.table::fwrite(x = activityDT, file = "data/tidyData.csv", quote = FALSE)
```

4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
Total_Steps <- activityDT [, c(lapply (.SD, sum)), .SDcols = c("steps"), by = .(date)]
Total_Steps[, . (Mean_Steps = mean (steps), Median_Steps = median (steps))]
```

```
##    Mean_Steps Median_Steps
## 1:    9354.23        10395
```


```r
ggplot(Total_Steps, aes (x = steps)) + geom_histogram (fill = "blue", binwidt = 1000) + labs (title = "Daily steps", x = "steps", y = "number of steps/day")
```

```
## Warning: Ignoring unknown parameters: binwidt
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

Result:
Type of data / Mean_Steps/ Median_Steps
Original data (with na)/ 10766 / 10765
Imputed data (filling in na with median)/ 9354/ 10395

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part. 

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating wether a given date is a weekday ou weekend day.


```r
activityDT$`wday` <-ifelse (as.POSIXlt(activityDT$date)$wday %in% c(1:5), 'weekday', 'weekend')
head(activityDT)
```

```
##    steps       date interval    wday
## 1:     0 2012-10-01        0 weekday
## 2:     0 2012-10-01        5 weekday
## 3:     0 2012-10-01       10 weekday
## 4:     0 2012-10-01       15 weekday
## 5:     0 2012-10-01       20 weekday
## 6:     0 2012-10-01       25 weekday
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in Github repository to see an example of what this plot should look like using simulated data.


```r
activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalDT <-activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `wday`)]
ggplot(IntervalDT, aes(x = interval, y = steps, color = `wday`)) + geom_line() + labs(title = "Average daily steps by weekdays or weekends", x = "interval", y = "steps") + facet_wrap(~`wday`, ncol = 1, nrow = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

