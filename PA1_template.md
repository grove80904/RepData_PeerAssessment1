---
title: "Reproducible Research Project 1"
output: html_document
---

## Install required packages

```r
require(knitr)
require(data.table)
require(ggplot2)
```

## Read/process the data

```r
# Save current working directory to return later
olddir <- getwd()
directory <- "./temp"
if (!dir.exists(directory)) {dir.create(directory)}
setwd("./temp")
fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
filedestination <- "coursera.zip"
download.file(fileurl, filedestination)
unzip(filedestination, list = FALSE) #unzip the file
unlink(filedestination) #remove the zip file after it has been unzipped
# Import data
a <- fread("activity.csv", na.strings = "NA")
# Convert the date variable to Date format
a[, date := as.Date(date, "%Y-%m-%d")]
# Convert the steps variable to numeric for later use
a[, steps := as.numeric(steps)]
```

## Create histogram of the total number of steps taken each day

```r
# Calculate daily total steps
histdata <- a[, .(steps.dailytotal = sum(steps, na.rm = T)), by = date]
# Create histogram of the total number of steps taken each day
qplot(x = steps.dailytotal, data = histdata, geom = "histogram")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

## Calculate the mean and median number of steps taken each day

```r
# Calculate mean and median of total daily steps
meanmedian <- histdata[, .(mean = mean(steps.dailytotal), median = median(steps.dailytotal))]
meanmedian
```

```
##       mean median
## 1: 9354.23  10395
```

## Create histogram of the average steps taken during each interval

```r
# Calculate average steps by interval
histdata <- a[, .(steps.intervalmean = mean(steps, na.rm = T)), by = interval]
# Create histogram of the total number of steps taken each day
qplot(x = interval, y = steps.intervalmean, data = histdata, geom = "line")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

## Find the interval that, on average, contains the maximum number of steps

```r
# Sort the data in ascending order of mean steps per interval
setkey(histdata, steps.intervalmean)
# Display the interval with the highest interval mean (last row)
histdata[.N, interval]
```

```
## [1] 835
```

## Identify and impute missing values

```r
# Calculate the number of missing values in the data
sum(!complete.cases(a))
```

```
## [1] 2304
```

```r
# Sort "histdata" and "a" by interval to join them
setkey(histdata, interval)
setkey(a, interval)
# Join "histdata" and "a"
b <- a[histdata]
# Replace NA values with the average value for that interval 
b <- b[is.na(steps), steps := steps.intervalmean]
# Remove "steps.intervalmean" variable from the new data table
b[, steps.intervalmean := NULL]
```

## Create histogram of the daily steps after missing values are imputed and look at differences in mean and median values

```r
# Calculate daily total steps
histdata <- b[, .(steps.dailytotal = sum(steps, na.rm = T)), by = date]
# Create histogram of the total number of steps taken each day
qplot(x = steps.dailytotal, data = histdata, geom = "histogram")
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
# Calculate mean and median of total daily steps
meanmedianimputed <- histdata[, .(mean = mean(steps.dailytotal), median = median(steps.dailytotal))]
meanmedianimputed
```

```
##        mean   median
## 1: 10766.19 10766.19
```

```r
# calculate the change in mean and median values after imputing missing data
meanmedianimputed - meanmedian
```

```
##        mean   median
## 1: 1411.959 371.1887
```

## Create panel plot comparing steps/interval across weekdays and weekends

```r
# Create factor variable for weekday/weekend
weekends <- b[, daytype:= "weekday"]
weekends <- weekends[weekdays(date) %in% c("Saturday", "Sunday"), daytype := "weekend"]
# Calculate average steps by interval by daytype
plotdata <- weekends[, .(steps.intervalmean = mean(steps, na.rm = T)), by = .(daytype, interval)]
# Create histogram of the total number of steps taken each day
ggplot(plotdata, aes(interval, steps.intervalmean)) + geom_line() + facet_wrap(~daytype, ncol = 1)
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

```r
# Return to original working directory
setwd(olddir)
```
