# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data


```r
knitr::opts_chunk$set(echo=TRUE,cache=TRUE)
```
### Load / transform the data 

```r
temp <- tempfile()
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
myData <- read.csv(unz(temp, "activity.csv"))
unlink(temp)
head(myData)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day


```r
# exclude missing values
myDataExNa <- myData[complete.cases(myData),]
stepPerDays <- aggregate( x=myDataExNa$steps, by=list(Day=myDataExNa$date ), FUN=sum )
colnames(stepPerDays) <- c('Day','Steps')
```

Make a histogram of the total number of steps taken each day

```r
hist(stepPerDays$Steps,
     main="Histogram of total number of steps taken each day",
     xlab="Number of steps",
     ylim = c(0,30))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

Calculate and report the mean and median of the total number of steps taken per day

```r
mean( stepPerDays$Steps )
```

```
## [1] 10766.19
```

```r
median( stepPerDays$Steps )
```

```
## [1] 10765
```

## What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepPerInt <- aggregate( steps ~ interval, data=myDataExNa, FUN=mean )
plot( x=stepPerInt$interval, y=stepPerInt$steps, type="l",
      main="Average daily activity", xlab="Interval", ylab="Number of steps" )
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
stepPerInt$interval[which.max(stepPerInt$steps)]
```

```
## [1] 835
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(myData))
```

```
## [1] 2304
```
Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
To replace NAs with mean of 5-minute interval

```r
nanDs <- is.na(myData$steps)
avgInt <- tapply( myData$steps, myData$interval, mean, na.rm=TRUE)
```

Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
myData2 <- myData
myData2$steps[nanDs] <- avgInt[as.character(myData2$interval[nanDs])]
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
# pot histogramcolnames(stepPerDays2) <- c('Day','Steps')
stepPerDays2 <- aggregate( steps ~ date, data=myData2, FUN=sum )
hist(stepPerDays2$steps,
     main="Histogram of total number of steps taken each day",
     xlab="Number of steps",
     ylim = c(0,35))
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
# calc mean and median
mean( stepPerDays2$steps )
```

```
## [1] 10766.19
```

```r
median( stepPerDays2$steps )
```

```
## [1] 10766.19
```

Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
on the estimates of the total daily number of steps?

Means remain the same. Median slightly increase.

## Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
library(lattice)
myData2$date <- as.Date(myData2$date, "%Y-%m-%d")
#myData2$daylevel1 <- factor( weekdays(myData2$date) )
myData2$daylevel <- factor( weekdays(myData2$date) )
levels(myData2$daylevel)<- list(
        Weekday = c("Monday","Tuesday","Wednesday","Thursday","Friday"),
        Weekend = c("Saturday","Sunday")
    )
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 

```r
stepPerInt2 <- aggregate( steps ~ interval+daylevel, data=myData2, FUN=mean )

xyplot( steps ~ interval | daylevel, stepPerInt2,layout = c(1, 2), type="l",
        xlab="Interval",ylab="Number of steps") 
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
