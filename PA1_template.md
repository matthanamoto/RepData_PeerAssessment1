# Reproducible Research Assignment 1
Matt Hanamoto  
August 21, 2016  

#Loading and preprocessing the data

##Load the data (i.e. read.csv()) *Set working directory to your file's location.

```r
setwd("C:\\Users\\Matt\\Downloads")
activity.file <- unzip("activity.zip")
activity.data <- read.csv("activity.csv", stringsAsFactors=FALSE)
```

##Process/transform the data (if necessary) into a format suitable for your analysis

```r
activity.data$date = as.Date(activity.data$date)
```

#What is mean total number of steps taken per day?

##Calculate the total number of steps taken per day


```r
steps.per.day = aggregate(activity.data$steps, by = list(unique.dates = activity.data$date), FUN = sum)
steps.per.day
```

```
##    unique.dates     x
## 1    2012-10-01    NA
## 2    2012-10-02   126
## 3    2012-10-03 11352
## 4    2012-10-04 12116
## 5    2012-10-05 13294
## 6    2012-10-06 15420
## 7    2012-10-07 11015
## 8    2012-10-08    NA
## 9    2012-10-09 12811
## 10   2012-10-10  9900
## 11   2012-10-11 10304
## 12   2012-10-12 17382
## 13   2012-10-13 12426
## 14   2012-10-14 15098
## 15   2012-10-15 10139
## 16   2012-10-16 15084
## 17   2012-10-17 13452
## 18   2012-10-18 10056
## 19   2012-10-19 11829
## 20   2012-10-20 10395
## 21   2012-10-21  8821
## 22   2012-10-22 13460
## 23   2012-10-23  8918
## 24   2012-10-24  8355
## 25   2012-10-25  2492
## 26   2012-10-26  6778
## 27   2012-10-27 10119
## 28   2012-10-28 11458
## 29   2012-10-29  5018
## 30   2012-10-30  9819
## 31   2012-10-31 15414
## 32   2012-11-01    NA
## 33   2012-11-02 10600
## 34   2012-11-03 10571
## 35   2012-11-04    NA
## 36   2012-11-05 10439
## 37   2012-11-06  8334
## 38   2012-11-07 12883
## 39   2012-11-08  3219
## 40   2012-11-09    NA
## 41   2012-11-10    NA
## 42   2012-11-11 12608
## 43   2012-11-12 10765
## 44   2012-11-13  7336
## 45   2012-11-14    NA
## 46   2012-11-15    41
## 47   2012-11-16  5441
## 48   2012-11-17 14339
## 49   2012-11-18 15110
## 50   2012-11-19  8841
## 51   2012-11-20  4472
## 52   2012-11-21 12787
## 53   2012-11-22 20427
## 54   2012-11-23 21194
## 55   2012-11-24 14478
## 56   2012-11-25 11834
## 57   2012-11-26 11162
## 58   2012-11-27 13646
## 59   2012-11-28 10183
## 60   2012-11-29  7047
## 61   2012-11-30    NA
```

##Make a histogram of the total number of steps taken each day


```r
hist(steps.per.day$x, breaks=20, xlab="Number of Steps per Day", main="Frequency of Total Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

##Calculate and report the mean and median of the total number of steps taken per day

```r
Avg.User.Steps = mean(steps.per.day$x, na.rm=TRUE)
Avg.User.Steps
```

```
## [1] 10766.19
```

```r
Median.User.Steps = median(steps.per.day$x, na.rm=TRUE)
Median.User.Steps
```

```
## [1] 10765
```

#What is the average daily activity pattern?

##Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
steps.per.interval = aggregate(steps ~ interval, data=activity.data, mean, na.rm=TRUE)

plot(steps.per.interval$interval, steps.per.interval$steps, type = "l", main = "Average Steps per 5 Minute Interval", xlab = "5 Min Interval", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

##Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
steps.per.interval[which.max(steps.per.interval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

#Imputing missing values

##Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

##Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity.data$steps))
```

```
## [1] 2304
```

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

##Create a new dataset that is equal to the original dataset but with the missing data filled in. (mean of 5-minute interval)


```r
activity.data.no.na = activity.data
avg.steps.per.interval = mean(steps.per.interval$steps, na.rm=TRUE)
activity.data.no.na[is.na(activity.data.no.na)] = avg.steps.per.interval
```

##Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
steps.per.day.no.na = aggregate(activity.data.no.na$steps, by = list(unique.dates = activity.data.no.na$date), FUN = sum)

hist(steps.per.day.no.na$x, breaks=20, xlab="Number of Steps per Day", main="Frequency of Total Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

#Do these values differ from the estimates from the first part of the assignment? 

##What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
new.mean = mean(steps.per.day.no.na$x)
new.mean
```

```
## [1] 10766.19
```

```r
new.median = median(steps.per.day.no.na$x)
new.median
```

```
## [1] 10766.19
```

##Are there differences in activity patterns between weekdays and weekends?


```r
Avg.User.Steps
```

```
## [1] 10766.19
```

```r
new.mean
```

```
## [1] 10766.19
```

```r
Median.User.Steps
```

```
## [1] 10765
```

```r
new.median
```

```
## [1] 10766.19
```

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')

activity.data.no.na$wDay <- factor((weekdays(activity.data.no.na$date) %in% weekdays1),levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))
```

##Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
steps.per.interval.no.na = aggregate(steps ~ interval + wDay, data=activity.data.no.na, mean)

library("lattice")
xyplot(steps ~ interval | factor(wDay), data = steps.per.interval.no.na, type = "l", xlab="Interval", ylab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
