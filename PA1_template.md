# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
    #unzip("activity.zip")
    data <- read.csv("activity.csv", header = T)
    str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```


```r
## Convert Date
    data$date <- as.Date(data$date, format = "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset. 
Make a histogram of the total number of steps taken each day

```r
# Convert to daily aggregates
    daily_steps <- aggregate(steps ~ date, data=data, sum)    
    hist(daily_steps$steps, breaks=20, main="Histogram of Steps per Day")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

Calculate and report the mean and median total number of steps taken per day

```r
    mean <- mean(daily_steps$steps)
    median <- median(daily_steps$steps)
```
- Mean total number of steps/day : 1.0766 &times; 10<sup>4</sup>
- Median steps/day : 10765


## What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
    daily_activity_pattern <- aggregate(steps ~ interval, data=data, FUN="mean")
    plot(daily_activity_pattern,
         type = "l", main="Time-Series Plot", 
         xlab ="5 min intervals", ylab="Ave no. of steps taken across all days")
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-5.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
    max <- daily_activity_pattern[which.max(daily_activity_pattern$steps), ]
```
- 835 interval contains the maximum number of 206.1698 steps 

## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
    missing <- sum(is.na(data$steps))
```
- There are 2304 rows with NAs.


For filling in all of the missing values in the dataset, I chose to use the mean for that 5-minute interval. Created a new dataset ("imputed_data") that is equal to the original dataset but with the missing data filled in.


```r
    imputed_data <- merge(data, daily_activity_pattern, by="interval")
    names(imputed_data) <- c("interval", "steps", "date", "mean_steps")

    #if there are missing values ("NA"), replace them with the mean steps of that interval
    imputed_data$steps <- ifelse(is.na(imputed_data$steps), imputed_data$mean_steps, imputed_data$steps)
```

Make a histogram of the total number of steps taken each day 

```r
    daily_steps_imputed <- aggregate(steps ~ date, data = imputed_data, FUN = "sum")    
    hist(daily_steps_imputed$steps, breaks=20, main="Histogram of Steps per Day(imputed)")
```

![plot of chunk unnamed-chunk-9](./PA1_template_files/figure-html/unnamed-chunk-9.png) 

Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact 
of imputing missing data on the estimates of the total daily number of steps?

```r
    imputed_mean <- mean(daily_steps_imputed$steps)
    imputed_median <- median(daily_steps_imputed$steps)
```
- Imputed Mean :1.0766 &times; 10<sup>4</sup> vs Original Mean 1.0766 &times; 10<sup>4</sup>
- Imputed Median :1.0766 &times; 10<sup>4</sup> vs Original Median 10765
- Conclusion : **No difference** and **no impact** on the estimates

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
    #assign the day of week to a new variable "day"
    data$day <- weekdays(as.Date(data$date))

    #reassign variable "day" to a factor variable
    data$day <- as.factor (ifelse(data$day %in% c("Saturday", "Sunday"), "weekend", "weekday"))
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
    par(mfrow = c(2, 1))
    for (dayType in c("weekday", "weekend")){
          ave_steps <- aggregate(steps ~ interval, 
                                 data = data, 
                                 subset = (data$day==dayType),
                                 FUN = "mean")
          plot( ave_steps,
                type = "l", main= dayType, 
                xlab ="5 min intervals", ylab="Ave no. of steps taken")  
    }
```

![plot of chunk unnamed-chunk-12](./PA1_template_files/figure-html/unnamed-chunk-12.png) 
