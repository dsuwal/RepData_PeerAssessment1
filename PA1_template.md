# Reproducible Research  Assessment 1

## Loading and preprocessing the data

```r
setwd("C:/WorkSpace/DataScience/5.ReproducibleResearch/Project/Assignment1")
data <- read.csv("Data/activity.csv")
```
## What is mean total number of steps taken per day?

```r
library(ggplot2)
TotalSteps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(TotalSteps, binwidth=1000, xlab="Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
mean(TotalSteps, na.rm=TRUE)
```

```
## [1] 9354.23
```

```r
median(TotalSteps, na.rm=TRUE)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

```r
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval), 
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) + geom_line() + xlab("5-minute interval") +
    ylab("Average number of steps taken")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
averages[which.max(averages$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
MissingValues <- is.na(data$steps)
# Total Number of Missing values in the data set 
table(MissingValues)
```

```
## MissingValues
## FALSE  TRUE 
## 15264  2304
```

All of the missing values are filled in with mean value of its 5-minute interval.


```r
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```
Histogram of the total number of steps taken each day after missing values are imputed

```r
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="Total number of steps taken each day")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
mean(total.steps)
```

```
## [1] 10766.19
```

```r
median(total.steps)
```

```
## [1] 10766.19
```

Mean and median values are higher after imputing missing data. The reason is
that in the original data, there are some days with `steps` values `NA` for 
any `interval`. The total number of steps taken in such days are set to 0s by
default. However, after replacing missing `steps` values with the mean `steps`
of associated `interval` value, these 0 values are removed from the histogram
of total number of steps taken each day.

## Are there differences in activity patterns between weekdays and weekends?
First, let's find the day of the week for each measurement in the dataset. In
this part, we use the dataset with the filled-in values.


```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
```

Now, let's make a panel plot containing plots of average number of steps taken
on weekdays and weekends.

```r
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

