# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
unzip <- unzip("activity.zip")
data <- read.csv(unzip)
```


## What is mean total number of steps taken per day?
### 1.Calculate the total number of steps taken per day

```r
total.step.daily <- tapply(data$steps, format(as.Date(data$date), '%Y-%m-%d'), sum)
total.step.daily <- as.data.frame(total.step.daily)         
```

### 2.Make a histogram of the total number of steps taken each day

```r
hist(total.step.daily$total.step.daily, 
	    xlab = "total number of steps",
      main = "Total Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

### 3.Calculate and report the mean and median of the total number of steps taken per day

```r
mean <- mean(total.step.daily$total.step.daily, na.rm = TRUE)
median <- median(total.step.daily$total.step.daily, na.rm = TRUE)

msg.old <- paste0("Mean Steps Taken Each Day: ", round(mean, 2), "\n",
		"Median Steps Taken Each Day: ", median, "\n")
cat(msg.old)
```

```
## Mean Steps Taken Each Day: 10766.19
## Median Steps Taken Each Day: 10765
```

## What is the average daily activity pattern?
### 1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
mean.step.interval <-aggregate(data$steps, by=list(data$interval), FUN=mean, na.rm=TRUE)
colnames(mean.step.interval) <- c("interval", "mean.step")

plot(mean.step.interval$interval, mean.step.interval$mean.step, type = "l",
     xlab = "five-minute interval",
     ylab = "mean number of steps",
     main="Mean Number of Steps By Interval") 
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

### 2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
max.mean.step <- mean.step.interval[which.max(mean.step.interval$mean.step), ]

msg <- paste0("The 5-minute interval #", max.mean.step[, 1], 
      ", on average across all the days in the dataset, ", "\n", 
	"contains the maximum number of steps: ", 
	round(max.mean.step[, 2], 0), " steps", "\n")
cat(msg)
```

```
## The 5-minute interval #835, on average across all the days in the dataset, 
## contains the maximum number of steps: 206 steps
```

## Imputing missing values
### 1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
missing <- is.na(data)
missing.count <- colSums(missing)
print(missing.count)
```

```
##    steps     date interval 
##     2304        0        0
```

### 2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
print("The strategy is to impute missing values with mean for that 5-minute interval.")
```

```
## [1] "The strategy is to impute missing values with mean for that 5-minute interval."
```

### 3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data.new <- data
for(i in seq(0, max(data.new$interval), 5)){
	data.new[is.na(data.new[ ,1]) & data.new$interval == i, 1] <- mean(data[data$interval == i, 1], na.rm = TRUE)
}
```
### 4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
total.step.daily.new <- tapply(data.new$steps, format(as.Date(data.new$date), '%Y-%m-%d'), sum)
total.step.daily.new <- as.data.frame(total.step.daily.new)         

hist(total.step.daily.new$total.step.daily, 
	xlab = "total number of steps",
      main = "Total Number of Steps Taken Each Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

```r
mean.new <- mean(total.step.daily.new$total.step.daily, na.rm = TRUE)
median.new <- median(total.step.daily.new$total.step.daily, na.rm = TRUE)

msg.new <- paste0("New Mean Steps Taken Each Day: ", round(mean.new, 2), "\n",
		"New Median Steps Taken Each Day: ", round(median.new, 2), "\n",
		"Mean value in original dataset and current dataset are the same.", "\n",
		"Median value in current dataset increased and is now same as mean value.", "\n",
		"Imputing missing value with mean for that 5-minute interval", "\n",
		"won't change mean value, but will make the dataset less skewed.", "\n")
cat(msg.new)
```

```
## New Mean Steps Taken Each Day: 10766.19
## New Median Steps Taken Each Day: 10766.19
## Mean value in original dataset and current dataset are the same.
## Median value in current dataset increased and is now same as mean value.
## Imputing missing value with mean for that 5-minute interval
## won't change mean value, but will make the dataset less skewed.
```

## Are there differences in activity patterns between weekdays and weekends?
### 1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
data.new$date <- as.Date(data.new$date)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
data.new$wDay <- factor((weekdays(data.new$date) %in% weekdays1), 
         levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 
head(data.new)
```

```
##       steps       date interval    wDay
## 1 1.7169811 2012-10-01        0 weekday
## 2 0.3396226 2012-10-01        5 weekday
## 3 0.1320755 2012-10-01       10 weekday
## 4 0.1509434 2012-10-01       15 weekday
## 5 0.0754717 2012-10-01       20 weekday
## 6 2.0943396 2012-10-01       25 weekday
```

### 2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
mean.step.interval.new <-aggregate(data.new$steps, 
	by=list(data.new$interval, data.new$wDay), FUN=mean, na.rm=TRUE)
colnames(mean.step.interval.new) <- c("interval", "wday", "mean.step")

new.weekday <- mean.step.interval.new[mean.step.interval.new$wday == "weekday", ]
new.weekend <- mean.step.interval.new[mean.step.interval.new$wday == "weekend", ]

par(mfcol = c(2, 1), oma = c(0, 0, 3, 0))
	plot(	new.weekday$interval, 
		new.weekday$mean.step, type = "l", ylim = c(-10, 250),
		xlab = "five-minute interval",
     		ylab = "mean number of steps",
		main = "Weekday")

    	plot(	new.weekend$interval, 
		new.weekend$mean.step, type = "l", ylim = c(-10, 250),
     		xlab = "five-minute interval",
     		ylab = "mean number of steps",
		main = "Weekend")

	title("Mean Number of Steps By Interval", outer = TRUE, cex = 1.5)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

