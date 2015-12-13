---
title: "activity_data"
author: "Leo"
date: "December 10, 2015"
output: html_document
---

## Loading

First of all, let's load the data.


```r
act = read.csv(file = "/home/leo/courses/reproducible/ass1/activity.csv")
summary(act)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

## Histogram

To show the histogram, we need to count the total number of steps in each day.


```r
# get all the different dates in the data set
dates = as.vector(unique(act$date))

# store the per-day totals in a vector
steps_by_day = c()

for(i in 1:length(dates)){
    # subset our data by date and then sum all the values in $steps
    steps_by_day[i] = sum(subset(act, as.Date(date) == as.Date(dates[i]))$steps)
}
# careful: steps_by_day is now a 1x61 matrix. To store it normally, use as.vector
steps_by_day = as.vector(steps_by_day)

steps_by_day
```

```
##  [1]    NA   126 11352 12116 13294 15420 11015    NA 12811  9900 10304
## [12] 17382 12426 15098 10139 15084 13452 10056 11829 10395  8821 13460
## [23]  8918  8355  2492  6778 10119 11458  5018  9819 15414    NA 10600
## [34] 10571    NA 10439  8334 12883  3219    NA    NA 12608 10765  7336
## [45]    NA    41  5441 14339 15110  8841  4472 12787 20427 21194 14478
## [56] 11834 11162 13646 10183  7047    NA
```

Now we can show the histogram.


```r
# hist() takes care of NA values.
hist(steps_by_day)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

The **mean** is 1.0766189 &times; 10<sup>4</sup> steps per day, and the **median**, 10765 steps per day.

## Average daily acitivity pattern

We are now interested in the average activity pattern. We first collect all averages from each day.


```r
# extract all possible intervals
all_intervals = unique(act$interval)

# compute the average for each interval
mean_int = c()
i = 1
while (i <= length(all_intervals)) {
  # take care to omit NA values
  mean_int[i] = mean(na.omit(subset(act, interval == all_intervals[i])$steps))
  i = i + 1
}

summary(mean_int)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.486  34.110  37.380  52.830 206.200
```

And now we're ready to plot.


```r
plot.ts(mean_int, xlab = "Interval", ylab = "Mean steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

The interval with the **maximum** average number of steps is 835, with 206.1698113 steps on average.

## Missing values

There are a few missing values in our data.


```r
head(act)
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

The total number of missing values in our data is 2304. To fill in these missing data, we will use the average for each interval, as computed before.


```r
fill = act
for (i in 1:length(fill$steps)) {
     if (is.na(act[i,1])) {
       index = match(fill[i,3], unique(act$interval))
       fill[i,1] = mean_int[index]
     }
}

head(fill)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

There are now 0 missing values, and we're ready to see the new histogram.


```r
# store the new per-day totals in a vector
steps_by_day_fill = c()

for(i in 1:length(dates)){
    # subset our filled-in data by date and then sum all the values in $steps
    steps_by_day_fill[i] = sum(subset(fill, as.Date(date) == as.Date(dates[i]))$steps)
}
# careful: steps_by_day is now a 1x61 matrix. To store it normally, use as.vector
steps_by_day_fill = as.vector(steps_by_day_fill)

hist(steps_by_day_fill)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

In our filled-in data, the **mean** is 1.0766189 &times; 10<sup>4</sup> steps, and the **meadian** is 1.0766189 &times; 10<sup>4</sup> steps.
In this case, our filling-in scheme leaves the mean exactly as it was before (which makes sense, since we used the mean of each interval to replace missing values) and the median has moved up a little bit (around 1.2 steps).

## Weekdays

Now we are interested in comparing activity between weekdays and weekends. First, we add a new factor variable to our data to identify weekdays and weekend days.


```r
# create a list of weekend days to compare to
weekend_days = c("Saturday", "Sunday")

# create a vector to store the "weekday"/"weekend" variable.
weekday = c()

for (i in 1:length(act$steps)) {
  # weekdays() returns a string with the day name of the given date
  if (is.na(match(weekdays(as.Date(act[i,2])), weekend_days))) {
       weekday[i] = "weekday"
   }
    else {
        weekday[i] = "weekend"
    }
}

# now we can add these data as a whole column
act_day = act
act_day$weekday = weekday
head(act_day)
```

```
##   steps       date interval weekday
## 1    NA 2012-10-01        0 weekday
## 2    NA 2012-10-01        5 weekday
## 3    NA 2012-10-01       10 weekday
## 4    NA 2012-10-01       15 weekday
## 5    NA 2012-10-01       20 weekday
## 6    NA 2012-10-01       25 weekday
```

Now we can subset our data by weekday/weekend, compute the average and show a simultaneous plot.


```r
# compute the average for each interval
mean_day = c()
mean_end = c()

i = 1
while (i <= length(all_intervals)) {
  # subset by weekday, then grab all intervals across days and apply mean()
  # take care to omit NA values
  mean_day[i] = mean(na.omit(subset(subset(act_day, weekday == "weekday"), interval == all_intervals[i])$steps))
  mean_end[i] = mean(na.omit(subset(subset(act_day, weekday == "weekend"), interval == all_intervals[i])$steps))
  i = i + 1
}

summary(mean_day)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.218  23.970  35.340  51.870 234.100
```

```r
summary(mean_end)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   1.107  32.040  43.080  75.570 175.000
```

```r
par(mfrow=c(2,1))
plot.ts(mean_day, xlab = "Interval", ylab = "Mean steps on weekdays")
plot.ts(mean_end, xlab = "Interval", ylab = "Mean steps on weekends")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 
