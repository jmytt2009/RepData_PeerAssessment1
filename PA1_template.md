---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data


```r
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

```r
temp<-tempfile()
data_url<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(data_url, temp)
dat <- read.csv(unz(temp,"activity.csv"))
dat$date <- as.Date(dat$date)
unlink(temp)
```

## What is mean total number of steps taken per day?

- Total number of steps taken per day


```r
total_steps <- dat %>% group_by(date) %>% summarise(total_steps = sum(steps))
hist(total_steps$total_steps,
     main = "Total Number of Steps Taken Each Daty",
     xlab = "Total Step Taken Each Day", col = "light blue")
```

![](PA1_template_files/figure-html/total_number_of_steps_per_day-1.png)<!-- -->
- The mean and median of the total number of steps taken per date


```r
mm_data <- dat %>% group_by(date) %>% 
  summarise(Mean = mean(steps, na.rm = TRUE),
  Median = median(steps, na.rm = TRUE))

print(mm_data, quote = FALSE, row_names = FALSE, n = dim(mm_data)[1])
```

```
## # A tibble: 61 x 3
##    date          Mean Median
##    <date>       <dbl>  <dbl>
##  1 2012-10-01 NaN         NA
##  2 2012-10-02   0.438      0
##  3 2012-10-03  39.4        0
##  4 2012-10-04  42.1        0
##  5 2012-10-05  46.2        0
##  6 2012-10-06  53.5        0
##  7 2012-10-07  38.2        0
##  8 2012-10-08 NaN         NA
##  9 2012-10-09  44.5        0
## 10 2012-10-10  34.4        0
## 11 2012-10-11  35.8        0
## 12 2012-10-12  60.4        0
## 13 2012-10-13  43.1        0
## 14 2012-10-14  52.4        0
## 15 2012-10-15  35.2        0
## 16 2012-10-16  52.4        0
## 17 2012-10-17  46.7        0
## 18 2012-10-18  34.9        0
## 19 2012-10-19  41.1        0
## 20 2012-10-20  36.1        0
## 21 2012-10-21  30.6        0
## 22 2012-10-22  46.7        0
## 23 2012-10-23  31.0        0
## 24 2012-10-24  29.0        0
## 25 2012-10-25   8.65       0
## 26 2012-10-26  23.5        0
## 27 2012-10-27  35.1        0
## 28 2012-10-28  39.8        0
## 29 2012-10-29  17.4        0
## 30 2012-10-30  34.1        0
## 31 2012-10-31  53.5        0
## 32 2012-11-01 NaN         NA
## 33 2012-11-02  36.8        0
## 34 2012-11-03  36.7        0
## 35 2012-11-04 NaN         NA
## 36 2012-11-05  36.2        0
## 37 2012-11-06  28.9        0
## 38 2012-11-07  44.7        0
## 39 2012-11-08  11.2        0
## 40 2012-11-09 NaN         NA
## 41 2012-11-10 NaN         NA
## 42 2012-11-11  43.8        0
## 43 2012-11-12  37.4        0
## 44 2012-11-13  25.5        0
## 45 2012-11-14 NaN         NA
## 46 2012-11-15   0.142      0
## 47 2012-11-16  18.9        0
## 48 2012-11-17  49.8        0
## 49 2012-11-18  52.5        0
## 50 2012-11-19  30.7        0
## 51 2012-11-20  15.5        0
## 52 2012-11-21  44.4        0
## 53 2012-11-22  70.9        0
## 54 2012-11-23  73.6        0
## 55 2012-11-24  50.3        0
## 56 2012-11-25  41.1        0
## 57 2012-11-26  38.8        0
## 58 2012-11-27  47.4        0
## 59 2012-11-28  35.4        0
## 60 2012-11-29  24.5        0
## 61 2012-11-30 NaN         NA
```

## What is the average daily activity pattern?


```r
im_data <- dat %>% group_by(interval) %>% 
  summarise(Mean = mean(steps, na.rm = TRUE)) 

plot(x=im_data$interval, y=im_data$Mean, type = "l", 
     main = "Average Number of Steps per Interval",
     xlab = "5-minute Interval", ylab = "Number of Steps", col = "blue")
```

![](PA1_template_files/figure-html/average_day_activity-1.png)<!-- -->

```r
print(im_data[im_data$Mean == max(im_data$Mean), ])
```

```
## # A tibble: 1 x 2
##   interval  Mean
##      <int> <dbl>
## 1      835   206
```

## Imputing missing values

- Total number of missing values.


```r
summarise(dat, total = sum(is.na(dat$steps)),
          percentage = mean(is.na(dat$steps)) * 100)
```

```
##   total percentage
## 1  2304   13.11475
```

- Filling the missing steps values with the mean for that 5-minute interval.


```r
dat1 <- dat
dat1$steps <- ifelse(is.na(dat1$steps),
                     im_data$Mean[match(dat1$interval, im_data$interval)],
                     dat1$steps)
total_steps <- dat1 %>% group_by(date) %>% summarise(total_steps = sum(steps))
hist(total_steps$total_steps,
     main = "Total Number of Steps Taken Each Daty Without Missing Data",
     xlab = "Total Step Taken Each Day", col = "light green")
```

![](PA1_template_files/figure-html/filling_the_missing_steps_values-1.png)<!-- -->

- The mean and median of the total number of steps taken per date without missing data.


```r
mm_data <- dat1 %>% group_by(date) %>% 
  summarise(Mean = mean(steps, na.rm = TRUE),
  Median = median(steps, na.rm = TRUE))

print(mm_data, quote = FALSE, row_names = FALSE, n = dim(mm_data)[1])
```

```
## # A tibble: 61 x 3
##    date         Mean Median
##    <date>      <dbl>  <dbl>
##  1 2012-10-01 37.4     34.1
##  2 2012-10-02  0.438    0  
##  3 2012-10-03 39.4      0  
##  4 2012-10-04 42.1      0  
##  5 2012-10-05 46.2      0  
##  6 2012-10-06 53.5      0  
##  7 2012-10-07 38.2      0  
##  8 2012-10-08 37.4     34.1
##  9 2012-10-09 44.5      0  
## 10 2012-10-10 34.4      0  
## 11 2012-10-11 35.8      0  
## 12 2012-10-12 60.4      0  
## 13 2012-10-13 43.1      0  
## 14 2012-10-14 52.4      0  
## 15 2012-10-15 35.2      0  
## 16 2012-10-16 52.4      0  
## 17 2012-10-17 46.7      0  
## 18 2012-10-18 34.9      0  
## 19 2012-10-19 41.1      0  
## 20 2012-10-20 36.1      0  
## 21 2012-10-21 30.6      0  
## 22 2012-10-22 46.7      0  
## 23 2012-10-23 31.0      0  
## 24 2012-10-24 29.0      0  
## 25 2012-10-25  8.65     0  
## 26 2012-10-26 23.5      0  
## 27 2012-10-27 35.1      0  
## 28 2012-10-28 39.8      0  
## 29 2012-10-29 17.4      0  
## 30 2012-10-30 34.1      0  
## 31 2012-10-31 53.5      0  
## 32 2012-11-01 37.4     34.1
## 33 2012-11-02 36.8      0  
## 34 2012-11-03 36.7      0  
## 35 2012-11-04 37.4     34.1
## 36 2012-11-05 36.2      0  
## 37 2012-11-06 28.9      0  
## 38 2012-11-07 44.7      0  
## 39 2012-11-08 11.2      0  
## 40 2012-11-09 37.4     34.1
## 41 2012-11-10 37.4     34.1
## 42 2012-11-11 43.8      0  
## 43 2012-11-12 37.4      0  
## 44 2012-11-13 25.5      0  
## 45 2012-11-14 37.4     34.1
## 46 2012-11-15  0.142    0  
## 47 2012-11-16 18.9      0  
## 48 2012-11-17 49.8      0  
## 49 2012-11-18 52.5      0  
## 50 2012-11-19 30.7      0  
## 51 2012-11-20 15.5      0  
## 52 2012-11-21 44.4      0  
## 53 2012-11-22 70.9      0  
## 54 2012-11-23 73.6      0  
## 55 2012-11-24 50.3      0  
## 56 2012-11-25 41.1      0  
## 57 2012-11-26 38.8      0  
## 58 2012-11-27 47.4      0  
## 59 2012-11-28 35.4      0  
## 60 2012-11-29 24.5      0  
## 61 2012-11-30 37.4     34.1
```

## Are there differences in activity patterns between weekdays and weekends?

- Create a day_type (weekday and weekend) variable

```r
dat2 <- mutate(dat1,day_type = ifelse(weekdays(dat1$date) %in%
  c("Saturday", "Sunday"),"weekend","weekday"))

dat2$day_type <- as.factor(dat2$day_type)

im_data_weekend <- dat2 %>% filter(day_type == "weekend") %>%
  group_by(interval) %>% summarise(Mean = mean(steps))

im_data_weekday <- dat2 %>% filter(day_type == "weekday") %>%
  group_by(interval) %>% summarise(Mean = mean(steps))

par(mfrow = c(2,1))
par(mar = c(1,4.1,2.1,2.1))
weekday_max = max(im_data_weekend$Mean)
weekend_max = max(im_data_weekday$Mean)
ymax = max(c(weekend_max,weekday_max))
plot(x=im_data_weekend$interval, y=im_data_weekend$Mean, type = "l", 
     main = "weekend", xaxt = "n", ylim = c(0, ymax), 
     xlab = "", ylab = "Number of Steps", col = "blue")
par(mar = c(2.1,4.1,1.1,2.1))
plot(x=im_data_weekday$interval, y=im_data_weekday$Mean, type = "l", 
     main = "weekday", ylim = c(0, ymax),
     xlab = "5-minute Interval", ylab = "Number of Steps", col = "blue")
```

![](PA1_template_files/figure-html/create_day_type_variable-1.png)<!-- -->
