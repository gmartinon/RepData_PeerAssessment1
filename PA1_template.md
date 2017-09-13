# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

First, we load the dplyr, ggplot2, lubridate and scales packages. The latter is appreciated for its date_format() function that we will use later on. As for lubridate, we use its wday() function at the very end of the project.

```r
library(dplyr)
library(ggplot2)
library(scales)
library(lubridate)
```

Then we read the data file.

```r
df <- read.csv("activity.csv")
```

Finally, we modify the interval column so as to get 4-digit numbers.

```r
df <- mutate(df, interval = formatC(interval, width = 4, flag = 0))
head(df)
```

```
##   steps       date interval
## 1    NA 2012-10-01     0000
## 2    NA 2012-10-01     0005
## 3    NA 2012-10-01     0010
## 4    NA 2012-10-01     0015
## 5    NA 2012-10-01     0020
## 6    NA 2012-10-01     0025
```

## What is mean total number of steps taken per day?

We first remove lines where steps is NA, we then group the data by *date* and compute the total number of steps for each date. This is done by the following combination of dplyr commands

```r
sumsteps <- df %>%
   filter(!is.na(steps)) %>%
   group_by(date) %>%
   summarise(nsteps = sum(steps))
head(sumsteps)
```

```
## # A tibble: 6 x 2
##         date nsteps
##       <fctr>  <int>
## 1 2012-10-02    126
## 2 2012-10-03  11352
## 3 2012-10-04  12116
## 4 2012-10-05  13294
## 5 2012-10-06  15420
## 6 2012-10-07  11015
```

We can then plot a histogram of the daily number of steps with ggplot.

```r
ggplot(sumsteps, aes(x = nsteps)) +
   geom_histogram(binwidth = 1000, fill = "orange", col = "blue", alpha = 0.8) +
   labs(x = "total number of steps per day") +
   ggtitle("Distribution of the daily number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

The mean and median of the daily number of steps are given respectively by

```r
c(mean(sumsteps$nsteps), median(sumsteps$nsteps))
```

```
## [1] 10766.19 10765.00
```

## What is the average daily activity pattern?

This time we group the (filtered) data set by *interval* and take the mean of the number of steps for each interval (i.e. across all days). This is performed with the following chain of dplyr commands

```r
avgsteps <- df %>%
   filter(!is.na(steps)) %>%
   group_by(interval) %>%
   summarise(msteps = mean(steps))
head(avgsteps)
```

```
## # A tibble: 6 x 2
##   interval    msteps
##      <chr>     <dbl>
## 1     0000 1.7169811
## 2     0005 0.3396226
## 3     0010 0.1320755
## 4     0015 0.1509434
## 5     0020 0.0754717
## 6     0025 2.0943396
```

We can then plot this result as a time series. In order to improve readability, we can convert the 4-digits interval into a datetime object. This is where the date_format() function of the scales package comes into play.

```r
ggplot(avgsteps, aes(x = as.POSIXct(interval, format = "%H%M"), y = msteps)) +
   geom_line(colour = "red") +
   scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone()), date_breaks = "4 hour") +
   labs(x = "time intervals (one every 5 minutes)", y = "average number of steps") +
   ggtitle("Distribution of the average number of steps along the day")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

The interval in which the maximum number of steps is taken (on average) is

```r
with(avgsteps, interval[which.max(msteps)])
```

```
## [1] "0835"
```

## Imputing missing values

The total number of rows with NAs is given by

```r
sum(!complete.cases(df))
```

```
## [1] 2304
```

Our strategy to fill in the NAs is the following. Each time we encounter an NA in the steps column, we replace it by the average value of the corresponding *interval*. In order to do that, we find useful to add a column displaying, for each line, the associated average accross all days for the corresponding interval (which was already computed in the previous section).
This is done with the following bunch of dplyr commands

```r
df2 <- df %>%
   group_by(interval) %>%
   mutate(mean_steps_by_interval = mean(steps, na.rm = TRUE)) %>%
   ungroup()
head(df2)
```

```
## # A tibble: 6 x 4
##   steps       date interval mean_steps_by_interval
##   <int>     <fctr>    <chr>                  <dbl>
## 1    NA 2012-10-01     0000              1.7169811
## 2    NA 2012-10-01     0005              0.3396226
## 3    NA 2012-10-01     0010              0.1320755
## 4    NA 2012-10-01     0015              0.1509434
## 5    NA 2012-10-01     0020              0.0754717
## 6    NA 2012-10-01     0025              2.0943396
```

It now remains only to substitute the NA by the mean_steps_by_interval value on the same line.

```r
na <- is.na(df$steps)
df2$steps[na] <- df2$mean_steps_by_interval[na]
head(df2)
```

```
## # A tibble: 6 x 4
##       steps       date interval mean_steps_by_interval
##       <dbl>     <fctr>    <chr>                  <dbl>
## 1 1.7169811 2012-10-01     0000              1.7169811
## 2 0.3396226 2012-10-01     0005              0.3396226
## 3 0.1320755 2012-10-01     0010              0.1320755
## 4 0.1509434 2012-10-01     0015              0.1509434
## 5 0.0754717 2012-10-01     0020              0.0754717
## 6 2.0943396 2012-10-01     0025              2.0943396
```

We then repeat the analysis of the previous sections, this time with the filled-in data set.

```r
sumsteps2 <- df2 %>% filter(!is.na(steps)) %>%
   group_by(date) %>%
   summarise(nsteps = sum(steps))
head(sumsteps2)
```

```
## # A tibble: 6 x 2
##         date   nsteps
##       <fctr>    <dbl>
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

And plot the results. It is noteworthy that, compared to the previous histogram, we largely increased the counts of the central peaks around the mean. This is due to our particular strategy of NA replacement.

```r
ggplot(sumsteps2, aes(x = nsteps)) +
   geom_histogram(binwidth = 1000, fill = "orange", col = "blue", alpha = 0.8) +
   labs(x = "total number of steps per day") +
   ggtitle("Distribution of the daily number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

The mean and median of the daily number of steps are now identical. This is again helped by our NA replacement strategy.

```r
c(mean(sumsteps2$nsteps), median(sumsteps2$nsteps))
```

```
## [1] 10766.19 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?

We start with the filled-in data set, and add a day column. In order to discriminate weekday from weekend, we use the wday function (lubridate), which returns the number of the day (1 for Sunday to 7 for Saturday). Applying this function on the date column, we can then check if the day is Saturday or Sunday, and give an explicit name to the factor levels instead of (FALSE, TRUE). 

```r
df2 <- mutate(df2, day = factor(wday(df2$date) %in% c(1,7), labels = c("weekday", "weekend")))
head(df2)
```

```
## # A tibble: 6 x 5
##       steps       date interval mean_steps_by_interval     day
##       <dbl>     <fctr>    <chr>                  <dbl>  <fctr>
## 1 1.7169811 2012-10-01     0000              1.7169811 weekday
## 2 0.3396226 2012-10-01     0005              0.3396226 weekday
## 3 0.1320755 2012-10-01     0010              0.1320755 weekday
## 4 0.1509434 2012-10-01     0015              0.1509434 weekday
## 5 0.0754717 2012-10-01     0020              0.0754717 weekday
## 6 2.0943396 2012-10-01     0025              2.0943396 weekday
```

We then repeat the analysis of the previous sections, but this time we group the data by day *and* interval. This is done by the following buckets of dplyr commands

```r
avgsteps2 <- df2 %>%
   group_by(day,interval) %>%
   summarise(msteps = mean(steps))
head(avgsteps2)
```

```
## # A tibble: 6 x 3
## # Groups:   day [1]
##       day interval     msteps
##    <fctr>    <chr>      <dbl>
## 1 weekday     0000 2.25115304
## 2 weekday     0005 0.44528302
## 3 weekday     0010 0.17316562
## 4 weekday     0015 0.19790356
## 5 weekday     0020 0.09895178
## 6 weekday     0025 1.59035639
```

We then plot the result using a facet argument that will split the plots in two panels, one for weekdays and another for weekends. There is clearly a pattern associated to weekdays: a peak in the morning around 8:35 AM.

```r
ggplot(avgsteps2, aes(x = as.POSIXct(interval, format = "%H%M"), y = msteps)) +
   geom_line(colour = "red") +
   facet_grid( ~ day) +
   scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone()), date_breaks = "4 hour") +
   labs(x = "time intervals (every 5 minutes)", y = "average number of steps") +
   ggtitle("Distribution of the average number of steps along the week")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
