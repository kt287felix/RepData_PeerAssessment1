---
title: "Reproducible Research: Peer Assessment 1"
author: ""
date: "2026-04-26"
output:
  html_document:
    keep_md: true
---



---

## Loading and preprocessing the data


``` r
# Load the data (activity.csv must be in the working directory)
activity <- read.csv("activity.csv")

# Convert date column from character to Date type
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")

# Inspect the data
head(activity)
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

``` r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

``` r
summary(activity)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

The dataset contains **17568** observations across three variables:
`steps`, `date`, and `interval`.

---

## What is the mean total number of steps taken per day?

*Missing values (`NA`) are ignored in this section.*


``` r
library(ggplot2)

# Calculate total steps per day
steps_per_day <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
```


``` r
# Histogram of total steps per day
ggplot(steps_per_day, aes(x = steps)) +
  geom_histogram(binwidth = 2000, fill = "steelblue", color = "white") +
  labs(
    title = "Histogram of Total Steps Taken Each Day",
    x     = "Total Steps per Day",
    y     = "Frequency"
  ) +
  theme_minimal()
```

![](PA1_template--1-_files/figure-html/histogram_steps-1.png)<!-- -->


``` r
mean_steps   <- mean(steps_per_day$steps)
median_steps <- median(steps_per_day$steps)

cat("Mean total steps per day:  ", round(mean_steps, 2), "\n")
```

```
## Mean total steps per day:   10766.19
```

``` r
cat("Median total steps per day:", median_steps, "\n")
```

```
## Median total steps per day: 10765
```

- **Mean** total number of steps taken per day: **10,766.19**
- **Median** total number of steps taken per day: **10,765**

---

## What is the average daily activity pattern?


``` r
# Compute average steps per 5-minute interval across all days
interval_avg <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)

# Time series plot
ggplot(interval_avg, aes(x = interval, y = steps)) +
  geom_line(color = "steelblue", linewidth = 0.8) +
  labs(
    title = "Average Number of Steps per 5-Minute Interval",
    x     = "5-Minute Interval",
    y     = "Average Number of Steps"
  ) +
  theme_minimal()
```

![](PA1_template--1-_files/figure-html/time_series-1.png)<!-- -->


``` r
max_interval  <- interval_avg$interval[which.max(interval_avg$steps)]
max_steps_val <- round(max(interval_avg$steps), 2)

cat("Interval with maximum average steps:", max_interval, "\n")
```

```
## Interval with maximum average steps: 835
```

``` r
cat("Average steps at that interval:     ", max_steps_val, "\n")
```

```
## Average steps at that interval:      206.17
```

The 5-minute interval that contains the maximum average number of steps across all days
is interval **835**, with an average of **206.17** steps.

---

## Imputing missing values


``` r
# Total number of rows with missing step values
total_na <- sum(is.na(activity$steps))
cat("Total number of missing values (rows with NA):", total_na, "\n")
```

```
## Total number of missing values (rows with NA): 2304
```

The total number of rows with missing values (`NA`) is **2304**.

**Imputation strategy:** Each missing `steps` value is replaced with the
**mean number of steps for that specific 5-minute interval**, averaged across
all non-missing days. This preserves the typical daily activity rhythm and is
a simple yet effective approach.


``` r
# Build a named lookup: interval -> mean steps
interval_means <- setNames(interval_avg$steps, interval_avg$interval)

# Copy original dataset and fill NAs
activity_imputed <- activity
na_rows <- is.na(activity_imputed$steps)
activity_imputed$steps[na_rows] <- interval_means[
  as.character(activity_imputed$interval[na_rows])
]

# Confirm no NAs remain
cat("Missing values remaining after imputation:", sum(is.na(activity_imputed$steps)), "\n")
```

```
## Missing values remaining after imputation: 0
```


``` r
# Recalculate total steps per day using imputed data
steps_per_day_imp <- aggregate(steps ~ date, data = activity_imputed, FUN = sum)

# Histogram after imputation
ggplot(steps_per_day_imp, aes(x = steps)) +
  geom_histogram(binwidth = 2000, fill = "darkorange", color = "white") +
  labs(
    title = "Total Steps Taken Each Day (After Imputation)",
    x     = "Total Steps per Day",
    y     = "Frequency"
  ) +
  theme_minimal()
```

![](PA1_template--1-_files/figure-html/imputed_histogram-1.png)<!-- -->


``` r
mean_imp   <- mean(steps_per_day_imp$steps)
median_imp <- median(steps_per_day_imp$steps)

cat("Mean total steps per day (imputed):  ", round(mean_imp, 2), "\n")
```

```
## Mean total steps per day (imputed):   10766.19
```

``` r
cat("Median total steps per day (imputed):", round(median_imp, 2), "\n")
```

```
## Median total steps per day (imputed): 10766.19
```

| Metric | Before Imputation | After Imputation |
|--------|:-----------------:|:----------------:|
| Mean   | 10,766.19 | 10,766.19 |
| Median | 10,765 | 10,766.19 |

**Impact of imputation:** Filling missing values with interval means increases
the count of days clustered around the mean — the central bar of the histogram
grows taller. The **mean** is unchanged (filling with means cannot shift the
mean), while the **median** shifts slightly upward to coincide with the mean.
Previously all-`NA` days now carry a realistic daily total, making the
distribution more symmetric.

---

## Are there differences in activity patterns between weekdays and weekends?

*This section uses the imputed dataset.*


``` r
# Create a two-level factor: "weekday" or "weekend"
activity_imputed$day_type <- ifelse(
  weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"),
  "weekend",
  "weekday"
)
activity_imputed$day_type <- factor(
  activity_imputed$day_type,
  levels = c("weekday", "weekend")
)

# Average steps per interval and day type
interval_daytype <- aggregate(
  steps ~ interval + day_type,
  data = activity_imputed,
  FUN  = mean
)
```


``` r
# Panel plot: weekday vs weekend
ggplot(interval_daytype, aes(x = interval, y = steps, color = day_type)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ day_type, ncol = 1) +
  scale_color_manual(values = c("weekday" = "steelblue", "weekend" = "darkorange")) +
  labs(
    title = "Average Steps per 5-Minute Interval: Weekdays vs Weekends",
    x     = "5-Minute Interval",
    y     = "Average Number of Steps"
  ) +
  theme_minimal() +
  theme(legend.position = "none")
```

![](PA1_template--1-_files/figure-html/panel_plot-1.png)<!-- -->

**Observations:**

- On **weekdays**, there is a pronounced activity spike around interval 835
  (approximately 8:35 AM), likely corresponding to a morning commute or
  exercise routine, followed by lower, relatively flat activity for the rest
  of the day.
- On **weekends**, activity is more evenly spread across the day, with several
  moderate peaks during daytime hours, reflecting a more flexible and active
  daily pattern.
- Overall, **weekends show higher sustained activity** throughout the day
  compared to weekdays, even though the single highest peak belongs to weekdays.
