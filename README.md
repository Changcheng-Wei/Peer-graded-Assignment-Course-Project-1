Peer-graded Assignment: Course Project 1
Repo

Valid GitHub URL

At least one commit beyond the original fork

Valid SHA-1

SHA-1 corresponds to a specific commit

Commit containing full submission

Code for reading in the dataset and/or processing the data

Histogram of the total number of steps taken each day

Mean and median number of steps taken each day

Time series plot of the average number of steps taken

The 5-minute interval that, on average, contains the maximum number of steps

Code to describe and show a strategy for imputing missing data

Histogram of the total number of steps taken each day after missing values are imputed

Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

All of the R code needed to reproduce the results (numbers, plots, etc.) in the report r: “Peer-graded Assignment: Course Project 1”

Load the data (i.e.  read.csv() read.csv()start color red, start verbatim, read.csv(), end verbatim, end color red) Process/transform the data (if necessary) into a format suitable for your analysis If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

library(plyr) # Load dplyr for data manipulation
library(dplyr) # Load dplyr for data manipulation
## 
## Attaching package: 'dplyr'

## The following objects are masked from 'package:plyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize

## The following objects are masked from 'package:stats':
## 
##     filter, lag

## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
activity <- read.csv("C:\\Users\\Changcheng\\Documents\\activity.csv")  # Read the dataset
activity$date <- as.Date(activity$date) # Convert date column to Date type
activity$interval <- as.numeric(activity$interval) # Convert interval column to numeric
head(activity) # Display the first few rows of the dataset
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
Calculate the total number of steps taken per day

If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

# Calculate total steps per day
total_steps_per_day <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
head(total_steps_per_day) # Display the first few rows of total steps per day)
##         date steps
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
# Create histogram of total steps per day
library(ggplot2) # Load ggplot2 for plotting
ggplot(total_steps_per_day, aes(x = steps)) +
  geom_histogram(fill = "blue", color = "black") +
  labs(title = "Total Steps per Day", x = "Total Steps", y = "Frequency") +
  theme_minimal() 
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.


Calculate and report the mean and median of the total number of steps taken per day

mean_steps <- mean(total_steps_per_day$steps, na.rm = TRUE) # Calculate mean
median_steps <- median(total_steps_per_day$steps, na.rm = TRUE) # Calculate median
# Print mean and median
print(paste("Mean steps per day:", mean_steps))
## [1] "Mean steps per day: 10766.1886792453"
print(paste("Median steps per day:", median_steps))
## [1] "Median steps per day: 10765"
Make a time series plot (i.e.  type = “l” type = “l”start color red, start verbatim, type = “l”, end verbatim, end color red) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

library(ggplot2) # Load ggplot2 for plotting
# Calculate average steps per interval
average_steps_per_interval <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
# Create time series plot
ggplot(average_steps_per_interval, aes(x = interval, y = steps)) +
  geom_line(color = "blue") +
  labs(title = "Average Steps per Interval", x = "5-Minute Interval", y = "Average Steps") +
  theme_minimal()
 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NA NAstart color red, start verbatim, NA, end verbatim, end color reds) Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

missing_values <- sum(is.na(activity$steps)) # Count missing values
# Print total missing values
print(paste("Total missing values:", missing_values))
## [1] "Total missing values: 2304"
# Impute missing values with the mean of the corresponding interval
activity_imputed <- activity
activity_imputed$steps[is.na(activity_imputed$steps)] <- ave(activity_imputed$steps, activity_imputed$interval, FUN = function(x) mean(x, na.rm = TRUE))
## Warning in activity_imputed$steps[is.na(activity_imputed$steps)] <-
## ave(activity_imputed$steps, : number of items to replace is not a multiple of
## replacement length
# Calculate total steps per day after imputation
total_steps_per_day_imputed <- aggregate(steps ~ date, data = activity_imputed, FUN = sum)
# Create histogram of total steps per day after imputation
ggplot(total_steps_per_day_imputed, aes(x = steps)) +
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  labs(title = "Total Steps per Day (Imputed)", x = "Total Steps", y = "Frequency") +
  theme_minimal()


# Calculate mean and median after imputation
mean_steps_imputed <- mean(total_steps_per_day_imputed$steps, na.rm = TRUE) # Calculate mean
median_steps_imputed <- median(total_steps_per_day_imputed$steps, na.rm = TRUE) # Calculate median
For this part the weekdays() weekdays()start color red, start verbatim, weekdays(), end verbatim, end color red function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e.  type = “l” type = “l”start color red, start verbatim, type = “l”, end verbatim, end color red) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

library(lubridate) # Load lubridate for date manipulation
## 
## Attaching package: 'lubridate'

## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
# Create a new variable for weekday/weekend
activity_imputed$day_type <- ifelse(weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
# Calculate average steps per interval for weekdays and weekends
average_steps_per_day_type <- aggregate(steps ~ interval + day_type, data = activity_imputed, FUN = mean)
# Create a factor for day_type
average_steps_per_day_type$day_type <- factor(average_steps_per_day_type$day_type, levels = c("weekday", "weekend"))

# Create panel plot
ggplot(average_steps_per_day_type, aes(x = interval, y = steps)) +
  theme_bw()+
  geom_line(color = "blue") +
  facet_grid(day_type~.)+
  labs(title = "Average Steps per Interval (Weekday vs Weekend)", x = "5-Minute Interval", y = "Average Steps")
# Peer-graded-Assignment-Course-Project-1
