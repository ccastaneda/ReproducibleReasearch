Project Description
-------------------

This is the first project for the Reproducible Research course in Coursera's Data Science specialization track.
The purpose of the project was two prong, to answer a series of questions using data collected from a FitBit while presenting the results using R-markdown and the principles of reproducible research discussed during these first two weeks.

The following are the steps taken in this process:

### Data Description

This assignment makes use of data from a personal activity monitoring device. The device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment was downloaded from the course web site:

**Dataset**: Activity monitoring data \[52K\]

The variables included in this dataset are:

-   steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
-   date: The date on which the measurement was taken in YYYY-MM-DD format
-   interval: Identifier for the 5-minute interval in which measurement was taken

### Data Processing

The data is downloaded, unzipped and read into the dataframe "actvityData".

``` r
fileUrl<- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
fileName <- "repdata%2Fdata%2Factivity.zip"

if (!file.exists(fileName)){
    download.file(fileUrl, fileName)
  unzip(fileName)
}

activityData <- read.csv("activity.csv")
```

### What is mean total number of steps taken per day?

``` r
stepsPerDay <- aggregate(steps ~ date, activityData, sum)
hist(stepsPerDay$steps, xlab="Number of Steps",
     ylab="Interval", col="green", 
     main = paste("Total Number of Steps Per Day"))
```

![](PA1_template_files/figure-markdown_github/totalNumStepsPerDay-1.png)

``` r
stepsPerDayMean <- mean(stepsPerDay$steps)
stepsPerDayMedian <- median(stepsPerDay$steps)
```

-   The missing values, NA, in the dataset were ignored. The total number of steps taken per day is represented graphically in the above histogram.

-   The mean and median of the total number of steps taken per day are 1.076618910^{4} and 10765 respectively. These two values are almost the same which tell us the data points are evenly distributed.

### What is the average daily activity pattern?

``` r
meanStepsForEachIntAllDays <- aggregate(steps ~ interval, activityData, mean)

plot(meanStepsForEachIntAllDays$interval,
     meanStepsForEachIntAllDays$steps, type="l",
     xlab="5-minute Interval", ylab="Number of Steps",
     main ="Average Number of Steps per Interval per Day" )

maxStepsPerInterval <- meanStepsForEachIntAllDays[which.max(meanStepsForEachIntAllDays$steps),1]
maxStep<-max(meanStepsForEachIntAllDays$steps)
abline(v=maxStepsPerInterval, col="red")
```

![](PA1_template_files/figure-markdown_github/averageDailyActivity-1.png)

-   The average daily activity pattern is represented in the time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

-   The 5-minute interval, on average across all the days in the dataset, containing the maximum number of steps is 835

### Imputing missing values

``` r
library(plyr)
```

    ## Warning: package 'plyr' was built under R version 3.2.5

``` r
missingValues <- sum(is.na(activityData))

impActivityData <- transform(activityData, steps = ifelse(is.na(activityData$steps), meanStepsForEachIntAllDays$steps[match(activityData$interval, meanStepsForEachIntAllDays$interval)], activityData$steps))

impStepsPerDay <- aggregate(steps ~ date, impActivityData, sum)

hist(impStepsPerDay$steps, xlab="Number of Steps",
     ylab="Interval", col="red",
     main = paste("Total Number of Steps Per Day"))
```

![](PA1_template_files/figure-markdown_github/dealindgWithNAs-1.png)

``` r
impStepsPerDayMean <- mean(impStepsPerDay$steps)
impStepsPerDayMedian <- median(impStepsPerDay$steps)

meanDiff <- impStepsPerDayMean - stepsPerDayMean
medianDiff <- impStepsPerDayMedian - stepsPerDayMedian

totalStepsDiff <- sum(impActivityData$steps) -    sum(activityData$steps, na.rm=TRUE)
```

-   The total number of missing values, NAs, in the dataset is 2304.

-   The strategy used for filling in all of the missing values in the activityData step column was to use the mean for that 5-minute interval.

-   The histogram of the total number of steps taken each day reflects the adjusted data.

-   The mean and median total number of steps taken per day for the imputed data are 1.076618910^{4} and 1.076618910^{4} respectively. These two values are the same.

-   There is no difference between the mean values estimated with the missing data and the mean estimated with the imputed data 0 and the difference between the median values is 1.1886792, a very small value.

-   The total number of steps difference between imputed and non-imputed data is 8.612950910^{4}.

### Are there differences in activity patterns between weekdays and weekends?

``` r
library(lattice)
```

    ## Warning: package 'lattice' was built under R version 3.2.5

``` r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
impActivityData$dow = as.factor(ifelse(is.element(weekdays(as.Date(impActivityData$date)),weekdays), "Weekday", "Weekend"))

impStepsPerDay <- aggregate(steps ~ interval + dow, impActivityData, mean)

xyplot(
  impStepsPerDay$steps~impStepsPerDay$interval|impStepsPerDay$dow, 
  type="l", xlab="Interval", ylab="Steps", layout=c(1,2),col="red",
  grid=TRUE,
  main="Average Steps by Interval,\n Given for Weekend and Weekday")
```

![](PA1_template_files/figure-markdown_github/weekendData-1.png)

-   The panel plot presents the difference betwwen the time series plots of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days and weekend days (y-axis).
