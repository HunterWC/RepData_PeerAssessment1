Reproducible Research - Peer Assignment 1
====================================================================
Hunter Clark
====================================================================


#Background
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

#Analysis
##Total steps per day
The first step in this analysis is to download the data set from a url and upload it into R.


```r
fileurl <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
temp <- tempfile()
wd <- getwd()

download.file(fileurl,temp)
```

```
## Warning in download.file(fileurl, temp): downloaded length 53559 !=
## reported length 53559
```

```r
unzip(temp, exdir = wd)
unlink(temp)

activity <- read.csv(paste(wd,"activity.csv", sep = "/"))
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

Next, we can see the total steps per day by grouping the data for each date, summing the steps, and making a histogram of the results.


```r
library(dplyr)
```

```
## Warning: package 'dplyr' was built under R version 3.1.3
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```


```r
total_steps <- summarize(group_by(activity,date), sum(steps, na.rm = TRUE))
colnames(total_steps) <- c("date", "sum")

hist(total_steps$sum, breaks = 10, main = "Histogram of Total Steps per Day", ylab="Frequency (number of days)", xlab="Total Steps per Day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 


```r
#mean and median of total steps per day
meantotsteps <- mean(total_steps$sum)
medtotsteps <- median(total_steps$sum)
```
This histogram coresponds to a mean of 9354.2295082 and a median of 10395. 

##Daily Activity Pattern
To determine the average daily activity pattern I created a time series plot of the average number of steps taken for each 5-minute interval, averaged across all days.


```r
avg_steps_int <- summarize(group_by(activity,interval), mean(steps, na.rm = TRUE))
colnames(avg_steps_int) <- c("interval", "mean")
plot(avg_steps_int$interval, avg_steps_int$mean, ylab = "Steps", xlab = "", type = "l", main = "Average steps taken for each 5-min interval, across all days")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

Using the summary function we can see that the largest mean value is 206.17.  Looking at that observation specifically, we see that that largest number of steps, on average, happen during the 835 minute interval, which coresponds with the peak of the plot. 


```r
summary(avg_steps_int)
```

```
##     interval           mean        
##  Min.   :   0.0   Min.   :  0.000  
##  1st Qu.: 588.8   1st Qu.:  2.486  
##  Median :1177.5   Median : 34.113  
##  Mean   :1177.5   Mean   : 37.383  
##  3rd Qu.:1766.2   3rd Qu.: 52.835  
##  Max.   :2355.0   Max.   :206.170
```

```r
#max of mean column is 206.170
avg_steps_int[avg_steps_int$mean >= 206,]
```

```
## Source: local data frame [1 x 2]
## 
##   interval     mean
## 1      835 206.1698
```

##Missing Values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
missing <- sum(!complete.cases(activity))
```

Using complete.cases we see that there are 2304 observations with missing values.

Sometimes missing values are simply removed from the data set or replaced with zeros.  Another strategy of dealing with the missing values is to replace them with the average of the other values in the same 5 minute interval.


```r
for (i in 1:nrow(activity)) {
    if (is.na(activity$steps[i])) {
        meanset <- activity[activity$interval == activity$interval[i],]
        meaninterval <- mean(meanset$steps, na.rm = TRUE)
        activity$steps[i] <- meaninterval
    }
}

head(activity)
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

Using this data we can re-create the histogram of total steps per day.


```r
total_steps <- summarize(group_by(activity,date), sum(steps, na.rm = TRUE))
colnames(total_steps) <- c("date", "sum")
hist(total_steps$sum, breaks = 10, main = "Histogram of Total Steps per Day", ylab="Frequency (number of days)", xlab="Total Steps per Day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 


```r
# create png file
png(file="hist1.png")
hist(total_steps$sum, breaks = 10, main = "Histogram of Total Steps per Day", ylab="Frequency (number of days)", xlab="Total Steps per Day")
dev.off()
```


```r
#mean and median of total steps per day
meantotsteps <- mean(total_steps$sum)
medtotsteps <- median(total_steps$sum)
```
The new histogram shows a loss of a significant maximum during the early minutes of each day.  This histogram also relates to a new mean of 1.0766189 &times; 10<sup>4</sup> and a median of 1.0766189 &times; 10<sup>4</sup>.

##Activity patterns of Weekdays and Weekends
We can further evaluate our new data set with missing values filled in to determine if each day is a weekday or weekend, and what effect that will have of the data.  I first split the observations between the two levels and then creted a plot to demonstrate the results.


```r
library(chron)
```

```
## Warning: package 'chron' was built under R version 3.1.3
```

```r
activity_weekday <- as.data.frame(is.weekend(activity$date))
colnames(activity_weekday) <- "weekend"
activity_weekbind <- cbind(activity,activity_weekday)
activity_weekgroup <- summarize(group_by(activity_weekbind,weekend,interval), mean(steps))
colnames(activity_weekgroup) <- c("weekend", "interval", "mean")

library(ggplot2)
activity_weekgroup[activity_weekgroup == "FALSE"] <- "Weekend"
activity_weekgroup[activity_weekgroup == "TRUE"] <- "Weekday"
qplot(interval, mean, data=activity_weekgroup, facets=weekend~., geom="line", main="Average steps taken for each 5-min interval, across all days")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

We can see that on average, the number of steps are higher later in the day for weekdays than for weekends.  



