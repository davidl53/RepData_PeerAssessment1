---
title: "Reproducible Research Assignment 1"
output: html_document
---



Load the libraries used later and then load the datafile into a dataframe called stepsData. Assumes activity.csv is in working directory.


```r
library(dplyr)
library(lubridate)
library(lattice)

stepsData<-read.csv("activity.csv")
```


### Frequency distribution of steps per day
group the data by date, and calculate the mean number of steps on each day
Plot a histogram of the distribution of the mean number of steps/day

```r
stepsGrouped<-group_by(stepsData,date)
stepsPerDay<-summarise(stepsGrouped,StepsPerDay=sum(steps,na.rm=TRUE))

hist(stepsPerDay$StepsPerDay,main="Histogram of Steps per Day",xlab="Number of Steps",ylab="Frequency")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

### Overall mean and median steps per day
Calculate mean and median steps per day

```r
meanspd<-mean(stepsPerDay$StepsPerDay, na.rm=TRUE)
medianspd<-median(stepsPerDay$StepsPerDay,na.rm=TRUE)
meanspd
```

```
## [1] 9354.23
```

```r
medianspd
```

```
## [1] 10395
```

###Steps per 5 minute interval
Group the data by interval and summarise the mean steps per interval

```r
stepsGroupedByInterval<-group_by(stepsData,interval)
stepsPerInterval=summarise(stepsGroupedByInterval, meanStepsInt=mean(steps,na.rm=TRUE))
plot(stepsPerInterval$meanStepsInt,type="l",main="  Mean Steps per Interval",xlab="Interval",ylab="Number of Steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

### Which interval has the highest mean number of steps?

The maximum value for mean steps

```r
maxInterval <- stepsPerInterval[which(stepsPerInterval$meanStepsInt==max(stepsPerInterval$meanStepsInt)),]
maxInterval
```

```
## Source: local data frame [1 x 2]
## 
##   interval meanStepsInt
## 1      835     206.1698
```

### How many missing values are there?
The number of missing values for steps

```r
numMissing<- sum(is.na(stepsData$steps))
numMissing
```

```
## [1] 2304
```


### Imputation.
I have chosen to replace the missing values for steps with the mean number of steps for  the relevant interval from all days. 
Replacing the missing values in this way would be expected to increase the number of steps on each day. The results confirm this and this is also reflecte din the greater mean and median steps per day.


```r
AddMeanStepsPerInterval<-inner_join(stepsGrouped,stepsPerInterval)
```

```
## Joining by: "interval"
```

```r
AddMeanStepsPerInterval$steps[is.na(AddMeanStepsPerInterval$steps)]<-AddMeanStepsPerInterval$meanStepsInt[is.na(AddMeanStepsPerInterval$steps)]

ImpStepsGroupedByDate <- group_by(AddMeanStepsPerInterval,date)
ImpStepsPerDay<-summarise(ImpStepsGroupedByDate,ImpSteps=sum(steps))

hist(ImpStepsPerDay$ImpSteps,main="Histogram of Steps per Day- Imputed Data",xlab="steps",ylab="frequency")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

### Mean and median steps per day - imputed data


```r
meanstpImp<-mean(ImpStepsPerDay$ImpSteps)
medianstpImp<-median(ImpStepsPerDay$ImpSteps)
meanstpImp
```

```
## [1] 10766.19
```

```r
medianstpImp
```

```
## [1] 10766.19
```

### How does the number of steps vary on weekdays and at the weekend?
Add a column to the data for weekend/weekday
Then create a copy of the original data, and change the steps column of the copy to the imputed values
Then Group by Interval and whether weekend or not, and summarise data to give mean nuber of steps for each interval for weekend and weekday


```r
stepsData<-mutate(stepsData,dayType=ifelse
          (weekdays(ymd(as.character(stepsData$date)))=="Saturday"|weekdays(ymd(as.character(stepsData$date)))=="Sunday"
           ,"Weekend","Weekday"))
stepsDataCopy<-stepsData
stepsDataCopy$steps<-AddMeanStepsPerInterval$steps
weekendGrouping<-group_by(stepsDataCopy,interval,dayType)
WeekendData<-summarise(weekendGrouping,dayTypeSteps=mean(steps))
xyplot(dayTypeSteps~interval|dayType,data=WeekendData,type="l",layout=c(1,2),main="Comparison of Steps Per 5 Minute Interval - Weekdays and Weekends", xlab="Time of Start of 5 Minute Interval",ylab="Steps")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 
