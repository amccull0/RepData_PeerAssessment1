# PA1
Adam McCullough  
August 23, 2016  
##Step 1
### Load and Process Data

Load the data(note: the data file was downloaded and extracted to the working directory)

```r
activity<-read.csv("activity.csv")
```

##Step 2
###Initial interpretation

Sum the number of steps by day and create a histogram of the output

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
sum1<- na.omit(activity) %>% group_by(date) %>% summarize(Steps=sum(steps))
hist(sum1$Steps, breaks = 10, xlab = "Steps", main = "Histogram of Steps by Day")
```

![](PA1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
meanbyday<-mean(sum1$Steps)
meanbyday
```

```
## [1] 10766.19
```

```r
medianbydate<-median(sum1$Steps)
medianbydate
```

```
## [1] 10765
```

##Step 3
###Average daily activity

Need to group and average steps by time interval, then plot a line graph

```r
library(dplyr)
sum2<- na.omit(activity) %>% group_by(interval) %>% summarize(Steps=mean(steps))
plot(sum2$interval,sum2$Steps, type = "l",xlab="Interval",ylab = "Steps",main = "Steps by Time Interval")
```

![](PA1_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Now determine and output the interval with the max average value across all days

```r
subset(sum2, sum2$Steps == max(sum2$Steps))
```

```
## # A tibble: 1 x 2
##   interval    Steps
##      <int>    <dbl>
## 1      835 206.1698
```

##Step 4 
###Imputing missing values

Calculate the total number of rows with missing values

```r
sum(is.na(activity))
```

```
## [1] 2304
```

Fill in these missing values with the mean value for that time interval across all other days

```r
activity2<-activity
activity2$steps[is.na(activity2$steps)] <- sum2$Steps[match(activity2$interval[is.na(activity2$steps)],sum2$interval)]
```

First summarize by day then, create histogram and output mean and median of imputted data

```r
sum3<- activity2 %>% group_by(date) %>% summarize(steps=sum(steps))
hist(sum3$steps, breaks = 10, xlab = "Steps", main = "Histogram of Steps by Day")
```

![](PA1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
meanbyday2<-mean(sum3$steps)
meanbyday2
```

```
## [1] 10766.19
```

```r
medianbydate2<-median(sum3$steps)
medianbydate2
```

```
## [1] 10766.19
```
##Step 5
###Weekdays vs Weekends

Determine whether each date is a weekday or weekend

```r
activity2$weekday<-weekdays(as.POSIXlt(activity2$date))
 activity2$wd<-ifelse(activity2$weekday %in% c("Sunday","Saturday"), "Weekend", "Weekday")
```

Plot two graphs representing the average steps by time interval grouped seperately by Weekday and Weekend

```r
par(mfrow=c(2,1))
weekday<-activity2[activity2$wd=="Weekday",]
plot1<- weekday %>% group_by(interval) %>% summarize(Steps=mean(steps))
weekend<-activity2[activity2$wd=="Weekend",]
plot2<- weekend %>% group_by(interval) %>% summarize(Steps=mean(steps))
plot(plot1$interval,plot1$Steps, type = "l",xlab="Interval",ylab = "Steps",main = "Steps by Time Interval- Weekday")
plot(plot2$interval,plot2$Steps, type = "l",xlab="Interval",ylab = "Steps",main = "Steps by Time Interval- Weekend")
```

![](PA1_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
