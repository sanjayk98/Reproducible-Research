PA1\_template
================
Sanjay Kumar
2018-11-11

1 Code for reading in the dataset and/or processing the data
------------------------------------------------------------

``` r
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile="C:/Users/sanjayx/Desktop/coursera/activity.zip")
unzip("C:/Users/sanjayx/Desktop/coursera/activity.zip",exdir="C:/Users/sanjayx/Desktop/coursera/activity")
activity<-data.table::fread("C:/Users/sanjayx/Desktop/coursera/activity/activity.csv")
names(activity)
```

    ## [1] "steps"    "date"     "interval"

``` r
dim(activity)
```

    ## [1] 17568     3

``` r
#Remove null data
sum(is.na(activity))
```

    ## [1] 2304

``` r
sum(is.na(activity$date))
```

    ## [1] 0

``` r
sum(is.na(activity$steps))
```

    ## [1] 2304

``` r
sum(is.na(activity$interval))
```

    ## [1] 0

``` r
ok <- complete.cases(activity$date,activity$steps)
sum(!ok) 
```

    ## [1] 2304

``` r
activity1<-activity
activity <- activity[ok,]
#Validate null value
sum(is.na(activity))
```

    ## [1] 0

2 Histogram of the total number of steps taken each day
-------------------------------------------------------

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
dailysum <-aggregate(activity$steps,list(yday(as.POSIXlt(activity$date))), FUN=sum,na.rm=TRUE, na.action=NULL)
names(dailysum)<-c("yearday","steps")
hist(dailysum$steps,col="blue",main="Histogram for Daily Steps",xlab="Steps")
```

![](PA_template_files/figure-markdown_github/unnamed-chunk-2-1.png) \#\#3 Mean and median number of steps taken each day

``` r
dailymean <-aggregate(activity$steps,list(yday(as.POSIXlt(activity$date))), FUN=mean,na.rm=TRUE, na.action=NULL)

dailymedian <-aggregate(activity$steps,list(yday(as.POSIXlt(activity$date))), FUN=median,na.rm=TRUE,na.action=NULL)
```

4 Time series plot of the average number of steps taken
-------------------------------------------------------

``` r
plot.ts(dailymean$x,type="l",xlab="day",ylab="steps",main="Avergae no of steps taken",col="red")
```

![](PA_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

5 The 5-minute interval that, on average, contains the maximum number of steps
------------------------------------------------------------------------------

``` r
maxstep<-max(activity$steps,na.rm=TRUE)

subset(activity,steps==maxstep)
```

    ##    steps       date interval
    ## 1:   806 2012-11-27      615

6 Code to describe and show a strategy for imputing missing data
----------------------------------------------------------------

``` r
sum(is.na(activity1))
```

    ## [1] 2304

``` r
sum(is.na(activity1$date))
```

    ## [1] 0

``` r
sum(is.na(activity1$steps))
```

    ## [1] 2304

``` r
sum(is.na(activity1$interval))
```

    ## [1] 0

``` r
#install.packages("imputeTS")
library(imputeTS)
x<-na.mean(activity1)

dailysum <-aggregate(x$steps,list(yday(as.POSIXlt(x$date))), FUN=sum,na.rm=TRUE, na.action=NULL)

dailymean <-aggregate(x$steps,list(yday(as.POSIXlt(x$date))), FUN=mean,na.rm=TRUE, na.action=NULL)

dailymedian <-aggregate(x$steps,list(yday(as.POSIXlt(x$date))), FUN=median,na.rm=TRUE,na.action=NULL)
```

7 Histogram of the total steps taken each day after missing values are imputed
------------------------------------------------------------------------------

``` r
hist(dailysum$x,col="blue",main="Histogram for Daily Steps after impute",xlab="Steps")
```

![](PA_template_files/figure-markdown_github/unnamed-chunk-7-1.png) \#\#8 Panel plot comparing the average number of steps taken per 5-minute interval across \#\# weekdays and weekends

``` r
wdactivity<-subset(x,wday(x$date)<=5)
wendactivity<-subset(x,wday(x$date)>5)


weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday",  "Friday")
x$weekdays = as.factor(ifelse(is.element(weekdays(as.Date(x$date)),weekdays), 1, 0))

steps_i <- aggregate(steps ~ interval + weekdays, x, mean)

library(lattice)
xyplot(steps_i$steps~steps_i$interval|steps_i$weekdays,steps_i,xlab="Interval",ylab="steps",main="Steps(5min)",type="l")
```

![](PA_template_files/figure-markdown_github/unnamed-chunk-8-1.png) \`\`\`\`\`
