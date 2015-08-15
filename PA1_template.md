# Reproducible Research: Peer Assessment 1
## Loading and preprocessing the data

```r
library(dplyr)
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
library(ggplot2)

data <- read.csv("activity.csv")
datag <- group_by(data, date)

#mean and median data
datas <- summarise(datag, steps=sum(steps))
datasf <- filter(datas, !is.na(steps))
# filter NA data and group by interval column
datagf <- data %>% filter(!is.na(steps)) %>% group_by(interval)
#create table that shows average steps per each interval accross all days
datagfs <- datagf %>% summarise(stepsa = mean(steps))
#filter out NA and summarise steps by date
datagfd <- data %>% filter(!is.na(steps)) %>% group_by(date) %>% summarise(steps = sum(steps))
#add column with rounded steps for data simulation later
datagfs <- datagfs %>% mutate(stepsr = round(stepsa))
```
## What is mean total number of steps taken per day?

```r
hist(datasf$steps, xlab="Steps per day", main="Average total number of steps per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

###Average total number of steps per day = 10766.188679
###Median total number of steps per day = 10765.000000

## What is the average daily activity pattern?

```r
plot(datagfs$interval, datagfs$stepsa, type="l", xlab="Interval", ylab="Steps", main="Average number of steps taken")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

## Imputing missing values

```r
# now create sub-table that contains ONLY rows with NA steps
datana <- data %>% filter(is.na(steps))
#perform left join with datagfs to create column with simulated steps
datanasimulated <- left_join(datana, datagfs)
```

```
## Joining by: "interval"
```

```r
#select only required columns to create a table that look identical to the original one but NA is replaced with mean value
datana1 <- datanasimulated %>% select(date, interval, steps = stepsr)
#now bind rows from the original activity table that filtered out NAs and with a new table that replaced NA with simulated data
datasimulated <- rbind(datana1, datagf)
```

### Total number of missing values (NA) = 2304

```r
#calculate average number of steps per interval from simulated data
datasimulatedg <- datasimulated %>% group_by(date)
datassim <- summarise(datasimulatedg, steps=sum(steps))
hist(datassim$steps, xlab="Steps per day", main="Average total number of steps per day - including simulated data")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

###Average total number of steps per day including simulated data = 10765.639344
###Median total number of steps per day including simulated data = 10762.000000
###Impact of imputing missing data:
###Delta average = -0.549335
###Delta median = -3.000000

## Are there differences in activity patterns between weekdays and weekends?

```r
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
datasteps <- data %>% filter(!is.na(steps))
dataw <- datasteps %>% mutate (weekday = factor(weekdays(as.Date(datasteps$date)) %in% weekdays1, levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')))
datawg <- dataw %>% group_by(interval, weekday) %>% summarise(steps = mean(steps))
ggplot(datawg, aes(x=interval,y=steps)) + geom_line() + facet_grid(weekday ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 
