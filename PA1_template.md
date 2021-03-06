---
output:
  html_document:
    fig_height: 6
    fig_width: 8
    keep_md: yes
    self_contained: no
  word_document: default
---
Reproducible Research: Peer Assessment 1
==========================================


## Set code visible to public


```r
echo = TRUE  
options(scipen = 1, digits = 0)
```


## Loading and preprocessing the data

- Loading data and store in dataset **data**


```r
data<-read.csv("activity.csv",stringsAsFactors=FALSE)
```

- Convert variable date to format **Date**


```r
data$date<-as.Date(data$date)
```

- Ignore missing values and store in dataset **dat**


```r
dat<-na.omit(data) 
```

## What is mean total number of steps taken per day?

### Calculate total steps taken by date

- Calculate **total steps** taken by date and store in variable **steps_date**


```r
steps_date<-aggregate(dat$steps, by=list(date=dat$date), FUN=sum)
```

- Calculate **Mean, Median** and **Standard Deviation** of total steps taken by date  and store in variables **mean_steps_date**, **median_steps_date** and **sd_steps_date**


```r
mean_steps_date<-mean(steps_date$x)
median_steps_date<-median(steps_date$x)
sd_steps_date<-sqrt(median_steps_date)
head(steps_date)
```

```
##         date     x
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

### Mean vs Median


```r
mean_steps_date
```

```
## [1] 10766
```

```r
median_steps_date
```

```
## [1] 10765
```

- We can see  the mean **10766** are very close but not equal the median **10765** that is showed on the plot below that 02 mean and median lines are very close

### Plot Histogram

- In order to see clearer the distribution of data, I plotted the Histogram and Density line of the total number of steps taken each day on the same plot. I also put in this plot Mean and Median lines in purple and black colors respectively
  

```r
dens<-density(steps_date$x)

hist(steps_date$x,probability=TRUE,col="cyan",border="blue", breaks = 20,ylim=range(0,0.00020),
     main = expression(atop("Histogram of Total Steps taken by date",italic("(NA values are filtered out Dataset)"))), xlab = list("Total Steps",cex=1.2,col="blue"),ylab=list("Density",cex=1.2,col="blue"))

lines(dens,lwd=3,col="red")
abline(v = mean_steps_date, col = "purple",lwd=3)
abline(v = median_steps_date, col = "black",lwd=3)
axis(1,round(mean_steps_date,digit=0),labels=TRUE,font=8,col.ticks="red",line=-1)
text(mean_steps_date,0.00020,labels="Mean line", pos=4, col="purple")
text(mean_steps_date,0.00018,labels="Median line", pos=4, col="black")
text(mean_steps_date+3500,0.00019,labels="are very close", pos=4, col="red")
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

## What is the average daily activity pattern?

### Calculate mean of number of steps taken across days by interval

- Calculate mean of number of steps taken across days by interval and store in variable **intervals**


```r
intervals<-aggregate(dat$steps, by=list(interval=dat$interval), FUN=mean)
names(intervals)<-c("Interval","Mean")
head(intervals)
```

```
##   Interval Mean
## 1        0    2
## 2        5    0
## 3       10    0
## 4       15    0
## 5       20    0
## 6       25    2
```

### Calculate maximum number of steps on average across all the days by interval and store in variables **max_interval**


```r
max_interval<-intervals[intervals$Mean == max(intervals$Mean),]
max_interval
```

```
##     Interval Mean
## 104      835  206
```

- The result shows that the interval **835** get maximum number of steps on average across all the days by interval **206** 

- Plot Time Series of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
par(col="dark green",lwd=2)
plot(intervals$Interval,intervals$Mean,type="l",xlim=range(0:2500),
     xlab=list("Interval",cex=1.2,col="blue"),ylab=list("Mean of Steps across all dates by interval",cex=1.2,col="blue"),main="The average number of steps taken, averaged across all days")
xlabel <- seq(0, 2500, by = 250)
axis(1, at = xlabel)
abline(v=max_interval[1], lty=3, col="red",lwd=2)                     
text(max_interval[1],max_interval[2],  
     labels=paste("max = ",as.character(round(max_interval[2]))), 
     pos=4, col="blue") 
axis(1,max_interval[1],labels=TRUE,font=8,col.ticks="red",line=-1)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

## Imputing missing values

### Calculate the total number of missing values

- Calculate number rows with NAs


```r
rows<-nrow(data)
rows_omit_na<-nrow(dat)
row_missing<-rows-rows_omit_na 
row_missing
```

```
## [1] 2304
```
- The result shows that there are **2304** rows with NAs

- Fill NAs of original dataset data by mean of intervals which is stored in variable **interval_mean**


```r
interval_mean<-aggregate(dat$steps, by=list(interval=dat$interval), FUN=mean)

for(i in 1:nrow(data))
{
  if(is.na(data[i,1]))
  {
    match_index<-match(data[i,3],interval_mean$interval)
    data[i,1] = interval_mean[match_index,2]
  }
}

head(data)
```

```
##   steps       date interval
## 1     2 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     2 2012-10-01       25
```

- Calculate total steps taken by date of **filled dataset** and store in variable **steps_date_full**


```r
steps_date_full<-aggregate(data$steps, by=list(date=data$date), FUN=sum)
head(steps_date_full)
```

```
##         date     x
## 1 2012-10-01 10766
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

### Mean vs Median after being filled missing values

- Calculate Mean and Median of total steps taken by date of **filled dataset** and store in variables **mean_steps_date_full** and **median_steps_date_full**


```r
mean_steps_date_full<-mean(steps_date_full$x)
median_steps_date_full<-median(steps_date_full$x)
sd_steps_date_full<-sqrt(median_steps_date_full)

mean_steps_date_full
```

```
## [1] 10766
```

```r
median_steps_date_full
```

```
## [1] 10766
```

- The result shows that the mean **10766** approximately equals the median **10766** that is showed on the plot below that 02 mean and median lines are likely overlapped

### Impact of imputing missing data

- Plot Histogram included Density line of the total number of steps taken each day of **filled dataset**

- In order to be easier to see the impact of imputing missing data on the estimates of the total daily number of steps, I also draw Density line of igonred-NAs dataset in black

1- We can see the mean values of 02 dataset nearly the same, both values approximate **10766**

2- However, the median changed a little bit, **10765** and **10766**, respectively

3- Imputing missing values made the data distribution closer to mean value, median equals mean. The distribution line is narrower


```r
dens_full<-density(steps_date_full$x)
hist(steps_date_full$x,probability=TRUE,col="cyan",border="blue",breaks = 20, ylim=range(0,0.00030),
     main = expression(atop("Histogram of Total Steps taken by date",italic("(NA values are filled by mean of intervals)"))), xlab = list("Total Steps",cex=1.2,col="blue"),ylab=list("Density",cex=1.2,col="blue"))
lines(dens_full,lwd=3,col="red")
lines(dens,lwd=3,col="black")
abline(v = mean_steps_date_full, col = "purple",lwd=3)
abline(v = median_steps_date_full, col = "chocolate",lwd=3)
axis(1,round(mean_steps_date_full,digit=0),labels=TRUE,font=8,col.ticks="red",line=-1)
text(mean_steps_date,0.00030,labels="Mean line", pos=4, col="purple")
text(mean_steps_date,0.00028,labels="Median line", pos=4, col="chocolate")
text(mean_steps_date+3600,0.00029,labels="are overlapped", pos=4, col="red")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16-1.png) 

## Are there differences in activity patterns between weekdays and weekends?

- Add variable weekday to dataset


```r
data$weekday<-weekdays(data$date)
```

- Store weekdays and weekend in variables **weekdays** and **weekend**


```r
weekdays<-c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
weekend<-c("Saturday", "Sunday")
```

- Store index of elements of dataset  which are in weekdays and weekend


```r
weekday_index<-which(data$weekday %in% weekdays)
weekend_index<-which(data$weekday %in% weekend)
```

- Subset data weekdays and weekend separated


```r
data_weekdays<-data[weekday_index,]
data_weekend<-data[weekend_index,]
```

- Calculate mean of number of steps taken across days by interval and store in variables **weekday_intervals** and **weekend_intervals**


```r
weekday_intervals<-aggregate(data_weekdays$steps, by=list(interval=data_weekdays$interval), FUN=mean)
weekend_intervals<-aggregate(data_weekend$steps, by=list(interval=data_weekend$interval), FUN=mean)
```

- Add variable **weekday** to these datasets


```r
weekday_intervals$weekday<-c(rep("Weekday",nrow(weekday_intervals)))
weekend_intervals$weekday<-c(rep("Weekend",nrow(weekend_intervals)))
```

- Join 02 datasets for ploting


```r
data_plot<-rbind(weekday_intervals,weekend_intervals)
```

- Load library **lattice**


```r
library(lattice)
```

- Plot Time Series of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all Weekdays and Weekend Days (y -axis) 


```r
bgColors <- c("cyan", "yellow")
txtColors <- c("black", "red")
myStyle <- function(which.panel, factor.levels, ...) {
    panel.rect(0, 0, 1, 1,
               col = bgColors[which.panel],
               border = 1)
    panel.text(x = 0.5, y = 0.5,
               font=2,
               lab = factor.levels[which.panel],
               col = txtColors[which.panel])
}    

newSet <- col.whitebg() 
trellis.par.set(newSet)

plot<-xyplot(x~interval|weekday,data = data_plot, type = "l", layout = c(1,2), 
       scales = list(x = list(at = seq(0, 2500, 250), limits = c(0, 2500))),lwd=2,
       xlab=list("Interval",col="blue",cex=1.2),ylab=list(" The average number of steps  taken",col="blue",cex=1.2),main=expression(atop("The average number of steps taken", "averaged across all weekday days or weekend days")),strip=myStyle)

plot
```

![plot of chunk unnamed-chunk-25](figure/unnamed-chunk-25-1.png) 
