---
title: "PA1_Template"
output: html_document
---

This file contains the R code chunks and the respective output for Peer Assessment 1, Reproducible research.

---
#Loading and Processing the Data:

Here is a sample of the data. 

```{r question 1, echo=TRUE}


dat <- read.csv("activity.csv")

head(dat)

```


#What is the mean total number of steps taken per day?

```{r question 2.1, echo = TRUE}

dat2 <- na.omit(dat)

sum_steps <- sum(dat2[,"steps"])



```

      The total number of steps take is `r sum_steps`.


```{r question 2.2&2.3, echo = TRUE}


total_steps_per_day <- aggregate(dat2$steps, by = list(date = as.Date(dat2$date)), FUN=sum)


hist(total_steps_per_day$x, main = "Histogram of Total Steps per Day", xlab = "Steps")

mean_steps_per_day <- format(mean(total_steps_per_day$x), digits = 2)

median_steps_per_day <- median(total_steps_per_day$x)

```      

      The mean steps per day is `r mean_steps_per_day`.
      
      The median steps per day is `r median_steps_per_day`.
     
     
#What is the average daily activity pattern?

A plot of the 5 minute intervals
        
```{r question 3.1, echo = TRUE}

mean_steps_per_interval <- aggregate(dat2$steps, by = list(interval = dat2$interval), FUN=mean)

plot(mean_steps_per_interval, type="l", main= "Average Steps per Interval", ylab = "Average Steps", xlab="Interval")


``` 


```{r question 3.2, echo = TRUE}

mspi <- mean_steps_per_interval$interval[mean_steps_per_interval$x==max(mean_steps_per_interval$x)]

```   

      The interval with the highest average steps is `r mspi`.

# Imputing Missing Values

```{r question 4.1, echo = TRUE}

total_NAs <- sum(is.na(dat$steps))


```  
        
      The total number of "NA" values is `r total_NAs`.


```{r question 4.2&4.3, echo = TRUE}

dat3 <- merge(dat, mean_steps_per_interval)


dat3$steps <- ifelse(is.na(dat3$steps),dat3$x, dat3$steps)

```  
      
```{r question 4.4, echo = TRUE}

dat3 <- merge(dat, mean_steps_per_interval)


dat3$steps <- ifelse(is.na(dat3$steps),dat3$x, dat3$steps)

new_tspd <- aggregate(dat3$steps, by = list(date = as.Date(dat3$date)), FUN=sum)

hist(new_tspd$x, main = "Histogram of Total Steps per Day with Imputing", xlab = "Steps")

new_mean <- format(mean(new_tspd$x), digits = 2)

new_median <- format(median(new_tspd$x), digits = 2)


```  


      The mean steps per day is `r new_mean`.
      
      The median steps per day is `r new_median`.
      
      Using imputed values sets the mean and the median equal to each other.
      
      
# Are there differences in activity patterns between weekdays and weekends?

```{r question 5.1&5.2, echo = TRUE}

dat3$dayofweek <- c(weekdays(as.Date(dat3$date)))

dat3$weekend_weekday <- ifelse(dat3$dayofweek %in% c("Saturday", "Sunday"), "Weekend", "Weekday")

wd_mspi <- aggregate(dat3$steps, by = list(interval = dat3$interval, part_of_week = dat3$weekend_weekday), FUN=mean)

library(lattice)

xyplot(steps ~ interval| weekend_weekday, data = dat3, type = "l", xlab = "Interval", ylab = "Average Number of steps", layout=c(1,2))

```


