<br><br>

Introduction
------------

In this project, I will analyze data from a personal activity monitoring
device. Questions addressed include:

-   What is mean total number of steps taken per day?
-   What is the average daily activity pattern?
-   How are missing data imputed?
-   Are there differences in activity patterns between weekdays and
    weekends?

<br><br>

Loading Data
------------

    setwd("/Users/Feng/Desktop/RepData_PeerAssessment1")
    rawdata <- read.csv("activity.csv", header = T)
    rawdata$date <- ymd(rawdata$date)
    dim(rawdata)

    ## [1] 17568     3

    head(rawdata)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

<br><br>

About the Total Number of Steps Taken Each Day
----------------------------------------------

I use dplyr package to calculate the total number of steps taken each
day.

    sumbyday <- rawdata %>% group_by(date) %>% summarize(sum.steps = sum(steps))
    dim(sumbyday)

    ## [1] 61  2

    sumbyday[c(1:3, (nrow(sumbyday) - 2):nrow(sumbyday)), ]

    ## # A tibble: 6 × 2
    ##         date sum.steps
    ##       <date>     <int>
    ## 1 2012-10-01        NA
    ## 2 2012-10-02       126
    ## 3 2012-10-03     11352
    ## 4 2012-11-28     10183
    ## 5 2012-11-29      7047
    ## 6 2012-11-30        NA

The histogram is plotted based on sumbyday data set generated in the
previous step.

    g <- ggplot(data = sumbyday, aes(x = sum.steps))
    g <- g + geom_histogram(color = "wheat4", fill = "hotpink", bins = 25) + 
         scale_x_continuous(name = "Total Number of Steps Taken Each Day") + 
         scale_y_continuous(name = "Count", breaks = seq(0, 10, 2)) + 
         ggtitle(label = "Histogram of the Total Number of Steps Taken Each Day")
    g_sumbyday <- g + theme(plot.margin = unit(c(1,1,1,1), "cm"), 
                            plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), 
                            axis.text = element_text(size = 12), axis.title = element_text(size = 14, face = "bold"))
    g_sumbyday

![](PA1_template_files/figure-markdown_strict/sum%20of%20steps%20by%20day%20Histograms-1.png)

Mean and median of total steps taken each day are extracted from the
summary of the sumbyday data set.

    summary(sumbyday[2])

    ##    sum.steps    
    ##  Min.   :   41  
    ##  1st Qu.: 8841  
    ##  Median :10765  
    ##  Mean   :10766  
    ##  3rd Qu.:13294  
    ##  Max.   :21194  
    ##  NA's   :8

    meanofsum <- gsub("[^0-9]", "", unclass(summary(sumbyday[2]))[4])
    medianofsum <- gsub("[^0-9]", "", unclass(summary(sumbyday[2]))[3])

The mean and median of the total number of steps taken per day are 10766
steps and 10765 steps respectively.

<br><br>

Average Daily Activity Pattern
------------------------------

### Data Reshaping

To facilitate further analysis, I reshape the long format data frame to
a wide format one. In the wide format data frame, there're 289 variables
and 61 observations. Variables are date and the 288 intervals.
Observations are the daily data from the 61 days.

    widedata <- reshape(rawdata, timevar = "interval", idvar = "date", direction = "wide")
    names(widedata) <- gsub("steps.","intvl.", names(widedata))
    rownames(widedata) <- c()
    dim(widedata)

    ## [1]  61 289

    tail(widedata[ , c(1:5, 285:289)])

    ##          date intvl.0 intvl.5 intvl.10 intvl.15 intvl.2335 intvl.2340
    ## 56 2012-11-25       0       0        0        0        176         94
    ## 57 2012-11-26       0       0        0        0          0          0
    ## 58 2012-11-27       0       0        0        0          0          0
    ## 59 2012-11-28       0       0        0        0          0          0
    ## 60 2012-11-29       0       0        0        0          0          0
    ## 61 2012-11-30      NA      NA       NA       NA         NA         NA
    ##    intvl.2345 intvl.2350 intvl.2355
    ## 56         26          0          0
    ## 57          0          0          0
    ## 58          0          0          0
    ## 59          0          0          0
    ## 60          0          0          0
    ## 61         NA         NA         NA

### Time Series Plot

A new data set is generated to record the average steps taken in each
interval.

    avgperintvl <- data.frame(index = 1:288, avg.steps = colMeans(widedata[-1], na.rm = T))
    #' The index variable is added to the data frame because there's a jump of the original interval index before 
    #' each i'th o'clock sharp (e.g., from 1255 to 1300 rather than to 1260), which would lead to discontinuity 
    #' in the figures to plot.
    dim(avgperintvl)

    ## [1] 288   2

    head(avgperintvl)

    ##          index avg.steps
    ## intvl.0      1 1.7169811
    ## intvl.5      2 0.3396226
    ## intvl.10     3 0.1320755
    ## intvl.15     4 0.1509434
    ## intvl.20     5 0.0754717
    ## intvl.25     6 2.0943396

A time series plot is presented to show the average number of steps
taken during each 5-minute interval throughout a day.

    g <- ggplot(data = avgperintvl, aes(x = index, y = avg.steps))
    g <- g + geom_line(color = "royalblue4", size = 1) + 
         geom_point(aes(x = which.max(avgperintvl$avg.steps), y = max(avg.steps)),
                    shape = 1, size = 5, color = "orange")
    g <- g + scale_x_continuous(name = "5-minute Interval Index", breaks = seq(0, 300, 50)) + 
         scale_y_continuous(name = "Average Steps Count") + 
         ggtitle(label = "Average Number of Steps Taken in Each Interval", subtitle = "(Averaged across All Days)")
    g_meanbyintvl <- g + theme(plot.margin = unit(c(1,1,1,1), "cm"), 
                               plot.title = element_text(hjust = 0.5, size = 14, face = "bold"), 
                               plot.subtitle = element_text(hjust = 0.5, size = 12), 
                               axis.text = element_text(size = 10), 
                               axis.title = element_text(size = 12, face = "bold"))
    g_meanbyintvl

![](PA1_template_files/figure-markdown_strict/time%20series%20plot%20of%20average%20steps%20by%20interval-1.png)

    maxsteps <- round(max(avgperintvl$avg.steps))
    maxindex <- which.max(avgperintvl$avg.steps)
    maxintvl <- str_extract(rownames(avgperintvl)[maxindex], "[0-9]+")

In all the 5-minute intervals, interval representing time at 835 (index
No.104) contains the maximum number of steps (206 steps) on average
across all the days in the data set.

<br><br>

Missing Data Imputation
-----------------------

### Imputing NA Value

In accordance with the project instructions, data frame rawdata is used
to calculate the total number of missing values in the original data
set.

    na_record <- tapply(rawdata$steps, rawdata$date, function(x) sum(is.na(x)))
    head(na_record)

    ## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
    ##        288          0          0          0          0          0

    na_totalnum <- sum(na_record)
    na_date <- names(na_record)[na_record > 0]

There are 2304 NA entries in the original data. The days that contain
the NA entries are: 2012-10-01, 2012-10-08, 2012-11-01, 2012-11-04,
2012-11-09, 2012-11-10, 2012-11-14, 2012-11-30. The data in the
remaining 53 days are complete.  
I use the 53-day mean steps taken in each interval to impute the missing
data in the 8 NA-value-filled days.

    raw_imputed <- rawdata
    na_index <- which(raw_imputed$date %in% as.Date(na_date))
    raw_imputed$steps[na_index] <- rep(avgperintvl$avg.steps, 8)
    head(raw_imputed)

    ##       steps       date interval
    ## 1 1.7169811 2012-10-01        0
    ## 2 0.3396226 2012-10-01        5
    ## 3 0.1320755 2012-10-01       10
    ## 4 0.1509434 2012-10-01       15
    ## 5 0.0754717 2012-10-01       20
    ## 6 2.0943396 2012-10-01       25

With the imputed data, I recalculate the total number of steps taken
each day. Note that the numbers are rounded to integers.

    sumbyday_imp <- raw_imputed %>% group_by(date) %>% summarize(sum.steps = round(sum(steps)))
    sumbyday_imp[c(1:3, (nrow(sumbyday_imp) - 2):nrow(sumbyday_imp)), ]

    ## # A tibble: 6 × 2
    ##         date sum.steps
    ##       <date>     <dbl>
    ## 1 2012-10-01     10766
    ## 2 2012-10-02       126
    ## 3 2012-10-03     11352
    ## 4 2012-11-28     10183
    ## 5 2012-11-29      7047
    ## 6 2012-11-30     10766

### Replotting the Histogram

The histogram of the total number of steps taken each day is replotted
based on the data from sumbyday\_imp.

    g <- ggplot(data = sumbyday_imp, aes(x = sum.steps))
    g <- g + geom_histogram(color = "slateblue3", fill = "peachpuff", bins = 25) + 
         scale_x_continuous(name = "Total Number of Steps Taken Each Day") + 
         scale_y_continuous(name = "Count", breaks = seq(0, 18, 2)) + 
         ggtitle(label = "Histogram of the Total Number of Steps Taken Each Day",  
                 subtitle = "(NA Data Imputed with Mean Value)")
    g_sumbyday_imp <- g + theme(plot.margin = unit(c(1,1,1,1), "cm"), 
                                plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                                plot.subtitle = element_text(hjust = 0.5, size = 12),
                                axis.text = element_text(size = 12),
                                axis.title = element_text(size = 14, face = "bold"))
    g_sumbyday_imp

![](PA1_template_files/figure-markdown_strict/Histogram%20with%20the%20new%20data-1.png)

    meanofsum_imp <- mean(sumbyday_imp$sum.steps)
    medianofsum_imp <- median(sumbyday_imp$sum.steps)
    sdsum <- sd(sumbyday$sum.steps, na.rm = T)
    sdsum_imp <- sd(sumbyday_imp$sum.steps)

In the imputed data, the mean and median of the total number of steps
taken per day are 10766 steps and 10766 steps respectively.  
The data imputation significantly changes the distribution of the total
number of steps taken per day, though the changes in its mean and median
are negligible. The bin representing the count of observations around
the mean value (10766 steps) is almost twice as tall as that in the
first histogram. The standard deviation of the total number of steps
taken per day decreases from 4269.1804927 to 3974.3907465.

<br><br>

Differences in Activity Patterns between Weekdays and Weekends
--------------------------------------------------------------

    raw_imputed$dayofweek <- factor(weekdays(raw_imputed$date) %in% c("Saturday","Sunday"), 
                                    labels = c("Weekday","Weekend"))
    by_dofw <- raw_imputed %>% group_by(dayofweek, interval) %>% summarize(mean.steps = mean(steps))
    by_dofw$intvl.index <- rep(1:288, 2)
    by_dofw[c(1:3, (nrow(by_dofw) - 2):nrow(by_dofw)), ]

    ## Source: local data frame [6 x 4]
    ## Groups: dayofweek [2]
    ## 
    ##   dayofweek interval mean.steps intvl.index
    ##      <fctr>    <int>      <dbl>       <int>
    ## 1   Weekday        0 2.25115304           1
    ## 2   Weekday        5 0.44528302           2
    ## 3   Weekday       10 0.17316562           3
    ## 4   Weekend     2345 1.70518868         286
    ## 5   Weekend     2350 0.02830189         287
    ## 6   Weekend     2355 0.13443396         288

Now I use lattace package to draw the panel plot.

    xyplot(data = by_dofw, mean.steps ~ intvl.index | dayofweek, type = "l", layout = c(1, 2),
           main = "Average Number of Steps Taken in Each Interval\n(Averaged across All Weekends and All Weekdays Respectively)", 
           xlab = list(label = "5-minute Interval Index", fontsize = 14),
           ylab = list(label = "Average Steps Count", fontsize = 14))

![](PA1_template_files/figure-markdown_strict/plots%20by%20day%20of%20the%20week-1.png)

Judging from the panel plot, the average number of steps taken in each
interval follows similar trend in weekends and weekdays. But the values
of the mean steps in each interval do differ appreciably between
weekends and weekdays.

    results <- by_dofw %>% group_by(dayofweek) %>%
               summarize(max.steps = max(mean.steps), 
                         max.int.index = intvl.index[mean.steps == max.steps], 
                         max.intvl = interval[mean.steps == max.steps])
    print(results)

    ## # A tibble: 2 × 4
    ##   dayofweek max.steps max.int.index max.intvl
    ##      <fctr>     <dbl>         <int>     <int>
    ## 1   Weekday  230.3782           104       835
    ## 2   Weekend  166.6392           112       915

As we can see from the table above, in weekdays the 104th interval
(representing time at 835) has the peak (230 steps) of the steps taken
throughout the day while in weekends the 112th interval (representing
time at 915) has the maximum number of steps taken (167 steps).

    #' At the end of the Rmd document, the command below is run in Console to produce PA1_template.md and
    #' PA1_template.html files.
    # render("PA1_template.Rmd", c("html_document", "md_document"))

<br><br> <br><br>
