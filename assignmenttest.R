## Load required libraries
require(dplyr)
require(lubridate)
require(ggplot2)
require(ggthemes)
require(timeDate)

## Loading and preprocessing the data

fURL <- "https://github.com/jrebane/RepData_PeerAssessment1/blob/master/activity.zip?raw=true"
zipFileName <- "./activity.zip"
csvFileName <- "./activity.csv"

## If csv file not in working directory, download from repository and extract to working directory
if (!file.exists(csvFileName)){
        message("Downloading and unpacking zip file")
        zipFile <- download.file(url = fURL,destfile = zipFileName, mode='wb')
        unzip(zipFileName)
} else {message("Data file found in working directory")}

## read data into table from txt file and convert date from character to POSIXLT
if (!exists("tblSteps")){
        ## Calculate a rough estimate of how much memory the dataset will require
        ## in memory before reading into R. Make sure your computer has enough memory
        fileSizeMb <- file.info(csvFileName)$size*1.0e-6
        memLimit <- memory.limit()
        
        ## Exit with error message if computer does not have enough memory
        if (fileSizeMb > memLimit){
                stop("Insufficient memory to read table - aborting script.")
        } else {
                message("Reading table from data file")
                tblSteps <- read.csv(file = csvFileName, header = TRUE,
                                     na.strings = "NA",
                                     colClasses = c("numeric", "character", 
                                                    "numeric"))
                tblSteps$date <- ymd(tblSteps$date)    
        }} else{ message("Using data table from memory")}

## What is mean total number of steps taken per day?,

## Ignoring Missing Values
stepsGroupedByDate <- 
        tblSteps %>%
        group_by(date) %>%
        summarize(count = sum(steps, na.rm = TRUE))

## 1. Making a histogram

xlab1 <- "Total Number of Steps Taken in a Day"
ylab1 <- "Frequency"
title1 <- "Histogram of Total Steps Taken per Day"
table1 <- ggplot(stepsGroupedByDate, aes(x=count)) + 
        geom_histogram(binwidth = diff(range(stepsGroupedByDate$count))/25, 
                       fill = "#014d64", colour = "#adadad") + 
        theme_economist() + 
        scale_colour_economist() + 
        xlab(xlab1) + 
        ylab(ylab1) + 
        ggtitle(title1)

print(table1)

## 2. a) Mean Number of Steps per Day
meansteps <- mean(stepsGroupedByDate$count)
print(meansteps)

## 2. b) Median Number of Steps per Day
mediansteps <- median(stepsGroupedByDate$count)
print(mediansteps)

## What is the average daily activity pattern?

stepsGroupedByInterval <- 
        tblSteps %>%
        group_by(interval) %>%
        summarize(average = mean(steps, na.rm = TRUE))

print(stepsGroupedByInterval)

## 1. Time Series Plot

xlab2 <- "5 Minute Interval"
ylab2 <- "Average Number of Steps"
title2 <- "Average Number of Steps Taken Per 5 Minute Interval"
table2 <- ggplot(stepsGroupedByInterval, aes(x=interval, y=average)) + 
        geom_line(colour = "#014d64", size = 0.75) + 
        theme_economist() + 
        scale_colour_economist() + 
        xlab(xlab2) + 
        ylab(ylab2) + 
        ggtitle(title2)        

print(table2)

## 2. 5 Minute Interval Max
stepsMax <- stepsGroupedByInterval[which.max(stepsGroupedByInterval$average),]
print(stepsMax)


## Imputing missing values

## Calculate and report the total number of missing values in the dataset 
sum(is.na(tblSteps$steps))

## Devise a strategy for filling in all of the missing values in the dataset.

## Create a new dataset that is equal to the original dataset but with 
## the missing data filled in.

tblSteps2 <- tblSteps

tblSteps2$steps[is.na(tblSteps$steps)] <- 
        ave(tblSteps$steps, 
        tblSteps$interval, 
        FUN=function(x) mean(x, na.rm = TRUE)) [is.na(tblSteps$steps)]

## Make a histogram of the total number of steps taken each day

stepsGroupedByDate2 <- 
        tblSteps2 %>%
        group_by(date) %>%
        summarize(count = sum(steps, na.rm = TRUE))

xlab3 <- "Total Number of Steps Taken in a Day"
ylab3 <- "Frequency"
title3 <- "Histogram of Total Steps Taken per Day (NA's Imputed)"
table3 <- ggplot(stepsGroupedByDate2, aes(x=count)) + 
        geom_histogram(binwidth = diff(range(stepsGroupedByDate2$count))/25, 
                       fill = "#014d64", colour = "#adadad") + 
        theme_economist() + 
        scale_colour_economist() + 
        xlab(xlab3) + 
        ylab(ylab3) + 
        ggtitle(title3)

print(table3)

##Calculate and report the mean and median total number of steps taken per day.

## 4. a) Mean Number of Steps per Day
meansteps2 <- mean(stepsGroupedByDate2$count)
print(meansteps2)

## 4. b) Median Number of Steps per Day
mediansteps2 <- median(stepsGroupedByDate2$count)
print(mediansteps2)


## Do these values differ from the estimates from the first part of the 
## assignment? 

## Difference in Mean

diffmean <- meansteps2 - meansteps
print(diffmean)

## Difference in Median

diffmedian <- mediansteps2 - mediansteps
print(diffmedian)

##Commentary!

## What is the impact of imputing missing data on the estimates of the total 
## daily number of steps?

## Something about Increase!


## Are there differences in activity patterns between weekdays and weekends?

## Use the dataset with the filled-in missing values for this part

## 1. Create a new factor variable in the dataset with two levels -- "weekday" 
## and "weekend" indicating whether a given date is a weekday or weekend day.

weekdayorweekend <- function(x) {
        ifelse(isWeekday(x), "Weekday", "Weekend")
}

withdates <- mutate(tblSteps2, daytype = weekdayorweekend(date))

weekdaysonly <- filter(withdates, daytype == "Weekday")

weekendsonly <- filter(withdates, daytype == "Weekend")

## Make a panel plot

## Weekday Plot

weekdaySteps <- 
        weekdaysonly %>%
        group_by(interval) %>%
        summarize(average = mean(steps, na.rm = TRUE))

xlab4 <- "5 Minute Interval"
ylab4 <- "Average Number of Steps"
title4 <- "Average Number of Steps Taken Per 5 Minute Interval (Weekdays)"
table4 <- ggplot(weekdaySteps, aes(x=interval, y=average)) + 
        geom_line(colour = "#014d64", size = 0.75) + 
        theme_economist() + 
        scale_colour_economist() + 
        xlab(xlab4) + 
        ylab(ylab4) + 
        ggtitle(title4)        

## Weekend Plot

weekendSteps <- 
        weekendsonly %>%
        group_by(interval) %>%
        summarize(average = mean(steps, na.rm = TRUE))

xlab5 <- "5 Minute Interval"
ylab5 <- "Average Number of Steps"
title5 <- "Average Number of Steps Taken Per 5 Minute Interval (Weekends)"
table5 <- ggplot(weekendSteps, aes(x=interval, y=average)) + 
        geom_line(colour = "#014d64", size = 0.75) + 
        theme_economist() + 
        scale_colour_economist() + 
        xlab(xlab5) + 
        ylab(ylab5) + 
        ggtitle(title5)        

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
        require(grid)
        
        # Make a list from the ... arguments and plotlist
        plots <- c(list(...), plotlist)
        
        numPlots = length(plots)
        
        # If layout is NULL, then use 'cols' to determine layout
        if (is.null(layout)) {
                # Make the panel
                # ncol: Number of columns of plots
                # nrow: Number of rows needed, calculated from # of cols
                layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                                 ncol = cols, nrow = ceiling(numPlots/cols))
        }
        
        if (numPlots==1) {
                print(plots[[1]])
                
        } else {
                # Set up the page
                grid.newpage()
                pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
                
                # Make each plot, in the correct location
                for (i in 1:numPlots) {
                        # Get the i,j matrix positions of the regions that contain this subplot
                        matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
                        
                        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                                        layout.pos.col = matchidx$col))
                }
        }
}


