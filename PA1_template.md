activityData <- read.csv ("activity.csv", header = T, sep = ",", stringsAsFactors = F)

activityData$date <- as.Date(activityData$date, "%Y-%m-%d")
str(activityData)

dim(activityData)

head(activityData)


## CALCULATE THE TOTAL STEPS PER DAY  BEGIN
library (dplyr)
AvgDay<-activityData %>% group_by(date) %>% 

summarize(total.steps = sum(steps,na.rm=T),mean.steps =mean(steps,na.rm = T))

library(ggplot2)
g <- ggplot(AvgDay, aes(x=total.steps))
g + geom_histogram(binwidth = 2500) + theme(axis.text = element_text(size = 12),  
      axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")

summary(AvgDay$total.steps)

summary (AvgDay$mean.steps)

## CALCULATE THE TOTAL STEPS PER DAY  END


## CALCULATE DAILY ACTIVITY PATTERN  BEGIN

AvgInterval <- activityData %>% group_by(interval) %>%
      summarize(mean.steps = mean(steps, na.rm = T))
g <- ggplot(AvgInterval, aes(x = interval, y = mean.steps))
g + geom_line() + theme(axis.text = element_text(size = 12), 
      axis.title = element_text(size = 14, face = "bold")) + 
      labs(y = "Mean number of steps") + labs(x = "Interval")

## CALCULATE DAILY ACTIVITY PATTERN  END



## CALCULATE MISSING VALUES  BEGIN
mean(is.na(activityData$steps))

sum(is.na(activityData$steps))

sum(is.na(AvgInterval$mean.steps))

newData <- activityData

for (i in 1:nrow(newData)) {
      if (is.na(newData$steps[i])) {
            index <- newData$interval[i]
            value <- subset(AvgInterval, interval==index)
            newData$steps[i] <- value$mean.steps
      }
}
head(newData)

newAvg <- newData %>% group_by(date) %>% summarize(total.steps = sum(steps, na.rm = T))
      
g <- ggplot(newAvg, aes(x=total.steps))
g + geom_histogram(binwidth = 2500) + theme(axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)) + labs(y = "Frequency") + labs(x = "Total steps/day")
      
      summary (AvgDay$total.steps)
      
      sd(AvgDay$total.steps, na.rm=T)
      
      summary (newAvg$total.steps)
      
      sd(newAvg$total.steps, na.rm=T)

## CALCULATE MISSING VALUES  END


## CALCULATE differences in activity patterns between weekdays and weekends BEGIN

newData$day <- ifelse(weekdays(newData$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

wkend <- filter(newData, day == "weekend")
wkday <- filter(newData, day == "weekday")

wkend <- wkend %>%
      group_by(interval) %>%
      summarize(mean.steps = mean(steps)) 
wkend$day <- "weekend"

wkday <- wkday %>%
      group_by(interval) %>%
      summarize(mean.steps = mean(steps)) 
wkday$day <- "weekday"

newInterval <- rbind(wkend, wkday)
newInterval$day <- as.factor(newInterval$day)
newInterval$day <- relevel(newInterval$day, "weekend")

g <- ggplot (newInterval, aes (interval, mean.steps))
g + geom_line() + facet_grid (day~.) + theme(axis.text = element_text(size = 12), 
      axis.title = element_text(size = 14)) + labs(y = "Number of Steps") + labs(x = "Interval")
      
      

## CALCULATE differences in activity patterns between weekdays and weekends END

