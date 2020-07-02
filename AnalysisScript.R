## 1. Loading the data
## -------------------
unzip(zipfile = "activity.zip")
unlink("activity.zip")

library(ggplot2)
library(plyr)
library(scales)
library(lattice) 
activity <- read.csv("activity.csv")

activity$date <- as.Date(activity$date, "%Y-%m-%d")
activity <- as.data.frame(activity)


## What is mean total number of steps taken per day?

png("histogram.png", width=600, height=600)
steps <- with(activity, tapply(steps, date, sum, na.rm = TRUE))
hist(steps, breaks=6, xlab="Steps", main = "Total Steps per Day")
dev.off()

 
mean(steps) 

median(steps)





##2. What is the average daily activity pattern?
sumTable <- aggregate(activity$steps ~ activity$date, FUN=sum, )
colnames(sumTable)<- c("Date", "Steps")

clean <- activity[!is.na(activity$steps),]

##create average number of steps per interval
intervalTable <- ddply(clean, .(interval), summarize, Avg = mean(steps))

##Create line plot of average number of steps per interval
png("Average Number of Steps per Interval.png", width=600, height=600)
p <- ggplot(intervalTable, aes(x=interval, y=Avg), xlab = "Interval", ylab="Average Number of Steps")
p + geom_line()+xlab("Interval")+ylab("Average Number of Steps")+ggtitle("Average Number of Steps per Interval")
dev.off()

##Maximum steps by interval
maxSteps <- max(intervalTable$Avg)

##Which interval contains the maximum average number of steps
intervalTable[intervalTable$Avg==maxSteps,1]





##3. Imputing missing values
activity$day <- weekdays(as.Date(activity$date))
activity$DateTime<- as.POSIXct(activity$date, format="%Y-%m-%d")
clean <- activity[!is.na(activity$steps),]
nrow(activity[is.na(activity$steps),])
avgTable <- ddply(clean, .(interval, day), summarize, Avg = mean(steps))
nadata<- activity[is.na(activity$steps),]
newdata<-merge(nadata, avgTable, by=c("interval", "day"))
newdata2<- newdata[,c(6,4,1,2,5)]
colnames(newdata2)<- c("steps", "date", "interval", "day", "DateTime")
mergeData <- rbind(clean, newdata2)
sumTable2 <- aggregate(mergeData$steps ~ mergeData$date, FUN=sum, )
colnames(sumTable2)<- c("Date", "Steps")

## Mean of Steps with NA data taken care of
as.integer(mean(sumTable2$Steps))

## Median of Steps with NA data taken care of
as.integer(median(sumTable2$Steps))

## Creating the histogram of total steps per day, categorized by data set to show impact
png("histogram of total steps per day, categorized by data set to show impact.png", width=600, height=600)
hist(sumTable2$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Blue")
hist(sumTable$Steps, breaks=5, xlab="Steps", main = "Total Steps per Day with NAs Fixed", col="Orange", add=T)
legend("topright", c("Imputed Data", "Non-NA Data"), fill=c("blue", "orange") )
dev.off()

## 4. Are there differences in activity patterns between weekdays and weekends?

## Create new category based on the days of the week
mergeData$DayCategory <- ifelse(mergeData$day %in% c("sabado", "domingo"),"Weekend","Weekday")
intervalTable2 <- ddply(mergeData, .(interval, DayCategory), summarize, Avg = mean(steps))

## Creating Plot Average Steps per Interval Based on Type of Day
png("Average Steps per Interval Based on Type of Day.png", width=1050, height=600)
xyplot(Avg~interval|DayCategory, data=intervalTable2, type="l",layout = c(0,2),main="Average Steps per Interval Based on Type of Day"
       ,ylab="Average Number of Steps", xlab="Interval")
dev.off()