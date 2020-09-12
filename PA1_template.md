#Reproducible Research Assessment by Punit Kapoor

#Step-1 : Loading and preprocessing the data
unzip("activity.zip")
walkData <- read.csv("activity.csv")

#Step-2 : What is mean total number of steps taken per day?
#2.1 - Calculate the total number of steps taken per day
totalStepsTaken <- aggregate(steps~date, data=walkData, sum, na.rm=TRUE)

#2.2 - Make a histogram of the total number of steps taken each day
hist(totalStepsTaken$steps, 
     main="Mean total steps taken per day", 
     xlab="Steps", 
     ylab="Number of Days", 
     ylim=c(0,35),
     col="blue")

#2.3 - Calculate and report the mean and median of the total number of steps taken per day
meanStepsTaken <- mean(totalStepsTaken$steps)
medianStepsTaken <- median(totalStepsTaken$steps)

#Step-3 : What is the average daily activity pattern?
#3.1 - Make a time series plot 
stepsToInterval <- aggregate(steps~interval, data=walkData, mean, na.rm=TRUE)
plot(steps~interval,data=stepsToInterval,type="l")

#3.2 - Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max5Minute <- stepsToInterval[which.max(stepsToInterval$steps),]$interval

#Step-4 : Imputing missing values
#4.1 : Calculating and reporting the total number of missing values in the dataset
totalMissingValues <- sum(is.na(walkData$steps))

##4.2 : Devising a strategy for filling in all of the missing values in the dataset
allTimeSlotsValues <- unique(walkData$interval)

#4.3 : Creating a new dataset that is equal to the original dataset but with the missing data filled in
newData <- walkData
setMissingDataValues <- function(timeSlot) {
  newData[which(walkData$interval==timeSlot & is.na(walkData$steps)), ]$steps <<- 
    mean(walkData[which(walkData$interval==timeSlot & !is.na(walkData$steps)), ]$steps)
}

lapply(allTimeSlotsValues, setMissingDataValues)

#4.4 : Making a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day
totalNewStepsTaken <- aggregate(steps~date, data=newData, sum, na.rm=TRUE)
hist(totalNewStepsTaken$steps, 
     main="Mean total steps taken per day (with NA values imputed)", 
     xlab="Steps", 
     ylab="Number of Days", 
     ylim=c(0,35),
     col="green")

meanNewStepsTaken <- mean(totalNewStepsTaken$steps)
medianNewStepsTaken <- median(totalNewStepsTaken$steps)

#Step-5 : Are there differences in activity patterns between weekdays and weekends?
#5.1 - Creating a new factor column with the Weekday or Weekend identifier
newData$type_of_day <- weekdays(as.Date(newData$date))
newData$type_of_day[newData$type_of_day %in% c('Saturday','Sunday')] <-"Weekend"
newData$type_of_day[newData$type_of_day != "Weekend"] <-"Weekday"
newData$type_of_day <- as.factor(newData$type_of_day)

Weekday_data <- subset(newData, type_of_day=="Weekday")
Weekday_stepsToInterval <- aggregate(steps~interval, data=Weekday_data, mean)

Weekend_data <- subset(newData, type_of_day=="Weekend")
Weekend_stepsToInterval <- aggregate(steps~interval, data=Weekend_data, mean)

#5.2 - Make a panel plot containing a time series plot
par(mfrow=c(2,1), mar=c(4,4,2,1), oma=c(0,0,2,0))

with(newData, {
  plot(steps~interval,data=Weekday_stepsToInterval,type="l",
       main="Steps for Weekdays", xlab="Time Interval", col="red")
  plot(steps~interval,data=Weekend_stepsToInterval,type="l",
       main="Steps for Weekends", xlab="Time Interval", col="red")
})
