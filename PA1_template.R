
## ------------------------------------------------------------------------------
library(ggplot2)
library(Hmisc)

## ---Loading and preprocessing the data------------------------------------------
if(!file.exists('activity.csv')){
  unzip('activity.zip')
}
data <- read.csv('activity.csv')


## ---Total steps per day---------------------------------------------------------
total_steps <- tapply(data$steps,data$date,FUN=sum,na.rm=TRUE)
mean(total_steps,na.rm=TRUE)
median(total_steps,na.rm=TRUE)
qplot(total_steps,binwidth=1000,xlab="Total Steps per Day")


## ---Time series plot------------------------------------------------------------
avg <- aggregate(x=list(steps=data$steps),by=list(interval=data$interval),FUN=mean,na.rm=TRUE)
ggplot(data=avg,aes(x=interval,y=steps))+
  geom_line()+
  xlab("5-min Interval")+
  ylab("Average steps taken")


## ---most_steps------------------------------------------------------------------
Most_Steps <- which.max(avg$steps)
Most_Steps


## ---Missing values--------------------------------------------------------------
MissingValues <- is.na(data$steps)
table(MissingValues)


## ---Impute_data-----------------------------------------------------------------
ImputedData <- data
ImputedData$steps <- impute(data$steps,fun=mean)


## ---Histogram of imputed data---------------------------------------------------
ImputedStepsPerDay <- tapply(ImputedData$steps,ImputedData$date,sum)
qplot(ImputedStepsPerDay,xlab='Total Steps per Day',ylab='Frequency of Steps',binwidth=500)


## ---Mean and median of imputed data---------------------------------------------
ImputedStepsPerDay_Mean <- mean(ImputedStepsPerDay)
ImputedStepsPerDay_Median <- median(ImputedStepsPerDay)
ImputedStepsPerDay_Mean
ImputedStepsPerDay_Median


## ---Function to determine type of day--------------------------------------------
TypeOfDay <- function(date){
  day <- weekdays(date)
  if (day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday"))
    return("weekday")
  else if (day %in% c("Saturday","Sunday"))
    return("weekend")
  else
    stop("not a valid date")
}

ImputedData$date <- as.Date(ImputedData$date)
ImputedData$day <- sapply(ImputedData$date,FUN=TypeOfDay)


## ---Weekday vs Weekend steps-----------------------------------------------------
avg2 <- aggregate(steps~interval+day,data=ImputedData,mean)
ggplot(avg2,aes(interval,steps))+
  geom_line()+
  facet_grid(day~.)+
  xlab("5-min interval")+
  ylab("Number of Steps")