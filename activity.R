act_dat <- read.csv("C:/Users/user/Documents/R/Git_Repos/RepData_PeerAssessment1/activity.csv")

#Requirement number 1
##Calculate the mean total number of steps taken per day

Tot_Num_Step <- aggregate(steps ~ date, data = act_dat, sum, na.rm = TRUE)


## Create a histogram of the total number of steps taken each day
hist(Tot_Num_Step$steps)

##the mean and median of the total number of steps taken per day
Mean_Total <- mean(Tot_Num_Step$steps)

median_Total <- median(Tot_Num_Step$steps)


#Requirement number 2
##time series plot of the 5-minute interval and the average number of steps taken, averaged across all days (y-axis)
AVG_Num_Step <- aggregate(steps ~ interval, data = act_dat, sum, na.rm = TRUE)

with(AVG_Num_Step, plot(Group.1, steps, type = "l"))

AVG_Num_Step <- AVG_Num_Step[order(-AVG_Num_Step$steps),]

AVG_Num_Step[1,]

#Requirement number 3
##total number of missing values in the dataset
sum(is.na(act_dat$steps))
sum(is.na(act_dat$date))
sum(is.na(act_dat$interval))

##Imputing missing values
###Get the day mean steps
install.packages("plyr")
install.packages("Hmisc")
library(plyr)
library(Hmisc)

act_dat2 <- ddply(act_dat, "interval", mutate, imputed_steps = impute(steps, mean))

hist(act_dat2$imputed_steps)


#Requirement number 4 
##activity patterns between weekdays and weekends
act_dat2$Weekdays <- weekdays(as.Date(act_dat2$date))

act_dat2$wrkday <- with(act_dat2, ifelse(Weekdays== "Saturday",FALSE, ifelse(Weekdays== "Sunday",FALSE, TRUE)))

library(ggplot2)

x <- ggplot(act_dat2,aes(interval,imputed_steps))
x+ geom_line() + facet_grid(.~wrkday)
