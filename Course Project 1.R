#Loading and preprocessing the data

Data <- read.csv("./data/activity.csv")
str(Data)

#What is mean total number of steps taken per day?

steps_by_day <- tapply(Data$steps, Data$date, FUN = sum, na.rm = TRUE)

hist(steps_by_day,
     main = "Histogram of the total number of steps taken each day",
     xlab = "Total number of steps",
     ylab = "Days",
     ylim = c(0,31))

mean_steps <- mean(steps_by_day)
median_steps <- median(steps_by_day)

#What is the average daily activity pattern?

time_serie_plot <- tapply(Data$steps, Data$interval, FUN = mean, na.rm = TRUE)
time_serie_plot

plot(time_serie_plot,
     type = "l",
     xlab = "5 min. Intervals",
     ylab = "Average steps all day", 
     main = "Time Series Graph")

max_avg_interval <- which.max(time_serie_plot)
max_avg_steps<-time_serie_plot[[which(names(time_serie_plot) == names(max_avg_interval))]]

names(max_avg_interval); max_avg_steps

# Imputing missing values

total_na_vector <- sum(is.na(Data$steps))

avg_steps_by_day <- tapply(Data$steps, Data$date, FUN = mean, na.rm = TRUE)
avg_steps <- mean(avg_steps_by_day, na.rm = TRUE)
positions_to_change <- which(is.na(Data$steps))
Data_without_na <- Data
Data_without_na$steps[positions_to_change] <- avg_steps
steps_by_day <- tapply(Data_without_na$steps, Data_without_na$date, FUN = sum)

hist(steps_by_day,
     main = "Histogram of the total number of steps taken each day",
     xlab = "Total number of steps",
     ylab = "Days",
     ylim = c(0,40))

mean_steps <- mean(steps_by_day)
median_steps <- median(steps_by_day)

# Are there differences in activity patterns between weekdays and weekends?

Data_without_na$date <- as.Date(Data_without_na$date)
weekdays <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
Data_without_na$TypeDay <- factor((weekdays(Data_without_na$date) %in% weekdays), levels=c(FALSE, TRUE), labels=c('weekend', 'weekday'))

library(ggplot2)
averages <- aggregate(steps ~ interval + TypeDay, data = Data_without_na, mean)
ggplot(averages, aes(interval, steps)) + 
        geom_line() + 
        facet_grid(TypeDay ~ .) + 
        xlab("5 min. Intervals") + 
        ylab("Number of Steps")
