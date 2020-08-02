
if(!require(tidyverse)){
    install.packages("tidyverse")
    require(tidyverse)
}

# 2. Histogram of the total number of steps taken each day

png("plot1.png", width=480, height=480)

activity %>%
    filter(!is.na(steps)) %>%
    select(steps, date) %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps)) %>%
    ggplot(aes(x = total_steps)) +
    geom_histogram() +
    labs(x = "Total number of steps", y="Frequency",
         title = "Total steps per day") +
    theme(
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size = 10),
        axis.text.y = element_blank())

# Closing device
dev.off()

# 3. Mean and median number of steps taken each day

# mean
activity %>%
    filter(!is.na(steps)) %>%
    select(steps, date) %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps)) %>%
    ungroup() %>%
    summarise(mean = mean(total_steps))

# median
activity %>%
    filter(!is.na(steps)) %>%
    select(steps, date) %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps)) %>%
    ungroup() %>%
    summarise(median = median(total_steps))

# 4. Time series plot of the average number of steps taken

png("plot2.png", width=480, height=480)

activity %>%
    filter(!is.na(steps)) %>%
    select(steps, interval) %>%
    group_by(interval) %>%
    summarise(mean_steps = mean(steps)) %>%
    ggplot(aes(x = interval, y = mean_steps)) +
    geom_line() +
    labs(x = "5-minute Interval", y = "Average number of steps",
         title = "Average daily activity pattern") +
    theme(
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size = 10),
        axis.text.y = element_blank())

# Closing device
dev.off()

q5 <- activity %>%
    filter(!is.na(steps)) %>%
    select(steps, interval) %>%
    group_by(interval) %>%
    summarise(mean_steps = mean(steps)) 

q5$interval[which.max(q5$mean_steps)]

# 6. Code to describe and show a strategy for imputing missing data

# looking where are the NA's
summary(activity)

# we can impute using the overall mean or multiple imputation

activity$steps[which(is.na(activity$steps))] <- mean(activity$steps,
                                                     na.rm = TRUE)



