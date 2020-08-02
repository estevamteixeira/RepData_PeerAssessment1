#source("1-download.R")

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

# looking where are the NA's: only steps variable
summary(activity)

# we can impute them using the overall mean or multiple imputation

# overall mean
activity2 <- activity
activity2$steps[which(is.na(activity2$steps))] <- mean(activity2$steps,
                                                       na.rm = TRUE)

# 7. Histogram of the total number of steps taken each day after
# missing values are imputed

png("plot3.png", width=480, height=480)

activity2 %>%
    select(steps, date) %>%
    group_by(date) %>%
    summarise(total_steps = sum(steps)) %>%
    ggplot(aes(x = total_steps)) +
    geom_histogram() +
    labs(x = "Total number of steps", y="Frequency",
         title = "Total steps per day - after imputation") +
    theme(
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size = 10),
        axis.text.y = element_blank())

# Closing device
dev.off()

## We can also plot both hisotgrams for comparison

activity2$type <- "Imputed"
activity$type <- "Observed"

all <- rbind(activity, activity2)

png("plot4.png", width=480, height=480)

all %>%
    select(steps, date, type) %>%
    group_by(date, type) %>%
    summarise(total_steps = sum(steps)) %>%
    ggplot(aes(x = total_steps, color = type, fill = type, alpha = type)) +
    geom_histogram(alpha = 0.5, position= "identity") +
    scale_color_manual(values=c("#5e3c99", "#e66101")) +
    scale_fill_manual(values=c("#5e3c99", "#e66101")) +
    scale_alpha_manual(values = c(0.4, 0.5)) +
    labs(x = "Total number of steps", y="Frequency",
         title = "Total steps per day - after imputation") +
    theme(
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size = 10),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(face="bold", size = 10),
        legend.title = element_blank(),
        legend.justification = "center")

# Closing device
dev.off()

# Panel plot comparing the average number of steps taken per 5-minute
# interval across weekdays and weekends

# # change system language to english otherwise days will be in portuguese
Sys.setlocale("LC_ALL","English")

# Specify weekdays and weekends
activity$Day_type <- ifelse(tolower(weekdays(activity$date)) %in% c("saturday","sunday"),
                            "Weekends", "Weekdays")

png("plot5.png", width=480, height=480)

activity %>%
    filter(!is.na(steps)) %>%
    select(steps, interval, Day_type) %>%
    group_by(interval, Day_type) %>%
    summarise(mean_steps = mean(steps)) %>%
    ggplot(aes(x = interval, y = mean_steps)) +
    geom_line() +
    facet_grid( ~ Day_type) +
    labs(x = "5-minute Interval", y = "Average number of steps",
         title = "Average daily activity pattern - Weedays x Weekends") +
    theme(
        axis.title.x = element_text(face="bold", size = 12),
        axis.title.y = element_text(face="bold", size = 12),
        axis.text.x = element_text(face="bold", size = 10),
        axis.text.y = element_blank(),
        strip.text.x = element_text(face="bold", size = 12))

# Closing device
dev.off()

