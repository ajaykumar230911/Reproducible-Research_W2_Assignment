---
title: "ReproducibleResearch"
author: "Ajay"
date: "August 20, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
## Load in the necessary packages

library(knitr)
opts_chunk$set(echo = TRUE)

library(dplyr)
library(lubridate)
library(ggplot2)


## Loading and preprocessing the data
## Reading in the data

data <- read.csv("activity.csv", header = TRUE, sep = ',', colClasses = c("numeric", "character",
data$date <- ymd(data$date)
str(data)
head(data)

## What is mean total number of steps taken per day?

# 1. Calculate the total number of steps taken per day.
steps <- data %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarize(steps = sum(steps)) %>%
    print

# 2. Use ggplot for making the histogram

ggplot(steps, aes(x = steps)) +
    geom_histogram(fill = "firebrick", binwidth = 1000) +
    labs(title = "Histogram of Steps per day", x = "Steps per day", y = "Frequency")

# 3. Calculate the mean and median of the total number of steps taken per day
mean_steps <- mean(steps$steps, na.rm = TRUE)
median_steps <- median(steps$steps, na.rm = TRUE)

mean_steps

median_steps

## What is the average daily activity pattern?

# 1. Calculate the average number of steps taken in each 5-minute interval per day 

interval <- data %>%
    filter(!is.na(steps)) %>%
    group_by(interval) %>%
    summarize(steps = mean(steps))

# Use ggplot for making the time series of the 5-minute interval and average steps taken
ggplot(interval, aes(x=interval, y=steps)) +
    geom_line(color = "firebrick")

# 2. Use which.max() to find out the maximum steps, on average, across all the days

interval[which.max(interval$steps),]

####  Imputing missing values

# 1. Summarize all the missing values
sum(is.na(data$steps))

# 2. Let’s take the approach to fill in a missing NA with the average number of steps in the same 5-min interval

##### Strategy: Use mean interval steps from Mean Steps for that interval.

# Create a new dataset as the original and use tapply for filling in the missing values with the average number of steps per 5-minute interval
data_full <- data
nas <- is.na(data_full$steps)
avg_interval <- tapply(data_full$steps, data_full$interval, mean, na.rm=TRUE, simplify=TRUE)
data_full$steps[nas] <- avg_interval[as.character(data_full$interval[nas])]

#Check that there are no missing values

sum(is.na(data_full$steps))

# 4. Calculate the number of steps taken in each 5-minute interval per day using dplyr and group by interval. Use ggplot for making the histogram

steps_full <- data_full %>%
    filter(!is.na(steps)) %>%
    group_by(date) %>%
    summarize(steps = sum(steps)) %>%
    print


ggplot(steps_full, aes(x = steps)) +
    geom_histogram(fill = "firebrick", binwidth = 1000) +
    labs(title = "Histogram of Steps per day, including missing values", x = "Steps per day", y = "Frequency")

# Calculate the mean and median steps with the filled in values
mean_steps_full <- mean(steps_full$steps, na.rm = TRUE)
median_steps_full <- median(steps_full$steps, na.rm = TRUE)

mean_steps_full
median_steps_full

## 1. Use dplyr and mutate to create a new column, weektype, and apply whether the day is weekend or weekday
data_full <- mutate(data_full, weektype = ifelse(weekdays(data_full$date) == "Saturday" | weekdays(data_full$date) == "Sunday", "weekend", "weekday"))
data_full$weektype <- as.factor(data_full$weektype)
head(data_full)


# 2. Calculate the average steps in the 5-minute interval and use ggplot for making the time series of the 5-minute interval for weekday and weekend, and compare the average steps
interval_full <- data_full %>%
    group_by(interval, weektype) %>%
    summarise(steps = mean(steps))
s <- ggplot(interval_full, aes(x=interval, y=steps, color = weektype)) +
    geom_line() +
    facet_wrap(~weektype, ncol = 1, nrow=2)
print(s)

#### From the two plots it seems that the test object is more active earlier in the day during weekdays compared to weekends, but more active throughout the weekends compared with weekdays (probably because the oject is working during the weekdays, hence moving less during the day).

```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
