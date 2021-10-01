---
title: "SC - Project2"
author: "Michelle Coen"
date: "10/1/2021"
output: html_document
---

```{r setup, include=FALSE}
library("tidyverse")
library("tidytuesdayR")
library("lubridate")
library("ggplot2")
library("dplyr")
```

### Part 1: Fun with Functions
## Part 1A: Exponential Transformation
```{r Part1A}
Exp <- function(x, k) {
  if (k<1) {
    stop("No calculation, k < 1", call. = FALSE)
  }
  if (k == 1){
    j <- (1 + x)
    print(j)
  }
  if (k > 1) { 
    K <- 2:k
    for (i in seq_along(K)) {
    m <- sum(1 + x + sum((x[i]^K)/factorial(K)))
    return(m[[1]])
    }}
}
Exp(1, 2)
```

## Part 1B: Sample Mean and Sample Standard Deviation
``` {r Part1B}
sample_mean <- function(x) {
  N <- length(x)
  f <- (sum(x)/N)
  return(f)
}
sample_mean(1:20)
mean(1:20) #Test
sample_sd <- function(x) {
  N <- length(x)
  g <- sqrt((sum((x-sample_mean(x))^2)/(N-1)))
  return(g)
}
sample_sd(1:20)
sd(1:20)
```

## Part 1C: Confidence Intervals
```{r Part1C}
calculate_CI <- function(x, conf) {
  N <- length(x)
  alpha <- (1 - conf)
  degrees_freedom <- (N - 1)
  standard_error <- sample_sd(x)/sqrt(N)
  t_score <- qt(p = alpha/2, df = degrees_freedom, lower.tail = FALSE)
  CI_lower <- sample_mean(x) - (t_score*standard_error)
  CI_upper <- sample_mean(x) + (t_score*standard_error)
  
  CI <- c(CI_lower, CI_upper)
  return(CI)
}
calculate_CI(c(1:20), 0.95)
```

### Part 2: Wrangling Data
``` {r Part2 setup}
#tuesdata <- tidytuesdayR::tt_load('2020-01-07')
#rainfall <- tuesdata$rainfall
#temperature <- tuesdata$temperature
#rainfall <- readRDS(here("data","tuesdata_rainfall.RDS"))
#temperature <- readRDS(here("data","tuesdata_temperature.RDS"))
library(here)
if(!file.exists(here("data","tuesdata_rainfall.RDS"))){
  tuesdata <- tidytuesdayR::tt_load('2020-01-07')
  rainfall <- tuesdata$rainfall
  temperature <- tuesdata$temperature
  
  # save the files to RDS objects
#  saveRDS(tuesdata$rainfall, file= here("data","tuesdata_rainfall.RDS"))
 # saveRDS(tuesdata$temperature, file= here("data","tuesdata_temperature.RDS"))
}
glimpse(rainfall)
glimpse(temperature)
```

``` {r Part2}
rainfall_free <- na.omit(rainfall) %>%
  mutate(date = paste(year, month, day, sep="-")) %>%
  mutate(date = ymd(date)) %>%
  select(station_code:date, -month, -day) %>%
  mutate(city_name = toupper(city_name))
combined <- inner_join(rainfall_free, temperature, by=c("city_name", "date"), keep=FALSE)
dim(combined)
```

### Part 3: Data Visualization
## Part 3A: Plotting Temperature Data Over Time
``` {r Part3A}
combined2014 <- combined %>%
  filter(year >= 2014)
combined2014 %>%
  ggplot(aes(x = date, y = temperature, color = temp_type)) +
  geom_line() +
  facet_wrap(city_name ~., scales = "free_x") +
  ggtitle("Minimum and Maximum Temperatures Over Time by City") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=45, hjust=1), plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.25)) +
  labs(y = "Temperature (Celcius)", x = "Year", subtitle = "Melbourne, Perth, Sydney, Brisbane, and Canberra have similar fluctuations \nof both minimum and maximum temperatures", caption = "Michelle Coen", color = "Temperature \nType")
```

## Part 3B: Plotting Rainfall Over Time
First, I set up a test function that allows the user to feed a specific city and year into the input and generate a stop message if the year/city combination do not exist in the data set.  If the input exists in the data, the test will generate a histogram of the log(average rainfall) by distribution of days within the specified city and year.
``` {r Part3B}
test <- function(city, time) {
  if ((!(city %in% combined$city_name)) | (!(time %in% combined$year))) {
    stop("Input does not exist in data set")}
  
histogram <- combined %>% 
  filter(city_name == city, year == time) %>% 
  ggplot(aes(log(rainfall))) + 
  geom_histogram(color = "black", fill = "purple") +
  ggtitle(paste("Distributution of Log of Average Rainfall by Days in", city, time)) +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.25)) +
  labs(y = "Number of Days", x = "Log of Average Rainfall", subtitle = "The number of days with each value of log of average rainfall within the specified city and year.", caption = "Michelle Coen", color = "Temperature \nType")
print(histogram)
}
test("PERTH", "2000")
```

### Part 4: Apply Functions and Plot
## Part 4A:
``` {r Part4A}
detach(package:plyr)
rain_df <- combined2014 %>%
  group_by(city_name, year) %>%
  summarize(avg_rainfall = sample_mean(rainfall), sd_rainfall = sample_sd(rainfall), lower_bound = calculate_CI(rainfall, conf=0.95)[1], upper_bound = calculate_CI(rainfall, conf = 0.95)[2], .groups = "keep")
```

## Part 4B: Tasks
``` {r Part4B}
rain_df %>%
  ggplot(aes(x = year, y = avg_rainfall)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymax = upper_bound, ymin = lower_bound), width = 0.3, color = "purple") +
  facet_wrap(city_name ~., scales = "free_x") +
  ggtitle("Average Rainfall per Year by City") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) +
  labs(y = "Average Rainfall (Inches)", x = "Year", subtitle = "Perth had the highest average rainfall per year and the widest 95% confidence interval, \nwhile Canberra had the lowest average rainfall per year and the smallest 95% confidence interval. \nBrisbane and Perth experienced the most extreme fluctuations.", caption = "Michelle Coen")
```