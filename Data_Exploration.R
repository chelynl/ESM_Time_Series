# Load packages
library(readr)
library(dplyr)
library(broom)
library(tseries)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(lubridate)
library(sqldf)
library(imputeTS)
library(tidyverse)

# Import data
data <- read_csv('Ozone_Raleigh2.csv', col_types = cols(Date = col_date(format = '%m/%d/%Y')))

# Change Daily Max column name so it is shorter and easier to handle
names(data)[5] <- "maxOzoneC"

### Using the daily data on ozone, how many days are missing? 60 days ###
print(data$Date[1]) # shows first date obs
print(tail(data$Date, n=1)) # shows last date obs

# Check for missing values in date by generate theoretical time sequence and then join with actual data
days_seq <- seq(as.Date("2014/01/01"), as.Date("2020/05/31"), by="days")
days_seq <- as.data.frame(days_seq)
colnames(days_seq) <- "Date"

# Merge days sequence with date variable in data set
# joined_data <- merge(x = data, y = days_seq, by.x = "Date", by.y = "days", all.y = TRUE)
ts_data <- left_join(days_seq, data, by='Date')
sum(is.na(ts_data$maxOzoneC)) # 60 missing days in data

# Still using the daily data, what year has the most missing days? 2017
ts_data %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(null_count = sum(is.na(maxOzoneC))) %>%
  arrange(desc(null_count))

# Rolling up the data to monthly, what is the mean max 8 hour Ozone Concentration for January 2017? 
# Keep precision to 3 decimal places. --> 0.029
ts_data %>%
  mutate(month = month(Date)) %>%
  mutate(year = year(Date)) %>%
  group_by(year, month) %>%
  filter(year==2017 & month==1) %>%
  summarise(mean_ozone = mean(maxOzoneC))

# Create a time plot of the mean monthly max 8 hour ozone concentration
monthly_data <- ts_data %>%
  mutate(month = month(Date)) %>%
  mutate(year = year(Date)) %>%
  group_by(year, month) %>%
  summarise(mean_ozone = mean(maxOzoneC, na.rm = T))

# Create monthly ts object
monthly_ts <- ts(monthly_data$mean_ozone, start = c(2014, 1), end = c(2020, 5), frequency = 12)
plot(monthly_ts) # monthly ts plot