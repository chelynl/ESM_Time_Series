# load packages
library(readr)
library(dplyr)
library(tseries)
library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(lubridate)
library(sqldf) # you could use sql to merge time sequence with data (I used base R though)

# set work directory 
setwd('C:/Users/chely/Documents/Time Series and Forecasting/Class Data')

# import data and convert Date column from character to date 
data <- read_csv('Ozone_Raleigh2.csv', col_types = cols(Date = col_date(format = '%m/%d/%Y')))

# Change Daily Max column name so it is shorter and easier to handle
names(data)[5] <- "maxOzoneC"


### Using the daily data on ozone, how many days are missing? 60 days. See next 3 steps. ###

# Step 1: see what the first and last dates are in your obs
print(data$Date[1])
print(tail(data$Date, n=1))

# Step 2: Make a theoretical time sequence using the first and last dates above
# This theoretical time sequence will be merged with the actual data to fill in the gaps of missing values
days_seq <- seq(as.Date("2014/01/01"), as.Date("2020/05/31"), by="days") # returns date sequence
days_seq <- as.data.frame(days_seq) # convert date sequence to dataframe
colnames(days_seq) <- "Date" # change col name to "Date"

# Step 3: Merge days sequence df with date variable in actual data set
# joined_data <- merge(x = data, y = days_seq, by.x = "Date", by.y = "days", all.y = TRUE)
ts_data <- left_join(days_seq, data, by='Date')
sum(is.na(ts_data$maxOzoneC)) # 60 missing days in data

# Still using the daily data, what year has the most missing days? 2017
ts_data %>%
  mutate(year = year(Date)) %>%
  group_by(year) %>%
  summarise(null_count = sum(is.na(maxOzoneC))) %>%
  arrange(desc(null_count))

# Rolling up the data to monthly, what is the mean max 8 hour Ozone Concentration for January 2017? 0.029
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

plot(monthly_ts, 
     main = 'Time Plot of Monthly Average Ozone',
     ylab = 'Ozone Concentration (ppm)')