# Make training data using: subset(ts_obj, end = length(ts_obj) - 17)
trainingTS <- subset(monthly_ts, end = length(monthly_ts) - 17)
# Make validation data using: subset(ts_obj, start = length(ts_obj) - 16, end = length(ts_obj) - 5)
validationTS <- subset(monthly_ts, start = length(monthly_ts) - 16, end = length(monthly_ts) - 5)
# Make testing data using: subset(ts_obj, start = length(ts_obj) - 4)
testingTS <- subset(monthly_ts, start = length(monthly_ts) - 4)
# Validation and test data combined
validtestTS <- subset(monthly_ts, start = length(monthly_ts) - 16)

# Explore training data
plot(trainingTS) # there is definitely seasonality, slight trend, seems to have additive variance
decomp_stl <- stl(monthly_ts, s.window = 20) # decompose ts with STL to separate components
plot(decomp_stl) # plots decomposed components

# STL decomposition was used due to its allowance for changing effects of seasons and trends

# Classical: seasonality is same over and over again... don't use this method on data w/ changing seasonality
add_decomp <- decompose(monthly_ts, type = c("additive")) # other method for additive
mult_decomp <- decompose(monthly_ts, type = c("multiplicative")) # decompose multiplicative
plot(add_decomp)
plot(mult_decomp)

############################## Try out different models w/ Training Data #######################################

# Single ESM
hw.ses <- ses(trainingTS, initial = "optimal", h = 17)
summary(hw.ses)

# Holt-Winters Additive ESM
hw.add_t <- hw(trainingTS, seasonal = "additive", h = 17)
summary(hw.add_t)

# Holt-Winters Multiplicative ESM
hw.mult_t <- hw(trainingTS, seasonal = "multiplicative", h = 17)
summary(hw.mult_t)

# Look at MAPE/MAE values in summary output to pick the best few models to be scored with validation data
# SES gave the highest MAPE/MAE values, so it will be dropped here and HW ESMs will be evaluated moving forward
add_results_t = summary(hw.add_t)
mult_results_t = summary(hw.mult_t)

# Create df with predicted values, actual values (validation and test), and date for the 17 month forecast
# This step was done to make plotting easier for predicted vs actual values
results_df <- data.frame(add_pred = add_results_t$`Point Forecast`,
                         mult_pred = mult_results_t$`Point Forecast`,
                         actual = validtestTS,
                         date = seq(as.Date('2019-01-01'), as.Date('2020-05-01'), by="months"))

############################################### PLOTS ###################################################

# Exploring the data: this is only done on training data

# Plot of ozone values with the trend overlay (training data)
plot(trainingTS, 
     col = "black", 
     lwd = 4, 
     main = "Ozone Values with Trend Overlay", 
     xlab="Year", 
     ylab="Ozone Concentration")

lines(decomp_stl$time.series[,2], col = "red", lwd = 2) # red trendline with 2 width

# Plot of ozone values with seasonally adjusted values (training data)
plot(trainingTS, 
     col = "black", 
     lwd = 4,
     main = "Ozone Values with Seasonally Adjusted Overlay", 
     xlab = "Year", 
     ylab="Ozone Concentration")

lines(trainingTS - decomp_stl$time.series[,1], col = "blue", lwd = 2) # seasonally adjusted = Yt - St

# Warning: This is one of the last parts to HW2 but I'm grouping with EDA plots
# Plot predicted vs actual values for validation and test data
# We are no longer exploring data, we've chosen HW additive to forecast our data
plot(results_df$add_pred ~ results_df$date, 
     col = "black", 
     main = "Max 8 Hour Ozone Concentration, Monthly Mean", 
     xlab = "Year", 
     ylab = "Ozone Concentration", 
     lwd = 2)

lines(results_df$actual ~ results_df$date, 
      col = "blue", 
      main = "Max 8 Hour Ozone Concentration, Monthly Mean", 
      xlab = "Month", 
      ylab = "Ozone Concentration", 
      lwd = 2)

################################ Calculate accuracy with Validation data #################################

# Only use validation data set here (not test data!) and compare MAE/MAPE values for HW additive/mult
# Choose final model with best MAPE/MAE to be scored with test data

# Forecast predictions from fitted Additive HW ESM model on validation (12 months)
v.addresults = forecast(hw.add_t, h = 12)
# Calculate validation data error with additive HW
add_error_v <- validationTS - v.addresults$mean
# Additive HW validation MAE = 0.0021
MAE_add_v <- mean(abs(add_error_v))
# Additive HW validation MAPE = 5.05
MAPE_add_v <- mean(abs(add_error_v)/abs(validationTS))*100


# Forecast predictions from fitted Multiplicative HW ESM model on validation (12 months)
v.multresults <- forecast(hw.mult_t, h = 12)
# Calculate validation data error with multiplicative HW
mult_error_v <- validationTS - v.multresults$mean
# Multiplicative HW validation MAE = 0.0029
MAE_mult_v <- mean(abs(mult_error_v))
# Multiplicative HW validation MAPE = 6.90
MAPE_mult_v <- mean(abs(mult_error_v)/abs(validationTS))*100

# HW additive had better MAPE/MAE values, so we will use that model for scoring test data.

############################### Calculate accuracy with Testing data ##################################### 

# Compute MAE/MAPE using final model to score accuracy on test data

# Forecast predictions from fitted Additive HW ESM model on testing (5 months)
vt.addresults <- forecast(hw.add_t, h = 17) # forecast of entire holdout (17 months)
vt_add.predictions <-  as.list(vt.addresults$mean) # list of holdout forecast
add_error_t <-  testingTS - vt_add.predictions[[1]][13:17] # test data error with additive HW (5 months)
MAE_add_t <-  mean(abs(add_error_t)) # 0.003981774
MAPE_add_t <- (mean(abs(add_error_t)/abs(testingTS)))*100 # 10.37411
