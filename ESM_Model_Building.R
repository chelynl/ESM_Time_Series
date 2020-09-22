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

# Plot decomposition
plot(decomp_stl,
     main = 'Decomposition of Time Series using STL')

# STL decomposition was used due to its allowance for changing effects of seasons and trends


######################################## Try out different models ########################################################################

# Single ESM: MAPE = 12.44395
hw.ses <- ses(trainingTS, initial = "optimal", h = 17)
summary(hw.ses)

# Holt-Winters Additive ESM: MAPE = 5.875792
hw.add_t <- hw(trainingTS, seasonal = "additive", h = 17)
summary(hw.add_t)

# Holt-Winters Multiplicative ESM: MAPE = 6.102974
hw.mult_t <- hw(trainingTS, seasonal = "multiplicative", h = 17)
summary(hw.mult_t)

# Create df with predicted values, actual values (validation and test), and date for the 17 month forecast
# Df makes plotting easier below
results_df <- data.frame(add_pred = add_results_t$`Point Forecast`,
                         mult_pred = mult_results_t$`Point Forecast`,
                         actual = validtestTS,
                         date = seq(as.Date('2019-01-01'), as.Date('2020-05-01'), by="months"))

############################################### PLOTS ####################################################################################

# Plot of ozone values with the trend overlay (training data)
plot(trainingTS, 
     col = "black", 
     lwd = 4, 
     main = "Ozone Values with Trend Overlay", 
     xlab="Year", 
     ylab="Ozone Concentration (ppm)")

lines(decomp_stl$time.series[,2], col = "red", lwd = 2) # red trendline with 2 width

# Plot of ozone values with seasonally adjusted values (training data)
plot(trainingTS, 
     col = "black", 
     lwd = 4,
     main = "Ozone Values with Seasonally Adjusted Overlay", 
     xlab = "Year", 
     ylab="Ozone Concentration (ppm)")

lines(trainingTS - decomp_stl$time.series[,1], col = "blue", lwd = 2) # seasonally adjusted = Yt - St

# Plot predicted vs actual values for validation and test data
plot(results_df$add_pred ~ results_df$date, 
     col = "black", 
     main = "Max 8 Hour Ozone Concentration, Monthly Mean", 
     xlab = "Year", 
     ylab = "Ozone Concentration (ppm)", 
     lwd = 2)

lines(results_df$actual ~ results_df$date, 
      col = "blue", 
      main = "Max 8 Hour Ozone Concentration, Monthly Mean", 
      xlab = "Month", 
      ylab = "Ozone Concentration (ppm)", 
      lwd = 2)

################################ Calculate accuracy with Validation data #################################################################

# Forecast predictions from fitted Additive HW ESM model on validation (12 months)
v.addresults = forecast(hw.add_t, h = 12)
# Calculate validation data error with additive HW
add_error_v <- validationTS - v.addresults$mean
# Additive HW validation MAE = 0.002137106
MAE_add_v <- mean(abs(add_error_v))
# Additive HW validation MAPE = 5.047895
MAPE_add_v <- mean(abs(add_error_v)/abs(validationTS))*100


# Forecast predictions from fitted Multiplicative HW ESM model on validation (12 months)
v.multresults <- forecast(hw.mult_t, h = 12)
# Calculate validation data error with multiplicative HW
mult_error_v <- validationTS - v.multresults$mean
# Multiplicative HW validation MAE = 0.002864497
MAE_mult_v <- mean(abs(mult_error_v))
# Multiplicative HW validation MAPE = 6.902708
MAPE_mult_v <- mean(abs(mult_error_v)/abs(validationTS))*100


# Forecast predictions from fitted SES model on validation (12 months)
v.sesresults <- forecast(hw.ses, h = 12)
# Calculate validation data error with SES
ses_error_v <- validationTS - v.sesresults$mean
# SES validation MAE = 0.0152816
MAE_ses_v <- mean(abs(ses_error_v))
# SES validation MAPE = 34.59344
MAPE_ses_v <- mean(abs(ses_error_v)/abs(validationTS))*100


# Due to the high MAE/MAPE values with the SES model, it was dropped and only additive and multiplicative HW models were being considered.

############################### Calculate accuracy with Testing data #####################################################################

# Forecast predictions from fitted Additive HW ESM model on testing (5 months)
vt.addresults <- forecast(hw.add_t, h = 17) # forecast of entire holdout (17 months)
vt_add.predictions <-  as.list(vt.addresults$mean) # list of holdout forecast
add_error_t <-  testingTS - vt_add.predictions[[1]][13:17] # test data error with additive HW (5 months)
MAE_add_t <-  mean(abs(add_error_t)) # 0.003981774
MAPE_add_t <- (mean(abs(add_error_t)/abs(testingTS)))*100 # 10.37411

# Forecast predictions from fitted Multiplicative HW ESM model on testing (5 months)
vt.multresults <- forecast(hw.mult_t, h = 17) # forecast of entire holdout (17 months)
vt_mult.predictions <- as.list(vt.multresults$mean) # list of holdout forecast
mult_error_t <-  testingTS - vt_mult.predictions[[1]][13:17] # test data error with mult HW (5 months)
MAE_mult_t <-  mean(abs(mult_error_t)) # 0.002899837
MAPE_mult_t <- (mean(abs(mult_error_t)/abs(testingTS)))*100 # 7.629787

# After scoring accuracy with the test data, we concluded that the additive HW model performed the best.
