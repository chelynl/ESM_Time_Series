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


######################################## Try out different models #######################################

# Single ESM: MAPE = 12.44395
hw.ses <- ses(trainingTS, initial = "optimal", h = 17)
summary(hw.ses)

# Holt-Winters Additive ESM: MAPE = 5.875792
hw.add_t <- hw(trainingTS, seasonal = "additive", h = 17)
summary(hw.add_t)

# Holt-Winters Multiplicative ESM: MAPE = 6.102974
hw.mult_t <- hw(trainingTS, seasonal = "multiplicative", h = 17)
summary(hw.mult_t)

# We decided not to drop Single ESM due to high MAPE/MAE values relative to HW additive and multiplicative models
# Note: do NOT report accuracy metrics from training data. It is only used for model selection.

add_results_t <- summary(hw.add_t) # forecast ozone levels for next 17 months using HW additive
mult_results_t <- summary(hw.mult_t) # forecast ozone levels for next 17 months using HW multiplicative

# Create df with predicted values, actual values (validation and test), and date for the 17 month forecast
# Df makes plotting easier below
results_df <- data.frame(add_pred = add_results_t$`Point Forecast`,
                         mult_pred = mult_results_t$`Point Forecast`,
                         actual = validtestTS,
                         date = seq(as.Date('2019-01-01'), as.Date('2020-05-01'), by="months"))

############################################### PLOTS ###################################################

# Plot of ozone values with the trend overlay
plot(trainingTS, 
     col = "black", 
     lwd = 4, 
     main = "Ozone Values with Trend Overlay", 
     xlab="Year", 
     ylab="Ozone Concentration (ppm)")

lines(decomp_stl$time.series[,2], col = "red", lwd = 2) # red trendline with 2 width

# Plot of ozone values with seasonally adjusted values
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

################################ Calculate accuracy with Test data #######################################

# Forecast predictions from fitted Additive HW ESM model on validation and test data
v.addresults = forecast(hw.add_t, h = 12) # for 12 month forecast (validation)
vt.addresults = forecast(hw.add_t, h = 17) # forecast for 17 months (validation + testing)

# Forecast predictions from fitted Multiplicative HW ESM model on validation and test data
v.multresults <- forecast(hw.mult_t, h = 12)
vt.multresults <- forecast(hw.mult_t, h = 17)

# Calculate prediction errors [actual - predicted] from forecast
add_error_v <- validationTS - v.addresults$mean # validation data error from additive
add_error_vt <- validtestTS - vt.addresults$mean # validation & test data error from additive
mult_error_v <- validationTS - v.multresults$mean # validation data error from multiplicative
mult_error_vt <- validtestTS - vt.multresults$mean # validation & test data error from multiplicative

# Calculate MAE and MAPE for validation and validation+test data
MAE_add <- mean(abs(add_error_v)) # 0.002137106
MAPE_add <- mean(abs(add_error_v)/abs(validationTS))*100 # 5.047895
MAE_mult <- mean(abs(mult_error_v)) # 0.002864497
MAPE_mult <- mean(abs(mult_error_v)/abs(validationTS))*100 # 6.902708

# MAE MAPE On the TEST Data
vt_addpredictions <-  as.list(vt.addresults$mean) # predicted values for validation and test (add)
vt_multpredictions <- as.list(vt.multresults$mean) # predicted values for validation and test (mult)
add_test_error <-  testingTS - vt_addpredictions[[1]][13:17] # actual - predicted for TEST data only (add)
mult_test_error <- testingTS - vt_multpredictions[[1]][13:17] # actual - predicted for TEST data only (mult)
add_MAE_test <- mean(abs(add_test_error)) # 0.003981774
add_MAPE_test <- (mean(abs(add_test_error)/abs(testingTS)))*100 # 10.37411
mult_MAE_test <- mean(abs(mult_test_error)) # 0.002899837
mult_MAPE_test <- (mean(abs(mult_test_error)/abs(testingTS)))*100 # 7.629787