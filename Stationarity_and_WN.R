# Run the ADF test on k=0,1,2 (3 lags) and save p-values in vector called ADF.Pvalues
ADF.Pvalues <- rep(NA, 3)

for(i in 0:2){
  ADF.Pvalues[i+1] <- adf.test(trainingTS, alternative = "stationary", k = i)$p.value
}

ADF.Pvalues # Fail to reject (at least 1 p-value above 0.05) --> RW exists. 

# Take first difference to see if you can convert RW to stationary
diff_data = diff(trainingTS)

# ADF on differenced data
ADF.Pvalues <- rep(NA, 3)

for(i in 0:2){
  ADF.Pvalues[i+1] <- adf.test(diff_data, alternative = "stationary", k = i)$p.value
}

ADF.Pvalues # Reject null (all below 0.05) --> stationary

# Plot the differences to see that we have now made the data stationary. 
plot(diff_data, 
     col="black", 
     lwd=4, 
     main="Differenced Ozone Values", 
     xlab="Year", 
     ylab="Ozone Concentration")

########################## Does the stationary time series exhibit white noise?##############################################################

# Perform Ljung-Box test with lag 1,2,..,10 to differenced ts (stationary) then store p-values in white.LB_diff
# fitdf arg tells how many autocorrelation terms to fit (sum of p and q), we set to 0 because we have not fit any AR or MA terms
# If there is no seasonality, then going back 10 lags is good enough to check (otherwise would need to increase lags to capture more seasons)

White.LB_diff <- rep(NA, 10)

for(i in 1:10){
  White.LB_diff[i] <- Box.test(diff_data, lag = i, type = "Lj", fitdf = 0)$p.value
}

# p-values >= 0.2 are recorded as 0.2 (for plotting purposes)
White.LB_diff <- pmin(White.LB_diff, 0.2)

# Let's look at a plot of these p-values (lags 1,2,...,10)
# The horizontal lines let us see which lags have p-values <0.05 and <0.01
barplot(White.LB_diff, 
        main = "Ljung-Box Test for Differenced Series", 
        ylab = "Probabilities (P-values)", 
        xlab = "Lags", 
        ylim = c(0, 0.2),
        names.arg = seq(0,9))

abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

# Lags 0-4 are not significant but the following lags are.
# This concludes that there autocorrelation exists in the differenced series --> Reject null.
# Still some unexplained variance in the data outside of white noise and more adjustments to the data will need to be done.
# We need to take care of dependencies by taking seasonality into consideration.
# We will explore ARMA/ARIMA models next!
