# Reading the timeseries dataset
time = read.csv("E:\\New folder\\timeseries.csv")
head(time)

# Checking whether there is any null value present
sum(is.na(time))

# Summary of our timeseries dataset
summary(time)

# Boxplot for determining is their any outliers
boxplot(time$SPC)

# Removing the outliers
Q = quantile(time$SPC, probs=c(.25, .75), na.rm = FALSE)
iqr = IQR(time$SPC)
up =  Q[2]+1.5*iqr # Upper Range  
low =  Q[1]-1.5*iqr # Lower Range
time_no_otlr = subset(time, time$SPC > (Q[1] - 1.5*iqr) & time$SPC < (Q[2]+1.5*iqr))

# Boxplot after removing the outlier from our dataset
boxplot(time_no_otlr$SPC)

# Comparing the summary between dataset with outliers and dataset after removing outliers
summary(time_no_otlr)
summary(time)

# Line Plot for Time Series 
plot(time_no_otlr$SPC ,type = 'l', xlab = "SPC", main = "LinePlot for Time Series")

# Histogram for Time Series
hist(time_no_otlr$SPC, xlab = "SPC", main = "Histogram for Time Series")

# Scatter plot for Time Series
plot(time_no_otlr$SPC, main = "Scatter Plot for Time Series", xlab = "SPC", type = "p")

# Normalization USing Min-Max technique
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
time_norm = (lapply(time_no_otlr[2], min_max_norm))
head(time_norm$SPC)
time_norm

# Normalization using Z-Score technique
time_norm_z = (time_no_otlr$SPC - mean(time_no_otlr$SPC)) / sd(time_no_otlr$SPC)
head(time_norm_z)
time_norm_z

# installing "forecast" & "tseries" libraries
install.packages("forecast")
library("forecast")
install.packages("tseries")
library("tseries")

head(time_no_otlr)

# ARIMA MODEL

# Changing SPC trend to Time Series
time_ts = ts(time_no_otlr$SPC,start = c(2012), frequency = 1 )

# Plotting SPC Time Series
autoplot(time_ts)

# Get auto p,q,d values 
auto.arima(time_ts)

# ADF test
adf.test(time_ts, k = 1)

# Perform ADF test on first difference
time_ts_d1 = diff(time_ts, differences = 1)
adf.test(time_ts_d1, k = 1)

# Plotting SPC Time series after performing ADF test & obtaining p-value as 0.01
autoplot(time_ts_d1)

# Obtaining p {AR or Lag} value by using PACF plot, here [p = 1]
Pacf(time_ts_d1)

# Obtaining q {MA or Moving Average} value by using ACF plot, here [q = 1 ]
Acf(time_ts_d1)

# Get auto p,q,d valeues
auto.arima(time_ts_d1)


# Fitting ARIMA(1,1,1)
time_Mod = Arima(y = time_ts, order = c(1,1,1))

# Model Summary
print(time_Mod)

# FOrecasting
forecast(time_Mod)

# Plot the Final Series with Forecast
autoplot(forecast(time_Mod, h = 12))



