library(tseries)
library(quantmod)
library(forecast)

#Set the date
date <- as.Date("2023-11-15")

#Get historical data of the time serie
?getSymbols
getSymbols("^BVSP", src = "yahoo", to = date)

#Extracting closing prices
asset <- BVSP$BVSP.Close
head(BVSP$BVSP.Close)

#Check for missing values
any(is.na(asset))
asset <- na.omit(asset)
any(is.na(asset))

str(asset)

#Decomposing the time serie
asset.ts <- ts(asset, frequency = 252)
plot(decompose(asset.ts))
plot(decompose(asset.ts, type = "additive", filter = NULL))
plot(decompose(asset.ts, type = "multiplicative", filter = NULL))

#Check for stationarity at the chart
plot.ts(asset)

#Augmented Dickey-Fuller Test (ADF) for stationarity
#H0 -> non-stationary, H1 -> stationary (alternative hyphothesis)
adf.test(asset)
#We do not reject H0 (null hypothesis) at a 5% of significance, so the data isn't stationary (p-value > 0.05)

#Phillips Perron Test (PP) for stationarity
#H0 -> non-stationary, H1 -> stationary
pp.test(asset)
#We do not reject H0 at a 5% of significance, so the data isn't stationary (p-value > 0.05)

#Transform the data for stationarity by taking the log-returns
d.asset <- diff(log(asset))

#Plotting the log-returns
plot.ts(d.asset)

#Omitting NA values
any(is.na(d.asset))
d.asset <- na.omit(d.asset)
any(is.na(asset))

#Test for stationarity again
adf.test(d.asset)
pp.test(d.asset)

plot.ts(d.asset)



#################################################################
########################## ARIMA

#Check autocorrelations of the time serie
acf(d.asset)
pacf(d.asset)

#Auto.arima function checks automatically the best ARIMA specifications (p,d,q) to fit the model
auto.arima(d.asset, trace = T)
auto.arima(d.asset, trace = T, stepwise = F, approximation = F)
?auto.arima

#Now we're able to check the estimated parameters (coefficients) for the model
#although auto.arima suggested (1,0,0) or (4,0,1), (4,0,2) gave the lowest AIC and the highest p-value for Ljung-box Test
arima_d.asset <- Arima(d.asset, order = c(4,0,2))
arima_d.asset

#### Diagnostic Checks

#Check the autocorrelation of the residuals
acf(arima_d.asset$residuals)
pacf(arima_d.asset$residuals)

#Plotting to check if there is a zero mean on the residuals of the ARIMA model
plot.ts(arima_d.asset$residuals)

#Check histogram to see if the residuals are normally distributed
gghistogram(arima_d.asset$residuals)
#It is bell shaped around zero

#Ljung-box Test
Box.test(arima_d.asset$residuals, lag = 10, type = c("Box-Pierce", "Ljung-Box"))
#H0 = Residuals are independent and identically distributed (iid), 
#in this case we do not reject our H0 because p-value > 0.05

#Forecasting
forecast_d.asset <- forecast(arima_d.asset, h=6)
forecast_d.asset
plot(forecast_d.asset)


####Reverting diff-log-returns

#Last observed value of the original time series
last_observed_price <- asset[length(asset)]

#List to store the predicted prices
predicted_prices <- numeric()

for (log_return in forecast_d.asset$mean) {
  #Calculate the next predicted price based on the forecasted log-return
  next_predicted_price <- last_observed_price * exp(log_return)
  
  #Add the predicted price to the list
  predicted_prices <- c(predicted_prices, next_predicted_price)
  
  #Update the "last observed price" for the next loop
  last_observed_price <- next_predicted_price
}

#Print the predicted prices
print(predicted_prices)


#This will provide the three predicted prices based on the log-returns forecasted by the ARIMA model. Once you have these prices, you can compare them with any actual future data you obtain for the original time serie.

##As always, when working with financial forecasts, it is crucial to exercise caution and not make investment decisions solely based on statistical models. Models provide valuable information, but there are always uncertainties and other factors to consider.



####Verifying the quality of the forecast with past values

count_values <- sum(!is.na(d.asset))
print(count_values)

#Omiting the last 3 values (h) of our original time serie to see if the forecasting is reasonable
#If we have 4173 values, we have to define [1:4170] to check the last 3 values of the time serie and compare it with the forecast.
arima_d.asset2 <- Arima(d.asset[1:4170], order = c(4,0,2))
forecast_d.asset2 <- forecast(arima_d.asset2, h = 3)
forecast_d.asset2

#Checking if they are good by comparing with the 3 last values of the original time serie
tail(d.asset, n = 3)






##############################################################################
################################## For weekly chart
##############################################################################

#Get data until the specified date
ibov.weekly <- to.weekly(getSymbols("^BVSP", src = "yahoo", to = date, auto.assign = FALSE))

ibov.weekly <- to.weekly(BVSP$BVSP.Close)
ibov.weekly1 <- ibov.weekly$`BVSP$BVSP.Close.Close`
head(ibov.weekly1)
str(ibov.weekly1)

any(is.na(ibov.weekly1))

#Checking stationarity with the charts first
plot.ts(ibov.weekly1)
#Clearly not stationary

#Augmented Dickey-Fuller Test (ADF) for stationarity
#H0 -> non-stationary, H1 -> stationary
adf.test(ibov.weekly1)
#We do not reject H0 (null hypothesis) at a 5% of significance, so the data isn't stationary (p-value > 0.05) according to ADF Test

#Phillips Perron Test (PP) for stationarity
##H0 -> non-stationary, H1 -> stationary
pp.test(ibov.weekly1)
#We do not reject H0 for 5% of significance, so the data isn't stationary (p-value > 0.05) according to PP Test


#Transform the data for stationarity by taking the log-returns
dibov_weekly <- diff(log(ibov.weekly1))

#Plotting the log-returns
plot.ts(dibov_weekly)

any(is.na(dibov_weekly))
dibov_weekly <- na.omit(dibov_weekly)
any(is.na(dibov_weekly))

#testing again
adf.test(dibov_weekly)
pp.test(dibov_weekly)



#################################################################
########################## ARIMA weekly

#See the autocorrelations of the time serie
acf(dibov_weekly)
pacf(dibov_weekly)

#Auto.arima
auto.arima(dibov_weekly)
auto.arima(dibov_weekly, trace = T, stepwise = F, approximation = F)

#Now we can see our paramaters for the model.
arima_dibov_weekly <- Arima(dibov_weekly, order = c(6,0,2))
arima_dibov_weekly

#### Diagnostic Checks

#Check the autocorrelation of the residuals
acf(residuals(arima_dibov_weekly))
pacf(residuals(arima_dibov_weekly))

#Plotting to check if there is a zero mean on the residuals of the ARIMA model
plot.ts(residuals(arima_dibov_weekly))

#Check histogram to see if the residuals are normally distributed
gghistogram(residuals(arima_dibov_weekly))
#It is bell shaped around zero

#Ljung-box Test
Box.test(residuals(arima_dibov_weekly), lag = 10, type = c("Box-Pierce", "Ljung-Box"))
#H0 = Residuals are independent and identically distributed (iid), in this case we do not reject our H0 because p-value > 0.05

count_values_weekly <- sum(!is.na(dibov_weekly))
print(count_values_weekly)

#Forecasting
forecast_ibov_weekly <- forecast(arima_dibov_weekly, h=3)
forecast_ibov_weekly
plot(forecast_ibov_weekly)

###Veryfing the quality of the forecast

#Omit the last 3 values (h) of our original time serie to see if the forecasting is reasonable
arima_dibov2 <- Arima(dibov_weekly[1:877], order = c(6,0,2))
forecast_ibov_weekly2 <- forecast(arima_dibov2, h = 3)
forecast_ibov_weekly2

#Check if they are good
tail(dibov_weekly, n = 3)



#Last observed value of the original series
last_observed_price <- ibov.weekly1[length(ibov.weekly1)]

#List to store the predicted prices
predicted_prices <- numeric()

for (log_return in forecast_ibov_weekly$mean) {
  #Calculate the next predicted price based on the forecasted log-return
  next_predicted_price <- last_observed_price * exp(log_return)
  
  #Add the predicted price to the list
  predicted_prices <- c(predicted_prices, next_predicted_price)
  
  #Update the "last observed price" for the next loop
  last_observed_price <- next_predicted_price
}

#Print the predicted prices
print(predicted_prices)
