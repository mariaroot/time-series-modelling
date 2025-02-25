install.packages("TTR")
install.packages("forecast")

library(TTR)
library(forecast)

#reading in csv files
marriages <- read.csv(file = 'UK_marriages.csv')

#making and plotting marriage time series 
marriages_timeseries <-ts(marriages,start=c(1887))
plot.ts(marriages_timeseries)
marriages_timeseries

#smoothing method to estimate trend component of time series 
marriages_timeseriesSMA3 <- SMA(marriages_timeseries,n=3)
plot.ts(marriages_timeseriesSMA3)

marriages_timeseriesSMA7 <- SMA(marriages_timeseries,n=7)
plot.ts(marriages_timeseriesSMA7)

#decomposing marriages time series to get trend, seasonal and irregular components
marriages_timeseries_components <- decompose(marriages_timeseries)

#Holt-Winters Exponential Smoothing for forecasting 
marriages_timeseries_forescasts <- HoltWinters(marriages_timeseries, gamma=FALSE)
marriages_timeseries_forescasts
plot(marriages_timeseries_forescasts)

marriages_timeseries_forescasts2 <- forecast(marriages_timeseries_forescasts, h=6)
plot(marriages_timeseries_forescasts2)

#checking if HW forecast model can be improved upon
acf(marriages_timeseries_forescasts2$residuals, lag.max=20, na.action = na.pass)
Box.test(marriages_timeseries_forescasts2$residuals, lag=20, type="Ljung-Box")

plot.ts(marriages_timeseries_forescasts2$residuals)
marriages_timeseries_forescasts2$residuals <- marriages_timeseries_forescasts2$residuals[!is.na(marriages_timeseries_forescasts2$residuals)]
plotForecastErrors(marriages_timeseries_forescasts2$residuals)

##ARIMA model 
#differencing the marriages time series to make it stationary 
marriages_timeseries_diff1 <- diff(marriages_timeseries, differences=1)
plot.ts(marriages_timeseries_diff1)

#plotting correlogram and partial correlogram of differenced time series 
acf(marriages_timeseries_diff1, lag.max=20)

pacf(marriages_timeseries_diff1, lag.max=20)

#short-cut: R's values for p, d and q in ARIMA model: 
auto.arima(marriages)

#fitting ARIMA(2,1,3) model 
marriages_timeseries_arima <- arima(marriages_timeseries, order=c(2,1,3))
marriages_timeseries_arima

#Using ARIMA model to make forecasts 
marriages_timeseries_forescasts_arima <- forecast(marriages_timeseries_arima, h=5)
marriages_timeseries_forescasts_arima
plot(marriages_timeseries_forescasts_arima)

##Checking forecast errors of ARIMA model 
#correlogram: 
acf(marriages_timeseries_forescasts_arima$residuals, lag.max=20)
Box.test(marriages_timeseries_forescasts_arima$residuals, lag=20, type="Ljung-Box")

#time plot forecast errors
plot.ts(marriages_timeseries_forescasts_arima$residuals)
mean(marriages_timeseries_forescasts_arima$residuals)

#histogram 
plotForecastErrors(marriages_timeseries_forescasts_arima$residuals)
