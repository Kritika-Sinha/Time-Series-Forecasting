###A simple arima model##


##What is Arima model###
# ARIMA is the abbreviation for AutoRegressive Integrated Moving Average. Auto Regressive (AR) terms
# refer to the lags of the differenced series, 
# Moving Average (MA) terms refer to the lags of errors and I is the number of difference used to make the time series stationary.
#   
  

# Assumptions of ARIMA model
# 1. Data should be stationary - by stationary it means that the properties of the series doesn't depend 
# on the time when it is captured. A white noise series and series with cyclic behavior can also be considered as stationary series.
# 2. Data should be univariate - ARIMA works on a single variable. Auto-regression is all about regression with the past values.


library('ggplot2')
library('forecast')
library('tseries')
library('fpp')

getwd()
setwd('C:/arima model')
daily_data=read.csv('day.csv',header=TRUE,stringsAsFactors = FALSE)

daily_data$date=as.Date(daily_data$dteday)

ggplot(daily_data,aes(date,cnt))+geom_line()+scale_x_date('month')+ylab("Daily Bike Checkout")+xlab("")

# In some cases, the number of bicycles checked out dropped below 100 on day and rose to over 4,000 the next day.
# These are suspected outliers that could bias the model by skewing statistical summaries. 
# R provides a convenient method for removing time series outliers: tsclean() as part of its 
# forecast package. tsclean() identifies and replaces outliers using series smoothing and decomposition.


##creating a time series object

count_ts=ts(daily_data[,c('cnt')])
count_ts
str(count_ts)

daily_data$clean_cnt = tsclean(count_ts)

ggplot(data=daily_data,aes(x=date,y=clean_cnt))+geom_line()+
  ylab('Daily Bicycle count-clean')
##the spikes have been removed

##tsclean is a package/function the at removes the outliers

##computing the moving average to get a smoother trend
##order=7 for weekly trend
daily_data$cnt_ma = ma(daily_data$clean_cnt,order=7)

##order=30 for a monthly trend
daily_data$cnt_ma30= ma(daily_data$clean_cnt,order=30)

###plotting the new data variables

ggplot()+
  geom_line(data=daily_data, aes(x=date,y=clean_cnt,colour="Counts"))+
  geom_line(data=daily_data, aes(x=date,y=cnt_ma,colour="Weekly Moving"))+
  geom_line(data=daily_data, aes(x=date,y=cnt_ma30,colour="Monthly Moving"))+
  ylab('Bicycle Count')




##decompose our the datat

##trying to understand seasonal component,trend component,cycle component
# Seasonal component refers to fluctuations in the data related to calendar cycles. 
# For example, more people might be riding bikes in the summer and during warm weather, and less during colder months. 
# Usually, seasonality is fixed at some number; for instance, quarter or month of the year.
# 
# Trend component is the overall pattern of the series: Is the number of bikes rented increasing or decreasing over time?
#   
# Cycle component consists of decreasing or increasing patterns that are not seasonal. 
# Usually, trend and cycle components are grouped together. 
# Trend-cycle component is estimated using moving averages.
# 
# Finally, part of the series that can't be attributed to seasonal, cycle, or trend components is referred to as residual or error.

#The process of extracting these components is referred to as decomposition.


# First,, we calculate seasonal component of the data using stl(). STL is a flexible function for decomposing and forecasting the series. 
# It calculates the seasonal component of the series using smoothing, 
# and adjusts the original series by subtracting seasonality in two simple lines:
# 
#   

count_ma = ts(na.omit(daily_data$cnt_ma),frequency = 30)
plot(count_ma)



decomp = stl(count_ma,s.window="periodic")
plot(decomp)

##alternative decompose function
decomp1=decompose(count_ma)
plot(decomp1)

deseasonal_cnt<-seasadj(decomp)
plot(deseasonal_cnt)

##comparing the time series models 
par(mfrow=c(2,1))
plot(count_ma)
plot(deseasonal_cnt)


##we dont observe any seaonality or trend in the data


# Note that stl() by default assumes additive model structure.
# Use allow.multiplicative.trend=TRUE to incorporate the multiplicative model.


##creating the a stationary time series


##Fitting an ARIMA model requires the series to be stationary. 
# A series is said to be stationary when its mean, variance, and autocovariance are time invariant. This assumption makes intuitive sense: 
# Since ARIMA uses previous lags of series to model its behavior, modeling stable series with consistent properties involves less uncertainty.
# An example of a stationary series, where data values oscillate with a steady variance around the mean of 1.
# For a non-stationary series;mean of this series will differ across different time windows.
# 


##doing the augmented Dickey-Fuller(ADF) test

##we see that the time series is stationary

##the null hypothesis-Series is non stationary 
##the alternate hypothesis-Series is stationary


# ADF procedure tests whether the change in Y can be explained by lagged value 
#and a linear trend. 
# If contribution of the lagged value to the change in Y is non-significant 
#and there is a presence 
# of a trend component, the series is non-stationary and null hypothesis 
#will not be rejected.

adf.test(count_ma,alternative="stationary")

##the test shows that series is non stationary

##inorder to treat this series we make a diff factor(d order) for the model

par(mfrow=c(1,1))

acf(count_ma,main='')
pacf(count_ma,main='')
gglagplot(count_ma)

##THE pacf plot show p component for AR model
##The ACF shows the Q component for MA model

##trying with diff degree of 1

count_d1=diff(count_ma,difference=1)

plot(count_d1)

adf.test(count_d1,alternative = "stationary")

##we have taken care of the non stationary element

##plot acf and pacf graphs again
par(mfrow=c(1,1))
acf(count_d1,main="ACF for Differenced Series")
Pacf(count_d1,main="PACF for Differenced Series")

##from the plots we see that the value of p,q range from 1-7

##trying ARIMA(1,1,1)

fit<-auto.arima(deseasonal_cnt,seasonal=FALSE)
fit
tsdisplay(residuals(fit),lag.max=45,main='(1,1,1)Model Residuals')
###from the plots we still see that we have significant spikes in the data 
##and repeating at 7


##trying ARIMA(1,1,7)

fit2<-arima(deseasonal_cnt,order=c(1,1,7))
tsdisplay(residuals(fit2),lag.max=45,main='(1,1,7)Model residuals')
fit2

##we see that all the spikes in the data have been accounted for by the model


##forecasting the trend using the model

fcast<-forecast(fit2,h=30)
plot(fcast)

##we see from the forecast that the trend balances
## we can check with a hold out sample

hold<-window(ts(deseasonal_cnt),start=700)
fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]),order=c(1,1,7))

fcast_no_holdout<-forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout,main=" ")
plot(deseasonal_cnt)

###we see a diff in the trend of the predicted data


