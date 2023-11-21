###
### ARIMA 
###

#Motivating example

library(astsa)
plot.ts(cardox)
dd = diff(diff(cardox),lag = 12)
plot.ts(dd)
acf2(dd,max.lag = 60)

#Some simulated ARIMA datasets:


#ARIMA(1,1,0)
dts <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = 100)
plot(dts, type = "o", main = "Simulated ARIMA(1,1,0) Datasets", ylab = "Data")
#Note that this dataset has 101 points. 
#In general, arima.sim generates datasets of length n + d
# Note that this process isn't stationary! But it's first difference will be. 

#ARIMA(0,1,1)
dts <- arima.sim(list(order = c(0,1,1), ma = 0.7), n = 100)
plot(dts, type = "o", main = "Simulated ARIMA(0,1,1) Datasets", ylab = "Data")

#ARIMA(1,1,1)
dts <- arima.sim(list(order = c(1,1,1), ar = 0.7, ma = 0.4), n = 100)
plot(dts, type = "o", main = "Simulated ARIMA(1,1,1) Dataset", ylab = "Data")



#ARIMA(1,2,1)
dts <- arima.sim(list(order = c(1,2,1), ar = 0.7, ma = 0.4), n = 100)
plot(dts, type = "o", main = "Simulated ARIMA(1,2,1) Dataset", ylab = "Data")



#Fitting ARIMA models
dts <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = 100)
plot(dts, type = "o", main = "Simulated ARIMA Datasets", ylab = "Data")

model1 = arima(dts, order = c(1, 1, 0), method = "CSS-ML") #We'll discuss "method" in later lecture(s). 
model1
plot.ts(model1$residuals)
model2 = arima(diff(dts), order = c(1, 0, 0), method = "CSS-ML")
model2
plot.ts(model2$residuals)

#Recall that the estimate of mu is given by the "intercept".
#The mean of the differenced series is assumed to be zero while using arima with d > 1. 
#If you need to fit a non-zero mean ARMA model to the differenced data, 
#use ARMA on the differenced series, or a different function (coming at later in the script)


###
### Second example: co2 dataset. Taken from the book by Cryer and Chan.
###
data(co2, package = "TSA")
help(co2, package = "TSA")
#There is a different dataset with the name co2 in the package datasets.

plot(co2, xlab = "year", ylab = "CO2 Levels", main = "monthly Carbon Dioxide Levels at Alert, NWT, Canada", type = "o")

#Both Trend and Seasonality:
#Seasonal difference:
t1 = diff(co2, 12)
plot(t1, main = "Seasonal Difference of co2")
#First difference of t1:
t2 = diff(t1)
plot(t2, main = "Seasonal and First Difference of co2") 
#No trend or seasonality now. We can now fit a stationary model. 

acf(t2)
pacf(t2)
#Large negative autocorrelation at lag 1 and then a bunch of small autocorrelations 
#followed by large autocorrelations at lags 11, 12, 13 (the one at lag 12 is quite large)
#PACF Not very interpretable, perhaps lags 1, 2, 11, and 12? 

# READ aloud in class: To fit a good model for this dataset, we need to learn about seasonal and multiplicative seasonal ARMA models.











###
### Part 2: Seasonal autocorrelations! 
###


#Seasonal ARMA and ARIMA models:
par(mfrow=c(2,1))

#Seasonal MA(1) model
th = c(rep(0, 11), 0.7)#theta = 0.7
L = 100

corrs = ARMAacf(ma = th, lag.max = L)
par.corrs = ARMAacf(ma = th, lag.max = L, pacf = T)
plot(x = 0:L, y = corrs, type = "h", xlab = "Lag k", ylab = "Autocorrelation")
abline(h = 0)
plot(x = 1:L, y = par.corrs, type = "h", xlab = "Lag k", ylab = "Partial Autocorrelation")
abline(h = 0)

#Seasonal MA(2) model
th = c(rep(0, 11), 0.7, rep(0, 11), 0.8)
L = 100
corrs = ARMAacf(ma = th, lag.max = L)
par.corrs = ARMAacf(ma = th, lag.max = L, pacf = T)
plot(x = 0:L, y = corrs, type = "h", xlab = "Lag k", ylab = "Autocorrelation")
abline(h = 0)
plot(x = 1:L, y = par.corrs, type = "h", xlab = "Lag k", ylab = "Partial Autocorrelation")
abline(h = 0)


#Seasonal AR(1) model
ph = c(rep(0, 11), 0.7)#phi = 0.7
L = 150
corrs = ARMAacf(ar = ph, lag.max = L)
par.corrs = ARMAacf(ar = ph, lag.max = L, pacf = T)
plot(x = 0:L, y = corrs, type = "h", xlab = "Lag k", ylab = "Autocorrelation")
abline(h = 0)
plot(x = 1:L, y = par.corrs, type = "h", xlab = "Lag k", ylab = "Partial Autocorrelation")
abline(h = 0)

#Seasonal AR(2) model
ph = c(rep(0, 11), 1.5, rep(0, 11), -0.75)
L = 150
corrs = ARMAacf(ar = ph, lag.max = L)
par.corrs = ARMAacf(ar = ph, lag.max = L, pacf = T)
plot(x = 0:L, y = corrs, type = "h", xlab = "Lag k", ylab = "Autocorrelation")
abline(h = 0)
plot(x = 1:L, y = par.corrs, type = "h", xlab = "Lag k", ylab = "Partial Autocorrelation")
abline(h = 0)

#Seasonal ARMA(1,1) model
ph = c(rep(0, 11), 0.5)#phi = 0.4
th = c(rep(0, 11), 0.8) #th = 0.8
L = 150
corrs = ARMAacf(ar = ph, ma = th, lag.max = L)
par.corrs = ARMAacf(ar = ph, ma = th, lag.max = L, pacf = T)
plot(x = 0:L, y = corrs, type = "h", xlab = "Lag k", ylab = "Autocorrelation")
abline(h = 0)
plot(x = 1:L, y = par.corrs, type = "h", xlab = "Lag k", ylab = "Partial Autocorrelation")
abline(h = 0)

#Therefore the acf and pacf of seasonal ARMA models look just like those of usual ARMA models at the seasonal lags.

###
### Multiplicative seasonal models
###

#ARMA(0, 1) X (1, 0)_12 model:
ph = c(rep(0, 11), 0.8) #Phi = 0.8
L = 50
corrs = ARMAacf(ar = ph, ma = -0.5, lag.max = L)
par.corrs = ARMAacf(ar = ph, ma = -0.5, lag.max = L, pacf = T)
plot(x = 0:L, y = corrs, type = "h", xlab = "Lag k", ylab = "Autocorrelation")
abline(h = 0)
plot(x = 1:L, y = par.corrs, type = "h", xlab = "Lag k", ylab = "Partial Autocorrelation")
abline(h = 0)


#ARMA(0, 2) X (1, 0)_12 model:
ph = c(rep(0, 11), 0.8) #Phi = 0.8
L = 50
corrs = ARMAacf(ar = ph, ma = c(-0.5, 1), lag.max = L)
par.corrs = ARMAacf(ar = ph, ma = c(-0.5, 1), lag.max = L, pacf = T)
plot(x = 0:L, y = corrs, type = "h", xlab = "Lag k", ylab = "Autocorrelation")
abline(h = 0)
plot(x = 1:L, y = par.corrs, type = "h", xlab = "Lag k", ylab = "Partial Autocorrelation")
abline(h = 0)

#ARMA(0, 1) X (0, 1)_12 model:
th = c(-0.6, rep(0, 10), -0.7, -0.6*-0.7)
corrs = ARMAacf(ma = th, lag.max = L)
par.corrs = ARMAacf(ma = th, lag.max = L, pacf = T)
plot(x = 0:L, y = corrs, type = "h", xlab = "Lag k", ylab = "Autocorrelation")
abline(h = 0)
plot(x = 1:L, y = par.corrs, type = "h", xlab = "Lag k", ylab = "Partial Autocorrelation")
abline(h = 0)


#To the co2 dataset:
#co2 dataset
data(co2, package = "TSA")
plot(co2, xlab = "year", ylab = "CO2 Levels", main = "monthly Carbon Dioxide Levels at Alert, NWT, Canada", type = "o")

#Both Trend and Seasonality:
#Seasonal difference:
t1 = diff(co2, 12)
plot(t1, main = "Seasonal Difference of co2")
#First difference of t1:
t2 = diff(t1)
plot(t2, main = "Seasonal and First Difference of co2")
#No trend or seasonality now. We can now fit a stationary model.

acf(t2, lag.max = 40)
#Large negative autocorrelation at lag 1 and then a bunch of 
#small autocorrelations followed by large autocorrelations at lags 11, 12, 13 (the one at lag 12 is quite large)
pacf(t2, lag.max = 40)
#Not very interpretable.

t3 = arima(t2, order=c(0,0,1), seasonal = list(order=c(0,0,1)))
plot.ts(t3$residuals)
acf2(t3$residuals)



#Seasonal ARIMA for the co2 dataset:
m1.co2 = arima(co2, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12))
m1.co2


all.plot = function(x){
  par(mfrow=c(3,1))
  plot.ts(x)
  acf(x)
  pacf(x)
  par(mfrow=c(1,1))
}
all.plot(m1.co2$residuals)

#Prediction
m = 24
fcast = predict(m1.co2, n.ahead = m, interval = 'predict')

newx = 1:(length(co2) + m)
newy = c(co2, fcast$pred)

plot(newx, newy, type = "l")
points(newx[((length(co2)+1):(length(co2) + m))], newy[((length(co2)+1):(length(co2) + m))], col = "red", type = "l" )
lines(newx[((length(co2)+1):(length(co2) + m))], newy[((length(co2)+1):(length(co2) + m))] + 2*fcast$se,col=2)
lines(newx[((length(co2)+1):(length(co2) + m))], newy[((length(co2)+1):(length(co2) + m))] - 2*fcast$se,col=2)


### fcast$se is the best way to use the forecast() function to get at an interval. The specifying the interval 
### option is not available for arima() models like it is for lm() models. 

###
### Part 3
###


library(astsa)
plot.ts(cardox)
acf2(diff(diff(cardox),lag = 12),max.lag = 60)

model = arima(cardox,order=c(3,1,1), seasonal=list(order=c(0,1,1),period=12))
all.plot(model$residuals)

library(forecast)
# We now know the inputs for the super-helpful sarima function. 
# If we have time, I'll introduce some of sarima()'s diagnostic plots today, 
# But we'll cover that in depth next time. 
sarima(cardox,p = 0,d=0,q=0,S = 12,D=0)

