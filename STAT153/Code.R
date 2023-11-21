#-------------------------#
#   Section 1             #
#-------------------------#


#  ACF plot/Correlogram
x = rnorm(301)
acf(x[1:300],lwd=4)  #linewide=4

#   ACF plot - simple Moving Average
y = .5*(x[1:300] + x[2:301])
acf(y,lwd=3)    
#A value of 𝑟h outside the blue bands is significant,confidence interval

#    Autoregressive Process AR(1)
Xt = arima.sim(model=list(ar=.9),n=100)
plot.ts(Xt)
acf(Xt)


#-------------------------#
#   Section 3             #
#-------------------------#


plot(rt(200,4,0),type='l')  
#n-number of observations; df-degrees of freedom;cp;non-centrality parameter delta;
?rt

#not wn but stationary
sss = rexp(200,4)         #rate λ f(x) = λ {e}^{- λ x}
plot(sss,type='l')

?rexp()

#Not Stationary - linear trend
lll = (1:200)/4+rt(200,4,0)
plot(lll,type='l')


#Not Stationary - heteroskedastic 异方差
hhh = rnorm(200,0,1:200)
plot(hhh,type='l')



#------------------------------------------------#
#   Section 5  Time Series Decomposition         #
#------------------------------------------------#

mod = lm(numpop ~ Year + I(Year^2),data=uspop)
plot(uspop$Year, uspop$numpop, type = "p", xlab = "Year(once every ten years")
lines(uspop$Year,predict(mod),col=2)



#sample 1 - additive

#Step 1: Import the Data
install.packages("fpp")
library(fpp)
data(ausbeer)
timeserie_beer = tail(head(ausbeer, 17*4+2),17*4-4)
plot(as.ts(timeserie_beer))

#Step 2: Detect the Trend
    #“centred moving average“
    #use a moving window of the exact size of the seasonality
    #Therefore we need to know the seasonality period: weekly, monthly, etc
    #beer follows annual seasonality. 
    #we use a moving average window of 4.

#install.packages("forecast")
library(forecast)

trend_beer = ma(timeserie_beer, order = 4, centre = T)
?ma
plot(as.ts(timeserie_beer))
lines(trend_beer)
plot(as.ts(trend_beer))

#Step 3: Detrend the Time Series
detrend_beer = timeserie_beer - trend_beer
plot(as.ts(detrend_beer))

#Step 4: Average the Seasonality
    #We add the seasonality together and divide by the seasonality period
    #we feed the time series into a matrix
    #we transform the matrix so each column contains elements of the same period (same day, same month, same quarter, etc…
    #compute the mean of each column

m_beer = t(matrix(data = detrend_beer, nrow = 4))
?matrix
?t  #转置
seasonal_beer = colMeans(m_beer, na.rm = T)
plot(as.ts(rep(seasonal_beer,16)))

#Step 5: Examining Remaining Random Noise
random_beer = timeserie_beer - trend_beer - seasonal_beer
plot(as.ts(random_beer))

#Step 6: Reconstruct the Original Signal
    #Some data points will be missing at the beginning and the end of the reconstructed time series, 
    #due to the moving average windows 

recomposed_beer = trend_beer+seasonal_beer+random_beer
plot(as.ts(recomposed_beer))





#sample 2 - multiplicative
install.packages("Ecdat")
library(Ecdat)
data(AirPassengers)
timeserie_air = AirPassengers
plot(as.ts(timeserie_air))

#Step 2: Detect the Trend
    #it is recorded monthly, so we choose a moving average window of 12
library(forecast)
trend_air = ma(timeserie_air, order = 12, centre = T)
plot(as.ts(timeserie_air))
lines(trend_air)
plot(as.ts(trend_air))

#Step 3: Detrend the Time Series
detrend_air = timeserie_air / trend_air
plot(as.ts(detrend_air))

#Step 4: Average the Seasonality
m_air = t(matrix(data = detrend_air, nrow = 12))
seasonal_air = colMeans(m_air, na.rm = T)
plot(as.ts(rep(seasonal_air,12)))

#Step 5: Examining Remaining Random Noise
    #Random = Time series / (Trend * Seasonal)
random_air = timeserie_air / (trend_air * seasonal_air)
plot(as.ts(random_air))

#Step 6: Reconstruct the Original Signal
recomposed_air = trend_air*seasonal_air*random_air
plot(as.ts(recomposed_air))





# DECOMPOSE( ) and STL()
    #To make life easier
    #some R packages provides decomposition with a single line of code.


#sample 1 - additive
    #The only requirement: seasonality is quarterly (frequency = 4)
ts_beer = ts(timeserie_beer, frequency = 4)
decompose_beer = decompose(ts_beer, "additive")

plot(as.ts(decompose_beer$seasonal))
plot(as.ts(decompose_beer$trend))
plot(as.ts(decompose_beer$random))
plot(decompose_beer)

ts_air = ts(timeserie_air, frequency = 12)
decompose_air = decompose(ts_air, "multiplicative")


#sample 2 - multiplicative
plot(as.ts(decompose_air$seasonal))
plot(as.ts(decompose_air$trend))
plot(as.ts(decompose_air$random))
plot(decompose_air)

#STL()
ts_beer = ts(timeserie_beer, frequency = 4)
stl_beer = stl(ts_beer, "periodic")
seasonal_stl_beer   <- stl_beer$time.series[,1]
trend_stl_beer     <- stl_beer$time.series[,2]
random_stl_beer  <- stl_beer$time.series[,3]

plot(ts_beer)
plot(as.ts(seasonal_stl_beer))
plot(trend_stl_beer)
plot(random_stl_beer)
plot(stl_beer)

#Conclusion


