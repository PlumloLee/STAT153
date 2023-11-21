#################Trend Models
#Consider the US population dataset

uspop = read.csv("/Users/li/Desktop/STAT153/HW2/uspop.csv", header = T)
uspop$numpop = as.numeric(gsub(",", "", uspop$Population))
plot(uspop$Year, uspop$numpop, type = "p", xlab = "Year(once every ten years)", ylab = "US Population", main = "Population of the United States") 

#How does the correlogram for the Raw US population data look like?
acf(uspop$numpop, lag.max = 20, type = "correlation", plot = T, main = "Correlogram for the Raw US population data")

#Fit a quadratic polynomial to this dataset
t = 1: length(uspop$numpop); tt = t^2
quad.model = lm(uspop$numpop ~ 1 + t + tt)
plot(uspop$Year, uspop$numpop, type = "p", xlab = "Year(once every ten years)", ylab = "US Population", main = "Population of the United States")
points(uspop$Year, quad.model$fitted, type = "l", col = "red")

#The fit looks very good. What about the residuals?
plot(uspop$Year, quad.model$residuals, type = "o", xlab = "Year", ylab = "Residuals after quadratic fit")
sd(quad.model$residuals)

#Do the residuals look like white noise?
acf(quad.model$residuals, lag.max = 20, type = "correlation", plot = T, main = "Correlogram of the Residuals from Quadratic Regression")
#All autocorrelations are within the bands. So random assumption is okay.
#Remember that the sample size here is only 23.

#Therefore quadratic polynomial + purely random noise is adequate for this dataset.
#Predicted US population for 2011:
sum(quad.model$coefficients * (c(1,24,24^2)))
#MUST be accompanied by an interval: two kinds of intervals here (prediction interval and confidence interval):
nw = data.frame(t = 24, tt = 24^2)
predict(quad.model, nw)
predict(quad.model, nw, interval = "confidence")
predict(quad.model, nw, interval = "prediction")
#These confidence and prediction intervals make not make sense if the residuals did not look like noise.


#Trends dataset for the query "google": 
google.raw = read.delim("/Users/li/Desktop/STAT153/HW2/googletrends21Jan2016.csv")
#The useful data is between row 4 and row 632
google.use = google.raw[4:632, 1]
len = length(google.use)
google = rep(0, len)
for(i in 1:len)
{
  google[i] = as.numeric(unlist(strsplit(as.character(google.use[i]), ","))[2])
}
plot(google, type = "l", ylab = "Google Search Popularity", xlab = "Weekly Time", main = "Google Trends Data for the query google") 

#Let us first fit a line: 
ts.dat = google
tme = 1: length(ts.dat)
lin.mod = lm(ts.dat ~ 1 + tme)
plot(tme, ts.dat, type = "l", xlab = "Weekly Time", ylab = "Google Search Volume Index", main = "Trends data for Google")
points(tme, lin.mod$fitted, type = "l", col = "red")

#Residuals:
plot(tme, lin.mod$residuals, type = "l", xlab = "Weekly Time", ylab = "Residuals after fitting the line") 
sd(lin.mod$residuals)

#Do the residuals look like noise?
acf(lin.mod$residuals, lag.max = 120, type = "correlation", plot = T, main = "Correlogram of the Residuals after Linear Regression")
#So much structure. Line + white noise is not a good model. 

#Smoothing
ts.dat = google
tme = 1:length(ts.dat)
q=15
sm.par = 1+2*q #This sm.par equals (1+2q) in our notation
mt = filter(ts.dat, rep(1, sm.par)/sm.par)
?rep
plot(ts.dat, type = "l", ylab = "Google Search Volume Index", xlab = "Weekly Time", main = "Trends data for Google")
plot(tme, mt, type = "l", ylab = "Smoothed Google Trends data", xlab = "Weekly Time")

#Time plot with smoothed trend
par(mfrow = c(1,1))
plot(ts.dat, type = "l", ylab = "Google Search Volume Index", xlab = "Weekly Time", main = "Trends data for Google with Smoothed Trend Estimate")
points(tme, mt, type = "l", col = "red")

#Let us plot the residuals from fitting the line and the smoothing residuals side by side:
sm.res = ts.dat - mt
par(mfrow = c(2, 1))
plot(tme, lin.mod$residuals, type = "l", xlab = "Weekly Time", ylab = "Residuals after fitting the line") 
plot(tme, sm.res, type = "l", xlab = "Weekly Time", ylab = "Residuals after Smoothing") 

#Compare the two Correlograms
par(mfrow = c(2,1))
acf(lin.mod$residuals, lag.max = 120, type = "correlation", plot = T, main = "Correlogram of the Residuals after Linear Regression")
acf(sm.res, lag.max = 120, type = "correlation", plot = T, main = "Correlogram of the Residuals after Smoothing", na.action = na.pass)

#Without na.action = na.pass, the acf command does not work. We then need to handle the missing values in some other way.

## Exponential Smoothing
alpha = .9
ts.dat = google
tme = 1:length(ts.dat)
mt = filter(ts.dat, filter = c(0,alpha^((1:80)-1)*(1-alpha)),sides = 1,method='con')
par(mfrow = c(1,1))
plot(ts.dat, type = "l", ylab = "Google Search Volume Index", xlab = "Weekly Time", main = "Trends data for Google")
lines(tme, mt, type = "l", ylab = "Smoothed Google Trends data", xlab = "Weekly Time",col=2)

#Isotonic Trend Fit
isest = isoreg(google)
names(isest)
par(mfrow = c(1,1))
plot(isest$x, isest$y, type = "l", ylab = "Google Search Popularity", xlab = "Weekly Time", main = "Google Trends Data for the query google")
points(isest$x, isest$yf, type = "l", col = "red")

#Residuals from isotonic regression
isest.res = isest$y - isest$yf
plot(isest$x, isest.res, type = "l") 

#Acf of the isotonic residuals
acf(isest.res, lag.max = 200, type = "correlation", plot = T, main = "Correlogram of the residuals after Isotonic Trend Fit")


#Differencing google trends data

ts.dat = google
diff.gt = diff(ts.dat)
tme = 1:length(ts.dat)
plot(tme[-1], diff.gt,type = "l", xlab = "Weekly Time", ylab = "Differenced Data", main = "First Differenced Series for Google Trends Data")  
acf(diff.gt, lag.max = 200, type = "correlation", plot = T, main = "Correlogram of the First Differenced Google Trends Series", na.action = na.pass) 
#Only one significant correlation.


######### Seasonality Models

#Some datasets with seasonality:
USAccDeaths
plot(USAccDeaths, type = "o", ylab = "Deaths", main = "Monthly Totals of Accidental Deaths in the US 1973-1978")

#Parametric Functions for Seasonality
#Simplest Periodic Functions of period $d$ are cos(2 pi f t/d) and sin (2 pi f t/d)
d = 12 
f = 3 #Plot with values of f from 1 to 12.
fcos = function(t){cos(2*pi*f*t/d)}
fsin = function(t){sin(2*pi*f*t/d)}
max.t = 36
tme = 0:max.t
par(mfrow=c(2,1))
plot(fcos, 0, max.t)
points(tme, fcos(tme))
plot(fsin, 0, max.t)
points(tme, fsin(tme))

#Linear Combinations of these:

f1 = 1; a1 = 1
f2 = 2; a2 = 5
fval = function(t){a1*cos(2*pi*f1*t/d) + a2*sin(2*pi*f2*t/d)}
par(mfrow=c(1,1))
plot(fval, 0, max.t)
points(tme, fval(tme))

#Fitting a parametric seasonality function to the US Accidents Data Set.
plot(USAccDeaths, type = "o", ylab = "Deaths", main = "Monthly Totals of Accidental Deaths in the US 1973-1978")

#Correlogram of the Raw US Accidents Data Set:
acf(USAccDeaths, lag.max = 40, type = "correlation", plot = T, main = "Correlogram of the Raw US Accidents Data Set")

t = 1: length(USAccDeaths)
#Start with the slowest frequency
f = 1
d = 12
v1 = cos(2*pi*f*t/d)
v2 = sin(2*pi*f*t/d)
lin.mod = lm(USAccDeaths ~ 1 + v1 + v2)
plot(t, USAccDeaths, type = "o", xlab = "Time", ylab = "Deaths", main = "Monthly Totals of Accidental Deaths in the US 1973-1978")
points(t, lin.mod$fitted, type = "l", col = "red")


#Do the residuals look like white noise?
acf(lin.mod$residuals, lag.max = 36, type = "correlation", plot = T, main = "Correlogram of the Residuals after Removing seasonality")
#Note the presence of a non-zero autocorrelation at the seasonal lag 12. This suggests that not all seasonality has been removed. 

#Fit two frequencies
t = 1: length(USAccDeaths)
f1 = 1
f2 = 2
d = 12
v1 = cos(2*pi*f1*t/d)
v2 = sin(2*pi*f1*t/d)
v3 = cos(2*pi*f2*t/d)
v4 = sin(2*pi*f2*t/d)
lin.mod = lm(USAccDeaths ~ 1 + v1 + v2 + v3 + v4)
plot(t, USAccDeaths, type = "o", xlab = "Time", ylab = "Deaths", main = "Monthly Totals of Accidental Deaths in the US 1973-1978")
points(t, lin.mod$fitted, type = "l", col = "red")

#Residuals:
plot(t, lin.mod$residuals, type = "o", xlab = "Year", ylab = "Residuals after fitting the seasonality function")
sd(lin.mod$residuals)

#Do the residuals look like white noise?
acf(lin.mod$residuals, lag.max = 20, type = "correlation", plot = T, main = "Correlogram of the Residuals after Removing seasonality")
#Note now that the value of the autocorrelation at lag 12 is much smaller than before. 

#Fit three frequencies: 
t = 1: length(USAccDeaths)
f1 = 1
f2 = 2
f3 = 3
d = 12
v1 = cos(2*pi*f1*t/d)
v2 = sin(2*pi*f1*t/d)
v3 = cos(2*pi*f2*t/d)
v4 = sin(2*pi*f2*t/d)
v5 = cos(2*pi*f3*t/d)
v6 = sin(2*pi*f3*t/d)
lin.mod = lm(USAccDeaths ~ 1 + v1 + v2 + v3 + v4 + v5 + v6)
plot(t, USAccDeaths, type = "o", xlab = "Time", ylab = "Deaths", main = "Monthly Totals of Accidental Deaths in the US 1973-1978")
points(t, lin.mod$fitted, type = "l", col = "red")

#Residuals:
plot(t, lin.mod$residuals, type = "o", xlab = "Year", ylab = "Residuals after fitting the seasonality function")
sd(lin.mod$residuals)

#Do the residuals look like white noise?
acf(lin.mod$residuals, lag.max = 36, type = "correlation", plot = T, main = "Correlogram of the Residuals after Removing seasonality")
#The autocorrelation at lag 12 is now negligible. 

#Smoothing for seasonality
#Let us apply it to the USAccDeaths dataset.
t = 1: length(USAccDeaths)
plot(t, USAccDeaths, type = "o", xlab = "Time", ylab = "Deaths", main = "Monthly Totals of Accidental Deaths in the US 1973-1978")
s.hat = rep(0, 12) #this vector (initialized to 0) will have the seasonality estimate after the following loop
for(i in 1:12)
{
  ind.i = seq(i, length(USAccDeaths), 12)  #seq(from = 1, to = 1, by =?)
  s.hat[i] = mean(USAccDeaths[ind.i])
}
s.hat
?seq

#Plotting this seasonality estimate along with the original data:
full.s = rep(0, length(USAccDeaths))
for(j in 1: length(USAccDeaths))
{
  i = j %% 12
  if(i == 0) {i = 12}  #如果i整除12则作为12
  full.s[j] = s.hat[i]
}

plot(t, USAccDeaths, type = "o", xlab = "Time", ylab = "Deaths", main = "Accidental Deaths in the US 1973-1978 and Smoothed Seasonality Estimate")
points(t, full.s, type = "l", col = "red")

res.sm = USAccDeaths - full.s
plot(t, res.sm, type = "l", main = "Residuals after Smoothing out Seasonality")
acf(res.sm, lag.max = 36, type = "correlation", plot = T, main = "Correlogram of the Residuals after Smoothing out seasonality")

#Differencing for Seasonality
diff.deaths = diff(USAccDeaths, lag = 12)
tme = 1:length(diff.deaths)
plot(tme, diff.deaths,type = "l", xlab = "Time", ylab = "Differenced Data", main = "Lag 12 Differenced Series for the US Accidental Deaths Data")
acf(diff.deaths, lag.max = 36, type = "correlation", plot = T, main = "Correlogram of the Lag 12 Differenced US Accidental Deaths Series", na.action = na.pass)
#A trend (linear) still remains. 

############ Most Datasets have both trend and seasonality. 

#See examples below.
UKgas
plot(UKgas)

AirPassengers
plot(AirPassengers)

#Fitting parametric trend and seasonal functions: can be done in any order.
#USAccDeaths dataset: We fitted a seasonal function (up to two frequencies) to the data:
t = 1: length(USAccDeaths)
f1 = 1
f2 = 2
f3 = 3
d = 12
v1 = cos(2*pi*f1*t/d)
v2 = sin(2*pi*f1*t/d)
v3 = cos(2*pi*f2*t/d)
v4 = sin(2*pi*f2*t/d)
v5 = cos(2*pi*f3*t/d)
v6 = sin(2*pi*f3*t/d)
lin.mod = lm(USAccDeaths ~ 1 + v1 + v2 + v3 + v4 + v5 + v6)
plot(t, USAccDeaths, type = "o", xlab = "Time", ylab = "Deaths", main = "Monthly Totals of Accidental Deaths in the US 1973-1978")
points(t, lin.mod$fitted, type = "l", col = "red")

#Here are the residuals from that fit:
plot(t, lin.mod$residuals, type = "o", xlab = "Year", ylab = "Residuals after fitting the seasonality function")
acf(lin.mod$residuals, lag.max = 40, type = "correlation", plot = T, main = "Correlogram of the Residuals after removing seasonality")

#It is clear that there is a quadratic trend in these residuals. Let us fit a quadratic function to them:
t = 1: length(USAccDeaths); tt = t^2
quad.model = lm(lin.mod$residuals ~ 1 + t + tt)
plot(t, lin.mod$residuals, type = "o", xlab = "Monthly Time", ylab = "Deaths",main = "Residuals after fitting the Seasonality Function")  
points(t, quad.model$fitted, type = "l", col = "red")

#Plotting the final model fit:
plot(t, USAccDeaths, type = "o", xlab = "Time", ylab = "Deaths", main = "Monthly Totals of Accidental Deaths in the US 1973-1978")
points(t, quad.model$fitted + lin.mod$fitted, type = "l", col = "red")

#The fit looks very good. What about the residuals?
plot(t, quad.model$residuals, type = "o", xlab = "Year", ylab = "Residuals from the final Model")
sd(quad.model$residuals)
acf(quad.model$residuals, lag.max = 36, type = "correlation", plot = T, main = "Correlogram of the Final Residuals")



####### Variance Stabilizing Transformations

UKgas
plot(UKgas, main = "Monthly gas consumption in the UK")
plot(log(UKgas))
plot(sqrt(UKgas))

AirPassengers
plot(AirPassengers)
plot(log(AirPassengers))
plot(sqrt(AirPassengers))


