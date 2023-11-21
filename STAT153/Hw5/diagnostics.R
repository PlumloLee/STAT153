library(astsa)
plot.ts(salmon)

plot.ts(diff(salmon))

# Perhaps Heteroscedastic?
l1= log(salmon)
plot.ts(l1)
d1 = diff(l1)
plot.ts(d1)
temp = acf2(d1,max.lag = 60)
# acf2 outputs the (p)acf values too, so save them somewhere if you don't want them on screen. 


model1 = sarima(l1,p=0,d=0,q=0,S=12,P=0,D=0,Q=0)

model2 = sarima(l1,p=1,d=1,q=0,S=12,P=0,D=0,Q=1)
#m2 = arima(l1, order=c(1,1,0), seasonal = list(order=c(0,0,1),period=12))

model3 = sarima(l1,p=1,d=1,q=1,S=12,P=1,D=1,Q=1)

model4 = sarima(l1,p=0,d=1,q=0,P=0,D=1,Q=0,S=6)

#model 2 vs model 4
temp = acf2(model2$fit$residuals)
temp = acf2(model4$fit$residuals)

#########################
### Note on the AIC's ###
#########################
model2$AIC 
model2$BIC
model2$AICc

model4$AIC 
model4$BIC
model4$AICc

# What if we take no VST log? 
model2nolog = sarima(salmon,p=1,d=1,q=0,S=12,P=0,D=0,Q=1)
model2nolog$AIC 
# Is this AIC comparable to model2$AIC?

########################
### Cross Validation ###
########################

# forecasting 2014, 2015, 2016 

sse = matrix(NA, nrow=3,ncol=3) # forecasting out 3 different times, with 3 models
for(i in 1:3){
  
  ## Split train/test
  # if using a time series object
  # train = window(l1,start=2004,end=2013+i-.0001)
  # test = window(l1, start=2013+i, end=2013+i+.999)
  # if using a standard vector
  train.test.split.point = 124+12*(i-1) # last point of train
  train = l1[5:train.test.split.point]
  test = l1[(train.test.split.point+1):(train.test.split.point+12)]
  
  ## Fit
  model1 = sarima.for(train,n.ahead = 12,p=0,d=1,q=0,S=12,P=0,D=1,Q=1)
  model2 = sarima.for(train,n.ahead = 12,p=1,d=1,q=0,S=12,P=0,D=0,Q=1)
  model3 = sarima.for(train,n.ahead = 12,p=1,d=1,q=1,S=12,P=1,D=1,Q=1)
  
  ## Test
  sse[i,1] = sum((test - model1$pred)^2)
  sse[i,2] = sum((test - model2$pred)^2)
  sse[i,3] = sum((test - model3$pred)^2)
  
}
apply(sse,2,sum) 

library(forecast)
auto.arima(l1)
