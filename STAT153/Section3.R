library('TSA')

## Plot ts
data(wages,package = 'TSA')

wages.ts = wages
wages = as.vector(wages)
t = as.vector(time(wages.ts))
nt = length(t)

head(t)  #Return the First or Last Parts of an Object
?head
# [1] 1981.500 1981.583 1981.667 1981.750 1981.833 1981.917 monthly data

plot(t,wages,type = 'l')

filter_weights = rep(1/5,5) #Replicate Elements of Vectors and Lists
?rep
filter_wages = filter(wages,filter_weights)
?filter
plot(t,wages,type = 'l')
lines(t,filter_wages,col='red')

#exponential smooth

filter_weights = c(0,0.5^(1:20))
# filter_weights = 0.5^(1:20) no diffence
filter_weights = filter_weights / sum(filter_weights) 
?c
filter_wages = filter(wages,filter_weights) #central
filter_wages = filter(wages,filter_weights,sides=1) 
head(filter_wages,n=20L) 
plot(t,wages,type = 'l')
lines(t,filter_wages,col='red')


fit <- lm(wages~t)
plot(t,wages,type = 'l')
abline(fit)
plot(t,resid(fit),type = 'l')
acf(resid(fit))
?predict
t[72]




fit <- lm(wages~t+I(t^2))
plot(t,wages,type = 'l')
lines(t,fitted(fit),col='red')
points(t, fitted(fit),col='red')
plot(t,resid(fit),type = 'l')
acf(resid(fit))
?I

#predict

ts=c(t,t[72]+1/12*(1:20))
tsss=data.frame(t = ts)
predict = predict(fit,data.frame(t = ts))
