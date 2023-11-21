HW1


dataset = read.csv("/Users/li/Desktop/STAT153/iPod.csv")
iPod = ts(dataset$iPod,start =c(2004,1),frequency = 12)
# Part 1
par(mfrow=c(2,1))
summary(fit1<-lm(iPod~time(iPod)),na.action=NULL)
ts.plot(iPod)
abline(fit1)
ts.plot(resid(fit1))

#Part 2
acf(resid(fit))
Q = factor(cycle(iPod))
summary(fit2<-lm(iPod~time(iPod)+Q))
par(mfrow=c(2,1))
ts.plot(iPod)
plot(fitted(fit2),type='l',row=1)
ts.plot(resid(fit2))

predict(fit2,newdata = data.frame(time=190))




1/5.1926
 1 - (0.95^100+100*0.05*0.95^99+99*50*0.05^2*0.95^98+98*99*100/6*0.05^3*0.95^97)
 0.2443*0.8418
 