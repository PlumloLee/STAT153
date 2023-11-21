n = 2000
library(astsa)
w = rnorm(n, mean = 0, sd = 1)
acf2(w)


x=w[2:n]+0.5*w[1:n-1]

acf2(x)

$$CI
sigma = (17/25)^2+4/25
error = (1+4/25)*rep(1,n)
error[1]= sigma

$$theoretical acf
pho = rep(0,n)
pho[1]=2/5

acf(x)
upper = pho + error*1.96/sqrt(n)
lower = pho - error*1.96/sqrt(n)
points(upper,col="red")
points(lower,col="red")

rm(list = ls())
data(nhtemp)
plot.ts(nhtemp)

acf(nhtemp)



$$remove trend
Dnhtemp = diff(nhtemp)
plot(Dnhtemp)
acf2(Dnhtemp)


testset = nhtemp[50:59]
trainset = nhtemp[1:49]

##AR(1)
model1 = sarima(trainset,1,1,0,0,0,0)

# choose season to be 5
model1 = sarima(trainset,1,1,0,0,0,5)

model_pred = 

