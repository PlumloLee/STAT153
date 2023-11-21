
library(astsa)

(r = round(acf(soi, 6, plot=FALSE)$acf[-1], 3)) # first 6 sample acf values

par(mfrow=c(1,2))
plot(lag(soi,-1), soi); legend('topleft', legend=r[1])
plot(lag(soi,-6), soi); legend('topleft', legend=r[6])

(4*10+2*20+1*20)/(1*10+1*20+0.5*20)
sqrt(2.8*2.5)
L=(1*10+1*20+0.5*20)/(1*40+1*40+0.5*40)
P=(4*10+2*20+1*20)/(4*40+2*40+1*40)
F=sqrt(L*P)
F
L=(3*10+2*20+1*20)/(4*10+2*20+1*20)
P=(3*10+2*30+1*10)/(4*10+2*30+1*10)
F=sqrt(L*P)
F

L=(4*10+2*30+1*10)/(4*10+2*20+1*20)
P=(3*10+2*30+1*10)/(3*10+2*20+1*20)
F=sqrt(L*P)
F


