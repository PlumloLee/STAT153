d365 = diff(Sales,lag = 365)
plot.ts(d365)
d7d365 = diff(d365,lag = 7)
plot.ts(d7d365)