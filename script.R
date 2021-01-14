a40 <- read.csv("a40.csv")
a49 <- read.csv("a49.csv")

a40$Flow <- round(a40$Flow)
a49$Flow <- round(a49$Flow)

str(a40)

a40$LinkDescription <- NULL
a40$AverageJT <- NULL
a40$DataQuality <- NULL
a40$LinkLength <- NULL

a49$LinkDescription <- NULL
a49$AverageJT <- NULL
a49$DataQuality <- NULL
a49$LinkLength <- NULL

ts_a40 <- ts(a40$Flow,frequency = 96)
ts_a49 <- ts(a49$Flow,frequency = 96)
plot(ts_a40)

plot(decompose(ts_a40))

library(forecast)

#model1 <- Arima(ts_a40,order=c(24,0,0),seasonal=list(order=c(6,0,0)))
  
fcast <- forecast(ts_a40, h=1*96)
f <- as.data.frame(fcast)
fcast_2 <- forecast(ts_a49, h=1*96)
f_2 <- as.data.frame(fcast_2)
plot(fcast,xlab="Time",ylab="Vehicle Count",main="Traffic Forecasting",xlim=c(180,201))
lines(testing, col='red')

