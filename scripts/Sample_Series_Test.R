set.seed(1)
library(lubridate)

index <- ISOdatetime(2010,1,1,0,0,0)+1:8759*60*60

head(index)

month <- month(index)
hour <- hour(index)

usage <- 1000+10*rnorm(length(index))-25*(month-6)^2-(hour-12)^2
usage <- ts(usage,frequency=24)


plot(usage)

#Create monthly dummies.  Add other xvars to this matrix

xreg <- model.matrix(~as.factor(month))[,2:12]

colnames(xreg) <- c('Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

library(forecast)

model1 <- Arima(usage, order=c(0,0,0), seasonal=list(order=c(1,0,0), period=24), xreg=xreg)
plot(usage)
lines(fitted(model1),col=2)

#Benchmark against other models
model2 <- tslm(usage~as.factor(month)+as.factor(hour))
plot(usage)
lines(fitted(model2),col=2)

model3 <- tslm(usage~as.factor(month))
plot(usage)
lines(fitted(model3),col=2)

model4 <- rep(mean(usage),length(usage))
plot(usage)
lines(fitted(model4),col=2)




