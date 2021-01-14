library(forecast)


births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))

acf(birthstimeseries)
pacf(birthstimeseries)

train_length <- 150
test_length <- length(birthstimeseries) - train_length
train_end <- time(birthstimeseries)[train_length]
test_start <- time(birthstimeseries)[train_length+1]

training <- window(birthstimeseries, end = train_end)
testing <- window(birthstimeseries, start = test_start)

## forecast
fcast <- forecast(training, h=test_length)
plot(fcast)
lines(testing, col='red')
acc_births <- accuracy(fcast$mean, testing)
