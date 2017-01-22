t_data <- read.csv("traffic_data.txt",header = F)

head(t_data)

new_t_data <- do.call( rbind , strsplit( as.character( t_data$V1 ) , " " ) )

t_data <- cbind( t_data , Time = new_t_data[,2] , Date = new_t_data[,1] )

t_data$Date=as.Date(t_data$Date,"%m/%d/%Y")

df_t_data <- data.frame(date = t_data$Date,
                 year = as.numeric(format(t_data$Date, format = "%Y")),
                 month = as.numeric(format(t_data$Date, format = "%m")),
                 day = as.numeric(format(t_data$Date, format = "%d")))

df_t_data$Time=t_data$Time
df_t_data$Total_Vehicle=t_data$V2

head(df_t_data)

t_data$Date_Time=paste(t_data$Date, t_data$Time, sep=' ')
head(t_data)

names(t_data) <- c("original_TS","vehicle_count","Time","Date","formatted_TS")
head(t_data)


#coverted into ts
ts_t_data <- data.frame(time=t_data$formatted_TS,count = t_data$vehicle_count)

ts_t_data <- ts(ts_t_data$count,frequency = 288)

#spit into training and testing
train_length <- 170*288
test_length <- length(ts_t_data) - train_length

train_end <- time(ts_t_data)[train_length]
test_start <- time(ts_t_data)[train_length+1]

training <- window(ts_t_data, end = train_end)
testing <- window(ts_t_data, start = test_start)

## Use forecast
fcast <- forecast(training, h=test_length)
plot(fcast)
lines(testing, col='red')
acc_births <- accuracy(fcast$mean, testing)
acc_births
