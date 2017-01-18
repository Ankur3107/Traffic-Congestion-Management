setwd("/Users/Ankur/Desktop/Traffic Project/")

train=read.csv("traffic_data.txt",header = F)

new <- do.call( rbind , strsplit( as.character( train$V1 ) , " " ) )

train=cbind( train , Time = new[,2] , Date = new[,1] )

train$Date=as.Date(train$Date,"%m/%d/%Y")
df <- data.frame(date = train$Date,
                 year = as.numeric(format(train$Date, format = "%Y")),
                 month = as.numeric(format(train$Date, format = "%m")),
                 day = as.numeric(format(train$Date, format = "%d")))

df$date<-NULL
df$Time=train$Time
df$Total_Vehicle=train$V2

train$D_T=paste(train$Date, train$Time, sep=' ')
str(train)
train$D_T <- as.POSIXct(train$D_T, format="%H:%M")

install.packages("forecast")
library(forecast)
#print(ts(train[,5],start =c(2005,4,10,0:00),frequency = 288 )[1:288])
train[,5]<-ts(train[,5],start =c(2005,4,10),frequency = 288 )
train[,4]<-ts(train[,4],start =c(2005,4),frequency = 288 )

ARIMAfit <- auto.arima(final[,2], approximation=FALSE,trace=FALSE)
final=data.frame(vehicle=train$V2,time=train$Date)
final$vehicle=as.numeric(final$vehicle)
fit<-Arima(final,order=c(1,1,288))


library(ggplot2)
library(dplyr)
ggplot(data=subset(df,df$day), aes(x=df$Time,y=Total_Vehicle)) + geom_point() 

train$datetime <- paste(train$Date,train$Time,sep = " ")

ggplot(data=train, aes(x=datetime,y=V2)) + geom_point() 


#### Ankur Code #####

tdata <- data.frame(time=paste(train$Date,train$Time,sep=" "),count = train$V2)

t <- ts(tdata$count,frequency = 288)

plot(t)

plot(decompose(t))

library(forecast)
fit <- auto.arima(t)

pred <- predict(fit,n.ahead = 288*30)

plot(t)

lines(pred$pred,col="red")
