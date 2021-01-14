tData <- read.csv("data.txt")
names(tData) <- c("timeStamp","count")


Date <- format(as.Date(tData$timeStamp,format="%m/%d/%Y"), "%d")
Month <- format(as.Date(tData$timeStamp,format="%m/%d/%Y"), "%m")
Year <- format(as.Date(tData$timeStamp,format="%m/%d/%Y"), "%Y")


# for Test

t<- as.Date( tData$timeStamp, '%m/%d/%Y')
tData$t <- t
require(ggplot2)
ggplot( data = tData, aes( t, count)) + geom_point() 

##########

# Shashank Code 

train=read.csv("data.txt",header = F)

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

#######################

## Continue with shashank code

allData <- df

t <- paste(train$Date,train$Time,sep = " ")

ggplot( data = subset(allData,month==4), aes( Time, Total_Vehicle)) + geom_line() 

# Date : 09-11-2016

train$DT <- paste(train$Date,train$Time,sep = " ")

tsData <- data.frame(train$DT,train$V2)

names(tsData) <- c("datetime","count")

hist(tsData$count)

a <- tsData[1:1500,]
a$datetime <- as.character(a$datetime)
a$datetime <- as.factor(a$datetime)
plot(a$datetime,a$count)

ggplot(a,aes(x=datetime,y=count)) + geom_line()

#####Continue with shashank code#########

library(ggplot2)

qplot(x=Total_Vehicle,data = df)


ggplot(data = df,aes(x=Total_Vehicle))+
  geom_bar()+
  scale_x_discrete(breaks = 1:100)
  

ggplot(data = df,aes(x=Total_Vehicle))+
  geom_bar()+
  scale_x_discrete(breaks = 1:100)+
  facet_wrap(~month,ncol = 3)


ggplot(df,aes(Time,Total_Vehicle)) + geom_point()

oneMonth <- subset(df,df$month==4)

ggplot(oneMonth,aes(Time,Total_Vehicle)) + geom_point()

ggplot(oneMonth[oneMonth$day==20,],aes(Time,Total_Vehicle)) + geom_jitter()

ggplot(oneMonth,aes(Time,Total_Vehicle)) + geom_point() + facet_wrap(~day,ncol = 3)


