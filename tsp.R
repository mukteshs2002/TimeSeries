library("readxl")
library(lubridate)
library(tseries)
library(zoo)
library(forecast)
library(tidyr)
#WEATHER

urlfile2<-'https://raw.githubusercontent.com/Mohsinrazaa/Time-Series-project/main/Weather%20Forecasting%20using%20SRIMAX%20Model/weather.csv'
y<-read.csv(urlfile2)  
y
sum(is.na(y$Rainfall))
colSums(is.na(y))



y$Rainfall<-ifelse(is.na(y$Rainfall),mean(y$Rainfall,na.rm = T),y$Rainfall)
y$Date
y$year<-year(y$Date)
View(y)
y$year
View(y)
ts_y<-ts((y$Rainfall),start=c(2008,12,01),end=c(2017,06,25),frequency = 365)
ts_y
summary(ts_y)
plot(ts_y)
plot(decompose(ts_y,type="additive"))
man<-decompose(ts_y,type="additive")
ts_yy<-ts_y-man$trend
(is.na(ts_yy))
ts_yyy<-na.remove(ts_yy)
adf.test(ts_yyy)
plot(acf(ts_yyy))
plot(pacf(ts_yyy))
auto.arima(ts_yyy)

