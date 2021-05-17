setwd("D:/2020_2학기/시계열분석")
require(graphics)
library(tidyverse)
library(ggplot2)
library(forecast)
library(gridExtra)
library(tseries)

Sys.setlocale('LC_ALL','C')
carrot <- read.table("CarrotPrices.txt")
gas <- read.table("gasrx.txt")
save <- read.table("SavingsRate.txt")
iga <- read.table("INGA_AmsterdamDiff.txt")

carrot <- carrot[-9,]
carrot <- as.vector(t(carrot))
ts_carrot <- ts(data=carrot, start=c(1999,1), frequency = 12)
par(mfrow=c(1,2))
autoplot(ts_carrot)
autoplot(stl(ts_carrot, s.window = "periodic", robust= TRUE))
tseries::kpss.test(diff(ts_carrot), null = "NULL")
par(mfrow=c(1,2))
acf(diff(ts_carrot))
pacf(diff(ts_carrot))
arima_model1 <- arima(diff(ts_carrot), order=c(1,0,1))
arima_model1
plot1 <- autoplot(arima_model1) + ggtitle("ARMA(1,1)")
plot1
arima_model2 <- arima(diff(ts_carrot), order=c(1,0,0))
arima_model2
plot2 <- autoplot(arima_model2) + ggtitle("AR(1)")
plot2

arima_model3 <- arima(diff(ts_carrot), order=c(0,0,1))
arima_model3
plot3 <- autoplot(arima_model3) + ggtitle("MA(1)")
plot3


colnames(gas) <- c("gas_furnace")
gas_ts <- ts(gas$gas_furnace, frequency = 60)
autoplot(gas_ts)
autoplot(stl(gas_ts, s.window="periodic", robust = TRUE))
adf.test(diff(gas_ts),alternative = "stationary", k=0) # 안정된 데이터\
tseries::kpss.test(diff(gas_ts), null = "Level")

par(mfrow=c(1,2))
acf(diff(gas_ts))
pacf(diff(gas_ts))
arima_model2 <- arima(diff(gas_ts), order=c(3,0,2))
plot2 <- autoplot(arima_model2) + ggtitle("ARMA(3,2)")
plot2

save <- as.vector(t(save))
save_ts <- ts(data=save, start=c(1955,1), end=c(1979,4), frequency = 4)
autoplot(save_ts)
autoplot(stl(save_ts, s.window="periodic", robust = TRUE))
tseries::kpss.test(diff(save_ts), null = "Level")
auto.arima(diff(save_ts))
acf(diff(save_ts))
pacf(diff(save_ts))
auto.arima(diff(save_ts))
arima_model1 <- arima(diff(save_ts), order=c(1,0,1))
arima_model1
plot1 <- autoplot(arima_model1) + ggtitle("ARMA(1,1)")
plot1
arima_model2 <- arima(diff(save_ts), order=c(1,0,0))
arima_model2
plot2 <- autoplot(arima_model2) + ggtitle("AR(1)")
plot2
arima_model3 <- arima(diff(save_ts), order=c(0,0,1))
arima_model3
plot3 <- autoplot(arima_model3) + ggtitle("MA(1)")
plot3

colnames(iga) <- c("price")
iga_ts <- ts(iga$price, frequency = 5)
autoplot(stl(iga_ts, s.window="periodic", robust = TRUE))
tseries::kpss.test(diff(iga_ts), null = "Level")
acf(diff(iga_ts))
pacf(diff(iga_ts))
auto.arima(diff(iga_ts))
arima_model1 <- arima(diff(iga_ts), order=c(1,0,0))
arima_model1
plot1 <- autoplot(arima_model2) + ggtitle("AR(1)")
plot1
