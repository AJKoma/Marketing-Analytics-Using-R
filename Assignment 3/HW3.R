##### Part 1 ######
library(readr)
library("forecast", lib.loc="~/R/win-library/3.4")
DB <- read_csv("D:/Rochester/MA USING R/Homework 3 Student Data.csv")
#a.Load Dataset as DB

attach(DB)
DBT <- aggregate(DB,by=list(overallWeekNum), FUN=sum)
DBTall<-data.frame(DBT$Group.1,DBT$totalCost)
#Aggregate totalcost by overallweeknum
names(DBTall) <- c('overallWeekNum','totalCost')
nrow(DBTall)
DBTTrain <- DBTall[1:(nrow(DBTall)-52),]
#training set
holdout <- DBTall[(nrow(DBTall)-51):nrow(DBTall),]
#holdout set
DBTTrain_ts <- ts(DBTTrain,frequency = 52)
sales_ts <- ts(DBTTrain$totalCost, frequency = 52)
sales_ts
#b.time series of sales

plot(sales_ts)
acf(sales_ts)
#c.plots of time series and autocorrelations

ARIMA <- Arima(sales_ts ,order=c(3,0,2),include.drift=TRUE)
#d.ARMA model

nss <- auto.arima(sales_ts,approximation=FALSE,D=0 ,seasonal=FALSE ,stepwise=FALSE)
ss <- auto.arima(sales_ts,approximation=FALSE,D=0 ,seasonal=TRUE ,stepwise=FALSE)
nss
ss
#e.Non-seasonal and seasonal ARMA model

AIC_ss <- AIC(ss)
AIC_nss <- AIC(nss)
AIC_nss
AIC_ss
Forecastss <- forecast(ss,h=52)
Forecastnss <- forecast(nss, h=52)
plot(Forecastss)
plot(Forecastnss) 
MSE_ss <- mean((holdout$totalCost-Forecastss$mean)^2)
MSE_nss <- mean((holdout$totalCost-Forecastnss$mean)^2)
MSE_ss
MSE_nss
MSE <- c(MSE_ss,MSE_nss)
AIC <- c(AIC_ss,AIC_nss)
performance <- data.frame(MSE,AIC)
rownames(performance) <- c('Seasonal','Non-seasonal')
performance
#f. AIC,MSE and plots of two models

##### Part 2 #####
PricePerCan <- DB$totalCost/DB$units
#a.calculation of PricePerCan

summary(lm(log(DB$units)~PricePerCan))
#b.regression of log(units) on price per can

unitpo <- aggregate(DB$units~DB$productNum+DB$overallWeekNum, FUN = mean)
salespo <- aggregate(DB$totalCost~DB$productNum+DB$overallWeekNum, FUN = mean)
PricePerCanpo <- salespo$`DB$totalCost`/unitpo$`DB$units`
summary(lm(log(unitpo$`DB$units`)~PricePerCanpo))
#c.regression of log(units) on price per can after aggregated to Productnum and Overallweeknum level

unito <- aggregate(DB$units~DB$overallWeekNum, FUN = mean)
saleso <- aggregate(DB$totalCost~DB$overallWeekNum, FUN = mean)
PricePerCano <- saleso$`DB$totalCost`/unito$`DB$units`
summary(lm(log(unito$`DB$units`)~PricePerCano))
#d.regression of log(units) on price per can after aggregated to Overallweeknum level

##### Part 3 #####
PricePerCan <- DB$totalCost/DB$units
#a.calculation of PricePerCan
summary(lm(log(DB$units)~PricePerCan))
#b.regression of log(units) on PricePercan
summary(lm(log(DB$units)~PricePerCan+DB$isFeature))
#c.repeat b, control for isFeatute
summary(lm(log(DB$units)~PricePerCan+DB$isFeature+DB$isDisplay))
#d.repeat c, control for isDisplay
summary(lm(log(DB$units)~PricePerCan+DB$isFeature+DB$isDisplay+factor(storeNum)))
#e.repeat d, control for factor of storeNum
summary(lm(log(DB$units)~PricePerCan+DB$isFeature+DB$isDisplay+factor(storeNum)+factor(productNum)))
#f.repeat e, control for factor of productNum
summary(lm(log(DB$units)~PricePerCan+DB$isFeature+DB$isDisplay+factor(storeNum)+factor(productNum)+factor(weekInYearNum)))
#g.repeat f, control for factor of weekinYearNum


