library(readr)
DB <- Homework_4_Student_Data <- read_csv("D:/Rochester/MA USING R/HW4/Homework 4 Student Data.csv")
###### Part 1 #####
Units <- aggregate(cbind(units)~productNum, data = DB, FUN = sum )
Units
BestSeller <- which.max(Units$units)
BestSeller
#a.Most popular product

upcFile <- subset(DB, DB$productNum == BestSeller)
upcFile
#b.Subset of best seller

aggUPCFile =aggregate(cbind(totalCost,units)~weekInYearNum+overallWeekNum+storeNum+isFeature+isDisplay,data=upcFile,FUN = sum)
aggUPCFile$pricePerCan = aggUPCFile$totalCost/aggUPCFile$units
model1 = lm(log(units)~pricePerCan+isFeature+isDisplay+poly(weekInYearNum,4)+factor(storeNum),data=aggUPCFile)
summary(model1)
#c.Run regression of model1

possiblePrices = data.frame(price = seq(0,10,.01))
possiblePrices
possiblePrices$demand = NA
newData =data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices$price)
newData
#d.New Dataframe.

possiblePrices$demand <- exp(predict(model1,newData))
possiblePrices
#e.Predict demand using model1.

possiblePrices$MarginProfit <- (possiblePrices$price-0.3) 
possiblePrices$ExpectProfit <- possiblePrices$demand * possiblePrices$MarginProfit
#f.Calculation of margin profit and expect profit.

optimalProfit <- possiblePrices$ExpectProfit[which.max(possiblePrices$ExpectProfit)]
optimalPrice <- possiblePrices$price[which.max(possiblePrices$ExpectProfit)]
optimal <- data.frame(optimalPrice,optimalProfit)
optimal
#g.Use which.max function to find the optimal price, and the expected profit at that price.

model2 =lm(log(units)~pricePerCan+poly(weekInYearNum,4),data=aggUPCFile)
possiblePrices2 = data.frame(price = seq(0,10,.01))
possiblePrices2
possiblePrices2$demand = NA
newData2 =data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices2$price)
newData2
possiblePrices2$demand <- exp(predict(model2,newData2))
possiblePrices2
possiblePrices2$MarginProfit <- (possiblePrices2$price-0.3) 
possiblePrices2$ExpectProfit <- possiblePrices2$demand * possiblePrices2$MarginProfit
optimalProfit2 <- possiblePrices2$ExpectProfit[which.max(possiblePrices2$ExpectProfit)]
optimalPrice2 <- possiblePrices2$price[which.max(possiblePrices2$ExpectProfit)]
optimal2 <- data.frame(optimalPrice2,optimalProfit2)
optimal2
#h.Repeat 1.c - 1.g using model2.

##### Part 2 #####
library('nnet')
set.seed(1)
nnet1 = nnet(log(units)~pricePerCan+isFeature+isDisplay+poly(weekInYearNum,4)+factor(storeNum),data=aggUPCFile,skip=TRUE,size=3,linout=1,maxit=10000)
nnet2 = nnet(log(units)~pricePerCan+poly(weekInYearNum,4),data=aggUPCFile,skip=TRUE,size=3,linout=1,maxit=10000)
#a.Estimate two neural networks models

possiblePrices3 = data.frame(price = seq(0,10,.01))
possiblePrices3
newData3 =data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices3$price)
newData3
possiblePrices3$demandnnet1 <- exp(predict(nnet1,newData3))
possiblePrices3$demandnnet2 <- exp(predict(nnet2,newData3))
possiblePrices3 
possiblePrices3[which(possiblePrices3$price == 0.50),]
possiblePrices3[which(possiblePrices3$price == 1.00),]
#b.Get prediction of demand using nnet1 and nnet2 when price is 1.0 and 0.5.

possiblePrices3 = data.frame(price = seq(0,10,.01))
possiblePrices3
newData3 =data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices3$price)
newData3
possiblePrices3$demandnnet1 <- exp(predict(nnet1,newData3))
possiblePrices3$demandnnet2 <- exp(predict(nnet2,newData3))
possiblePrices3$MarginProfitnnet1 <- (possiblePrices3$price -0.3) 
possiblePrices3$MarginProfitnnet2 <- (possiblePrices3$price -0.3)
possiblePrices3$ExpectProfitnnet1 <- possiblePrices3$demandnnet1 * possiblePrices3$MarginProfitnnet1
possiblePrices3$ExpectProfitnnet2 <- possiblePrices3$demandnnet2 * possiblePrices3$MarginProfitnnet2
optimalProfit3_nnet1 <- possiblePrices3$ExpectProfitnnet1 [which.max(possiblePrices3$ExpectProfitnnet1)]
optimalProfit3_nnet2 <- possiblePrices3$ExpectProfitnnet2 [which.max(possiblePrices3$ExpectProfitnnet2)]
optimalPrice3_nnet1 <- possiblePrices3$price[which.max(possiblePrices3$ExpectProfitnnet1)]
optimalPrice3_nnet2 <- possiblePrices3$price[which.max(possiblePrices3$ExpectProfitnnet2)]
optimal3_nnet1 <- data.frame(optimalPrice3_nnet1,optimalProfit3_nnet1)
optimal3_nnet2 <- data.frame(optimalPrice3_nnet2,optimalProfit3_nnet2)
optimal3_nnet1
optimal3_nnet2
#c.Repeat 1d - 1g using model nnet1`and nnet2.`

possiblePrices3
pic1 <- plot(possiblePrices3$price, possiblePrices3$ExpectProfitnnet1)
abline(v= min(aggUPCFile$pricePerCan) , new = TRUE)
abline(v=max(aggUPCFile$pricePerCan), new = TRUE)
min(aggUPCFile$pricePerCan)
max(aggUPCFile$pricePerCan)
#Find the range of observed data.
pic2 <- plot(possiblePrices3$price, possiblePrices3$ExpectProfitnnet2)
possiblePrices4 = data.frame(price = seq(0.29,1.19,.01))
#Fix the data range to [0.29,1.19].
possiblePrices4
newData4 =data.frame(weekInYearNum=1,overallWeekNum=1,storeNum=1,isFeature=FALSE,isDisplay=FALSE,pricePerCan=possiblePrices4$price)
newData4
possiblePrices4$demandnnet1_new <- exp(predict(nnet1,newData4))
possiblePrices4$MarginProfitnnet1_new <- (possiblePrices4$price -0.3)
possiblePrices4$ExpectProfitnnet1_new <- possiblePrices4$demandnnet1_new * possiblePrices4$MarginProfitnnet1_new
optimalProfit4_nnet1 <- possiblePrices4$ExpectProfitnnet1_new [which.max(possiblePrices4$ExpectProfitnnet1_new)]
optimalPrice4_nnet1 <- possiblePrices4$price[which.max(possiblePrices4$ExpectProfitnnet1_new)]
optimal4_nnet1 <- data.frame(optimalPrice4_nnet1,optimalProfit4_nnet1)
optimal4_nnet1
pic3 <- plot(possiblePrices4$price, possiblePrices4$ExpectProfitnnet1_new)
#d. Fix the model and get the optimal price and optimal profit using nnet1.
