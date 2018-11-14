library(readr)

DB <- read_csv("Homework 1 Data - 436R.csv")                        
#a.Read CSV file as DB

DB$rDate <- as.Date(paste(DB$date),format="%m/%d/%Y")  
#b.Add new cloumn $rDate, transfered to date type from data of $date

PeriodStart <- min(DB$rDate[DB$isTreatmentPeriod==1])                       
PeriodStart                                                         
#c.Output of min date where DB$DB$isTreatmentPeriod is 1

TreatGroup <- subset(DB,DB$isTreatmentGroup==1)
TreatGroup                                              
#d.Subset of DB where DB$isTreatmentGroup is 1

summary(lm(formula=log(TreatGroup$revenue)~TreatGroup$isTreatmentPeriod))
#  Regresion compares log(revenue) of the treatment group in the pre-treatment period and in the treatment period

PreTreatPeriod <- subset(DB,DB$isTreatmentPeriod==0)
summary(lm(formula=log(PreTreatPeriod$revenue)~PreTreatPeriod$isTreatmentGroup))
#  Regresion compares log(revenue) of the treatment group and the control group in the pre-treatment period

PostTreatPeriod <- subset(DB,DB$isTreatmentPeriod==1)
summary(lm(formula=log(PostTreatPeriod$revenue)~PostTreatPeriod$isTreatmentGroup))
#e.Regression of log(revenue) on whether theDMA is the treatment group

month<-substr(paste(PostTreatPeriod$date),1,1)
month
#f.Vector variable of month
summary(lm(formula=log(PostTreatPeriod$revenue)~PostTreatPeriod$isTreatmentGroup*month))
#  Regression of log(revenue) on month interacted with DMA




