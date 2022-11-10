rm(list=ls())
library(rio)
library(stargazer)
library(ggcorrplot)



# Importing specific sheets into R using the read_excel()
install.packages('readxl')
library(readxl)

Snackchain<-read_excel("SnackChain.xlsx", 
                       sheet = 1)
Snackchain_stores<-read_excel("SnackChain.xlsx", 
                       sheet = "stores")
Snackchain_products<-read_excel("Snackchain.xlsx",
                       sheet = "products")
Snackchain_transactions<-read_excel("Snackchain.xlsx",
                       sheet = "transactions")




str(Snackchain_stores)
colnames(Snackchain_stores) = c("STORE_NUM", "STORE_NAME", "CITY","STATE", "MSA", "SEGMENT", "PARKING", "SIZE","AVG_WEEKLY_BASKETS")
total_data=merge(x = Snackchain_products, y = Snackchain_transactions , by=c("UPC"))
total_dataa=merge(x = total_data, y = Snackchain_stores, by=c("STORE_NUM"))


snack_chain.data<-total_dataa[!(total_dataa$CATEGORY=="ORAL HYGIENE PRODUCTS" ),]


hist((snack_chain.data$HHS),col="red",
     main="Purchasing Households",
     probability = TRUE)


hist((snack_chain.data$SPEND),col="red",
     main="Spend",
     probability = TRUE)


hist((snack_chain.data$VISITS),col="red",
     main="Store Visits",
     probability = TRUE)


hist(log(snack_chain.data$HHS),col="red",
     main="Purchasing Households",
     probability = TRUE)


hist(log(snack_chain.data$SPEND),col="red",
     main="Spend",
     probability = TRUE)

hist(log(snack_chain.data$VISITS),col="red",
     main="Visits",
     probability = TRUE)

write.csv(snack_chain.data, file = "snack_chaindata.csv", row.names = FALSE)
str(total_dataa)



snack_chain.data$year <- format(as.Date(snack_chain.data$WEEK_END_DATE, format="%d/%m/%Y"),"%Y")
snack_chain.data$month <- format(as.Date(snack_chain.data$WEEK_END_DATE, format="%d/%m/%Y"),"%m")
snack_chain.data$day <- format(as.Date(snack_chain.data$WEEK_END_DATE, format="%d/%m/%Y"),"%d")

snack_chain.data$year=factor(format(snack_chain.data$WEEK_END_DATE, "%Y"))
snack_chain.data$month=factor(format(snack_chain.data$WEEK_END_DATE, "%m"))
snack_chain.data$day=factor(format(snack_chain.data$WEEK_END_DATE, "%d"))

snack_chain.data$STORE_NUM=as.factor(snack_chain.data$STORE_NUM)
snack_chain.data$CATEGORY=as.factor(snack_chain.data$CATEGORY)
snack_chain.data$SUB_CATEGORY=as.factor(snack_chain.data$SUB_CATEGORY)
snack_chain.data$DESCRIPTION=as.factor(snack_chain.data$DESCRIPTION)


str(snack_chain.data)
temp <- snack_chain.data[, c(2,9:17,21,23:25)]
library(PerformanceAnalytics)
chart.Correlation(temp)

library(lme4)

snack_chain.data=snack_chain.data[snack_chain.data$SPEND>0,]
snack_chain.data=snack_chain.data[snack_chain.data$UNITS>0, ]
snack_chain.data=snack_chain.data[snack_chain.data$HHS>0, ]


install.packages("lme4")

library(lme4) 


m3 <- lmer(log(SPEND) ~  DISPLAY*SEGMENT+ FEATURE*SEGMENT+ TPR_ONLY*SEGMENT+ DISPLAY*CATEGORY+ FEATURE*CATEGORY+ TPR_ONLY*CATEGORY+DISPLAY+ FEATURE + TPR_ONLY+PRICE*DESCRIPTION+DESCRIPTION+(1|STORE_NUM)+PRICE+UNITS, data=snack_chain.data, REML=FALSE)
summary(m3)
m4 <- lmer(log(UNITS) ~  DISPLAY+ FEATURE + TPR_ONLY+(1|STORE_NUM)+PRICE+DESCRIPTION, data=snack_chain.data, REML=FALSE)
m5 <- lmer(log(HHS) ~  DISPLAY+ FEATURE + TPR_ONLY+(1|STORE_NUM), data=snack_chain.data, REML=FALSE)


stargazer(m3,m4,m5, type="text", single.row=TRUE)



table(is.na(snack_chain.data))
na.omit(snack_chain.data) 
str(snack_chain.data)


#m6 <- lmer(log(SPEND) ~  DISPLAY*SEGMENT+ FEATURE*SEGMENT+ TPR_ONLY*SEGMENT+ DISPLAY*CATEGORY+ FEATURE*CATEGORY+ TPR_ONLY*CATEGORY+SEGMENT+ CATEGORY+(1|STORE_NUM), data=snack_chain.data, REML=FALSE)

summary(m6)

table(snack_chain.data$SPEND)

library(dplyr)



#What are the five most price elastic and five least price elastic products? 
#Price elasticity is the change in units sold for change in product price. (3 points)

#m6 <- lmer(log(SPEND) ~  DISPLAY*SEGMENT+ FEATURE*SEGMENT+ TPR_ONLY*SEGMENT+ DISPLAY*CATEGORY+ FEATURE*CATEGORY+ TPR_ONLY*CATEGORY+SEGMENT+ CATEGORY+(1|STORE_NUM), data=snack_chain.data, REML=FALSE)
#As the retailer, which products would you lower the price
#to maximize (a) product sales and (b) unit sales, and why? (1 points)

str(snack_chain.data)



 
