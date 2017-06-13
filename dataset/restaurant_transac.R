library(xlsx)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)
library(reshape2)
tranc_data <-read.csv("iChef data for BAFT 2016 class.csv",
                      stringsAsFactors = F) %>% select(-c(X,X.1,X.2,X.3))
summary(tranc_data)

table(tranc_data$restaurant_uuid) # 6d0ebab3-edf8-4e04-a947-1973e76ab11f 這間餐廳最多筆資料
restaur_id <- unique(tranc_data$restaurant_uuid)
restaur_6d <- subset(tranc_data,restaurant_uuid==restaur_id[1])


#saveRDS(restaur_6d,"tran5.rds")





#restaur_6d$timestamp[1:10]
#unix_t <- restaur_6d$unix[1]
#as.POSIXct(unix_t, origin="1970-01-01",tz = "GMT")
trans_time <- function(unix_t){
  return(as.character(as.POSIXct(unix_t, origin="1970-01-01",tz = "Asia/Taipei")))
}
trans_time2 <- function(unix_t){
  hourly <-as.POSIXct(unix_t, origin="1970-01-01",tz = "Asia/Taipei") %>% format("%Y-%m-%d %H")
  return(hourly)
}
#先分成24小時，看每小時的來客數
restaur_6d$timestamp <- sapply(restaur_6d$unix,trans_time)

MyDatesTable <- as.data.frame(table(cut(as.POSIXlt(restaur_6d$timestamp,tz = "Asia/Taipei"), breaks="hour")),
                              stringsAsFactors = F)



MyDatesTable <- as.data.frame(table(cut(as.POSIXlt(restaur_6d$timestamp,tz = "Asia/Taipei"), breaks="hour"),item_name),
                              stringsAsFactors = F)
MyDatesTable$time <- sapply(MyDatesTable$Var1, hour)

# choose freq non zero
#saveRDS(MyDatesTable,"res6.rds")
freq_non_zero <- subset(MyDatesTable,MyDatesTable$Freq != 0 & MyDatesTable$time <= as.POSIXct("2017-05-10"))

#boxplot for each hour
ggplot(data=freq_non_zero , aes(as.factor(time),Freq,fill = as.factor(time))) +
  geom_boxplot()+
  guides(fill=FALSE)

#time series plot
CutDatesTable <-subset(MyDatesTable,MyDatesTable$Var1<as.POSIXct("2016-04-04") &
                         MyDatesTable$Var1 > as.POSIXct("2016-03-30"))
ggplot(CutDatesTable, aes(as.POSIXct(Var1), Freq)) + geom_line() +
  xlab("") + ylab("Daily Views")

#forecast
library(forecast)
library(zoo)
library(xts)
x<- zoo(CutDatesTable$Freq,as.POSIXct(CutDatesTable$Var1))
xts_obj<- xts(CutDatesTable$Freq, order.by = as.POSIXct(CutDatesTable$Var1,format="%y-%m-%d %H:%M:%S"
                                                        ),frequency = 24)
attr(xts_obj, 'frequency') <- 24


library(ggfortify)
autoplot(forecast(auto.arima(xts_obj),h=24),xaxt = "n")
autoplot(forecast(ets(xts_obj),h=24),xaxt = "n")
summary(auto.arima(xts_obj))
summary(ets(xts_obj))


plot(forecast(ets(xts_obj), 24))
c(as.POSIXct(CutDatesTable$Var1) + 12)

#regression


fit <- tslm(ts(as.vector(xts_obj),frequency = 24)~ trend + season)
plot(forecast(fit, h=24))
summary(fit)
#with weather
write.csv(CutDatesTable, "CutDatesTable.csv")
cut_with_weather <- read.csv("CutDatesTable.csv")
xts_obj<- xts(CutDatesTable$Freq, order.by = as.POSIXct(CutDatesTable$Var1,format="%y-%m-%d %H:%M:%S"
),frequency = 24)

fit <- tslm(ts(as.vector(xts_obj),frequency = 24)~ trend + season)
plot(forecast(fit, h=24))
summary(fit)

fit <- tslm(ts(as.vector(xts_obj),frequency = 24)~ trend + season+ts(as.vector(cut_with_weather$temp),frequency = 24)+
            ts(as.vector(cut_with_weather$rain),frequency = 24))
plot(forecast(fit, h=24))

plot(residuals(fit))
summary

#### forecast base on product ####
restaur_6d$timehourly <- sapply(restaur_6d$unix,trans_time2)
restaur_6d %>% View()
class(restaur_6d$timehourly)
rest_cast <- as.data.frame(with(restaur_6d,
                                table(cut(as.POSIXct(timestamp, tz = "Asia/Taipei"),
                                                                breaks = "hour"),item_name))) %>% dcast(Var1 ~ item_name, value.var = "Freq")
View(rest_cast)
write.csv(rest_cast,"productByTime.csv")




#association rules 

library(arulesViz)
require(xlsx)
library(gdata)
library(dplyr)
restaur_oneP <- subset(restaur_6d, people ==1 , dining = "indoor")
View(restaur_oneP)

transac <- restaur_6d %>% select(invoice_uuid, item_name)
transac$invoice_uuid <- as.factor(transac$invoice_uuid)
transac$item_name <- as.factor(transac$item_name)
#str(transac)

#a <- split(transac$item_name,transac$invoice_uuid)

transac <- as(split(transac$item_name,transac$invoice_uuid),"transactions")

summary(transac) #find most popular item


rules <- apriori(transac, parameter=list(support=0.001, confidence=0.1))
#rules
sorted_lift=sort(rules,by= 'lift' )

#inspect(sorted_lift)
subset.matrix=is.subset(sorted_lift,sorted_lift)
redundant=colSums(subset.matrix) > 1
which(redundant)
rulepruned=sorted_lift[!redundant]

#inspect(rulepruned)
#rulepruned %>% length()
sortN=head(sort(rulepruned,by ="lift"),10)
#inspect(head(sort(rulepruned,by ="lift"),30))

plot(head(sort(rulepruned,by ="lift"),100), method='scatterplot')

plot(head(sort(rulepruned,by ="lift"),30), method='graph', control = list(type='items'))

plot(head(sort(rulepruned,by ="lift"),30), method = 'grouped')


#production weather exploration
productByTime <- read.csv("C:/Users/tsunh/Desktop/Schoolwork/DataMining/dataset/productByTime.csv",
                          stringsAsFactors = FALSE)
View(productByTime)
for (i in c(1:length(productByTime$Var1))){
  print(i)
  i <- as.numeric(i)
  productByTime$h[i] <- hour(as.POSIXct(productByTime$Var1[i]))
}
productByTime_hottime <- productByTime %>% filter(h>10 & h <22 )    
write.csv(productByTime_hottime,"productByTime_hottime2.csv")

clean_productByTime <- read.csv("C:/Users/tsunh/Desktop/clean_product_weather.csv",
                          stringsAsFactors = FALSE)

clean_productByTime <- clean_productByTime %>% filter(as.numeric(wind_dir) < 150 & as.numeric(wind_dir) > 90 | as.numeric(wind_dir)>250)
clean_productByTime <- clean_productByTime %>% mutate(wind_dir_cat = ifelse(as.numeric(wind_dir) < 150 & as.numeric(wind_dir) > 90,0,1))
write.csv(clean_productByTime,"productByTime_hottime3.csv")
