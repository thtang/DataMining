library(xlsx)
library(dplyr)
library(zoo)
library(lubridate)
library(ggplot2)
tranc_data <-read.csv("C:/Users/tsunh/Desktop/Schoolwork/DataMining/dataset/iChef data for BAFT 2016 class.csv",
                      stringsAsFactors = F) %>% select(-c(X,X.1,X.2,X.3))
table(tranc_data$restaurant_uuid) # 6d0ebab3-edf8-4e04-a947-1973e76ab11f 這間餐廳最多筆資料
restaur_id <- unique(tranc_data$restaurant_uuid)
restaur_6d <- subset(tranc_data,restaurant_uuid==restaur_id[6])

#restaur_6d$timestamp[1:10]
#unix_t <- restaur_6d$unix[1]
#as.POSIXct(unix_t, origin="1970-01-01",tz = "GMT")
trans_time <- function(unix_t){
  return(as.character(as.POSIXct(unix_t, origin="1970-01-01",tz = "Asia/Taipei")))
}
#先分成24小時，看每小時的來客數
restaur_6d$timestamp <- sapply(restaur_6d$unix,trans_time)

MyDatesTable <- as.data.frame(table(cut(as.POSIXlt(restaur_6d$timestamp,tz = "Asia/Taipei"), breaks="hour")),
                              stringsAsFactors = F)
MyDatesTable$time <- sapply(MyDatesTable$Var1, hour)

# choose freq non zero
saveRDS(MyDatesTable,"res6.rds")
freq_non_zero <- subset(MyDatesTable,MyDatesTable$Freq != 0 & MyDatesTable$time <= as.POSIXct("2017-05-10"))

#boxplot for each hour
ggplot(data=freq_non_zero , aes(as.factor(time),Freq,fill = as.factor(time))) +
  geom_boxplot()+
  guides(fill=FALSE)

#time series plot
CutDatesTable <-subset(MyDatesTable,MyDatesTable$Var1<=as.POSIXct("2016-05-10"))
ggplot(CutDatesTable, aes(as.POSIXct(Var1), Freq)) + geom_line() +
  xlab("") + ylab("Daily Views")
