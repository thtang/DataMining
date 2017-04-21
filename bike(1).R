library(Hmisc)
library(dplyr)
library(lattice)
require(useful)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(ggmap)
bike <- read.csv("C:/Users/tsunh/Desktop/schoolwork/Datamining/dataset/ubikeweatherbig5.csv",stringsAsFactors = F)
describe(bike)
class(bike$date)

bike <- bike[as.Date(bike$date)<="2015-01-09",]
bike$avg.sbi.ratio <- bike$avg.sbi/bike$tot
t1 <- mutate(bike, time = paste(bike$date,paste0(formatC(bike$hour, width=2, flag="0"),":00"),sep=" "))
t2 <- select(t1, time, avg.sbi.ratio, sna) %>% spread(time, avg.sbi.ratio)
t2 <- t2[complete.cases(t2),]
rownames(t2) <- t2$sna
t3 <- select(t2, -sna)

pca <- prcomp(t3)
dat.loadings <-pca$x[,1:2]
cl <- kmeans(dat.loadings, centers = 4)
pca1 <-pca$x[,1]
pca2 <-pca$x[,2]
mydf<-data.frame(ID=names(pca1),PCA1=pca1, PCA2=pca2, Cluster=factor(cl$cluster))
ggplot(mydf, aes(x=PCA1, y=PCA2, label=ID, color=Cluster)) + 
  geom_point()+
  geom_text_repel(size = 3, aes(colour = mydf$Cluster), force =0.1)
PlotHartigan(FitKMeans(dat.loadings))

#ggmap
map <- bike[1:221,]
map <- map %>% inner_join(mydf,by = c(sna="ID"))
ggmap(get_googlemap(center='taipei city', zoom=12, maptype='roadmap'), extent='device') + geom_point(show.legend = T, data=map, aes(size=(tot-min(tot))/(max(tot)-min(tot)),x=lng, y=lat,colour=map$Cluster))

#temperature
temp <- select(t1, time, temp, sna) %>% spread(time, temp)
temp<- temp[complete.cases(temp),]
rownames(temp) <- temp$sna
temp<- select(temp, -sna)
temp$mean.temp <- apply(temp,1,median)
temp$mean.temp <- (temp$mean.temp-min(temp$mean.temp))/(max(temp$mean.temp)-min(temp$mean.temp))
temp$ID<- row.names(temp)
temp.mean <- temp %>% select(ID,mean.temp)
map <- map %>% inner_join(temp.mean,by = c(sna="ID"))

ggmap(get_googlemap(center='taipei city', zoom=12, maptype='roadmap'), extent='device') + geom_point(show.legend = T, data=map, aes(size=mean.temp,x=lng, y=lat,colour=map$Cluster))


#rainfall 
rainfall  <- select(t1, time, rainfall , sna) %>% spread(time, rainfall )
rainfall <- rainfall [complete.cases(rainfall ),]
rownames(rainfall ) <- rainfall $sna
rainfall <- select(rainfall , -sna)
rainfall$mean.rainfall  <- apply(rainfall ,1,mean)
rainfall$mean.rainfall  <- (rainfall$mean.rainfall -min(rainfall$mean.rainfall))/(max(rainfall$mean.rainfall)-min(rainfall$mean.rainfall))
rainfall$ID<- row.names(rainfall)
rainfall.mean <- rainfall %>% select(ID,mean.rainfall)
map <- map %>% inner_join(rainfall.mean,by = c(sna="ID"))

ggmap(get_googlemap(center='taipei city', zoom=12, maptype='roadmap'), extent='device') + geom_point(show.legend = T, data=map, aes(size=mean.rainfall,x=lng, y=lat,colour=map$Cluster))

#relation between rainfall and avg.avg.sbi

t3$mean.avg.sbi <- apply(t3,1,median)
sbi.mean <- t3 %>% select(mean.avg.sbi)
sbi.mean$ID <- rownames(sbi.mean)
map <- map %>% inner_join(sbi.mean,by = c(sna="ID"))

ggplot(map , aes(x=mean.rainfall, y=(mean.avg.sbi-min(mean.avg.sbi))/(max(mean.avg.sbi)-min(mean.avg.sbi)))) +
  geom_point(shape=1)




hsnu <- bike %>% filter(sno==41)
print(dim(hsnu))
print(hsnu %>% head())

splom(~hsnu[c(9,13,17,21)])
hsnu$log_temp <- hsnu$temp %>% log()
hsnu$log_avg.sbi <- hsnu$avg.sbi %>% log()
str(hsnu$date[1])
str(hsnu$date[1])
hsnu <- hsnu %>% mutate(time = paste(hsnu$date,paste0(formatC(hsnu$hour, width=2, flag="0"),":00"),sep=" ")) 
rownames(hsnu) <- hsnu$time
hsnu$weeday <- weekdays(as.Date(hsnu$date))
write.csv(hsnu,"C:/Users/wendy-MM/Desktop/hsnu.csv")
hsnu_morning <- hsnu %>% filter(hour %in% c(6,7,8))
write.csv(hsnu_morning ,"C:/Users/wendy-MM/Desktop/hsnu_morning .csv")



