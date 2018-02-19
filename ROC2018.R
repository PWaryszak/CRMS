#HOW "VegEnvDataNew2018.csv" was produced?====
#New data was produced by Emily (efarrer@tulane.edu) in Jan 2018 as shown in 2 following files:
#"HydrologicDataECF.R" and VegECF.R"
#then...
#Pawel (pwaryszak@tulane.edu) JOINED VEG6 with ENVC4 data objects as per below:
#ENVC4 is an object produced in "HydrologicDataECF.R" file off raw CRMS environmental data.
library(tidyverse)
library(vegan)

#WaterData<-as.data.frame(envc4)#Creating data to merge with veg6, or:
WaterData <- read_csv("CRMS_MeanWaterDepth_Salinity_envc4.csv")
dim(WaterData)#2994 obs. of  14 variables:
WaterDataThin <- as.data.frame(select( WaterData, StationFront.year, meanwaterdepthcm, MeanSalinity,floodedpercent))

#Veg6 is an object produced in "VegECF.R":
VegData<-as.data.frame(veg6)#veg6 is produced in above lines or as saved csv file:
VegData<-read.csv("CRMS_Veg2018.csv")
VegData$StationFront.year<-interaction(VegData$StationFront, VegData$year)#We need that for joining
dim(VegData)#3090 obs. of  439 variables:

#Join VegData with WaterData
#by StationFront.year, some levels of "StationFront.year" do not overlap:
VegEnvData<-inner_join(VegData,WaterDataThin , by="StationFront.year" )
dim(VegEnvData)# 2410  442
#write.csv(VegEnvData, file = "VegEnvDataNew2018.csv", row.names = F)

#Water Salinity Change from 2009 to 2016========
VegEnvData <- read.csv("VegEnvDataNew2018.csv")#Data contains cover values for all plant species

v1<- select(VegEnvData,StationFront.year, year, meanwaterdepthcm, MeanSalinity, Phraaust)%>%
  mutate(Phragmites = ifelse(Phraaust > 0, "Present","Absent"))
#View All Data, where Phragmites is present:
ggplot(v1, aes(meanwaterdepthcm, MeanSalinity)) +
  geom_point(aes(color = Phragmites, shape = Phragmites))+
  theme_classic()

#Define Phragmites Presences based on its presence in 2016 (current final year of survey)
v2<- select(VegEnvData, StationFront,year, MeanSalinity, Phraaust)%>%
  mutate(Phragmites = ifelse(Phraaust > 0, "Present","Absent"))%>%
  na.omit()%>%
  filter(year == "2016")%>%
  spread(year, MeanSalinity)

#Select data of 2009 for merging with 2016 data
v3<- select(VegEnvData, StationFront, year, MeanSalinity)%>%
  filter(year=="2009")%>%
  spread(year, MeanSalinity)%>%
  na.omit()

#Mergin 2009 & 2016 data
v4 <- left_join(v2, v3, by = "StationFront")
v4$SalinityChange <- v4$`2016`-v4$`2009`#computing the rate of change in MeanSalinity
v4$SalinityROC <- (v4$`2016`/v4$`2009`-1)*100 #Compute RAte of Change (ROC = {(current value / previous value) - 1} x 100)


#Compute MEAN WATER TABLE, the same logic as above:
v5<- select(VegEnvData, StationFront,year, meanwaterdepthcm)%>%
  filter(year == "2016")%>%
  spread(year, meanwaterdepthcm)

v6<- select(VegEnvData, StationFront, year,meanwaterdepthcm)%>%
  filter(year=="2009")%>%
  spread(year,meanwaterdepthcm)%>%
  na.omit()

v7 <- left_join(v5, v6, by = "StationFront")
v7$WaterTableChange <- v7$`2016`-v7$`2009`#compute change
v7$WaterROC <- (v7$`2016`/v7$`2009`-1)*100 #Compute RAte of Change (ROC = {(current value / previous value) - 1} x 100)
v7a <- v7 [, c(1,4,5)] #Leave only StationFron, WaterROC, and WaterTableChange column
v8 <- inner_join(v4,v7a, by = "StationFront")

#Add Community for extra factor in plotting:
c <- select(VegEnvData, Community, StationFront, year)%>%
  filter(year == "2016")%>%
  select(StationFront, Community)

v8c <- left_join(v8,c, by = "StationFront")
names(v8c)


#PLOT The change and Phrag:
change1<-ggplot(v8c, aes( WaterTableChange,SalinityChange))+
  geom_point(aes(color = Phragmites, shape=Community, size =SalinityChange), alpha = 0.5) +
  theme_classic()+
  ggtitle("Rate of Change (2016 - 2009) and Presence of Phragmites")+
  labs(x = "Water Table Change (WT2016 - WT2009)",y="Salinity Change (S2016- S2009)",colour="Phragmites")+
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) 

change1
#ggsave('ChangeLandscape_Plot2.jpeg', dpi=300, height=5, width=9)

#PLOT ROC:
ggplot(v8c, aes( WaterROC, SalinityROC))+
  geom_point(aes(color = Phragmites, shape=Community)) +
  theme_classic()+
  ggtitle("Rate of Change (2016 - 2009) and Presence of Phragmites")+
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) 

hist(v9$WaterROC)
hist(v9$SalinityROC)

#4 MORE on 'Rate Of Change - ROC', go: https://www.investopedia.com/terms/r/rateofchange.asp
#Rate of change is used to mathematically describe the percentage change in value over a defined period of time, and it represents the momentum of a variable. The calculation for ROC is simple in that it takes the current value of a stock or index and divides it by the value from an earlier period. Subtract one and multiply the resulting number by 100 to give it a percentage representation.
#ROC = {(current value / previous value) - 1} x 100.
range(v8c$WaterROC, na.rm=T)#Values above 1000% seem ridiculous, Remove them:
#Create v9 for STATS that contains no outliers (above and below 1000%)=
v9 <- v8c [ !v8c$WaterROC > 1000 & !v8c$WaterROC< -1000, ]
dim(v9) / dim(v8c) *100 # 95%  of records were kept after removing the outliers.

#STATS:
summary(glm(ifelse(v9$Phragmites=="Present",1,0) ~ abs(WaterROC)+abs(SalinityROC)+v9$'2016', data = v9, family = "binomial"))
###############     Estimate Std. Error z value Pr(>|z|)  
#Intercept        -0.6432947  0.6527732  -0.985   0.3244  
#abs(WaterROC)     0.0009264  0.0010177   0.910   0.3626  
#abs(SalinityROC) -0.0114678  0.0093823  -1.222   0.2216  
#v9$"2016"        -0.3487879  0.1383936  -2.520   0.0117 * = MeanSalinity in 2016.


#n of NA-s in v9 =========
sum(is.na(v9$StationFront))#64 NA-s
v10 <- na.omit(v9)
unique(v10$StationFront)#314 stations kept.
314-64 # = 250
sum(is.na(v9$WaterTableChange))#64 NA-s
sum(is.na(v9$SalinityROC))#76 NA-s
sum(is.na(v9$'2016'))#76 NA-s
314-76 # = 238 data-full site stations.

#Min & Max in salinity per community:=======
VegEnvData <- read.csv("VegEnvDataNew2018.csv")#Data contains cover values for all plant species
#Phrag is most present in Intermiediate communities, with the widest salinity range:
PhragSalt<- group_by(VegEnvData, Community, na.rm = T)%>%
  summarize(SalinityMin = min(MeanSalinity,na.rm = T), SalinityMax = max(MeanSalinity,na.rm = TRUE))
PhragSalt #HUGE RANGE!!!
#Community    na.rm SalinityMin SalinityMax
# Brackish     T          0.243        30.5 
# Freshwater   T          0.0563        4.21
# Intermediate T          0.171        30.7 
# Saline       T          1.01         26.6 


#Min & Max in water table level per community:=======
VegEnvData <- read.csv("VegEnvDataNew2018.csv")#Data contains cover values for all plant species
#Phrag is most present in Intermiediate communities, with the widest salinity range:
PhragTable<- group_by(VegEnvData, Community, na.rm = T)%>%
  summarize(TableMin = min(meanwaterdepthcm,na.rm = T), TableMax = max(meanwaterdepthcm,na.rm = TRUE))
PhragTable #HUGE RANGE!!!
Community    na.rm TableMin TableMax
#1 Brackish     T        -28.6     29.1
#2 Freshwater   T        -32.9     55.9
#3 Intermediate T        -37.2     35.2
#4 Saline       T        -32.8     21.6