library(vegan)
library(tidyverse)
library(Metrics) #provied ae functin for absolute difference (crucial to compute Water Table change)
VegEnvData <- read.csv("VegEnvDataNew2018.csv")#Data contains cover values for all plant species
dim(VegEnvData)#2410  442
unique(VegEnvData$year)#2007 2008 2009 2010 2011 2012 2013 2014 2015 2016
table(VegEnvData$year)#2007 and 2008 small on data
levels(VegEnvData$Community)#"Brackish"     "Freshwater"   "Intermediate" "Saline"

#Phrag is most present in Intermiediate communities, with widest salinity range:
PhragSalt<- group_by(VegEnvData, Community, na.rm = T)%>%
  summarize(SalinityMin = min(MeanSalinity,na.rm = T), SalinityMax = max(MeanSalinity,na.rm = TRUE))
PhragSalt #HUGE RANGE!!!
#Community    na.rm SalinityMin SalinityMax
# Brackish     T          0.243        30.5 
# Freshwater   T          0.0563        4.21
# Intermediate T          0.171        30.7 
# Saline       T          1.01         26.6 


#SALINITY ~ WATER correlation:======
salt <- VegEnvData$MeanSalinity
table <- VegEnvData$meanwaterdepthcm
qplot(salt,table) + geom_point()+
  geom_line(stat="smooth",method = "lm",size=1.8, color = "red")
  
cor.test(abs(table), abs(salt), method = "pearson")#-0.21 as Table up Salt down.
#t = -10.2, df = 2253, p-value < 2.2e-16
summary(lm(abs(table)~ abs(salt), data = VegEnvData))#-0.21 as Table up Salt down.
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 10.45102    0.21974   47.56   <2e-16 ***
# abs(salt)   -0.27114    0.02658  -10.20   <2e-16 ***


#Change from 2010 to 2016========
#see where Phragmites falls in terms of change in water table and salinity:
v1<- select(VegEnvData,StationFront.year, year, meanwaterdepthcm, MeanSalinity, Phraaust)%>%
  mutate(Phragmites = ifelse(Phraaust > 0, "Present","Absent"))
View(v1)

change1<-ggplot(v1, aes(meanwaterdepthcm, MeanSalinity)) + geom_point(aes(color = Phragmites, shape = Phragmites))
change2<-change1+theme_classic()
change2

v2<- select(VegEnvData, StationFront,year, MeanSalinity, Phraaust)%>%
  mutate(Phragmites = ifelse(Phraaust > 0, "Present","Absent"))%>%
  na.omit()%>%
  filter(year == "2016")%>%
  spread(year, MeanSalinity)

v3<- select(VegEnvData, StationFront, year, MeanSalinity)%>%
  filter(year=="2010")%>%
  spread(year, MeanSalinity)
  
  
View(v3)

v4 <- left_join(v2, v3, by = "StationFront")
v4$SalinityChange <- v4$`2016`-v4$`2010`
View(v4)


#MEAN WATER TABLE:
v5<- select(VegEnvData, StationFront,year, meanwaterdepthcm)%>%
  filter(year == "2016")%>%
  spread(year, meanwaterdepthcm)
View(v5)

v6<- select(VegEnvData, StationFront, year,meanwaterdepthcm)%>%
  filter(year=="2010")%>%
  spread(year,meanwaterdepthcm)


View(v6)

v7 <- left_join(v5, v6, by = "StationFront")
v7$WaterTableChange <- v7$`2016`-v7$`2010`
View(v7)
v7a <- v7 [, c(1,4)]
v8 <- inner_join(v4,v7a, by = "StationFront")
View(v8)

c <- select(VegEnvData, Community, StationFront, year)%>%
  filter(year == "2016")%>%
  select(StationFront, Community)

v8c <- left_join(v8,c, by = "StationFront")
View(v8)
#PLOT The change and Phrag:
change1<-ggplot(v8c, aes( WaterTableChange,SalinityChange))+
  geom_point(aes(color = Phragmites, shape=Community, size =SalinityChange), alpha = 0.5) +
  theme_classic()+
  ggtitle("Rate of Change (2016 - 2010) and Presence of Phragmites")+
  labs(x = "Water Table Change (WT2016 - WT2010)",y="Salinity Change (S2016- S2010)",colour="Phragmites")+
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) 

change1
ggsave('ChangeLandscape_Plot2.jpeg', dpi=300, height=5, width=9)



#Change from 2009 to 2016========
#see where Phragmites falls in terms of change in water table and salinity:
VegEnvData <- read.csv("VegEnvDataNew2018.csv")#Data contains cover values for all plant species

v1<- select(VegEnvData,StationFront.year, year, meanwaterdepthcm, MeanSalinity, Phraaust)%>%
  mutate(Phragmites = ifelse(Phraaust > 0, "Present","Absent"))
#View All Data
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


#MEAN WATER TABLE, the same logic as above:
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
table(v8c$WaterROC)#Values above 500% seem ridiculous, Remove
v9 <- v8c [ !v8c$WaterROC > 1000 & !v8c$WaterROC< -1000, ]
dim(v9) / dim(v8c) *100 # 95%  rows kept after outliers rows remvoved.


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
ggplot(v9, aes( WaterROC, SalinityROC))+
  geom_point(aes(color = Phragmites, shape=Community), alpha = 0.5) +
  theme_classic()+
  ggtitle("Rate of Change (2016 - 2009) and Presence of Phragmites")+
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) 

hist(v9$WaterROC)
hist(v9$SalinityROC)

#BREAKING DOWN 'Rate Of Change - ROC': https://www.investopedia.com/terms/r/rateofchange.asp
#Rate of change is used to mathematically describe the percentage change in value over a defined period of time, and it represents the momentum of a variable. The calculation for ROC is simple in that it takes the current value of a stock or index and divides it by the value from an earlier period. Subtract one and multiply the resulting number by 100 to give it a percentage representation.
#ROC = {(current value / previous value) - 1} x 100.

#STATS:
summary(glm(ifelse(v9$Phragmites=="Present",1,0) ~ abs(WaterROC)+abs(SalinityROC)+v9$'2016', data = v9, family = "binomial"))
###############     Estimate Std. Error z value Pr(>|z|)  
#Intercept        -0.6432947  0.6527732  -0.985   0.3244  
#abs(WaterROC)     0.0009264  0.0010177   0.910   0.3626  
#abs(SalinityROC) -0.0114678  0.0093823  -1.222   0.2216  
#v9$"2016"        -0.3487879  0.1383936  -2.520   0.0117 * = MeanSalinity in 2016.

v10 <- na.omit(v9)
unique(v10$StationFront)#314 stations kept.
sum(is.na(v10$StationFront))#64 NA-s
314-64# 250
sum(is.na(v9$WaterTableChange))#64 NA-s
sum(is.na(v9$SalinityROC))#76 NA-s
sum(is.na(v9$'2016'))#76 NA-s
314-76 #= 238 data-full site stations.
