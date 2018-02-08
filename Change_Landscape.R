library(vegan)
library(tidyverse)
library(Metrics) #provied ae functin for absolute difference (crucial to compute Water Table change)
VegEnvData <- read.csv("VegEnvDataNew2018.csv")#Data contains cover values for all plant species
dim(VegEnvData)#2410  441
unique(VegEnvData$year)#2007 2008 2009 2010 2011 2012 2013 2014 2015 2016
table(VegEnvData$year)#2007 and 2008 small on data

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

