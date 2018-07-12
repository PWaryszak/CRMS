#Data +libraries + Apriori Model ========
library("ggplot2")
library("gridExtra")
library("grid")

#Freshwater_Data==========
Freshwater_Data <- read.csv("Freshwater_Data4SEM.csv")
names(Freshwater_Data)

f1 = ggplot(Freshwater_Data,aes(x=floodedpercent,y=Native_Richness))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Freshwater_Data")

f2 = ggplot(Freshwater_Data,aes(x=Introduced_Cover,y=Native_Cover))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Freshwater_Data")

f3 = ggplot(Freshwater_Data,aes(x=meanwaterdepthcm,y=Native_Richness))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Freshwater_Data")

f4 = ggplot(Freshwater_Data,aes(x=meanwaterdepthcm,y=Native_Cover ))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Freshwater_Data")


grid.arrange(f1,f2,f3,f4, ncol = 2)

#Intermediate_Data============
Intermediate_Data <- read.csv("Intermediate_Data4SEM.csv")
names(Intermediate_Data)

i1 = ggplot(Intermediate_Data,aes(x=floodedpercent,y=Native_Richness))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Intermediate_Data")

i2 = ggplot(Intermediate_Data,aes(x=Mean_SoilSalinity,y=Native_Cover))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Intermediate_Data")

i3 = ggplot(Intermediate_Data,aes(x=Mean_SoilSalinity,y=Introduced_Cover))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Intermediate_Data")

i4 = ggplot(Intermediate_Data,aes(x=Mean_SoilSalinity,y=Native_Richness))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Intermediate_Data")

grid.arrange(i1,i2,i3,i4, ncol = 2)



#Brackish_Data==========
Brackish_Data <- read.csv("Brackish_Data4SEM.csv")
names(Brackish_Data)

b1 = ggplot(Brackish_Data,aes(x=floodedpercent,y=Native_Richness))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Brackish_Data")

b2 = ggplot(Brackish_Data,aes(x=floodedpercent,y=Native_Cover))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Brackish_Data")

b3 = ggplot(Brackish_Data,aes(x=Mean_SoilSalinity,y=Native_Richness))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Brackish_Data")

b4 = ggplot(Brackish_Data,aes(x=Mean_SoilSalinity,y=V1))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Brackish_Data")+ ylab("Native Composition")


grid.arrange(b1,b2,b3,b4, ncol = 2)

#Saline_Data==========
Saline_Data <- read.csv("Saline_Data4SEM.csv")
names(Saline_Data)

s1 = ggplot(Saline_Data,aes(x=floodedpercent,y=Native_Richness))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Saline_Data")

s2 = ggplot(Saline_Data,aes(x=meanwaterdepthcm,y=Native_Cover))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Saline_Data")

s3 = ggplot(Saline_Data,aes(x=Mean_SoilSalinity,y=Native_Richness))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Saline_Data")

s4 = ggplot(Saline_Data,aes(x=floodedpercent,y=Native_Cover ))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Saline_Data")


grid.arrange(s1,s2,s3,s4, ncol = 2)

#Is Depth meanwaterdepthcm to floodedpercent=======
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")
#Run Correlation test:
cor.test(VegAllEnvData$meanwaterdepthcm, VegAllEnvData$floodedpercent) #0.9258418 !
#Plot the correlation:
ggplot(VegAllEnvData,aes(x=floodedpercent,y=meanwaterdepthcm ))+
  geom_point()+ geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("R2=0.92, P < 0.001") +theme_minimal()
