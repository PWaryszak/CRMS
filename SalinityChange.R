#Load Data and libraries:===========
#setwd("~/Pawel/CRMS")#Tulane Lab Comp
library(tidyverse)
library(vegan)

env<-read.csv("CRMS_Soil.csv")
str(env)#7384 obs. of  19 variables: // $ StationID: Factor w/ 388 levels
table(env$year)#sampling effort across years
#2006 2007 2008 2009 2011 2012 2014 2015 
#1869 1764 2546  374    3   18  807    3 
#Measurements were very haphazard. I am able to look at the change between 2008 & 2014:
obs<-as.data.frame(table(env$year))
write.csv(obs, file = "TableOfObsPerYearinEnvData.csv")

#Salinity change computations:============
e2008<-env[env$year == "2008",c("StationFront","SoilSalinity.ppt", "year")]
dim(e2008)# 2546    2
length(levels(droplevels(e2008$StationFront)))#152 instead of 2546!!! many stations were sampled multiple times

e2014 <- env[env$year == "2014", c("StationFront" , "SoilSalinity.ppt", "year")]
dim(e2014)#807   2 - consisten with the table above, 807 obs.
stations2014<- env[ which (env$StationFront  %in%  e2014$StationFront), c("StationFront","SoilSalinity.ppt", "year")]
str(stations2014)#1667 obs. of  3 variables:

s2008<-stations2014[stations2014$year=="2008",] #731   3 - station that were in both 2014 and 2008 recorded

eFrom2008to2014<-dplyr::inner_join(e2014,s2008, by = "StationFront")
str(eFrom2008to2014)#10608  obs. of  4 variables:#many records are duplicated:

#Compute transtion in salinity:
eFrom2008to2014$Transition <- eFrom2008to2014$SoilSalinity.ppt.y - eFrom2008to2014$SoilSalinity.ppt.x #y=2008,x =2014
range(eFrom2008to2014$Transition)#-0.4  1.7
dim(eFrom2008to2014[eFrom2008to2014$Transition > 0,] )# 6 times was recorded an increase
dim(eFrom2008to2014[eFrom2008to2014$Transition < 0,] )# 6 times was recorded an increase
plot(table(eFrom2008to2014$Transition), main = "Salinity Change from 2008 to 2014")
table(eFrom2008to2014$Transition)


#Computing Community-switches:============
setwd("~/Desktop/CRMS/CRMS")
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#293757 obs. of  24 variables:

t <- count_(veg, c("year", "StationID","Community" ))
t #dificult to compute number of switches and direction of switches. It needs thinking.

#I will look at one random plot = CRMS0002_V54:
#Subset only these 2007 station from entire data set to have a consistent set of plot across years:
CRMS0002_V54  <- filter(t2, StationID == "CRMS0002_V54")
CRMS0002_V54 
1   2007 CRMS0002_V54     Brackish     5
2   2008 CRMS0002_V54     Brackish     4
3   2009 CRMS0002_V54     Brackish     5
4   2010 CRMS0002_V54     Brackish     7
5   2011 CRMS0002_V54     Brackish     7
6   2012 CRMS0002_V54     Brackish     6
7   2013 CRMS0002_V54     Brackish     7
8   2014 CRMS0002_V54     Brackish     7
9   2015 CRMS0002_V54     Brackish     8
10  2016 CRMS0002_V54 Intermediate     8

#BAR PLOT of SAlinity & Communities in 2008============
#setwd("~/Desktop/CRMS/CRMS") #set working directory
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#293757 obs. of  24 variables:

NoSwamp<-veg[veg$Community != "Swamp",] #Swamp contains no Phragmites! Remove swamp.
NoSwamp$Cover <- ifelse(NoSwamp$Cover ==0,0,1)
v<-NoSwamp[,c("StationID","StationFront","Community","SpecCode","Cover","CoverTotal","year")]
v.wide<-spread(v,key = SpecCode, value = Cover, fill = 0)#species indices can be computed in a wide format only= each species has its own column.

vegData<-v.wide[ v.wide$year=="2008" , c("StationID", "StationFront", "Community" )]
envData<-env[env$year=="2008", c("StationID","StationFront","StationBack","SoilSalinity.ppt" )]
data<-dplyr::inner_join(vegData,envData, by = "StationFront" )
str(data)#17871 obs. of  6 var.
#Re-arrange levels order acc to salinity levels.
data$Community <- factor(data$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))


library(Rmisc)
See.SoilSalinity.ppt <- summarySE(data, groupvars = "Community",
                                  measurevar = "SoilSalinity.ppt")
See.SoilSalinity.ppt$year<-"Year2008"
See.SoilSalinity.ppt
#GGPLOT of 2008 salinity:
theme_pw3<-theme(axis.text.y=element_text(size=22),
                 axis.title.y=element_text(size=24),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=22),
                 panel.grid.minor.x = element_blank(),
                 legend.position = "none",
                 strip.text = element_text(size= 22),
                 plot.title = element_text(lineheight=1.1, face="bold", size = 25,hjust = 0.5),
                 legend.text = element_text(size = 18),
                 legend.title = element_text(size = 18))



pd<-position_dodge(0.9)
f<-ggplot(See.SoilSalinity.ppt, aes(x=Community, y=SoilSalinity.ppt, fill = Community , width=.75))
f1<-f + geom_bar(position=pd, stat="identity")+ geom_errorbar(aes(ymin=SoilSalinity.ppt-se, ymax=SoilSalinity.ppt+se),width=.4, position=pd)
f1
f2<- f1 +xlab("Community Type") + ylab("Soil Salinity (ppt)") + theme_classic()+theme_pw3 
Salinity <- f2 + ggtitle("Soil Salinity in Five Community Types (2008)")
Salinity
