#Load Data and libraries first===========
#setwd("~/Pawel/CRMS")#Tulane Lab Comp
library(tidyverse)

env<-read.csv("CRMS_Soil.csv")
str(env)#194840 obs. of  12 variables: 
#This is a new env data with more balanced soil salinity Measurements
#DESCRIPTION of variables we most interested:
#env$StationID   =  Factor w/ 5213 levels (n of plots)
#env$StationFront = Factor w/ 400 levels ( n of sites)
#env$year = 2
table(env$year)#good sampling effort across years. Cut out year 2001 & 2006 to
#fit veg data.
range(env$SoilPorewaterSalinity.ppt)#0 3067


#MEAN +SD+ CV of Salinity per year per station(site)============
#Salinity was measured in one location per Stations (site):
Salinity<-summarise(group_by(env,StationFront,year), MeanSalinity=mean(SoilPorewaterSalinity.ppt),
                            SDSalinity=sd(SoilPorewaterSalinity.ppt),CVSalinity= SDSalinity/MeanSalinity)
str(Salinity)#4227 obs. of  5 variables:
head(Salinity)

#GGPLOT of Salinity CHANGE across 10 years======== 
levels(Salinity$StationFront)
salt<-as.data.frame(Salinity)
#subset 20 stations:
s20<- salt[  salt$StationFront =="CRMS0002"|
             salt$StationFront =="CRMS0086"|
             salt$StationFront =="CRMS0065"|
             salt$StationFront =="CRMS0061"|
             salt$StationFront =="CRMS0035"|
             salt$StationFront =="CRMS0038"|
             salt$StationFront =="CRMS0039"|
             salt$StationFront =="CRMS0089"|
             salt$StationFront =="CRMS0004"|
               salt$StationFront =="CRMS0003"|
               salt$StationFront =="CRMS0290"|
               salt$StationFront =="CRMS0260"|
               salt$StationFront =="CRMS0097"|
               salt$StationFront =="CRMS0117"|
               salt$StationFront =="CRMS0171"|
               salt$StationFront =="CRMS0575"|
               salt$StationFront =="CRMS0567"|
               salt$StationFront =="CRMS0574"|
               salt$StationFront =="CRMS5167"|
               salt$StationFront =="CRMS6302",]

head(s20)
s20$year<-as.factor(s20$year)

#GGPLOT change in Swamp Salinity in 2007-2016
SaltPlot<-ggplot(s20, aes(x = year, y = MeanSalinity, group=StationFront, color=StationFront))  
SaltPlot1 <- SaltPlot + geom_point() + geom_line()
SaltPlot2<- SaltPlot1 + theme_classic() +ggtitle("Salinity Change on Lousiana Coast (2007-2016)")
SaltPlot2

#There are some problematic plots too:
CRMS6302<- salt[salt$StationFront =="CRMS6302",]
CRMS6302
CRMS6302b<- env[env$StationFront =="CRMS6302",]
CRMS6302b

#CV=SD/Mean over time over communities========
# Measureof variability in salinity in each plot over time.
# a cut-off  plots < 5
Salinity<-summarise(group_by(env,StationFront,year), MeanSalinity=mean(SoilPorewaterSalinity.ppt),
                    SDSalinity=sd(SoilPorewaterSalinity.ppt),CVSalinity= SDSalinity/MeanSalinity)
str(Salinity)#4227 obs. of  5 variables:
head(Salinity)

CutOff<-summarise(Salinity, Cut = length(StationFront)) %>% filter(Cut > 5)
range(CutOff$Cut)#6 13
CutOff# A tibble: 388 × 2 = only 12 sites were underserveyed.

Salinity2<- Salinity[ which(Salinity$StationFront %in%  CutOff$StationFront) , ]

#Match Community types with Salinity=====
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#133612 obs. of  24 variables
levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline"
veg$Community<-factor(veg$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels

#Let us define the community-type in each StationFront based on  n of Comms:
StationComm<- group_by(veg,StationFront,Community ) %>% count(Count=StationFront)
StationComm#It gives us count of communities per StationFront (862)
SCwide<- spread(StationComm, key = Community, value = n, fill = 0)#make it wide
SCwide$WhichMax<-colnames(SCwide)[apply(SCwide,1,which.max)]#while wide we can see which Comm is predominant
SCwide
StationCommDefined<-SCwide[, c(1,7)]
StationCommDefined
colnames(StationCommDefined)[2] <- "Community" #Renaming WhichMAx back to Community

#Looking at Community Switch in one random plot = CRMS0002_V54:
#Subset only these 2007 station from entire data set to have a consistent set of plot across years:
CRMS0002_V54  <- filter(t2, StationID == "CRMS0002_V54")
CRMS0002_V54 #This plot would be classified as Brakish
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


#Matching Communities with Salinity Data by StationFront:
SalinityCommunity<-left_join(Salinity, StationCommDefined, by = "StationFront")
SalinityCommunity<-as.data.frame(SalinityCommunity)
str(SalinityCommunity)#4227 obs. of  6 variables:

#Compute mean CV(oef of variation = sd/mean) per year per Community:
cv<- group_by(SalinityCommunity, Community,year)
cvSum<-summarise(cv, Mean_CV = mean(CVSalinity), SD_CV = sd(CVSalinity))
cvSum$year<-as.factor(cvSum$year)
cvSum2<-cvSum[! cvSum$year == 2001 & !cvSum==2006, ] #years we do not care about
cvSum2$Community<-factor(cvSum2$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels
cvSum3<- cvSum2[! is.na(cvSum2$Community),]
cvSum3
#GGPLOT cvSum:
cvPlot<-ggplot(cvSum3, aes(x = year, y = Mean_CV, group=Community, shape=Community))  
cvPlot1 <- cvPlot + geom_point(size=4)
cvPlot1
cvPlot1 +geom_line() + theme_classic() +ggtitle("Mean Salinity CV Change in 4 Communities on Lousiana Coast (2007-2016)")



#Salinity change computations (2008 to 2014):============
e2008<-env[env$year == "2008",c("StationFront","SoilSalinity.ppt", "year")]
dim(e2008)# 2546    2
length(levels(droplevels(e2008$StationFront)))#152 instead of 2546!!! many stations were sampled multiple times

e2014 <- env[env$year == "2014", c("StationFront" , "SoilSalinity.ppt", "year")]
dim(e2014)#807   3 - consistent with the table above (obs), 807 obs.
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


#BAR PLOT of Salinity & Communities in 2008============
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
Salinity <- f2 + ggtitle("Soil Salinity in Four Community Types (2008)")
Salinity
