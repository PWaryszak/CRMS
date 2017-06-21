#Load Data and libraries first===========
#setwd("~/Pawel/CRMS")#Tulane Lab Comp
library(tidyverse)

env<-read.csv("CRMS_Soil.csv")
str(env)#7384 obs. of  19 variables: // $ StationID: Factor w/ 388 levels
table(env$year)#sampling effort across years
#2006 2007 2008 2009 2011 2012 2014 2015 
#1869 1764 2546  374    3   18  807    3 
#Measurements were very haphazard. I am able to look at the change between 2008 & 2014:

#Matching Salinity data with Communities  ============
#Salinity was measured in Stations not in each plots seperatly:
Salinity<-summarise(group_by(env,StationFront,year), MeanSalinity=mean(SoilSalinity.ppt),
                            SDSalinity=sd(SoilSalinity.ppt),CVSalinity= SDSalinity/MeanSalinity)
str(Salinity)#439
Salinity

#Match 439  Community types with Salinity :
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#293726 obs. of  24 variables:
levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline"  "Swamp"
veg$Community<-factor(veg$Community, levels = c( "Swamp","Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels

#Let us define the community-type in each StationFront based on year 2006 and n of Comms:
StationComm<- group_by(veg,StationFront,Community ) %>% count(Count=StationFront)
StationComm#It gives us count of communities per StationFront (862)
SCwide<- spread(StationComm, key = Community, value = n, fill = 0)#make it wide
SCwide$WhichMax<-colnames(SCwide)[apply(SCwide,1,which.max)]#while wide we can see which Comm is predominant
SCwide
StationCommDefined<-SCwide[, c(1,8)]
colnames(StationCommDefined)[2] <- "Community" #Renaming WhichMAx back to Community
View(StationCommDefined)

#Matching Comm with Salinity Data
SalinityCommunity<-left_join(Salinity, StationCommDefined, by = "StationFront")
str(SalinityCommunity)
head(SalinityCommunity)

#plotting change in salinity across years in 4 communities:
#Freshwater:
Fresh <- SalinityCommunity[SalinityCommunity$Community=="Freshwater", c("MeanSalinity","year","StationFront","Community")]
Fresh
FreshWide<-spread(Fresh, key = year, value = MeanSalinity)
FreshWide# Unfortunatly data was only recored in one year in Freshwater communities
#WE can look at the Swamp and produce a script we can use later when more data available.

#Change over years (Swamp only, 2008-2014)========
env<-read.csv("CRMS_Soil.csv")
Salinity<-summarise(group_by(env,StationFront,year), MeanSalinity=mean(SoilSalinity.ppt),
                    SDSalinity=sd(SoilSalinity.ppt),CVSalinity= SDSalinity/MeanSalinity)
str(Salinity)#439
Salinity

#Match 439 plots with Community type with Salinity :
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#293726 obs. of  24 variables:
levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline"  "Swamp"
veg$Community<-factor(veg$Community, levels = c( "Swamp","Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels

#Let us define the community-type in each StationFront based on year 2006 and n of Comms:
StationComm<- group_by(veg,StationFront,Community ) %>% count(Count=StationFront)
StationComm#It gives us count of communities per StationFront (862)
SCwide<- spread(StationComm, key = Community, value = n, fill = 0)#make it wide
SCwide$WhichMax<-colnames(SCwide)[apply(SCwide,1,which.max)]#compute which community is the most predominant
SCwide#StationFronts (sites) were assigned with Comunity type based on their n of records per StationFront
StationCommDefined<-SCwide[, c(1,8)]
colnames(StationCommDefined)[2] <- "Community" #Renaming WhichMAx back to Community

#Matching Communities with Salinity Data
SalinityCommunity<-left_join(Salinity, StationCommDefined, by = "StationFront")

#Only Swamp have stations recorded in two years that is for 2008 & 2014:
Swamp <- SalinityCommunity[SalinityCommunity$Community=="Swamp", c("MeanSalinity","year","StationFront","Community")]
Swamp
SwampWide<-spread(Swamp, key = year, value = MeanSalinity)
SwampWide
S20 <- SwampWide[1:20, ] #First 21 rows
S20
S20$Salinity2008to2014<- ifelse(S20$`2008` - S20$`2014` < 0 , "increased", "decreased")

#GGPLOT change in Swamp Salinity in 2008-2014
s20dat<-S20[ , c(1,2,5,7,9)]#columns we need for plotting
s20dat
s20data<-gather(s20dat, key = Year, value = MeanSalinity, 3:4)
s20data

SwampPlot<-ggplot(s20data, aes(x = Year, y = MeanSalinity, group=StationFront, color=Salinity2008to2014))  
SwampPlot1 <- SwampPlot + geom_point() + geom_line()
SwampPlot1 + theme_classic() +ggtitle("Salinity Change in Swamp Communities on Lousiana Coast (2008-2014)")

#Other way of Salinity change computations (2008 to 2014):============
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


#Looking at Community Switch in one random plot = CRMS0002_V54:=======
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
