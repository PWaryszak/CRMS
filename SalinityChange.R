#Load Data and libraries first===========
#setwd("~/Pawel/CRMS")#Tulane Lab Comp
library(ggplot2)
library(dplyr)
library(tidyr)

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

#Having Salinity that is higher than 50 PPT is super unusual. 
#20 PPT is the maximum recorded in the field in LA coast. Typos? possibly.
env[which.max(env$SoilPorewaterSalinity.ppt),]#comment says 0.6 PPT, not sure why???

#Remove anything with PPT > 50 (arbitrary choice of PPT threshold)
Above50PPT<- subset( env, env$SoilPorewaterSalinity.ppt > 50 )
dim(Above50PPT)#18 12 # there is 18 extraordinary values that need to be removed.
table(Above50PPT$SoilPorewaterSalinity.ppt)#Number of PPT values above normal max (Outliers/Typos)
#54.7   57   71  867 1307 1514 1564 1603 1819 1976 2280 2324 2363 2440 2479 2574 2639 3067 
####1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1    1 
env2<- subset (env, env$SoilPorewaterSalinity.ppt < 50 )
dim(env)#194840 12 that is the original dataset of salinity
dim(env2)#194822  12 orig - 18 records with PPT >50

#MEAN +SD+ CV of Salinity per year per station(site)============
#Salinity was measured in one location per Stations (site), so each plot withing station (Site)
#should be the same within one site in the same year:

Salinity<-summarise(group_by(env2,StationFront,year), MeanSalinity=mean(SoilPorewaterSalinity.ppt),
                            SDSalinity=sd(SoilPorewaterSalinity.ppt),CVSalinity= SDSalinity/MeanSalinity)
str(Salinity)#4227 obs. of  5 variables:

#CV=SD/Mean========
# Measure of variability in salinity in each plot over time.

Salinity<-summarise(group_by(env,StationFront,year), MeanSalinity=mean(SoilPorewaterSalinity.ppt),
                    SDSalinity=sd(SoilPorewaterSalinity.ppt),CVSalinity= SDSalinity/MeanSalinity)
str(Salinity)#4227 obs. of  5 variables:
head(Salinity)

# a Cut-off  threshold < 5 (arbitraty threshold to remove stations with too little measuerments over 10 years)
CutOff<-summarise(Salinity, Cut = length(StationFront)) %>% filter(Cut > 5)
range(CutOff$Cut)#6 13
dim(CutOff)## A tibble: 388 x 2
Salinity2<- Salinity[ which(Salinity$StationFront %in%  CutOff$StationFront) , ]
head(Salinity2)


#Match Salinity with Veg Data =====
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#133612 obs. of  24 variables
levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline"
veg$Community<-factor(veg$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels

#Let us define the community-type in each StationFront based on  n of Comms:
#As community type switches based on veg change over time
#For example If 9 years was categorized Freshwater and 1 year was intermiediate = > Freshwater
StationComm<- group_by(veg,StationFront,Community ) %>% count(Count=StationFront)
StationComm#It gives us count of communities per StationFront (740)
SCwide<- spread(StationComm, key = Community, value = n, fill = 0)#make it wide
SCwide$WhichMax<-colnames(SCwide)[apply(SCwide,1,which.max)]#while wide we can see which Comm is predominant
SCwide# A tibble: 320 x 7
StationCommDefined<-SCwide[, c(1,7)]
StationCommDefined
colnames(StationCommDefined)[2] <- "Community" #Renaming WhichMAx back to Community

#Merging Communities with Salinity Data by StationFront
SalinityCommunity<-left_join(Salinity2, StationCommDefined, by = "StationFront")
SalinityCommunity<-as.data.frame(SalinityCommunity)
str(SalinityCommunity)#4204 obs. of  6 variables:

#Mean_CV(coef of variation = sd/mean) per year per Community:####
cv<- group_by(SalinityCommunity, Community,year)
cvSum<-summarise(cv, Mean_CV = mean(CVSalinity), SD_CV = sd(CVSalinity))
cvSum$year<-as.factor(cvSum$year)
cvSum2<-cvSum[! cvSum$year == 2001 & !cvSum$year==2006 & !cvSum$year==2017, ] #years we do not care about, 2017 incomplete
cvSum2$Community<-factor(cvSum2$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels
cvSum3<- cvSum2[! is.na(cvSum2$Community),]
cvSum3
#GGPLOT cvSum:
cvPlot<-ggplot(cvSum3, aes(x = year, y = Mean_CV, group=Community, shape=Community))  
cvPlot1 <- cvPlot + geom_point(size=4) +xlab("Year")
cvPlot1
cvPlot2<-cvPlot1 +geom_line() + theme_classic() +ggtitle("SD/Mean Salinity (CV) in 4 Communities\n on Lousiana Coast (2007-2016)")
cvPlot2
#My own theme for ggplotting (run first):
theme_mine<-theme(axis.text.y=element_text(size=22),
                 axis.title.y=element_text(size=24),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=22),
                 panel.grid.minor.x = element_blank(),
                 legend.position = "right",
                 strip.text = element_text(size= 22),
                 plot.title = element_text(lineheight=1.1, face="bold", size = 25,hjust = 0.5),
                 legend.text = element_text(size = 18),
                 legend.title = element_text(size = 18))
cvPlot3<- cvPlot2+theme_mine
cvPlot3


#Mean_SD per year per Community:####
sd<- group_by(SalinityCommunity, Community,year)
sdSum<-summarise(sd, Mean_SD = mean(SDSalinity))
sdSum$year<-as.factor(sdSum$year)
sdSum2<-sdSum[! sdSum$year == 2001 & !sdSum$year==2006 & !sdSum$year==2017, ] #years we do not care about, 2017 incomplete
sdSum2$Community<-factor(sdSum2$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels
sdSum3<- sdSum2[! is.na(sdSum2$Community),]
sdSum3
#GGPLOT sdSum:
#library(ggplot2)
sdPlot<-ggplot(sdSum3, aes(x = year, y = Mean_SD, group=Community, shape=Community))  
sdPlot1 <- sdPlot + geom_point(size=4)+xlab("Year")
sdPlot1
sdPlot2<-sdPlot1 +geom_line() + theme_classic() +ggtitle("Mean Salinity SD in 4 Communities\n on Lousiana Coast (2007-2016)")
sdPlot2
sdPlot3<-sdPlot2+theme_mine
sdPlot3 #hye 2015 so weird?


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
