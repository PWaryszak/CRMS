#LOAD VEG DATA============
library(tidyverse)

veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#133612 obs. of  24 variables:
levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline" 
length(levels(veg$SpecCode))#430, see comments in "CRMSDataCleaning.R" file to learn more
veg$Community<-factor(veg$Community, levels = c("Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels

#Filter out plots with Phrag only:
Phragmites<-filter(veg, SpecCode == "Phraaust") #subsetting only data we are most interested in code 1&2 = tavy
str(Phragmites)#799 obs. of  24 variables:

PhragPresence<-as.data.frame(table(Phragmites$StationFront))
PhragPresence#Phrag occurences by Station (site)
PhragStations<-PhragPresence[PhragPresence$Freq > 0 , ]#selecting Station with more than 0 cover of Phrag
PhragStationsDescending <- PhragStations[order(- PhragStations$Freq),] #sorting in descending mannert to have a look 
PhragStationsDescending#what stations are the most abundant in Phragmites?

colnames(PhragStations)[1] <- "StationFront" #renaming Var1 column to what it is = StationFront.
length(levels(droplevels(PhragStations$StationFront)))#52 Stations recorded Phrag in their plots that have consiste salinity data
levels(droplevels(Phragmites$StationFront))

#Phrag to Salinity Correlation==========
#Let us subset the PhragStations from env data:
env<-read.csv("CRMS_Soil.csv")
envPhrag<- env[ which ( env$StationFront %in% PhragStations$StationFront) , ]
str(envPhrag)#23458 obs. of  12 variables:
table(envPhrag$year)
#2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 
#389 1600 2499 2876 2936 2590 3126 2645 1656 1249 1615  277
envPhrag2007to2016<-envPhrag[! envPhrag$year=="2006"& ! envPhrag$year=="2017",] #removing low n years
table(envPhrag2007to2016$year)
#2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 
#1600 2499 2876 2936 2590 3126 2645 1656 1249 1615 


#Compute mean salinity per StationFront. There were ~10 StationsID-s per StationFront
envPhragSalinity<-summarise(group_by(envPhrag2007to2016,StationFront,year), MeanSalinity=mean(SoilPorewaterSalinity.ppt),
                SDSalinity=sd(SoilPorewaterSalinity.ppt),CVSalinity= SDSalinity/MeanSalinity)
#View(envPhragSalinity) # data frame [505 x 5]
envPhragSalinity$StationFront.Year<-interaction(envPhragSalinity$StationFront,envPhragSalinity$year)

#Merge Phrag data with salinity data:
#Produce new unique column "StationFront.Year" to be merged by with Phrag:
Phragmites$StationFront.Year<-interaction(Phragmites$StationFront, Phragmites$year)
Phrag<- Phragmites[ , c("StationFront.Year","StationID", "Cover","SpecCode")]

PhragCoverSalinity <- dplyr::full_join(Phrag,envPhragSalinity, by = "StationFront.Year" )
dim(PhragCoverSalinity)# 1019  obs.
head(PhragCoverSalinity, n=1)
#StationFront.Year  StationID   Cover SpecCode StationFront year MeanSalinity SDSalinity CVSalinity
#1 CRMS0662.2010  CRMS0662_V50  0.25  Phraaust     CRMS0662 2010     4.405882   1.129419  0.2563435


#GGPLOT with not outliers:
p <- ggplot(PhragCoverSalinity, aes( MeanSalinity,Cover))
p2<-p + geom_point(aes(color=MeanSalinity)) +theme_classic()
p2 +ggtitle("Mean Salinity and Phragmites Cover over 2007-2016 period")

#Two extreme outliers were identified in p2 plot + NAs = to be removed:
PhragCoverSalinity2<-PhragCoverSalinity[- which.max(PhragCoverSalinity$MeanSalinity),]
PhragCoverSalinity3<-PhragCoverSalinity2[- which.max(PhragCoverSalinity2$MeanSalinity),]
PhragCoverSalinity4<-PhragCoverSalinity3[ - is.na(PhragCoverSalinity2$MeanSalinity),]
PhragCoverSalinity4$year<-as.factor(PhragCoverSalinity4$year)
dim(PhragCoverSalinity4)#1016    9
PhragCoverSalinity5<-PhragCoverSalinity4[!is.na(PhragCoverSalinity4$StationFront),]
dim(PhragCoverSalinity5) #1000    9

#GGPLOT with no outliers:
pp <- ggplot(PhragCoverSalinity5, aes( MeanSalinity,Cover))
pp2<-pp + geom_point(aes(color=MeanSalinity)) +theme_classic()
pp2 +ggtitle("Mean Salinity and Phragmites Cover over 2007-2016 period")


#Phrag Cover Change to Salinity Change Correlation=============
dim(PhragCoverSalinity5) #1000    9 - see above how we got here.
#spread Cover over years 
CoverChange<- PhragCoverSalinity5[ , c("StationID", "Cover", "StationFront", "year")]
CoverChangeWide <- spread(CoverChange, key = year, value = Cover)
dim(CoverChangeWide)#191  12
CoverChangeWide$CoverChange1<- - CoverChangeWide$`2007` + CoverChangeWide$`2008`
CoverChangeWide$CoverChange2<- - CoverChangeWide$`2008` + CoverChangeWide$`2009`
CoverChangeWide$CoverChange3<- - CoverChangeWide$`2009` + CoverChangeWide$`2010`
CoverChangeWide$CoverChange4<- - CoverChangeWide$`2010` + CoverChangeWide$`2011`
CoverChangeWide$CoverChange5<- - CoverChangeWide$`2011` + CoverChangeWide$`2012`
CoverChangeWide$CoverChange6<- - CoverChangeWide$`2012` + CoverChangeWide$`2013`
CoverChangeWide$CoverChange7<- - CoverChangeWide$`2013` + CoverChangeWide$`2014`
CoverChangeWide$CoverChange8<- - CoverChangeWide$`2014` + CoverChangeWide$`2015`
CoverChangeWide$CoverChange9<- - CoverChangeWide$`2015` + CoverChangeWide$`2016`

#Spread Salinity over years:
SalinityChange<- PhragCoverSalinity5[ , c(  "StationID","MeanSalinity", "StationFront" ,"year")]
#SalinityChange$rows<- 1:nrow(SalinityChange)
SalinityChangeWide <- spread(SalinityChange, key = year, value = MeanSalinity)
dim(SalinityChangeWide)#191  12
SalinityChangeWide
SalinityChangeWide$SalinityChange1<- - SalinityChangeWide$`2007`+ SalinityChangeWide$`2008`
SalinityChangeWide$SalinityChange2<- - SalinityChangeWide$`2008`+ SalinityChangeWide$`2009`
SalinityChangeWide$SalinityChange3<- - SalinityChangeWide$`2009`+ SalinityChangeWide$`2010`
SalinityChangeWide$SalinityChange4<- - SalinityChangeWide$`2010`+ SalinityChangeWide$`2011`
SalinityChangeWide$SalinityChange5<- - SalinityChangeWide$`2011`+ SalinityChangeWide$`2012`
SalinityChangeWide$SalinityChange6<- - SalinityChangeWide$`2012`+ SalinityChangeWide$`2013`
SalinityChangeWide$SalinityChange7<- - SalinityChangeWide$`2013`+ SalinityChangeWide$`2014`
SalinityChangeWide$SalinityChange8<- - SalinityChangeWide$`2014`+ SalinityChangeWide$`2015`
SalinityChangeWide$SalinityChange9<- - SalinityChangeWide$`2015`+ SalinityChangeWide$`2016`

PhragCoverSalinityChange<- cbind (CoverChangeWide, SalinityChangeWide)
head(PhragCoverSalinityChange)

#GGPLOT of change of cover vs salinity:
Change1 <- ggplot(data = PhragCoverSalinityChange, aes( CoverChange1,SalinityChange1))+ geom_point()+geom_smooth() +theme_classic()+ggtitle("Over 2007-2008 period")+geom_hline(yintercept=0, linetype = 2)
Change2 <- ggplot(data = PhragCoverSalinityChange, aes( CoverChange2,SalinityChange2))+ geom_point()+geom_smooth() +theme_classic()+ggtitle("Over 2008-2009 period")+geom_hline(yintercept=0, linetype = 2)
Change3 <- ggplot(data = PhragCoverSalinityChange, aes( CoverChange3,SalinityChange3))+ geom_point()+geom_smooth() +theme_classic()+ggtitle("Over 2009-2010 period")+geom_hline(yintercept=0, linetype = 2)
Change4 <- ggplot(data = PhragCoverSalinityChange, aes( CoverChange4,SalinityChange4))+ geom_point()+geom_smooth() +theme_classic()+ggtitle("Over 2010-2011 period")+geom_hline(yintercept=0, linetype = 2)
Change5 <- ggplot(data = PhragCoverSalinityChange, aes( CoverChange5,SalinityChange5))+ geom_point()+geom_smooth() +theme_classic()+ggtitle("Over 2011-2012 period")+geom_hline(yintercept=0, linetype = 2)
Change6 <- ggplot(data = PhragCoverSalinityChange, aes( CoverChange6,SalinityChange6))+ geom_point()+geom_smooth() +theme_classic()+ggtitle("Over 2012-2013 period")+geom_hline(yintercept=0, linetype = 2)
Change7 <- ggplot(data = PhragCoverSalinityChange, aes( CoverChange7,SalinityChange7))+ geom_point()+geom_smooth() +theme_classic()+ggtitle("Over 2013-2014 period")+geom_hline(yintercept=0, linetype = 2)
Change8 <- ggplot(data = PhragCoverSalinityChange, aes( CoverChange8,SalinityChange8))+ geom_point()+geom_smooth() +theme_classic()+ggtitle("Over 2014-2015 period")+geom_hline(yintercept=0, linetype = 2)
Change9 <- ggplot(data = PhragCoverSalinityChange, aes( CoverChange9,SalinityChange9))+ geom_point()+geom_smooth() +theme_classic()+ggtitle("Over 2015-2016 period")+geom_hline(yintercept=0, linetype = 2)

require(gridExtra)
grid.arrange(Change1,Change2,Change3, ncol=3)
grid.arrange(Change4,Change5,Change6, ncol=3)
grid.arrange(Change7,Change8,Change9, ncol=3)
