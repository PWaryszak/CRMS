#Filter out plots with Phrag:
library(tidyverse)

veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#293726 obs. of  24 variables:
levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline"  "Swamp"
length(levels(veg$SpecCode))#747, AaaProblem and  Wete are gone as these were not species (bare ground, Wate (open water))see comments in "CRMSDataCleaning" R file
veg$Community<-factor(veg$Community, levels = c( "Swamp","Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels

#Where is Phragmites? Phrag Cover Plot============
Phragmites<-filter(veg, SpecCode == "Phraaust") #subsetting only data we are most interested in code 1&2 = tavy
str(Phragmites)#3031 obs. of  28 variables
View(Phragmites)
PhragPresence<-as.data.frame(table(Phragmites$StationFront))
PhragStations<-PhragPresence[PhragPresence$Freq > 0 , ]#selecting Station with more than 0 cover of Phrag
length(levels(droplevels(PhragStations$StationFront)))#89 Stations recorded Phrag in their plots,
colnames(PhragStations)[1] <- "StationFront"
Abundant <- PhragStations[order(- PhragStations$Freq),] #sorting in descending mannert to have a look 
#what station are most abundant in Phragmites
Abundant
head(PhragStations)
levels(droplevels(Phragmites$StationFront))

#Let us subset the PhragStations from env data:
env<-read.csv("CRMS_Soil.csv")
envPhrag<- env[ which ( env$StationFront %in% PhragStations$StationFront) , ]
str(envPhrag)#1485 obs. of  19 variables:
table(envPhrag$year)
#2006 2007 2008 2009 #only these years were measured and had Phrag in it.
#378  600  417   90
#Compute mean salinity per StationFront. There were ~10 StationsID-s per StationFront
envPhragSalinity<-summarise(group_by(envPhrag,StationFront,year), MeanSalinity=mean(SoilSalinity.ppt),
                SDSalinity=sd(SoilSalinity.ppt),CVSalinity= SDSalinity/MeanSalinity)
envPhragSalinity #tible 85x5 = 85 stations only were measured that had Phrag in them

#For year 06 & 07 these saliniites do not sync.
#let us see what plots were measured consistently across years:
PlotsMeasured<-summarise(group_by(env,StationID,year), number=length(StationID)) #summarize
PlotsMeasured2<-filter(PlotsMeasured, year==2006|year==2007|year==2008| year==2009)#select 
PlotsMeasured2
PlotsMeasured3<-spread(PlotsMeasured2, key = year, value = number,fill=0)
M<-PlotsMeasured3[ , 2:5]
M[M>0] <- 1 #Change values to 0 1 
M$suming<-rowSums(M)
range(M$suming)#1 1 #if none of the rows is bigger than one it means 
#none of the years overlap in measuring salinity

#EXAMPLE BELOW for no overlap in measurments between 2006 & 2007==============
#Change in Phrag cover (06-07) + change in salinity over these years = lack of data to do this:
Phragmites<-filter(veg, SpecCode == "Phraaust") #subsetting only data we are most interested in code 1&2 = tavy
str(Phragmites)#3031 obs. of  28 variables

#YEAR 2006:
Phrag06<- Phragmites[Phragmites$year=="2006", c("StationID", "StationFront", "Cover")]
str(Phrag06)#83 obs. of  3 variables:
length(levels(droplevels(Phrag06$StationID)))#83 plots
length(levels(droplevels(Phrag06$StationFront)))#21 Stations with Phragmites

#Subset Stations measured in 2006:
plots06<-envPhragSalinity[envPhragSalinity$year=="2006",]
plots06#21x5

envPhrag06<-envPhragSalinity[ which(envPhragSalinity$StationFront %in% plots06$StationFront) , c("StationFront","MeanSalinity","CVSalinity", "year") ]
length(levels(droplevels(envPhrag06$StationFront)))#21 Stations with Phragmites
str(envPhrag06)#378 obs. of  3 variables:

#Merge Phrag06 stations with envPhrag06 Stations
Joined06 <- dplyr::left_join(Phrag06,envPhrag06, by = "StationFront" )
Joined06
Joined06<-Joined06[ ! is.na(Joined06$MeanSalinity),] #removing one NA row
Joined06
#YEAR 2007:
Phrag07<- Phragmites[Phragmites$year=="2007", c("StationID", "StationFront", "Cover")]
str(Phrag07)#218 obs. of  3 variables:
length(levels(droplevels(Phrag07$StationID)))#218 plots
length(levels(droplevels(Phrag07$StationFront)))#47 Stations with Phragmites

#Subset Stations measured in 2007:
plots07<-envPhragSalinity[envPhragSalinity$year=="2007",]
plots07#35x5
envPhrag07<-envPhragSalinity[ which(envPhragSalinity$StationFront %in% plots07$StationFront) , c("StationFront","MeanSalinity","CVSalinity", "year") ]
length(levels(droplevels(envPhrag07$StationFront)))#35 Stations with Phragmites

#Merge Phrag07 stations with envPhrag07 Stations
Joined07 <- dplyr::left_join(Phrag07,envPhrag07, by = "StationFront" )
names(Joined07)<-c("StationID","StationFront", "Cover07" ,"MeanSalinity07", "CVSalinity07", "year07")
Joined07<-Joined07[,-2]# We do not need this column for merging qirh Joined06
Joined07
Joined07<-Joined07[ ! is.na(Joined07$MeanSalinity07),] #removing one NA row
Joined07

#Compare Joined06 with Joined07:
Year06_07<- left_join(Joined06,Joined07, by = "StationID")
Year06_07
