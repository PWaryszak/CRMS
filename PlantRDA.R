#Produce veg data for RDA ANALYSIS ==========
# Load packages
library(vegan)
library(tidyverse)

#Create veg matrices for each year:
#Let us subset the plots (stationID) present in year 2007 from all data.
#We also remove all records from Swamp station as they contain no records our target species Phragmites:

veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#133612 obs. of  24 variables:
samples2007<-veg[veg$year ==2007,] 
length(levels(droplevels(samples2007$StationID)))#2558 = number of good plots in 2007 

#Subset only these 2007 station from entire data set to have a
#consistent set of plot across years:

DF2007to2016<- veg[ which (veg$StationID  %in%  samples2007$StationID), c("year", "StationID", "StationFront","StationBack", "SpecCode", "Cover", "Community", "CoverTotal")]
str(DF2007to2016)#118570 obs. of  8 variables:
table(DF2007to2016$year)# a quite balanced sampling!!!!

#Reshape to wide format to compute plant composition indices:
DF2007to2016$Cover <- ifelse(DF2007to2016$Cover ==0,0,1) #turning cover to presence/absence data
v<-DF2007to2016[,c("StationID","StationFront","Community","SpecCode","Cover","CoverTotal","year")] #select most important variables

v.wide<-spread(v,key = SpecCode, value = Cover, fill = 0)#species indices can be computed in a wide format only= each species has its own column.
#write.csv(v.wide, file = "VegConsistentPlotsData.csv", row.names = FALSE)

#RDA Analysis, with For Loop ===========
#TRAIL:
#Subset one plot for trial RDA run for Christina:
CRMS0002_V54<- v.wide[v.wide$StationID=="CRMS0002_V54",]
dim(CRMS0002_V54)#10 by 409
CRMS0002_V54[, 1:5] #years not in asc order
#Put years in order first
trail<-CRMS0002_V54[ order(CRMS0002_V54$year),]
trail[,1:5]
dim(trail)#10 409

#Compute Disimilarity distances:
DisTrail <- vegdist(trail[6:409], distance = "bray", binary = TRUE, upper=TRUE, diag=FALSE)
class(DisTrail)

#Convert Dis to matrix"
m<-as.matrix(DisTrail)
colnames(m) <- c( "Year2007", "Year2008", "Year2009","Year2010","Year2011","Year2012","Year2013","Year2014","Year2015","Year2016")
rownames(m) <- c( "Year2007", "Year2008", "Year2009","Year2010","Year2011","Year2012","Year2013","Year2014","Year2015","Year2016")
m[ row(m) == col(m) ] <- NA #replacing diagonal with NA
m
m2<-as.data.frame(m)
#Averaging by column:
#This will givethe average dissimilarity of each year to all other years:
m3<- colMeans(m2, na.rm = TRUE)
m3

#CREATE FOR LOOP to repeat these actions across all plots:

Output2 <- NULL #we need to set an empty shelf for data called Output

for ( i in unique(v.wide$StationID) ){
  #create a subset data per each plot (StationID)
  subset_plot <- subset(v.wide, StationID== i)
  
  order_plot<- subset_plot[ order(subset_plot$year), ]
  
  subset_veg <- order_plot[ ,6:409] #subsetting veg matrix only
  
  Dissimilarity_Matrix <- vegdist(subset_veg, distance = "bray", binary = TRUE, upper=TRUE, diag=FALSE)
  
  m <- as.matrix(Dissimilarity_Matrix) #turning "dist" into "matrix to allow further computations
  
  m[ row(m) == col(m) ] <- NA #replacing diagonal with NA

  m <- as.matrix(Dissimilarity_Matrix)
  
  m[ row(m) == col(m) ] <- NA #replacing diagonal with NA
  
  m_data_average<- as.data.frame( colMeans(m, na.rm = TRUE)) #Averaging by column:
  
  rownames(m_data_average)[1] <- c( "Year2007", "Year2008", "Year2009","Year2010","Year2011","Year2012","Year2013","Year2014","Year2015","Year2016")
  colnames(m_data_average)[1] <- "Average_Dissimilarity"
  
  namePlot <- i
  saveoutput <- data.frame(m_data_average, StationID = namePlot)
  Output2 <- rbind(Output, saveoutput)
}

head(Output2, n = 30)
Output2$year2rest<- substr(rownames(Output2), 1, 8)#fixing the row names
Output2$StationFront<- substr(Output2$StationID, 1,8)
str(Output2)#25590 obs. of  4 variables:
#write.csv(Output2, file="DissimilarityOutput.csv")

#Q: How dissimilar plots are per community types:==========
#Get veg data and define StationsFront (Site) prevelent community:
str(veg)#133612 obs. of  24 variables
levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline"
veg$Community<-factor(veg$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels

#Define the community-type in each StationFront based on  n of Comms:
StationComm<- group_by(veg,StationFront,Community ) %>% count(Count=StationFront)
StationComm#It gives us count of communities per StationFront (740)
SCwide<- spread(StationComm, key = Community, value = n, fill = 0)#make it wide
SCwide$WhichMax<-colnames(SCwide)[apply(SCwide,1,which.max)]#while wide we can see which Comm is predominant
SCwide
StationCommDefined<-SCwide[, c(1,7)]
colnames(StationCommDefined)[2] <- "Community" #Renaming WhichMAx back to Community
StationCommDefined #320 stationFronts

#Join Dissimilarity Output2 with Community-type variables:
DisOutput<-read.csv("DissimilarityOutput.csv")#running For loop takes times so I saved output in this file
DissCommunity <- left_join(DisOutput, StationCommDefined, by = "StationFront")
sum(is.na(DissCommunity))#0 YAY! no NA!
head(DissCommunity, n= 1)
#Average_Dissimilarity    StationID year2rest StationFront Community
#    0.3544974          CRMS0002_V54  Year2007     CRMS0002  Brackish
length(levels(DissCommunity$StationID))#2558 YAY! as many as before for loop.
unique(DissCommunity$year2rest)#10 levels

#GGPLOT of variability over time=========
overtime<-summarize(group_by(DissCommunity,year2rest,Community), MeanDiss = mean(Average_Dissimilarity))
overtime$Community<-factor(overtime$Community, levels = c("Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels

VariabilityPlot <- ggplot(overtime, aes(y = Community, x = MeanDiss,color=year2rest,shape =year2rest))+ geom_point(position = "jitter", size=4)
VariabilityPlot + scale_shape_manual(values = c(15,15,16,16,18,18,24,24,13,13)) +theme_classic() +ggtitle("Mean community variability across time")




#PHRAG Cover, Phrag PRESENCE vs Salinity in RDA Analysis============
#Define the  StationFront based on  presence of Phragmites:
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#133612 obs. of  24 variables:
samples2007<-veg[veg$year ==2007,] 
length(levels(droplevels(samples2007$StationID)))#2558 = number of good plots in 2007 
#Subset only these 2007 station from entire data set to have a
#consistent set of plot across years:
DF2007to2016<- veg[ which (veg$StationID  %in%  samples2007$StationID), c("year", "StationID", "StationFront","StationBack", "SpecCode", "Cover", "Community", "CoverTotal")]
str(DF2007to2016)#118570 obs. of  8 variables:
#Reshape to wide format to compute plant composition indices:
DF2007to2016$Cover <- ifelse(DF2007to2016$Cover ==0,0,1) #turning cover to presence/absence data
v<-DF2007to2016[,c("StationID","StationFront","Community","SpecCode","Cover","CoverTotal","year")] #select most important variables
v.wide<-spread(v,key = SpecCode, value = Cover, fill = 0)#species indices can be computed in a wide format only= each species has its own column.
v.wide$PhragPresence<-ifelse(v.wide$Phraaust == 1, "PhragPresent", "PhragAbsent")

#StationFront    StationID year PhragCover StationID.Year
#    CRMS0662 CRMS0662_V50 2010       0.25  CRMS0662_V50.2010

#Mean Salinity:
env<-read.csv("CRMS_Soil.csv")
str(env)#194840 obs. of  12 variables // $ StationID: Factor w/ 388 levels
table(env$year)#sampling effort across years
#2001  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016  2017 
#2  3165 12903 20298 24173 24200 22979 23370 19983 13536 11770 15697  2764#Measurements were very haphazard. I am able to look at the change between 2008 & 2014:

#Remove year 2006 and 2017:
env1<-env[! env$year=="2001" ,] #removing low n years
env2<-env1[! env1$year=="2006" ,] #removing low n years
env2007to2016<-env2[! env2$year=="2017" ,] #removing low n years
table(env2007to2016$year)
#2007  2008  2009  2010  2011  2012  2013  2014  2015  2016 
#2903 20298 24173 24200 22979 23370 19983 13536 11770 15697


#CV (coefficient of variation = sd/mean)
#to measure variability in salinity in each site over time. 

#Compute mean salinity per StationFrontper year:
env2007to2016_Salinity<-summarise(group_by(env2007to2016,StationFront,year), MeanSalinity=mean(SoilPorewaterSalinity.ppt),
                                  SDSalinity=sd(SoilPorewaterSalinity.ppt),CVSalinity= SDSalinity/MeanSalinity)
env2007to2016_Salinity # data frame [818 x 5]
#Produce new unique column "StationFront.Year" to be merged by with veg accordingly:
env2007to2016_Salinity$StationFront.Year<-interaction(env2007to2016_Salinity$StationFront,env2007to2016_Salinity$year)
env2007to2016_Salinity
#Merge veg data with salinity data:
table(v.wide$year)#veg data is alraed cleand = from 2007 to 2016:
#2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 
#2558 2559 2558 2558 2558 2558 2558 2558 2558 2557

v.wide$StationFront.Year<-interaction(v.wide$StationFront, v.wide$year)
v.wide2 <- v.wide[ , - c(2,5)]

PhragPresenceSalinity <- dplyr::left_join(v.wide2,env2007to2016_Salinity, by = "StationFront.Year" )
names(PhragPresenceSalinity)
PhragPresenceSalinity$StationID.Year<-interaction(PhragPresenceSalinity$StationID, PhragPresenceSalinity$year)

#Phrag Cover to merge later on to salinity and phrag presence and all:
PhragCover<- veg[veg$SpecCode=="Phraaust", c("StationID", "year", "Cover")]
dim(PhragCover)#799 3
colnames(PhragCover)[3]<- "PhragCover"
PhragCover$StationID.Year<-interaction(PhragCover$StationID, PhragCover$year)
head(PhragCover, n=21)
PhragCover2<-PhragCover[ , - c(1,2)]
EntireData<-dplyr::left_join( PhragPresenceSalinity, PhragCover2, by = "StationID.Year" )
EntireData$PhragCover[is.na(EntireData$PhragCover)] <- 0 #tur NAs into zeros
table(EntireData$PhragCover)
sum(is.na(EntireData$PhragCover))

#veg and env data to write.csv:======
dim(EntireData)#25580   416
CRMSveg<- EntireData[ , 4:407]
CRMSenv<- EntireData[ , -c(4:407)]#StationID.Year needs splitting:
CRMSenv2 <- separate(data = CRMSenv, col = StationID.Year, into = c("StationID", "year"), sep = "\\.")
write.csv(CRMSenv2, file = "EnvDataAll.csv", row.names = FALSE)
write.csv(CRMSveg, file = "VegDataAll.csv", row.names = FALSE)
#these are the file for CB to resolve Question 5,

