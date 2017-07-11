#Produce data for RDA ANALYSIS ==========
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

#FOR LOOP to produce veg matrices==========
#Subset one plot for trial RDA run for Christina:
CRMS0002_V54<- v.wide[v.wide$StationID=="CRMS0002_V54",]
dim(CRMS0002_V54)
#write.csv(CRMS0002_V54, file = "OnePlotRDATrail.csv")

#The plots measured in 2007 were not always consistently surveyed across 10 years
#We need to find them and remove them. Pivot in excel works better here for some reason
#write.csv(v.wide, file = "VegConsistentPlotsData.csv", row.names = FALSE)
#NEW CRMS_Marsh_Veg.csv created (see DataCleanin.R for details)

Richness <-specnumber(v.wide[ , 6:218])
range(Richness)#1 24 = our richness range per plot

#To subset each year we may need a loop to avoif repeating this code 10 times for each year
veg.matrix2007<-v.wide[v.wide$year=="2007",6:218]#let us subset a species matrix only.
Richness2007 <-specnumber(veg.matrix2007)
range(Richness2007)#1 24 that is our range of species per plot in 2007

#The loop below produces cvs file of veg matrix for each year separetly:

for ( i in unique(v.wide$year) ){
  
  VegMatrix <- subset(v.wide, year== i)#create a subset data for each plot2 level
  
  write.csv(VegMatrix, file = paste0("VegMatrix_Year", i, ".csv"), row.names = FALSE)
  
}


#RDA Analysis===========
#TRAIL
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

#Averaging by column:
#This will givethe average dissimilarity of each year to all other years:
str(m)
m3<- colMeans(m2, na.rm = TRUE)
m3

#CREATE LOOP to repeat these actions across all plots:

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

str(Output2)
#write.csv(Output, file="Output.csv")

#Running the code outside for loop:
subset_plot <- subset(v.wide, StationID=="CRMS0002_V54")
dim(subset_plot)#10 409
order_plot<- subset_plot[ order(subset_plot$year), ]
order_plot
subset_veg <- subset_plot[, 6:409 ]
Dissimilarity_Matrix <- vegdist(subset_veg, distance = "bray", binary = TRUE, upper=TRUE, diag=FALSE)
m <- as.matrix(Dissimilarity_Matrix)
colnames(m) <- c( "Year2007", "Year2008", "Year2009","Year2010","Year2011","Year2012","Year2013","Year2014","Year2015","Year2016")
rownames(m) <- c( "Year2007", "Year2008", "Year2009","Year2010","Year2011","Year2012","Year2013","Year2014","Year2015","Year2016")
m[ row(m) == col(m) ] <- NA #replacing diagonal with NA
m_data<-as.data.frame(m)
m_data
m_data_average<- as.data.frame( colMeans(m, na.rm = TRUE)) #Averaging by column:
rownames(m_data_average) <- c( "Year2007", "Year2008", "Year2009","Year2010","Year2011","Year2012","Year2013","Year2014","Year2015","Year2016")
colnames(m_data_average)[1] <- "Average_Dissimilarity"
m_data_average#This will give us the average dissimilarity of each year to all other years:
data_M <- assign(paste0("VegMatrixAverage_Plot_", "Plot"), m_data_average)
data_M
namelm <- paste0("Plot.", i)
saveoutput <- data.frame(datalm, Species=namelm)
Output <- rbind(Output, saveoutput)


#Salinity change in subsample==========
env<-read.csv("CRMS_Soil.csv")
str(env)#7384 obs. of  19 variables: // $ StationID: Factor w/ 388 levels
table(env$year)#sampling effort across years
#2006 2007 2008 2009 2011 2012 2014 2015 
#1869 1764 2546  374    3   18  807    3 
#Measurements were very haphazard. I am able to look at the change between 2008 & 2014:
#But First compute:
#CV (coefficient of variation = sd/mean)
#to measure variability in salinity in each plot over time. 
env2<- env[env$year == "2008"|env$year == "2014",]
dim(env2)#3353   19

#Computing 3 Salinity measures in 2008 and 2014 joined together:
env3<-summarise(group_by(env2,StationFront), MeanSalinity=mean(SoilSalinity.ppt),
                SDSalinity=sd(SoilSalinity.ppt),CVSalinity= SDSalinity/MeanSalinity,
                MeanOrganicMatter=mean(OrganicMatter), MeanWetpH= mean(WetSoilpH) )
env3# A tibble: 164 × 4
range(env3$CVSalinity)#0.000000 1.804334

#SUBSET station of year 2008:
Stations2008<-env[env$year == "2008",c("StationFront","year" )]
dim(Stations2008)# 2546    2

#Subset 2008 StationFronts of env3 data
S08<- env3 [ which (env3$StationFront %in%  Stations2008$StationFront), c("StationFront", "MeanSalinity","SDSalinity","CVSalinity","MeanWetpH", "MeanOrganicMatter")]
S08# A tibble: 152 × 6
S08$year <- as.factor("2008")
S08


#Matrices for 2008 RDA (Christina)==================
#GET VEG in 2008 
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#293726 obs. of  24 variables:

#Join available env data by StationFront with year 2008 env data :
vegEnv<- veg[ which (veg$StationFront  %in%  S08$StationFront), c("In.Out","year", "StationID", "StationFront", "SpecCode", "Cover","CoverTotal", "Community")]
dim(vegEnv)#119590      8
names(vegEnv)#

#veg+Env Data.Frame in YEAR2008:
veg08<- vegEnv[ vegEnv$year=="2008", c("StationID","StationFront","Cover","CoverTotal","SpecCode","Community",  "year","In.Out")]
str(veg08)#10707 obs. of  8 variables:
veg08in <- veg08 [ ! veg08$In.Out == "Out",] #cutting out outside the plot records.
str(veg08in)#6644 obs. of  8 variables:
veg08in$Cover = ifelse(veg08in$Cover == 0, 0, 1) #turning Cover into presence/absence
veg08in<-veg08in[ , -7] #removing year column as it is causing issues with right_join

names(vegEnv08)
vegEnv08<-dplyr::right_join(veg08in,S08, by = "StationFront")
str(vegEnv08)#6647 obs. of  12 variables: NA records exist where not met with veg data
vegEnv08<-vegEnv08[ ! is.na(vegEnv08$StationID),] #removing one NA row
str(vegEnv08)#YAY! 6644 obs.as before joining.

#WE need to spread the VegEnv08 to wide format:
vegEnv08wide<-spread(vegEnv08,key = SpecCode, value = Cover, fill = 0)#species indices can be computed only when species are 
str(vegEnv08wide)#1478 obs. of  260 variables:

# Remove Swamp as we know it contains no Phrag:
vegEnv08wideNoSwamp<-vegEnv08wide[vegEnv08wide$Community !="Swamp",]
str(vegEnv08wideNoSwamp)#1081 obs. of  260

#Create veg-only matrix for year 2008, No Swamp:
vegData2008NoSwamp <- vegEnv08wideNoSwamp[,12:260]
range(rowSums(vegData2008NoSwamp))# 1 20 = richness range
sum(vegData2008[is.na(vegData2008NoSwamp)])#ZERO NAs!! YAY!!!
write.csv(vegData2008NoSwamp, file = "Veg2008NoSwamp.csv", row.names = FALSE)#Sent to CB to do RDA.

#Create environmental data (soil data)
envData2008NoSwamp<- vegEnv08wideNoSwamp[ , c("year", "MeanSalinity", "Community","CVSalinity","MeanOrganicMatter","MeanWetpH")]
write.csv(envData2008NoSwamp, file = "Env2008NoSwamp.csv", row.names = FALSE)#Sent to CB to do RDA.

#With Swamp:
#Create veg-only matrix for all year 2008
vegData2008 <- vegEnv08wide[,12:260]
range(rowSums(vegData2008))# 1 20 = richness range
sum(vegData2008[is.na(vegData2008)])#ZERO NAs!! YAY!!!
#Create environmental data (soil data)
envData2008<- vegEnv08wide[ , c("year", "MeanSalinity", "Community","CVSalinity","MeanOrganicMatter","MeanWetpH")]

