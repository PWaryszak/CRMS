#DatCleaning=========
setwd("~/Desktop/CRMS/CRMS")

#The excel file "CRMS_Marsh_Veg.csv" has been edited in excel to:
#remove spaces and signs not suitable for R like > /,
#fix the date format - some dates were in text format, some in a date format
#Bare Ground was renamed to "AaaProblem" and needs to go as it codes lack of species

#Cover (%) had non-numeric values: "<1" and "solitary" and empty records. I have transformed them as:
#"<1" = 0.5
#"solitary" = 0.25
#empty = 0.0 (In some case Cover Total was lower than single covers!!! Weird - I just left it as it is)

#I also created a new column called "SpecCode" that contains first 4 of Genus and first 4 of species names
#for ordination in vegan package. Vegan does not like long/uneven names.


#LOAD DATA:=============
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#295644 obs. of  28 variables:
names(veg)
[1] "StationID"  #Site.plot                        
[2] "StationFront" #Site = name before hypen
#for example for StationID "CRMS0002-V54" it is "CRMS0002"
[3] "StationBack"   # plot = name after hypen
#for example for StationID "CRMS0002-V54" it is "V54"                    
[4] "CollectionDate"   #full date                  
[5] "month"                              
[6] "day"                                
[7] "year"                               
[8] "Community"     # 5 types :
#"Brackish","Freshwater","Intermediate","Saline", "Swamp"  
[9] "CoverTotal" # Cover per plot sometimes is higher than 100%. 
[10] "Cover"        # cover of single plant                      
[11] "FieldName"                          
[12] "Genus"                              
[13] "Species"                            
[14] "SpecCode"    #8 letters = 4 genus + 4 species 
#to be used in Vegan ordinations
[15] "CoverTree"                          
[16] "CoverShrub"                         
[17] "CoverHerb"                          
[18] "CoverCarpet"                        
[19] "AvHeightDominant.cm"                
[20] "AvHeightTree.cm"                    
[21] "AvHeightShrub.cm"                   
[22] "AvHeightHerb.cm"                    
[23] "Av.HeightCarpet.cm"                 
[24] "Common_Name_As_Currently_Recognized"
[25] "BraunBlanquetRank"                  
[26] "In.Out"                             
[27] "Comments"                           
[28] "SpecCodeFull_Explanation"   

#2. Further Datacleanig & Reduction========
#I decided to delete Aaaaprobl
#as they code for bare ground or nothing related to plants species,these were the plots that were hard to access and not surveyed:
  
#The species were coded with error so I fixed them as:
#Bolb(Asc to Bolbxxxx,   Dich(Hit to Dichxxxx,  Phan(Raf to Phanxxxx
#Scho(Rch to Schoxxxx,    CeraL to Ceraxxxx,     CeraBron to Ceraxxxx
#CentL to Centxxxx, CephL to Cephxxxx,    ZizaDöll to Zizaxxxx
#IvaL to Ivaxxxxx and so on to make all names 8 long in characters (to make work smoothly in Vegan).

# I also removed the field name column and the comments. 
#Colums with species common names and additional explanation & comment columns removed. Currently 24 columns
#File size went down to ~30MB.
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#293726 obs. of  24 variables:
names(veg) #Current Variable:
[1] "StationID"          
[2] "StationFront"       
[3] "StationBack"        
[4] "CollectionDate"     
[5] "month"              
[6] "day"                
[7] "year"               
[8] "Community"          
[9] "CoverTotal"         
[10] "Cover"              
[11] "SpecCode"           
[12] "Genus"              
[13] "Species"            
[14] "CoverTree"          
[15] "CoverShrub"         
[16] "CoverHerb"          
[17] "CoverCarpet"        
[18] "AvHeightDominant.cm"
[19] "AvHeightTree.cm"    
[20] "AvHeightShrub.cm"   
[21] "AvHeightHerb.cm"    
[22] "Av.HeightCarpet.cm" 
[23] "BraunBlanquetRank"  
[24] "In.Out"

#3. Further Data Cleaning============
#Load DATA:
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
#We agreed to remove Swamp records (Community = "Swamp") as they had no records of our target species= Phraaust
#We also agreed on removing all records from outside the plot where In.Out = "Out"
veg2<-veg[veg$Community != "Swamp",] #removing Swamo records
dim(veg2)#236382     24
veg3<-veg2[veg2$In.Out != "Out",]
dim(veg3)#151489     24

#Save it and commit to Github today [21June2017]
write.csv(veg3, file = "MarshData.csv", row.names = FALSE)

#4. Further cleaning:
#Data contains 21 duplicates for example:
CRMS0258_V70<- v.wide[v.wide$StationID=="CRMS0258_V70", ]
CRMS0258_V70[,1:5]
#Duplicated records as CRMS team measured the same plot twice withing one year sometimes.
# + 566 plots that were not consistently measured.

#Creating (08jul2017) new veg data with 2007 plots consistently surveyed across all years===========
library(vegan)
library(tidyverse)

veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
samples2007<-veg[veg$year ==2007,] 
length(levels(droplevels(samples2007$StationID)))#3145 = number of plots in 2007 

#Subset only these 2007 station from entire data set to have a
#consistent set of plot across years:

ourDF<- veg[ which (veg$StationID  %in%  samples2007$StationID), c("year", "StationID", "StationFront","StationBack", "SpecCode", "Cover", "Community", "CoverTotal")]
str(ourDF)#136274 obs. of  8 variables:
table(ourDF$year)# a balanced sampling!!!!

# We decided to skip year 2006 (small n of plots in 2006)
DF2007to2016<- ourDF[ourDF$year!="2006",]
str(DF2007to2016)#129095 obs. of  8 variables:

#Reshape to wide format to compute plant composition indices:
DF2007to2016$Cover <- ifelse(DF2007to2016$Cover ==0,0,1) #turning cover to presence/absence data
v<-DF2007to2016[,c("StationID","StationFront","Community","SpecCode","Cover","CoverTotal","year")] #select most important variables

v.wide<-spread(v,key = SpecCode, value = Cover, fill = 0)#species indices can be computed in a wide format only= each species has its own column.
write.csv(v.wide, file = "VegConsistentPlotsData.csv", row.names = FALSE)
#The plots measured in 2007 were not always consistently surveyed across 10 years
#We need to find them and remove them. Pivot in excel works better here for some reason
write.csv(v.wide, file = "VegConsistentPlotsData.csv", row.names = FALSE)

#After running pivot in excel we identyfied the plots we need to remove (lack of consistent surveys):
#21 duplicated plots (errors?) and 566 undersurveyed plots:
ErrorPlots<-read.csv("ErrorPlots.csv")# after pivoting in "VegConsistentPlotsData.csv"

veg<- read.csv("CRMS_Marsh_Veg_OLD.csv")
dim(veg)#151489     24
veg2<- veg[ ! veg$year=="2006",]#remove plots of year 2006 (under-surveyed year)
dim(veg2)#144137     24

veg3<- veg2[ - which (veg2$StationID %in% ErrorPlots$StationID), ]
dim(veg3)#133612     24
write.csv(veg3, file = "CRMS_Marsh_VegNEW.csv", row.names = FALSE)

#SOIL DATA=========
#021jun2017 we downloaded a more robust data off CRMS website.
#During clearning  11991 rows were removed as salinity was not measured there 
#(Mostly due to deviced suffering from clogging by mud and organics, as per comments)
env<-read.csv("CRMS_Soil.csv")
str(env)#194840 obs. of  12 variables: 


#PHRAG Cover, Phrag PRESENCE, Salinity merging with Veg Data============
#these our extra variable to be matched with veg data for RDA analysis

#Define the StationFront based on  presence of Phragmites:
#Load data from your directory or straigh from the github:
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
#OR LOAD OFF WEB:
#veg<-read.csv(url("https://raw.githubusercontent.com/PWaryszak/CRMS/master/CRMS_Marsh_Veg.csv"))
#str(veg)#133612 obs. of  24 variables:
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

#Mean Salinity:
#We got an updated(bigger) file on soil data of CRMS site. To free up some space on my Github
#I created a website where these clean files are not hanging (= https://sites.google.com/site/phragmitesproject/)
#These files can be loaded into your R via url:
env<-read.csv(url("https://sites.google.com/site/phragmitesproject/file-cabinet/CRMS_Soil.csv?attredirects=0"))
#env<-read.csv("CRMS_Soil.csv")# this works only if you env data is downloaded to your working directory
str(env)#194840 obs. of  12 variables // $ StationID: Factor w/ 388 levels

table(env$year)#sampling effort across years
#2001  2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016  2017 
#   2  3165 12903 20298 24173 24200 22979 23370 19983 13536 11770 15697  2764 #Measurements were uneven.
#So...
#Remove year 2001, 2006 and 2017:
env1<-env[! env$year=="2001" ,] #removing low n years
env2<-env1[! env1$year=="2006" ,] #removing low n years
env2007to2016<-env2[! env2$year=="2017" ,] #removing low n years

table(env2007to2016$year)
#2007  2008  2009  2010  2011  2012  2013  2014  2015  2016 
#2903 20298 24173 24200 22979 23370 19983 13536 11770 15697

#CV of Salinity across years
#CV = coefficient of variation = sd/mean
#to measure variability in salinity in each site over time. 
#Compute mean salinity per StationFront per year:
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
names(v.wide)
v.wide2 <- v.wide[ , - c(2,5)]#removing "StationFront"  & "year" to avoid repetition in left_join 

PhragPresenceSalinity <- dplyr::left_join(v.wide2,env2007to2016_Salinity, by = "StationFront.Year" )
names(PhragPresenceSalinity)
PhragPresenceSalinity$StationID.Year<-interaction(PhragPresenceSalinity$StationID, PhragPresenceSalinity$year)

#Phrag Cover to merge later on to salinity and phrag presence and all:
PhragCover<- veg[veg$SpecCode=="Phraaust", c("StationID", "year", "Cover")]
dim(PhragCover)#799 3
colnames(PhragCover)[3]<- "PhragCover"
PhragCover$StationID.Year<-interaction(PhragCover$StationID, PhragCover$year)
head(PhragCover, n=21)
PhragCover2<-PhragCover[ , - c(1,2)]#removing "StationID"  & "year" to avoid repetition in left_join 

EntireData<-dplyr::left_join( PhragPresenceSalinity, PhragCover2, by = "StationID.Year" )
EntireData$PhragCover[is.na(EntireData$PhragCover)] <- 0 #tur NAs into zeros
table(EntireData$PhragCover)#lots of zeros = 24982
sum(is.na(EntireData$PhragCover))#YAY! no NA-s!

#write.csv(EntireData, file = "VegEnvData.csv", row.names = FALSE) #this is our entire data
#that matches together veg and env variables. I will hang it on our Phrag web

#To seperate EntireData into veg and env data frames again, go:
CRMSveg<- EntireData[ , 4:407] #this is our veg only matrix
CRMSenv<- EntireData[ , -c(4:407)]#this is our env data that matches veg only matrix above

#NOTES:
#Now you can load VegData  and EnvData off web:
CRMSveg<- read.csv(url("https://sites.google.com/site/phragmitesproject/file-cabinet/VegDataAll.csv?attredirects=0"))
CRMSenv<- read.csv(url("https://sites.google.com/site/phragmitesproject/file-cabinet/EnvDataAll.csv?attredirects=0"))
