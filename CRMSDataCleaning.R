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
