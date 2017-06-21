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
