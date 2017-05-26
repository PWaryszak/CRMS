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


#LOAD DATA:
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