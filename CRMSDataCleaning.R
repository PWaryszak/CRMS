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
str(veg)#295644 obs. of  32 variables:
levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline"  "Swamp"
levels(veg$SpecCode)
