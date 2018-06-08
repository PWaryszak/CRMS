#CRMS_Cleaning
#DATA ("CRMS_Marsh_Vegetation_RAW.csv") accessed on 08june2018 from CRMS the web:
#CRMS ONLY:https://cims.coastal.louisiana.gov/RequestedDownloads/ZippedFiles/CRMS_Marsh_Vegetation.zip
library(tidyverse)
#REMOVE flying × if any in raw data (there was one at Agropogon Fourn)

crms <- read.csv("CRMS_Marsh_Vegetation_RAW.csv") #325609 obs. of  30 variables on 08june2018:

crms1<- crms %>% select (Station.ID, Collection.Date..mm.dd.yyyy.,
                         Community,Vegetation.Type,X..Cover,
                         Scientific.Name.As.Currently.Recognized,
                         In.Out,Comments) %>%
  rename(StationID = Station.ID, CollectionDate = Collection.Date..mm.dd.yyyy.,
         CoverTotal = X..Cover,
         SpName = Scientific.Name.As.Currently.Recognized)
                           
#Replace all special character in SpName with nothing:======
#Web: http://www.endmemo.com/program/R/gsub.php
crms1a <- crms1
crms1a$SpName <- iconv(crms1a$SpName, to = "ASCII//TRANSLIT") #Convert to ASCII format
crms1a$SpName <- gsub("[[:punct:]]", "", crms1a$SpName)#replace special characters with nothing
unique(crms1a$SpName)

#Split the CollectionDate into 3 columns: day/month/year:
crms2<-crms1a %>% separate(col = CollectionDate ,
                          into=c("month","day","year"),sep="/",
                          remove = F)

#Select time and veg set you want to work with:======
crms3 <- filter(crms2,  year < 2018  &  year > 2006,
                Vegetation.Type == "Natural")
#Water was recorded as species in 2006, hence it is gone now.
dim(crms3)#312582     11

#Inspect the comments=====
i1 <- grepl("Phragmites\\scover\\swas\\sestimated\\sto\\sbe", crms3$Comments)# \\s = regular expression for space
sum(i1==TRUE)#964 plots with comment on estimated Phragmites cover as the sites were not surveyed
#These i1 sites were too dense to access.
#Phragmites was estimated between 70% & 100%
crms3$Category <- i1 
crms3$Category2 <- ifelse(crms3$Category==TRUE, "PhragInComment", "NA") #estra column to keep track of what TRUE means
crms3$In.Out[crms3$Category== TRUE] <- "Both" #we need to change In.Out to Both to keep this record.

#Keep "Out" out: Column In.Out assigns IN/OUT/BOTH categories to all species.
#If a species occurs both IN and OUT of a plot, it is recorded as BOTH. 
#If a species is rooted OUT of the plot, but is hanging over the plot, it is also recorded as BOTH.
#CoverTotal measures the cover within the quadrat only. Out-records contain to CoverTotal value.

crms4 <- filter(crms3, In.Out != "Out")
dim(crms4) #192369     13


#Rename species to standard specCode (vegan-friendly):=====
#seperate complex spName into genus and species:
crms5 <- crms4  %>% separate(col = SpName, into = c("genus", "species", "extra_sp_info"),
                        fill= "right", extra= "merge", sep= "\\s" , remove = F)

#Cut 4 letters of genus and 11 of species and turn into specCode (Vegan-friendly):
crms6 <- crms5 %>% mutate(spec = strtrim(genus, 4), Code = strtrim(species, 11)) %>%
  unite(specCode, spec, Code, sep = "_") %>%
  select( StationID, day, month, year, Community, CoverTotal, Category2,SpName, genus, species, specCode)%>%
  mutate (StationID.year = interaction(StationID,year),
          genus.species  = interaction(genus,species)) #extra column
dim(crms6)# 192369      13
length(unique(crms6$specCode))#570
unique(crms6$specCode)

#Remove duplicates if any:
crms7<- distinct(crms6)
crms7$i <- 1:nrow(crms7)#assign id to each row to make "spread" function work smoothly.
dim(crms7)#  192369     14

#Remove Swamp Comunities 
#Mutate CoverTotal (<1 and Solitary) to numeric value:
#Replace "<1" with 0.5 and "Solitary" with 0.1 (CPRA-recommended)

crms8  <-   filter(crms7, Community !="Swamp") %>%
  mutate(CoverTotal = recode(CoverTotal, "<1" = "0.5")) %>%
  mutate(CoverTotal = recode(CoverTotal, "Solitary" = "0.1"))

sum(is.na(crms8$CoverTotal))#Double check if NA-s produced = 0, If NA-s turn them to 0= is.na(crms8$CoverTotal) <-0
dim(crms8)#160691     14

#Turn factors into numeric values and characters into factors:=====
crms8$CoverTotal <- as.numeric(as.character(crms8$CoverTotal))
crms8$Community <- factor(crms8$Community)
crms8$specCode <- factor(crms8$specCode)
length(unique(crms8$specCode))#455
(unique(crms8$specCode))

#Spread specCode into columns (produce final Veg matrix):=====
crms8$specCode <- factor(crms8$specCode)#to remove empty levels.
crms8a <- select(crms8, - c(genus, species, genus.species, Category2))

#Check if specCode-s duplicate (the same specCode assigned to different species)====
#It should be 455 rows of specCodes:
sp <- crms8a %>% 
  group_by(specCode, SpName) %>%
  summarise(n=n())
str(sp) #YAY!!!  sp$ specCode     : Factor w/ 451 levels
#Save crms8 for joining species names with trait data:
#write.csv(sp, file = "CRMS_Marsh_Veg_SpeciesList.csv" )
#Check if any specCode is duplicated:
spCode <- as.data.frame(sp$specCode)
specCode_duplicated <- spCode[duplicated(spCode),]
specCode_duplicated #Ludw_grandiflora and  Phra_australis but these look OK!
#Ludwigia grandiflora Michx Greuter  Burdet versus Ludwigia grandiflora Michx Greuter  Burdet ssp hexapetala Hook  Arn GL Nesom  Kartesz
#Phragmites australis Cav Trin ex Steud versus Phragmites australis Cav Trin ex Steud ssp australis


#Produce Veg MAtrix:====
crms8b <- select(crms8, - c(genus, species, genus.species, SpName))
names(crms8b)

crms9 <- spread (crms8b, key = specCode, value = CoverTotal, fill=0)
dim(crms9)#160691   463

#Fill in Phraaust with 77.77 if category2 = TRUE (plots unaacessible due to dense stand of Phragmites):
unique(crms8b$Category2)#"NA"   "PhragInComment"
crms9$Phra_australis <- ifelse(crms9$Category2  == "NA" ,crms9$Phra_australis,77.77)
sum(crms9$Phra_australis  == 77.77)#964 = correct.

#Split StationID (site.plot) into StationFront(site) and StationBack (plot 2mx2m)
crms9 <- separate(crms9, StationID, into = c("StationFront", "StationBack"), sep = "-", remove = FALSE)
dim(crms9)#160691   465


#Remove NA, WateNA (open water) and BareGround:====
delete <- c("_NA", "BareGround", "WateNA")
veg <- crms9[, !(names(crms9) %in% delete)] 
dim(veg)# 160691   464 

#check if all rows > 0:
vegveg <- veg[ , 11:464]
occur.row<-apply(vegveg,1,sum)
zeroRows<-vegveg[occur.row <= 0 ,]
nrow(zeroRows)# 263 zero rows to be removed

#check if all columns > 0:
occur.col<-apply(vegveg,2,sum)
oneColumns<-vegveg [,occur.col < 0]
ncol(oneColumns)#0

#Remove zero rows from veg data:
v <- veg[ ! occur.row == 0 ,]
dim(v)# 160428    464

#Creat new "CRMS_Marsh_Veg.csv"
#=> end clean file = "CRMS_Marsh_Veg.csv"
#write.csv(v, file = "CRMS_Marsh_Veg.csv", row.names = FALSE)


#Join crms8a with Emilys LA_Plants list (Traits+Provenance) for future analysis:=====
Emilys_Plants <- read.csv("LA_Plants.csv")
str(Emilys_Plants)#data.frame':	3379 obs. of  11 variables:
CRMS_Plants <- crms8 %>% select(specCode, genus.species) %>%
  group_by(specCode, genus.species) %>%   summarise(n=n())
length(levels(droplevels(CRMS_Plants$specCode))) #455 species
Plants<- left_join(CRMS_Plants,Emilys_Plants, by = "specCode")
#write.csv(Plants, file = "Plants.csv")# check in excel the missing specCode names



