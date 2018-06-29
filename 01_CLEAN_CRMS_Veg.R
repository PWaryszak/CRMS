#CRMS_Cleaning
#DATA ("CRMS_Marsh_Vegetation_RAW.csv") accessed on 08june2018 from CRMS the web:
#CRMS ONLY:https://cims.coastal.louisiana.gov/RequestedDownloads/ZippedFiles/CRMS_Marsh_Vegetation.zip
library(tidyverse)
#REMOVE flying × if any in raw data (there was one at Agropogon Fourn)
#LOAD DATA:
crms <- read.csv("CRMS_Marsh_Vegetation_RAW.csv") #325609 obs. of  30 variables on 08june2018:

#Rename variable to human-friendly:
crms1<- crms %>% select (Station.ID, Collection.Date..mm.dd.yyyy.,
                         Community,Vegetation.Type,X..Cover,
                         Scientific.Name.As.Currently.Recognized,
                         In.Out,Comments) %>%
  rename(StationID = Station.ID, CollectionDate = Collection.Date..mm.dd.yyyy.,
         Cover = X..Cover,
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
dim(crms2) #325609     11
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
crms3$Category2 <- ifelse(crms3$Category==TRUE, "PhragInComment", "NA") #extra column to keep track of what TRUE means
table(crms3$In.Out[crms3$Category=="TRUE"])#All Phrag in comment is recorded as OUT:
#Both   In  Out 
#   0    0  964 
#crms3$In.Out[crms3$Category== TRUE] <- "Both" #change In.Out to Both to keep this record.
#substitute - for _ so it is R-friendly:
crms3$StationID<-as.factor(sub("-","_",crms3$StationID))

#Keep "Out" out: Column In.Out assigns IN/OUT/BOTH categories to all species.
#If a species occurs both IN and OUT of a plot, it is recorded as BOTH. 
#If a species is rooted OUT of the plot, but is hanging over the plot, it is also recorded as BOTH.
#Cover measures the cover within the quadrat only. Out-records add to CoverTotal value.

crms4 <- filter(crms3, In.Out != "Out")
dim(crms4) # 191405     13

#Rename species to standard specCode (vegan-friendly):=====
#seperate complex spName into genus and species:
crms5 <- crms4  %>% separate(col = SpName, into = c("genus", "species", "extra_sp_info"),
                             fill= "right", extra= "merge", sep= "\\s" , remove = F)

#Cut 4 letters of genus and 11 of species and turn into specCode (Vegan-friendly):
crms6 <- crms5 %>% mutate(spec = strtrim(genus, 4), Code = strtrim(species, 11)) %>%
  unite(specCode, spec, Code, sep = "_") %>%
  select( StationID, day, month, year, Community, Cover, Category2,SpName, genus, species, specCode) %>%
  mutate (StationID.year = interaction(StationID,year),
          genus.species  = interaction(genus,species)) #extra column
dim(crms6)# 191405     13
length(unique(crms6$specCode))#570
unique(crms6$specCode)

#Remove duplicates if any:
crms7<- distinct(crms6)
crms7$i <- 1:nrow(crms7)#assign id to each row to make "spread" function work smoothly.
dim(crms7)#  191405     14

#Remove Swamp Comunities 
#Mutate Cover (<1 and Solitary) to numeric value:
#Replace "<1" with 0.5 and "Solitary" with 0.1 (CPRA-recommended)

crms8  <-   filter(crms7, Community !="Swamp") %>%
  mutate(Cover = recode(Cover, "<1" = "0.5")) %>%
  mutate(Cover = recode(Cover, "Solitary" = "0.1"))

sum(is.na(crms8$Cover))#Double check if NA-s produced = 0, If NA-s turn them to 0= is.na(crms8$Cover) <-0
dim(crms8)# 159727     14

#Turn factors into numeric values and characters into factors:=====
crms8$Cover <- as.numeric(as.character(crms8$Cover))
crms8$Community <- factor(crms8$Community)
crms8$specCode <- factor(crms8$specCode)
crms8$StationID <- factor(crms8$StationID)
crms8$year<- factor(crms8$year)
crms8$specCode<- factor(crms8$specCode)
str(crms8)

length(unique(crms8$specCode))#455
(unique(crms8$specCode))

#Spread specCode into columns (produce final Veg matrix):=====
crms8$specCode <- factor(crms8$specCode)#to remove empty levels.

#Check if specCode-s duplicate (the same specCode assigned to different species)====
#It should be 455 rows of specCodes:
sp <-  select(crms8, - c(genus, species, genus.species, Category2))%>%
  group_by(specCode, SpName) %>%
  summarise(n=n()) %>% distinct()
str(sp) # sp$ specCode     : Factor w/ 455 levels
#Check if any specCode is duplicated:
spCode <- as.data.frame(sp$specCode)
specCode_duplicated <- spCode[duplicated(spCode),]
specCode_duplicated #Ludw_grandiflora and  Phra_australis but these look OK!
#Ludwigia grandiflora Michx Greuter  Burdet versus Ludwigia grandiflora Michx Greuter  Burdet ssp hexapetala Hook  Arn GL Nesom  Kartesz
#Phragmites australis Cav Trin ex Steud versus Phragmites australis Cav Trin ex Steud ssp australis

#Checking if these two species are recorded togehter
Ludw_grandiflora <- crms8 %>% filter (specCode=="Ludw_grandiflora")%>%
  select (StationID, specCode, SpName, year) %>% group_by(StationID, SpName, year) %>% summarise(n=n())
Ludw_grandiflora
range(Ludw_grandiflora$n)#1 1 = means no two SpName-s at the same plot, good to spread()

Phra_australis <- crms8 %>% filter (specCode=="Phra_australis")%>%
  select (StationID, specCode, SpName, year) %>% group_by(StationID, SpName, year) %>% summarise(n=n())
Phra_australis
range(Phra_australis$n)#1 1 = means no two SpName-s at the same plot, good to spread()

#Save crms8 for joining species names with trait data:
#write.csv(sp, file = "CRMS_Marsh_Veg_SpeciesList.csv" )


#As per email from CPRA in regards to duplicated records (LeighAnne.Sharp@la.gov):
#That would be the regularly scheduled vegetation assessment. 
#The second trip in October was conducted by CPRA (then DNR) staff.
#That would be a post hurricane assessment (H. Gustav was 9/1/08; H. Ike was 9/13/08). 
#We re-sampled select sites after the 2008 storms.  You can sort those out by Organization.  
#The data is spread across post- and prio-hurricance months (06-10) 
sort( table (crms8$month))
#12    11    05    06     6     7    10    09     9     8    07    08 
#24    28   402   875  1018  4829  7016  7927 13006 38459 41789 44354


#Find duplicate StaionFront.month records and remove the latest:
#Fix the month names:
crms8$month <- as.factor(crms8$month)
levels(crms8$month)[levels(crms8$month)== "05"] <- "5"
levels(crms8$month)[levels(crms8$month)== "06"] <- "6"
levels(crms8$month)[levels(crms8$month)== "07"] <- "7"
levels(crms8$month)[levels(crms8$month)== "08"] <- "8"
levels(crms8$month)[levels(crms8$month)== "09"] <- "9"
sort( table (crms8$month))
#12    11     5     6    10     9     7     8 
#24    28   402  1893  7016 20933 46618 82813

#Create site (StationFront) variable:
crms8.dupl <- separate(crms8, StationID, into = c("StationFront", "StationBack"), 
                       sep = "_", remove = FALSE)
#Create Station.Front.month.year variable:
crms8.dupl$StationID.year<- interaction(crms8.dupl$StationID,
                                                   crms8.dupl$year)

#See if any StationFront was measured more than once (two different monts):
Find.duplicated <- crms8.dupl %>%
  group_by(StationID.year, month) %>%
  summarise(TotalCover = sum(Cover), N = n()) 
#View(Find.duplicated) 
#Create a  data.frame with duplicated surveys:
duplicated.ID <- Find.duplicated[duplicated(Find.duplicated$StationID.year),]
as.data.frame(duplicated.ID) #25

#Create StationID.year.month variable to ID rows to remove
duplicated.ID$StationID.year.month <- interaction (duplicated.ID$StationID.year,
                                                   duplicated.ID$month)
#Create StationID.year.month variable to match duplicated.ID
crms8.dupl$StationID.year.month<- interaction(crms8.dupl$StationID.year,
                                        crms8.dupl$month)


#Remove the duplicated levels of StationID.year.month:
crms8c <- crms8.dupl [ ! crms8.dupl$StationID.year.month %in% duplicated.ID$StationID.year.month, ]
dim(crms8c)#159677     18
dim(crms8.dupl)#159727     18

#Other good way to fix duplicated surveys issue is to run average of Covers on specific group:
#crms8_clean <- crms8c %>%  group_by(StationID.year, Community, specCode, year) %>%  summarise(Cover = mean(Cover))
#But we agreed with Emily & Christina that removing the duplicated records is more sound.

#Create wide species matrix:
crms9 <- spread (crms8_clean, key = specCode, value = Cover, fill = 0) #WORKS NOW after removing duplicated surveys!
dim(crms9)#35212   458

#Split StationID (site.plot) into StationFront(site) and StationBack (plot 2mx2m)
crms9 <- separate(crms9, StationID.year, into = c("StationFront", "StationBack"), sep = "_", remove = FALSE)
dim(crms9) #35212   460

#DOUBLE-CHECK DATA with Excel Raw File=========
crms9$StationFront.year<-interaction(crms9$StationFront, crms9$year)#We need that for joining
x <- select(crms9, StationFront.year, Spar_patens) %>%
  filter(StationFront.year == "CRMS0002.2008")
mean(x$Spar_patens)#All good if =  66.7%, we got this value in pivot table in raw csv file.

#Remove NA, WateNA (open water) and BareGround:====
delete <- c("_NA", "BareGround", "WateNA")
veg <- crms9[, !(names(crms9) %in% delete)] 
dim(veg)# 35207   460

#check if all rows > 0:
vegveg <- veg[ , 6:458]
occur.row<-apply(vegveg,1,sum)
zeroRows<-vegveg[occur.row <= 0 ,]
nrow(zeroRows)# 250 zero rows to be removed

#check if all columns > 0:
occur.col<-apply(vegveg,2,sum)
oneColumns<-vegveg [,occur.col < 0]
ncol(oneColumns)#0

#Remove zero rows from veg data:
v <- veg[ ! occur.row == 0 ,]
dim(v)# 34957   460
names(v)
v <- separate(v, StationFront.year, into = c("StationID", "RemoveThisYear"),
              sep = "\\.", remove = FALSE)
dim(v)#34957   462
v <- select (v, -RemoveThisYear)
dim(v)#34957   461

z <- select(v, StationFront.year, Spar_patens) %>%
  filter(StationFront.year == "CRMS0002.2008")
mean(z$Spar_patens)#All good if mean(z) =  66.7%, we got this value in pivot table in raw csv file.

#Create new clean "CRMS_Marsh_Veg.csv"
#write.csv(v, file = "CRMS_Marsh_Veg.csv", row.names = FALSE)
#Proceed to: 02_CLEAN_CRMS_Hydro, 03_CLEAN_CRMS_Soil, 04_MERGE_VegHydroSoil 
