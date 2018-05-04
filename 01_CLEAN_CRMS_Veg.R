#CRMS_Cleaning
#DATA ("CRMS_Marsh_Vegetation.csv") accessed on 03May2018 from CRMS web => end clean file = "CRMS_Marsh_Veg.csv"
#CRMS ONLY: https://cims.coastal.louisiana.gov/FullTableExports.aspx

library(tidyverse)
crms <- read.csv("CRMS_Marsh_Vegetation.csv") #325609 obs. of  30 variables on 03may2018:

crms1<- crms %>% select (Station.ID, Collection.Date..mm.dd.yyyy.,
                         Community,Vegetation.Type,X..Cover,
                         Scientific.Name.As.Currently.Recognized,
                         In.Out,Comments) %>%
  rename(StationID = Station.ID, CollectionDate = Collection.Date..mm.dd.yyyy.,
         CoverTotal = X..Cover,
         SpName = Scientific.Name.As.Currently.Recognized)
                           

#Split the CollectionDate into 3 columns: day/month/year:
crms2<-crms1 %>% separate(col = CollectionDate ,
                          into=c("month","day","year"),sep="/",
                          remove = F)

#Select time and veg set you want to work with:
crms3 <- filter(crms2,  year < 2018  &  year > 2006,
                Vegetation.Type == "Natural")
  
dim(crms3)#312582     11

#Inspect the comments
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
#CoverTotal measures the cover within the quadrat only.

crms4 <- filter(crms3, In.Out != "Out")
dim(crms4) #192369     13


#Rename species to standard max 8 digits specCode:
#seperate complex spName into genus and species:
crms5 <- crms4  %>% separate(col = SpName, into = c("genus", "species", "extra_sp_info"),
                        fill= "right", extra= "merge", sep= "\\s")

#Cut 4 letters of genus and species and turn into specCode (Vegan-friendly):
crms6 <- crms5 %>% mutate(spec = strtrim(genus, 4), Code = strtrim(species, 4)) %>%
  unite(specCode, spec, Code, sep = "") %>%
  select( StationID, day, month, year, Community, CoverTotal, Category2, specCode)%>%
  mutate (StationID.year = interaction(StationID,year)) #extra column
dim(crms6)# 192369      9

#Remove duplicates if any:
crms7<- distinct(crms6)
crms7$i <- 1:nrow(crms7)#assign id to each row to make spread function work smoothly.
dim(crms7)#  202346      10 

#Remove Swamp Comunities 
#Mutate CoverTotal (<1 and Solitary) to numeric value:
#Replace "<1" with 0.5 and "Solitary" with 0.1 (CPRA-recommended)

crms8  <-   filter(crms7, Community !="Swamp") %>%
  mutate(CoverTotal = recode(CoverTotal, "<1" = "0.5")) %>%
  mutate(CoverTotal = recode(CoverTotal, "Solitary" = "0.1"))

sum(is.na(crms8$CoverTotal))#Double check if NA-s produced = 0, If NA-s turn them to 0= is.na(crms8$CoverTotal) <-0
dim(crms8)#160691     10

#Turn factors into numeric values and characters into factors:
crms8$CoverTotal <- as.numeric(as.character(crms8$CoverTotal))
crms8$Community <- factor(crms8$Community)
crms8$specCode <- factor(crms8$specCode)

#Convert Non-alphaNumeric in specCode to alphnumeric:
crms8$specCode <- iconv(crms8$specCode, to = "ASCII//TRANSLIT")

#Convert special characters in specCode to X:
crms8$specCode <- gsub("[[:punct:]]", "X", crms8$specCode)

#Spread specCode into columns (produce final Veg matrix):
crms8$specCode <- factor(crms8$specCode)#to remove empty levels.
crms9 <- spread (crms8, key = specCode, value = CoverTotal, fill=0)
dim(crms9)#160691    459

#Fill in Phraaust with 77.77 if category2 = TRUE (plots unaacessible due to dense stand of Phragmites):
unique(crms8$Category2)#"NA"   "PhragInComment"
crms9$Phraaust <- ifelse(crms9$Category2  == "NA" ,crms9$Phraaust,77.77)
sum(crms9$Phraaust == 77.77)#964 = correct.

#Split StationID (site.plot) into StationFront(site) and StationBack (plot 2mx2m)
crms9 <- separate(crms9, StationID, into = c("StationFront", "StationBack"), sep = "-", remove = FALSE)
dim(crms9)#160691    461


#Remove NA, WateNA (open water) and BareGround:
delete <- c("NA", "BareGrou", "WateNA")
veg <- crms9[, !(names(crms9) %in% delete)] 
dim(veg)# 160691    459

#check if all rows > 0:
vegveg <- veg[ , 11:459]
occur.row<-apply(vegveg,1,sum)
zeroRows<-vegveg[occur.row <= 0 ,]
nrow(zeroRows)# 263 zero rows to be removed

#check if all columns > 0:
occur.col<-apply(vegveg,2,sum)
oneColumns<-vegveg [,occur.col <= 1]
ncol(oneColumns)#19
str(oneColumns)

#Remove zero rows from veg data:
v <- veg[ ! occur.row == 0 ,]
dim(v)#160428    459

#Creat new "CRMS_Marsh_Veg.csv" , 03may2018:
#write.csv(v, file = "CRMS_Marsh_Veg.csv", row.names = FALSE)
