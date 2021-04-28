#Species Table by Marsh Type=====
library("tidyverse")


VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
dim(VegAllEnvData) # 3498  473
names(VegAllEnvData)
names(Plant_Info)

#SpeciesList across 4 marsh types ========
SpeciesList <-
  VegAllEnvData %>%
  select(Community, Acer_rubrum:Ziza_miliacea) %>%
  gather(specCode, Count, -Community) %>% #turn wide to long format
  group_by(Community,specCode) %>% #Group by Community and Species
  summarise(TotalCount = sum(Count)) %>% #Species that do not occur their TotalCount = 0
  filter(TotalCount>0) #Remove all species below zero


#join SpeciesList & Plant_List to see which species are native/introduced:
SpeciesTable <- left_join(SpeciesList,Plant_Info, by = "specCode") %>%
  separate(genus.species, into = c("Genus", "Species") , remove = F)

View(SpeciesTable )
write.csv(SpeciesTable, file = "CRMS_SpeciesTableByMarshType.csv", row.names = F)
