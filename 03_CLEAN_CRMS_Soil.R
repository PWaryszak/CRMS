##### Raw env data from CRMS website #####
#The data is cleaned to be added to veg data of the same period off the same CRMS website.
#DATA ("CRMS_Discrete_Hydrographic.csv") accessed on 11june2018 from CRMS web:
#CRMS ONLY: https://cims.coastal.louisiana.gov/FullTableExports.aspx

library(tidyverse)
library(plotrix)
library(nlme)
library(chron)#install.packages("chron")
library(vegan)#install.packages("vegan")

env<-read.csv("CRMS_Discrete_Hydrographic.csv") #take the file and delete the degree sign in two of the columns
#This is Soil Pore Water Data, measured on average once a month with the sippers,
dim(env)#	 230502  rows of  35 variables:
names(env)
#for env data, the unit of measurement is StationID (which is composed of Station and site),
#even though there are many sites sampled within a station, they are unique measurements.
#I will use only the P stations, not the veg stations
#clean up, make some new columms, select only P stations for "soil porewater" stations 
#that are collected monthly, and select the shallowest depth 10cm depth measurements
#Remove year 2001, 2006 and 2017, remove error-looking outliers
#all the salinity values above 56 look incorrect by comparing to conductance,
#either a decimal is missing or columns were switched around. The value at 54.7 looks correct. 
#I didn't check the other high ones just beneath 56 
#substitute - for _ so it is the same as the veg data

env2<-env %>%
  rename(StationID = CPRA.Station.ID)%>%
  mutate(Date=Date..mm.dd.yyyy. , StationFront=StationID)%>%
  separate(Date,into=c("month","day","year"),sep="/")%>%
  #mutate(yearadd=20)%>%
  #unite(year,yearadd,year,sep="")%>%
  separate(StationFront,into=c("StationFront","StationBack"),sep="-")%>%
  mutate(StationType=StationBack)%>%
  separate(StationType,into=c("StationType","StationBackNum"),sep=1)%>%
  filter(Measurement.Depth..ft.==0.328, StationType=="P",
         year %in% c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017),
         Soil.Porewater.Salinity..ppt. < 56) %>%
  mutate(StationID.year=paste(StationID, year ,sep="."))

dim(env2)#73115    43
hist(env2$Soil.Porewater.Salinity..ppt.)
unique(env2$StationType)#"P"
table(env2$year)

#filter out any plots that had fewer than 5 or whatever samplings per year
counts<-env2%>%
  group_by(StationID.year)%>%
  summarise(n())
counts2<-counts$StationID.year[which(counts$`n()`>4)]
env3<-env2[which(env2$StationID.year%in%counts2),]
dim(env3)#62589    43

#filter out plots sampled in sept-dec (if desired, as veg surveys were done ~sep), 
#summarise means per site (over the 3 p plots at each site),
#and summarise means per year, then summarize means over all the years.
#for max, I could do it either way, below is the max salnity ever recorded at a sampling point
#(averaged across the three P plots)
env4<-env3%>%
  filter(month %in% c(1,2,3,4,5,6,7,8,9)) %>%
  group_by(StationFront, year, month, day, Date..mm.dd.yyyy.) %>%
  summarise(salinityppt = mean(Soil.Porewater.Salinity..ppt.)) %>%
  group_by(StationFront,year) %>%
  summarise(Mean_SoilSalinity=mean(salinityppt),Max_SoilSalinity=max(salinityppt),Min_SoilSalinity=min(salinityppt),
            SD_SoilSalinity = sd(salinityppt), CV_SoilSalinity = SD_SoilSalinity/Mean_SoilSalinity) %>%
  mutate(StationFront.year=paste(StationFront,year,sep="."))
dim(env4)# 2782  8

#then filter out and only keep plots that have at least 7 8 9 or 10 years of data
counts<-env4%>%
  group_by(StationFront)%>%
  summarise(n())
counts2<-counts$StationFront[ which(counts$`n()` > 6 )]

env5<-env4[ which(env4$StationFront %in% counts2 ),] #to be merged with veg data, at least of 6 years of continous data off env4
length(unique(env5$StationFront))#247 sites have at least 7 years-long string of data in them.
dim(env5)#2198    8    #USE env5 for merging with veg data.
#write.csv(as.data.frame(env5), file = "CRMS_Mean_SoilSalinity_env5_11june2018.csv", row.names = F)

#MERGE env5 with VEgEnvData===
str(env5) #'data.frame':	2198 obs. of  8 variables:
env5 <- as.data.frame(env5)
env5.light <- select (env5, Mean_SoilSalinity,Max_SoilSalinity,
                      Min_SoilSalinity,SD_SoilSalinity, CV_SoilSalinity,
                      StationFront.year) 

names(env5.light)

veg6<- read.csv("CRMS_veg6_11june2018.csv") #As produced in "VegECF.R" file
veg6$StationFront.year <- interaction( veg6$StationFront, veg6$year)
dim(veg6)#3606  462

data5 <- left_join(veg6, env5.light, by = "StationFront.year") %>% na.omit()
dim(data5) # 1918  469
names(data5)

#Table 1 of plots per year analyzed with soil pore water========
MyTable <- group_by(data5, year, Community) %>%
  summarise(n()) %>% spread(Community, `n()` )
#write.csv (MyTable, file = "PlotNumberTable_11jun2018.csv", row.names = F)

