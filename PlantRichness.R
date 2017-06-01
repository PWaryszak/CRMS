#Load Data and Libraries==============
library(tidyverse)
library(vegan)
#Let us start the process of subsetting data we need to answer the question
#QUESTION: How plant composition (richness) was changing across years of veg surveys in 4 habitat types:
#"Brackish" "Freshwater" "Intermediate" "Saline". Column In.Out records plants outside and inside the survey plot.

setwd("~/Desktop/CRMS/CRMS")
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#293757 obs. of  24 variables:
levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline"  "Swamp"
length(levels(veg$SpecCode))#747, AaaProblem and  Wete are gone as these were not species (bare ground, Wate (open water))see comments in "CRMSDataCleaning" R file
veg$Community<-factor(veg$Community, levels = c( "Swamp","Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels

#Where is Phragmites? Phrag Cover Plot============
Phragmites<-filter(veg, SpecCode == "Phraaust") #subsetting only data we are most interested in code 1&2 = tavy
str(Phragmites)#3031 obs. of  28 variables

#Custome made theme:
theme_pw2<-theme(axis.text.y=element_text(size=22),
                 axis.title.y=element_text(size=24),
                 axis.title.x=element_text(size=24),
                 axis.text.x=element_text(size=18, angle = 90),
                 panel.grid.minor.x = element_blank(),
                 legend.position = "none",
                 strip.text = element_text(size= 22),
                 plot.title = element_text(lineheight=1.1, face="bold", size = 25, hjust = 0.5),
                 legend.text = element_text(size = 18),
                 legend.title = element_text(size = 18))


j1<-ggplot(data = Phragmites, aes(as.factor(year), Cover),shape = Community, color = as.factor(year)) 
j2<-j1+ geom_boxplot(outlier.size = 0) #+geom_jitter(position=position_jitter(width=1), alpha=0.7) 
j3<-j2+theme_bw()#+scale_color_manual(values=c(c("#2FEB7A", "red"))) + scale_shape_manual(values=c(15,17)) 
j4<-j3 + xlab("Year") + ylab("Cover (%)")+ facet_grid(~Community)
PhragCover<-j4+ggtitle("Cover (%) of Phragmites australis: Louisiana Coast (2007-2016)") +theme_pw2
PhragCover
#Swamp contains no Phragmites! Most Phragmites record in intermiedate.

#N of our Stations:=========
samples2006<-veg[veg$year ==2006 & veg$Community != "Swamp",] 
length(levels(droplevels(samples2006$StationID)))#1952 = number of stations
#To compare with n of station samples in 2007-2016:
samples2007_2016<-veg[veg$year !=2006,]
length(levels(droplevels(samples2007_2016$StationID)))#4127 = N of Stations were recorded in 2007-2016

#Let us subset the stations present in year 2007 from all data.
#We also remove all records from Swamp station as they contain no records our target species Phragmites:
samples2007<-veg[veg$year ==2007 & veg$Community != "Swamp",] 
length(levels(droplevels(samples2007$StationID)))#3158 = number of stations

#Subset only these 2007 station from entire data set to have a
#consistent set of plot across years:
ourDF<- veg[ which (veg$StationID  %in%  samples2007$StationID), c("year", "StationID", "StationFront","StationBack", "SpecCode", "Cover", "Community")]
str(ourDF)#211988 obs. of  7 variables:
table(ourDF$year)# a balanced sampling!!!!

# We decided to skip year 2006 (small n of plots in 2006)
DF2007to2016<- ourDF[ourDF$year!="2006",]
str(DF2007to2016)#200864 obs. of  7 variables:

#Compute Plant richness per one plot across years===============
#Turn Cover values into Presence/Absence values
#to compute richness:
range(DF2007to2016$Cover)# 0 100
DF2007to2016$Cover <-ifelse ( DF2007to2016$Cover == 0,0,1)

#Richness in a random plot with no Phra==========
#we need to turn our long one-plot data to wide data:
#Station CRMS0570-V05 is random choice:
CRMS0570<-DF2007to2016[DF2007to2016$StationID =="CRMS0570_V05",]
CRMS0570

CRMS0570.wide<-spread(CRMS0570,key = SpecCode, value = Cover, fill = 0)#species indices can be computed only when species are 
str(CRMS0570.wide)#8 obs. of  17 variables:
CRMS0570.wide
CRMS0570.wide$Richness <-specnumber(as.matrix(CRMS0570.wide[,6:17]))#compute richness
range(CRMS0570.wide$Richness)#1 3 = super low!!!

#Richness in a random plot with Phra==========
#Station CRMS0156-V81 is random choice with Phragmites in ti:
CRMS0156<-DF2007to2016[DF2007to2016$StationID =="CRMS0156_V81",]
CRMS0156
#Transform long data to wide data to compute richness
CRMS0156.wide<-spread(CRMS0156,key = SpecCode, value = Cover, fill = 0)#species indices can be computed only when species are 
str(CRMS0156.wide)#9 obs. of  10 variables:
#For vegan we need to have a species-only matrix:
veg.matrix<-CRMS0156.wide[,6:10]#let us subset a species matrix only
CRMS0156.wide$Richness <-specnumber(veg.matrix)
range(CRMS0156.wide$Richness)#0 5 = quite low.
#TO remember: PLANT SPECIES FOUND OUTSIDE THE PLOT WERE ALSO MARKED WITH 0 COVER!



#In-plot Richness Overall =================
#setwd("~/Desktop/CRMS/CRMS") #set working directory
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#293757 obs. of  24 variables:

NoSwamp<-veg[veg$Community != "Swamp",] #Swamp contains no Phragmites! Remove swamp.
NoSwamp$Cover <- ifelse(NoSwamp$Cover ==0,0,1)
v<-NoSwamp[,c("StationID","StationFront","Community","SpecCode","Cover","CoverTotal","year")]
v.wide<-spread(v,key = SpecCode, value = Cover, fill = 0)#species indices can be computed in a wide format only= each species has its own column.
veg.matrix<-v.wide[,7:610]#let us subset a species matrix only, avoiding AaaProblem!
v.wide$Richness <-specnumber(veg.matrix)
dim(v.wide)#34730 obs. of  611 variables when ,"CoverTotal" included
range(v.wide$Richness)#  0 30



#consistent set of plot across years, based on 2007 set of  plots:
only2007<-NoSwamp[NoSwamp$year ==2007 ,] #we want to look at the plots that were present in survey from 2007 to 2016
str(only2007)#20880 obs. of  26 variables:

ourDF<- v.wide[ which (v.wide$StationID  %in%  only2007$StationID),]
str(ourDF)#31602 obs. of  611 variables:
ourDF <- ourDF[ourDF$year!="2006",]#removing 2006 year.
str(ourDF)#29689 obs. of  611 variables:

library(Rmisc)
P<-ourDF[ , c("Community","Richness", "year")]
bar <- summarySE(P, groupvars = c("Community","year"),measurevar = "Richness")
bar#computed means +- SE
#always have Freshwater, Intermediate, Brackish, Saline order
bar$Community<-factor( bar$Community, levels = c("Freshwater", "Intermediate", "Brackish", "Saline"))
pd<-position_dodge(0.9)
theme_pw4<-theme(axis.text.y=element_text(size=20),
                 axis.title.y=element_text(size=22),
                 axis.title.x=element_text(size=22),
                 axis.text.x=element_blank(),
                 strip.text = element_text(size=15),
                 panel.grid.minor.x = element_blank(),
                 legend.position = "bottom",
                 plot.title = element_text(lineheight=1.1, face="bold", size = 23, hjust = 0.5))

f<-ggplot(bar, aes(x=Community, y=Richness, fill = Community, width=.75))
f1<-f + geom_bar(position=pd, stat="identity")+ geom_errorbar(aes(ymin=Richness-se, ymax=Richness+se),width=.4, position=pd)
f2<-f1 +scale_fill_manual(values=c("darkblue","lightblue","grey", "black"))  
f3<-f2 + xlab("Community Type") + ylab("Species Richness") + theme_pw4
Richness.Barplot<-f3+ggtitle("In-Plot Species Richness in LA Coastal Communities (2007-2016)") +facet_grid(.~year)
Richness.Barplot









#In-and-Outside plot Richness Overall =================
#setwd("~/Desktop/CRMS/CRMS") #set working directory
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#293757 obs. of  24 variables:

NoSwamp<-veg[veg$Community != "Swamp" ,] #Swamp contains no Phragmites! Remove swamp.
NoSwamp$Cover <- ifelse(NoSwamp$Cover == 0,1,1) #This will include species with Cover = 0 (present outside the plot)
v<-NoSwamp[,c("StationID","StationFront","Community","SpecCode","Cover","CoverTotal","year")]
v.wide<-spread(v,key = SpecCode, value = Cover, fill = 0)#species indices can be computed in a wide format only= each species has its own column.
dim(v.wide)#34730   611
veg.matrix<-v.wide[,6:611]#let us subset a species matrix only, avoiding AaaProblem!
v.wide$Richness <-specnumber(veg.matrix)
str(v.wide)#34730 obs. of  611 variables when ,"CoverTotal" included
range(v.wide$Richness)#  0 28


#consistent set of plot across years, based on 2007 set of  plots:
only2007<-NoSwamp[NoSwamp$year ==2007 ,] #we want to look at the plots that were present in survey from 2007 to 2016
str(only2007)#20880 obs. of  26 variables:

ourDF<- v.wide[ which (v.wide$StationID  %in%  only2007$StationID),]
str(ourDF)#31602 obs. of  611 variables:
ourDF <- ourDF[ourDF$year!="2006",]#removing 2006 year.
str(ourDF)#29689 obs. of  611 variables:

library(Rmisc)
P<-ourDF[ , c("Community","Richness", "year")]
bar <- summarySE(P, groupvars = c("Community","year"),measurevar = "Richness")
bar#computed means +- SE
#always have Freshwater, Intermediate, Brackish, Saline order
bar$Community<-factor( bar$Community, levels = c("Freshwater", "Intermediate", "Brackish", "Saline"))
pd<-position_dodge(0.9)
theme_pw4<-theme(axis.text.y=element_text(size=20),
                 axis.title.y=element_text(size=22),
                 axis.title.x=element_text(size=22),
                 axis.text.x=element_blank(),
                 strip.text = element_text(size=15),
                 panel.grid.minor.x = element_blank(),
                 legend.position = "bottom",
                 plot.title = element_text(lineheight=1.1, face="bold", size = 23, hjust = 0.5))

f<-ggplot(bar, aes(x=Community, y=Richness, fill = Community, width=.75))
f1<-f + geom_bar(position=pd, stat="identity")+ geom_errorbar(aes(ymin=Richness-se, ymax=Richness+se),width=.4, position=pd)
f2<-f1 +scale_fill_manual(values=c("darkblue","lightblue","grey", "black"))  
f3<-f2 + xlab("Community Type") + ylab("Species Richness") + theme_pw4
Richness.Barplot<-f3+ggtitle("In-and-Outside Plot Species Richness in LA Coastal Communities (2007-2016)") +facet_grid(.~year)
Richness.Barplot







