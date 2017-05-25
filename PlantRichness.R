#Load Data and Libraries==============
library(tidyverse)
library(vegan)

#Let us start the process of subsetting data we need to answer the question
#QUESTION: How plant composition (richness) was changing across years of veg surveys in 4 habitat types:
#"Brackish" "Freshwater" "Intermediate" "Saline" 

setwd("~/Desktop/CRMS/CRMS")
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#295644 obs. of  32 variables:
levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline"  "Swamp"
levels(veg$SpecCode)#750, AaaProblem needs to go as it codes lack of species/ see comments in file
veg$Community<-factor(veg$Community, levels = c( "Swamp","Freshwater","Intermediate","Brackish","Saline"))

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
#Swamo contains no Phragmites!

#N of our Stations:=========
samples2006<-veg[veg$year ==2006 & veg$Community != "Swamp",] 
length(levels(droplevels(samples2006$StationID)))#1968 = number of stations
#To compare with n of station samples in 2007-2016:
samples2007_2016<-veg[veg$year !=2006,]
length(levels(droplevels(samples2007_2016$StationID)))#4129 = N of Stations were recorded in 2007-2016
#It is a large increase in Stations over time.

#Let us subset the entire data including only the stations present in year 2007:
#We also remove all records from Swamp station as they contain no records our target species Phragmites:

samples2007<-veg[veg$year ==2007 & veg$Community != "Swamp",] 
length(levels(droplevels(samples2007$StationID)))#3166 = number of stations

#Subset only these 2007 station from entire data set to have a
#consisten set of plot across years:
ourDF<- veg[ which (veg$StationID  %in%  samples2007$StationID), c("year", "StationID", "StationFront","StationBack", "SpecCode", "Cover", "Community")]
str(ourDF)#213965 obs. of  8 variables:
table(ourDF$year)# a balanced sampling!!!!
#2006  2007  2008  2009  2010  2011  2012  2013  2014  2015  2016 
#12769 20888 18853 19695 19635 20373 20052 19827 20804 20755 20314

#year 2006 contains half as much of records again
# We decided to skip year 2006
DF2007to2016<- ourDF[ourDF$year!="2006",]
str(DF2007to2016)#201196 obs. of  7 variables:

#Compute Plant richness per one plot across years===============
#Turn Cover values into Presence/Absence values
#to compute richness:
range(DF2007to2016$Cover)# 0 100
sum(is.na(DF2007to2016$Cover))
DF2007to2016$Presence <-ifelse ( DF2007to2016$Cover == 0,0,1)

#Richness in a random plot with no Phra==========
View(DF2007to2016)
#we need to turn our long one-plot data to wide data:
#Station CRMS0570-V05 is random choice:
CRMS0570<-DF2007to2016[DF2007to2016$StationID =="CRMS0570-V05",]
CRMS0570

CRMS0570.wide<-spread(CRMS0570,key = SpecCode, value = Presence)#species indices can be computed only when species are 
str(CRMS0570.wide)#26 obs. of  20 variables:
CRMS0570.wide
#For vegan we need to have a species-only matrix:
veg.matrix<-CRMS0570.wide[,9:20]#let us subset a species matrix only, exluding AaaProblem!
sum(is.na(veg.matrix))# 279 = LOTS of NA that = zeros!!!
veg.matrix[is.na(veg.matrix)] <- 0 #turnig NA into 0 values
sum(is.na(veg.matrix))# = 0  YAY!

CRMS0570.wide$Richness <-specnumber(veg.matrix)
range(CRMS0570.wide$Richness)#0 1 = super low!!!

#Richness in a random plot with Phra==========
#Station CRMS0156-V81 is random choice with Phragmites in ti:
CRMS0156<-DF2007to2016[DF2007to2016$StationID =="CRMS0156-V81",]
CRMS0156
#in order to change it to wide format and compute Richness
#We have to remove Cover column beforehand:
drops <- "Cover"
CRMS0156b<- CRMS0156[, !(names(CRMS0156) %in% drops)]

#Transform long data to wide data to compute richness
CRMS0156.wide<-spread(CRMS0156b,key = SpecCode, value = Presence)#species indices can be computed only when species are 
str(CRMS0156.wide)#9 obs. of  10 variables:
CRMS0156.wide
#For vegan we need to have a species-only matrix:
veg.matrix<-CRMS0156.wide[,6:10]#let us subset a species matrix only, exluding AaaProblem!
sum(is.na(veg.matrix))# 27 = LOTS of NA that = zeros!!!
veg.matrix[is.na(veg.matrix)] <- 0 #turnig NA into 0 values
sum(is.na(veg.matrix))# = 0  YAY!

CRMS0156.wide$Richness <-specnumber(veg.matrix)
range(CRMS0156.wide$Richness)#0 5 = quite low.

#Data is tricky  See line 3 in:
CRMS0156.wide  # Zeros species but CoverTotal= 60%, Comments in raw data says: 
#"Water Level is Below Marsh. Bare Ground is dead plants and unvegetated surface." Cover = 0. 
#Dead plants = no plants then.

#Conclusion:
#Species richness is very low and hard to see impact of Phra on it. 
#It looks like Phra comes and goes in this CRMS0156 plot for example:

#CRMS0156 contains zero Covers we do not need but after looking at the comments in raw data
#we know they were 100% cover !! as they were hard to access:
#Changing 0 to 100:
CRMS0156$CoverReal<- ifelse( CRMS0156$Cover == 0, CRMS0156$Cover+100, CRMS0156$Cover+0 )
CRMS0156 #but coloescu located in row 4 was zero though:
CRMS0156 <-CRMS0156 [-4, ] #removing the false record

#Plot of cover across years with zero Phraaust:
cover1<-ggplot(CRMS0156, aes(year, Cover))
cover2<- cover1+ geom_point(aes(color = SpecCode, shape=SpecCode, size = Cover))
coverFinal<-cover2+theme_classic() + scale_size_continuous(range = c(5,10))
coverFinal

#Plot of cover across years with CoverReal of Phraaust:
cover1<-ggplot(CRMS0156, aes(year, CoverReal))
cover2<- cover1+ geom_point(aes(color = SpecCode, shape=SpecCode, size = CoverReal))
coverFinal2<-cover2+theme_classic() + scale_size_continuous(range = c(5,10))
coverFinal2





#Richness overall=================
#NO IDEA WHY SPREAD FUNCTION DOES NOT WORK PROPERLY. STOPPED HERE:

setwd("~/Desktop/CRMS/CRMS")
veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#295644 obs. of  32 variables:

#Let us subset the entire data including only the stations present in year 2007:
#We also remove all records from Swamp station as they contain no records our target species Phragmites:

#consistent set of plot across years, with 2007 plots:
samples2007<-veg[veg$year ==2007 & veg$Community != "Swamp",] 
ourDF<- veg[ which (veg$StationID  %in%  samples2007$StationID), c("year", "StationID", "StationFront","StationBack", "SpecCode", "Cover", "Community")]
str(ourDF)#213965 obs. of  8 variables:
# We decide to skip year 2006:
DF2007to2016.row<- ourDF[ourDF$year!="2006",]
str(DF2007to2016.row)#201196 obs. of  7 variables:

#we need to turn our long data to wide data:
DF2007to2016.row$row <- 1:nrow(DF2007to2016)#For big data we need to run this first to avoid:
#"Error: Duplicate identifiers for rows"
DF.wide<-spread(data = DF2007to2016.row,key = SpecCode, value = Cover,, fill = 0)#species indices can be computed only when species are 
str(DF.wide)#201196 obs. of  520 variables = YES exactly as dimension of orignal DF2007to2016 data

#For vegan we need to have a species-only matrix:
veg.matrix<-as.matrix(DF.wide[,8:520])#let us subset a species matrix only, exluding AaaProblem!
sum(is.na(veg.matrix))# 103 012 508 = LOTS of NA that = zeros!!!
veg.matrix[is.na(veg.matrix)] <- 0 #turnig NA into 0 values
sum(is.na(veg.matrix))# = 0  YAY!

range(rowSums(veg.matrix))


DF.wide$Richness <-specnumber(veg.matrix)
range(DF.wide$Richness)#0 14
table(DF.wide$Phraaust)
###0     1 
#1271 1324 #very balanced!


DF.wide$PhragIn<-ifelse(DF.wide$Phraaust == 0, "No.Phrag", "With.Phrag")


#SPecies Richness all:
DF.wide$Richness <-specnumber(veg.matrix)
range(DF.wide$Richness)#0 14
unique(DF.wide$year)#2007-2016
str(DF.wide)#data.frame':	84803 obs. of  520 variables::
#First let us turn Cover into Presence/absence data of our target Phraaust:
table(DF.wide$Phraaust)
###0     1 
#84072   731 
DF.wide$PhragIn<-ifelse(DF.wide$Phraaust == 0, "No.Phrag", "With.Phrag")


#Compute Av of Richness across 4 habitats=================
library(Rmisc)
Richness <- summarySE(DF.wide, groupvars = c("Community", "year"),measurevar = "Richness")
Richness

#Boxplot of Richness by community across years [with zero plots]=========
DF.wide$year<-as.factor(DF.wide$year)
Richness <- summarySE(DF.wide, groupvars = c("Community", "year"),measurevar = "Richness")
Richness2 <- Richness[Richness$Community !="Swamp",] #cut our swamp!
str(Richness2)
Richness2$year<-as.factor(Richness2$year)
Richness2$year <- factor(Richness2$year, levels = c("2006", "2007", "2008", "2009", "2010","2011","2012","2013","2014","2015", "2016"))
Richness2$Community<-as.factor(Richness2$Community)
Richness2$Community<-factor(Richness2$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))
DF.wide[is.na(DF.wide)] <- 0
str(Richness2)#44 obs. of  7 variables:

theme_pw3<-theme(axis.text.y=element_text(size=22),
                 axis.title.y=element_text(size=24),
                 axis.title.x=element_text(size=22),
                 axis.text.x=element_text(size=14,hjust = 0, angle = 90),
                 panel.grid.minor.x = element_blank(),
                 legend.position = "none",
                 strip.text = element_text(size= 22),
                 plot.title = element_text(lineheight=1.1, face="bold", size = 25,hjust = 0.5),
                 legend.text = element_text(size = 18),
                 legend.title = element_text(size = 18))


pd<-position_dodge(0.9)
f<-ggplot(Richness2, aes(x=year, y=Richness,fill = Community , width=.75))
f1<-f + geom_bar(position=pd, stat="identity")+ geom_errorbar(aes(ymin=Richness-se, ymax=Richness+se),width=.4, position=pd)
f2<- f1 +xlab("Year") + ylab("Plant Richness") + theme_classic()+theme_pw3 
f3<-f2 + facet_grid(~ Community)
f3
RichnessPlot <- f3 + ggtitle("Plant Richness (zero plots included)\n in Four Community Types (2006-2016)")
RichnessPlot


#Boxplot of Richness by community across years & PhragIn [without zero plots]=========

DF.wide$ZeroPlots<-ifelse(DF.wide$Richness==0,"zero","full")
sum(DF.wide$ZeroPlots=="zero")#15910 empty plots
sum(DF.wide$ZeroPlots=="full")#68893 with-something plots.
DF.wide2<-DF.wide[DF.wide$ZeroPlots=="full",]
DF.wide2<-DF.wide[DF.wide$Community !="Swamp",]

#Means if richness for Barplots:
r <- summarySE(DF.wide2, groupvars = c("Community", "year", "PhragIn"),measurevar = "Richness")
r

r$year<-as.factor(r$year)
r$year <- factor(r$year, levels = c("2006", "2007", "2008", "2009", "2010","2011","2012","2013","2014","2015", "2016"))
r$Community<-as.factor(r$Community)
r$Community<-factor(r$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))
r[is.na(r)] <- 0 #some plots produce NA as no Pgragmites recordec.
str(r)
theme_pw3<-theme(axis.text.y=element_text(size=22),
                 axis.title.y=element_text(size=24),
                 axis.title.x=element_text(size=22),
                 axis.text.x=element_text(size=14,hjust = 0, angle = 90),
                 panel.grid.minor.x = element_blank(),
                 legend.position = "none",
                 strip.text = element_text(size= 22),
                 plot.title = element_text(lineheight=1.1, face="bold", size = 25,hjust = 0.5),
                 legend.text = element_text(size = 18),
                 legend.title = element_text(size = 18))


pd<-position_dodge(0.9)
f<-ggplot(r, aes(x=year, y=Richness,fill = Community , width=.75))
f1<-f + geom_bar(position=pd, stat="identity")+ geom_errorbar(aes(ymin=Richness-se, ymax=Richness+se),width=.4, position=pd)
f2<- f1 +xlab("Year") + ylab("Plant Richness") + theme_classic()+theme_pw3 
f3<-f2 + facet_grid(PhragIn ~ Community)
f3
RichnessPlot2 <- f3 + ggtitle("Plant Richnes (2006-2016) \n by Phragmites occurance (With.Phrag & No.Phrag)")
RichnessPlot2


