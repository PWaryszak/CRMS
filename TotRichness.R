#Load packages:
library(vegan)
library(tidyr)
library(Rmisc)
library(ggplot2)

#LOAD DATA off Internet (Clean Data, quite big):
CRMSveg<- read.csv(url("https://sites.google.com/site/phragmitesproject/file-cabinet/VegDataAll.csv?attredirects=0"))
dim(CRMSveg)#25580 rows & 404 = 404 cols of plant species
CRMSenv<- read.csv(url("https://sites.google.com/site/phragmitesproject/file-cabinet/EnvDataAll.csv?attredirects=0"))
veg <-cbind(CRMSveg,CRMSenv)#Bind by columns not rows.
str(veg)#25580 obs. of  415 variables:
unique(veg$year)

#Create veg matrices for each year:====
veg$TotRichness <- specnumber(veg[ ,1:404])
range(veg$TotRichness)# range of total richness = from 0 to 30
table(veg$TotRichness)#contingence table of TotRichness.
#write.csv(veg, file = "VegConsistentPlotsData.csv", row.names = FALSE)

#Change levels in Community as per salinity gradient:
veg$Community<- factor(veg$Community, levels = c("Freshwater", "Intermediate", "Brackish","Saline"))

#TotRichness Barplot=======
names(veg)#check names
#Extra Clean - there is not data for 2017, NA instead:
veg2<- veg [ ! is.na(veg$year),] #removes all NA-s
dim(veg2)#25425   416
dim(veg)#25580   416

RichData<-summarySE(veg2, measurevar="TotRichness", groupvars=c("year","Community"))
RichData

pd<-position_dodge(0.9)
theme_pw<-theme(axis.text.y=element_text(size=20),
                 axis.title.y=element_text(size=22),
                 axis.title.x=element_text(size=22),
                 axis.text.x=element_blank(),
                 strip.text = element_text(size=15),
                 panel.grid.minor.x = element_blank(),
                 legend.position = "bottom",
                 plot.title = element_text(lineheight=1.1, face="bold", size = 23, hjust = 0.5))

MyCanvas<-ggplot(RichData, aes(x=Community, y=TotRichness, fill = Community, width=.75))
MyCanvas
Barplot<- MyCanvas + geom_bar(position=pd, stat="identity")+ geom_errorbar(aes(ymin=TotRichness-se, ymax=TotRichness+se),width=.4, position=pd)
Barplot
Barplot2<-Barplot +scale_fill_manual(values=c("darkblue","lightblue","grey", "black"))  
Barplot2
Barplot3<-Barplot2 + xlab("Community Type") + ylab("Species Richness") + theme_pw
Barplot3

Richness.Barplot<-Barplot3+ggtitle("Species Richness in LA Coastal Communities (2007-2016)") +facet_grid(.~year)
Richness.Barplot
ggsave(filename = "RichnessBarplot.png", width = 10, height = 4, dpi = 300)#saves png in working directory


#Plot histogram of Richness with grey background of all TotRichness values=========
#Source:https://drsimonj.svbtle.com/plotting-background-data-for-groups-with-ggplot2

#Frequencies of Richness Values acros 4 Communities (per plot):
#Background data (in grey) shows the frequencies of all TotRichness Values (regardless the Community)
#Frequencies of Mean Richness Values acros 4 Comms:
d<-veg[ ,c("year", "Community","TotRichness")]# Full data set needed
d_bg <- data.frame(TotRichness = d[, "TotRichness"])  # Background Data needed

ggplot(d, aes(x = TotRichness, fill = Community)) +
  geom_histogram(data = d_bg, fill = "grey", alpha = .5) +
  geom_histogram(colour = "black") + xlab("Total Richness Values (per plot)")+ ylab("Count")+
  facet_wrap(~ Community, nrow = 1) + ggtitle("Richness frequencies across four Communities \n Lousiana Coast (2007-2016)")+
  guides(fill = FALSE) +  # to remove the legend
  theme_bw() +theme(axis.text.y=element_text(size=20),
       axis.title.y=element_text(size=22),
       axis.title.x=element_text(size=22),
       strip.text = element_text(size=15),
       panel.grid.minor.x = element_blank(),
       legend.position = "bottom",
       plot.title = element_text(lineheight=1.1, face="bold", size = 23, hjust = 0.5))+
 ggsave(filename = "RichnessHistogram2.png", width = 10, height = 4, dpi = 300)









#OLD NOTES========
#Let us subset the plots (stationID) present in year 2007 from all data.
#We also remove all records from Swamp station as they contain no records our target species Phragmites:

#veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv in local directory
str(veg)#133612 obs. of  24 variables:
samples2007<-veg[veg$year == 2007,] 
length(levels(droplevels(samples2007$StationID)))#2558 = number of good plots in 2007 
length(levels(droplevels(samples2007$StationFront)))#282 = number of stations (sites) in 2007 

#Subset only these 2007 station from entire data set to have a
#consistent set of plot across years:

DF2007to2016<- veg[ which (veg$StationID  %in%  samples2007$StationID), c("year", "StationID", "StationFront","StationBack", "SpecCode", "Cover", "Community", "CoverTotal")]
str(DF2007to2016)#118570 obs. of  8 variables:
table(DF2007to2016$year)# a well balanced sampling!!!!
#2007  2008  2009  2010  2011  2012  2013  2014  2015  2016 
#11757 11225 11624 11692 11527 11573 11936 12473 12504 12259 

#Reshape to wide format to compute plant composition indices:
DF2007to2016$Cover <- ifelse(DF2007to2016$Cover ==0,0,1) #turning cover to presence/absence data
v<-DF2007to2016[,c("StationID","StationFront","Community","SpecCode","Cover","CoverTotal","year")] #select most important variables

v.wide<-spread(v,key = SpecCode, value = Cover, fill = 0)#species indices can be computed in a wide format only= each species has its own column.
str(v.wide)#25580 obs. of  409 variables:
#Compute richness for each row of species:
v.wide$TotRichness <- specnumber(v.wide[ ,6:410])
range(v.wide$TotRichness)# range of total richness = from 0 to 31
table(v.wide$TotRichness)#contingence table of TotRichness.
#write.csv(v.wide, file = "VegConsistentPlotsData.csv", row.names = FALSE)

#Compute lm(TotRichness~Community) using for-loop, each year =====
names(v.wide)
unique(v.wide$year)#2007 2008 2010 2009 2013 2015 2011 2012 2014 2016

#Change levels in Community as per salinity gradient:
v.wide$Community<- factor(v.wide$Community, levels = c("Freshwater", "Intermediate", "Brackish","Saline"))



#Compute lm(TotRichness~Community) using for-loop, each year =====
names(veg)
unique(veg$year)#2007 2008 2010 2009 2013 2015 2011 2012 2014 2016

#Automate LM with for-loop
Output <- NULL #we need to set an empty shelf for computations outputs to store on
for ( i in unique(unique(v.wide$year)) ){
  #create a subset data
  data_sub <- subset(v.wide, year== i)
  
  #create the linear model for each year
  datalm <- assign(paste0("lm.", i), round(coef(summary(lm(TotRichness~Community,data=data_sub))), digits =3))
  namelm <- paste0("Year.", i)#extra column indicating Year
  
  #save output of lm above with extra column for Year
  saveoutput <- data.frame(datalm, Year=namelm)
  Output <- rbind(Output, saveoutput)
}
Output
write.csv(Output, file = "CRMS_TotRichness_ANOVA_Output.csv")


#TotRichness Barplot=======
library(Rmisc)
names(v.wide)#check names
pa<-summarySE(v.wide, measurevar="TotRichness", groupvars=c("year","Community"))
pa
total<-summarySE(v.wide, measurevar="TotRichness")
total

library(ggplot2)
pd<-position_dodge(0.9)
theme_pw4<-theme(axis.text.y=element_text(size=20),
                 
                 axis.title.y=element_text(size=22),
                 
                 axis.title.x=element_text(size=22),
                 
                 axis.text.x=element_blank(),
                 
                 strip.text = element_text(size=15),
                 
                 panel.grid.minor.x = element_blank(),
                 
                 legend.position = "bottom",
                 
                 plot.title = element_text(lineheight=1.1, face="bold", size = 23, hjust = 0.5))



f<-ggplot(pa, aes(x=Community, y=TotRichness, fill = Community, width=.75))

f1<-f + geom_bar(position=pd, stat="identity")+ geom_errorbar(aes(ymin=TotRichness-se, ymax=TotRichness+se),width=.4, position=pd)

f2<-f1 +scale_fill_manual(values=c("darkblue","lightblue","grey", "black"))  

f3<-f2 + xlab("Community Type") + ylab("Species Richness") + theme_pw4

Richness.Barplot<-f3+ggtitle("Species Richness in LA Coastal Communities (2007-2016)") +facet_grid(.~year)

Richness.Barplot
