#Veg data

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotrix)
library(vegan)
library(nlme)
library(chron)
library(vegan)

##### Raw veg data #####
#5 => 75 percent cover; 4 = 50-75 percent cover; 3 = 25-50 percent cover; 2 = 5-25 percent cover; 1 = numerous, but less than 5 percent cover, or scattered, with cover up to 5 percent; + = few, with small cover; and r = rare, solitary, with small cover.
veg <- read.csv("CRMS_Marsh_Veg.csv")
levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline"
veg$Community<-factor(veg$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))

length(unique(veg$StationID))
length(unique(veg$StationFront))


#Could filter out plots sampled in month 10 or 11 or 12 if I wanted to try harder to make sure that post-hurricanes were not sampled. or I could only look at plots sampled in 10,11,12 if I wanted to make sure only post hurricane is sampled, however there aren't many plots like this
#Make data wide
veg2<-veg%>%
  select(StationID,StationFront,StationBack,month,day,year,Community,CoverTotal,Cover,SpecCode)%>%
  spread(SpecCode,Cover,fill=0)
veg2[1:15,1:15]
colnames(veg2)
max(veg2$year)

#Make sure each plot has a community for every year (or at least 6 of the 10 years) 2007-2016
table(veg2$year)

counts<-veg2%>%
  group_by(StationID)%>%
  summarise(n())
counts2<-counts$StationID[which(counts$`n()`>5)]
veg3<-veg2[which(veg2$StationID%in%counts2),]
length(unique(veg3$StationID))
table(veg3$year)
#there must be some duplicate years

#take out the october measurement from years that are duplicates
counts<-veg3%>%
  group_by(StationID,year)%>%
  summarise(n())
counts[which(counts$`n()`!=1),]

veg3<-veg3[-which(veg3$StationID=="CRMS0121_V34"&veg3$month==10),]#
veg3<-veg3[-which(veg3$StationID=="CRMS3601_V27"&veg3$month==10),]
veg3<-veg3[-which(veg3$StationID=="CRMS3601_V57"&veg3$month==10),]
table(veg3$year)

veg3[1:15,1:15]


# make a commtype2007 vector
# instead of doing this, I switched to use Pawel's method of taking the community type that was most common
# Commtype2007<-veg3 %>%
#   filter(year==2007)%>%
#   select(StationID,Community)
# 
# length(unique(veg3$StationID))-dim(Commtype2007)[1]
# #342 stations were not sampled in 2007
# notsampled2007<-setdiff(veg3$StationID,Commtype2007$StationID)
# 
# Commtype2008<-veg3 %>%
#   filter(year==2008,StationID %in% as.factor(notsampled2007))%>%
#   select(StationID,Community)
# dim(Commtype2008)
# #225 sampled in 2008, 117 left
# 
# notsampled20072008<-setdiff(notsampled2007,Commtype2008$StationID)
# length(notsampled20072008)
# 
# Commtype2009<-veg3 %>%
#   filter(year==2009,StationID %in% as.factor(notsampled20072008))%>%
#   select(StationID,Community)
# dim(Commtype2009)
# #77 more, 40 left
# 
# notsampled200720082009<-setdiff(notsampled20072008,Commtype2009$StationID)
# length(notsampled200720082009)
# 
# Commtype2010<-veg3 %>%
#   filter(year==2010,StationID %in% as.factor(notsampled200720082009))%>%
#   select(StationID,Community)
# dim(Commtype2010)
# #31, 9 left
# 
# notsampled2007200820092010<-setdiff(notsampled200720082009,Commtype2010$StationID)
# length(notsampled2007200820092010)
# 
# Commtype2011<-veg3 %>%
#   filter(year==2011,StationID %in% as.factor(notsampled2007200820092010))%>%
#   select(StationID,Community)
# dim(Commtype2011)
# 
# Commtype2007b<-rbind(Commtype2007,Commtype2008,Commtype2009,Commtype2010,Commtype2011);colnames(Commtype2007b)[2]<-"Community2007"
# 
#this is where I merged them before
#veg4<-merge(veg3,Commtype2007b)


#Using Pawel's method:
#first define community type by most common type in stationfront
#then average species comp and div and species rich
#then merge accretion data with plant community data (veg4)

#Define the community-type in each StationFront based on n of Commuity-type Counts per station:
StationComm <- group_by(veg3,StationFront,Community) %>% 
  count(Count=StationFront)
StationComm#It gives us count of communities per StationFront (740)
SCwide<- spread(StationComm, key = Community, value = n, fill = 0)#make it wide
SCwide$WhichMax<-colnames(SCwide)[apply(SCwide,1,which.max)]#while wide we can see which Comm is predominant
SCwide
StationCommDefined<-SCwide[, c(1,7)]
colnames(StationCommDefined)[2] <- "Community" #Renaming WhichMAx back to Community
StationCommDefined #320 stationFronts


#Join with veg3 to create a plot/year-level (StationID*year level) data set
veg4<-veg3%>%
  rename(Community.yr=Community)%>%
  left_join(StationCommDefined,by="StationFront")
  
veg4$richness<-specnumber(veg4[,9:438])
veg4$shannon<-diversity(veg4[,9:438],index="shannon")
veg4$tot<-rowSums(veg4[,9:438])

#rearrange columns
veg5<-veg4%>%
  select(StationID:Community.yr,Community,CoverTotal,richness:tot,Acerrubr:ZiziMill)


#Then create a StationFront*year-level dataset
#Summarize means across species/diversity by stationfront and year. there will be an NA for meanaccmmpery when there is veg data but no acc data
veg6<-veg5%>%
  group_by(StationFront,Community,year)%>%
  summarise_at(vars(CoverTotal:ZiziMill),mean,na.rm=T)

#then replace richness, shannon, and tot, if you want to recalculat them based on the new averaged stationfront-level species data (rather than have them be the average across the small plots)
veg6$richness<-specnumber(veg6[,8:437])
veg6$shannon<-diversity(veg6[,8:437],index="shannon")
veg6$tot<-rowSums(veg6[,8:437])


#Then create a StationFront-level dataset (average over years)
veg7<-veg6%>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(CoverTotal:ZiziMill),mean,na.rm=T)



#veg only questions
#Does phrag increase over time?

m1<-veg5%>%
  group_by(year,Community)%>%
  summarise(mean=mean(Phraaust),se=std.error(Phraaust))
ggplot(m1,aes(x=year,y=mean,color=Community))+
  labs(x = "",y="",colour="Community type")+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community,scale="free")



