# Accretion data

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotrix)
library(vegan)
library(nlme)
library(chron)
library(vegan)


##### General cleaning and averaging  ######
acc<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/CoastalMarsh/CRMS/Figures&Stats/CRMS_Accretion.csv")
head(acc)

#The station ID is unique to a core that was inserted on one particular date and then extracted on multiple other dates. The Group refers to the year that the core was started, PS1 is 2008, PS2 is 2010, PS3 is 2012, PS4 is 2014. There are multiple cores started in for example in 2008 at a given station front (CRMS0002)

#Sometimes multiple cores were taken on a particular date at a particular stationID b/c the first core was poor for example, but many have at least one measurement for multiple cores if they were taken out on the same date. average the 4 measurements per core first, then average over the two or more cores at each sampling time. Then delete all the NaN values (due to cores being taken but being bad and no measuremnts were written)
acc2<-acc%>%
  mutate(accmm1=rowMeans(dplyr::select(acc, starts_with("Accretion")), na.rm = TRUE))%>%
  mutate(StationFront=Station.ID)%>%
  separate(StationFront,into=c("StationFront","StationBack"),sep="-")%>%
  group_by(Station.ID,StationFront,StationBack,Group, Sample.Date..mm.dd.yyyy.,Establishment.Date..mm.dd.yyyy.)%>%
  summarise(accmm=mean(accmm1,na.rm=T))%>%
  group_by(Station.ID)%>%
  arrange(chron(as.character(Sample.Date..mm.dd.yyyy.)),.by_group=TRUE)%>%
  filter(is.na(accmm)==F)%>%
  filter(!(Station.ID=="CRMS0392-A05"&Sample.Date..mm.dd.yyyy.=="2/11/2009"))#delete things that look odd (i think they recorded it in cm not mm): CRMS0392-A05 in 2009

dim(acc2) #227726 x 7
#acc2[which(acc2$Station.ID=="CRMS0669-A01"),]
as.data.frame(acc2)[1:10,]





##### Calculating one accretion rate per plot via regression #####
#using mthods from Jankowski et al 2017, they averaged the measurements from one core, then averaged the measurements from all cores taken at one time period. Then did regressions (using a slope and intercept, and not including a 0,0 point)

acc2$sampledate<-chron(as.character(acc2$Sample.Date..mm.dd.yyyy.))
acc2$estabdate<-chron(as.character(acc2$Establishment.Date..mm.dd.yyyy.))

#trying it out for the plot Jankowski shows in the supplement. it looks like Jankowski only used PS1, I don't see why we shouldn't use all cores regardless of when they were initiated
test<-acc2%>%
  filter(StationFront=="CRMS0549")%>%# CRMS0549 CRMS0174
  mutate(days=sampledate-estabdate,years=days/365)%>%
  group_by(Group,Sample.Date..mm.dd.yyyy.,days,years)%>%
  summarise(meanaccmm=mean(accmm))%>%
  filter(meanaccmm>0)#,Group=="PS1" ,Group%in%c("PS3","PS4")

as.data.frame(test)[1:30,]
test<-as.data.frame(test)
plot(test$years,test$meanaccmm)
coef(lm(meanaccmm~years,data=test))[2]
lm(meanaccmm~days,data=test)

#Doing calcs on all plots
#after chcking the data, I filtered plots that had fewer than 5 measurements, since a slope on n=4 is sketchy. this removed the negative slopes I was getting.
acc2a<-acc2%>%
  mutate(days=sampledate-estabdate,years=days/365)%>%
  group_by(StationFront,Group,Sample.Date..mm.dd.yyyy.,days,years)%>%
  summarise(meanaccmm=mean(accmm))%>%
  filter(meanaccmm>0)%>%
  group_by(StationFront)%>%
  summarise(acc=coef(lm(meanaccmm~years))[2],n=n())%>%
  filter(n>4)
as.data.frame(acc2a)[1:30,]

#check that it is same as above, yes
acc2a[acc2a$StationFront=="CRMS0549",]
sort(acc2a$acc)
acc2a[which(acc2a$acc<0),]
acc2a[which(acc2a$acc>100),]
# the plot CRMS0174 has a value over 100mm/yr, this looks actually correct based on the data, although there is a note in the raw data file that one mreasurement was affected by a large storm deposit, so I could delete if it looks really odd. there is no veg data from this station








##### Calculating yearly accretion rates for each plot #####

#need to check that the number of unique(Station>ID) is equal to the number of places where sample and establishment dates are the same
length(unique(acc2$Station.ID)) #5108 unique stations
length(which(as.character(acc2$Sample.Date..mm.dd.yyyy.)==as.character(acc2$Establishment.Date..mm.dd.yyyy.))) #5100 have sample=estab
#8 more station.ids compared to when sample and establ are same date

indwith<-acc2$Station.ID[which(as.character(acc2$Sample.Date..mm.dd.yyyy.)==as.character(acc2$Establishment.Date..mm.dd.yyyy.))]
todelete<-setdiff(unique(acc2$Station.ID),indwith)

acc3<-acc2%>%
  filter(!(Station.ID%in%c(todelete)))

acc3$accmmprev<-c(NA,acc3$accmm[1:(length(acc3$accmm)-1)])
acc3$sampledateprev<-c(chron("02/12/16"),chron(as.character(acc3$Sample.Date..mm.dd.yyyy.))[1:(length(acc3$Sample.Date..mm.dd.yyyy.)-1)]) #make the previous sampling date for the first line arbitrarily in the future

dim(acc2)
dim(acc3)
head(acc3)

#delete all rows where the sample date and the establishment dates are equal
acc4<-acc3%>%
  filter(as.character(Sample.Date..mm.dd.yyyy.)!=as.character(Establishment.Date..mm.dd.yyyy.))

head(as.data.frame(acc4))
dim(acc4)

#now I'm ready to calculate accumulation per time period per day. for each year, sum the total amount of accretion, and the total amount of days (since there are often two sampling tims per year). then divide
acc4$sampledate<-chron(as.character(acc4$Sample.Date..mm.dd.yyyy.))

#here is where I average over all the station backs
acc5<-acc4%>%
  mutate(accmmdif=accmm-accmmprev,days=sampledate-sampledateprev,Sample.Date..mm.dd.yyyy.2=Sample.Date..mm.dd.yyyy.)%>%
  separate(Sample.Date..mm.dd.yyyy.2,into=c("smonth","sday","syear"),sep="/")%>%
  group_by(Station.ID, syear)%>%
  summarise(accmm=sum(accmmdif),days=sum(days))%>%
  mutate(accmmpery=accmm/days*365)%>%
  mutate(StationFront=Station.ID)%>%
  separate(StationFront,into=c("StationFront","StationBack"),sep="-")%>%
  group_by(StationFront,syear)%>%
  summarise(meanaccmmpery=mean(accmmpery))%>%
  mutate(syear=as.numeric(syear))

as.data.frame(acc5)[1:20,]

#checking
hist(acc5$accmmpery)
sort(acc5$accmmpery)
min(acc5$accmmpery)
acc5[which(acc5$accmmpery==min(acc5$accmmpery)),]
acc5[which(acc5$accmmpery<(-330)),]

#up above delete things that look odd: CRMS0392-A05 in 2009


#merging accretion with veg data          
#first define community type by most common type in stationfront
#then average species comp and div and species rich
#then merge accretion data with plant community data (veg4)

veg5<-veg4%>%
  mutate(StationFront=StationID)%>%
  separate(StationFront,into=c("StationFront","StationBack"),sep="_")

#Define the community-type in each StationFront based on n of Commuity-type Counts per station:
StationComm <- group_by(veg5,StationFront,Community) %>% 
  count(Count=StationFront)
StationComm#It gives us count of communities per StationFront (740)
SCwide<- spread(StationComm, key = Community, value = n, fill = 0)#make it wide
SCwide$WhichMax<-colnames(SCwide)[apply(SCwide,1,which.max)]#while wide we can see which Comm is predominant
SCwide
StationCommDefined<-SCwide[, c(1,7)]
colnames(StationCommDefined)[2] <- "Community" #Renaming WhichMAx back to Community
StationCommDefined #320 stationFronts

#join and then summarize means across species/diversity by stationfront and year. there will be an NA for meanaccmmpery when there is veg data but no acc data
veg6<-veg5%>%
  left_join(StationCommDefined,by="StationFront")%>%
  group_by(StationFront,Community.y,year)%>%
  select(-Community2007)%>%
  summarise_at(vars(CoverTotal:tot),mean,na.rm=T)%>%
  left_join(acc5,by=c("StationFront","year"="syear"))

dim(veg6)
as.data.frame(acc5)[1:40,]
as.data.frame(veg6)[1:15,]
#just double checking that it did it correctly
#veg5[which(veg5$StationFront=="CRMS0002"&veg5$year==2007),"shannon"]
#CRMS0002 in 2007 .7427 

#I'm gtting the same pattern as Jankowski that fresh and saline ahve highest rates of accretion, inter/brack lower rates. However, my numbers here are much higher than theirs (theirs frsh 12.2, mine frsh 20.6)
veg6%>%
  group_by(Community.y,year)%>%
  summarise(meandiv=mean(shannon),meancovertotal=mean(CoverTotal),meanacc=mean(meanaccmmpery,na.rm=T),seacc=std.error(meanaccmmpery,na.rm=T))

veg7<-veg6%>%
  group_by(Community.y,year)%>%
  summarise(meandiv=mean(shannon),meancovertotal=mean(CoverTotal),meanacc=mean(meanaccmmpery,na.rm=T),seacc=std.error(meanaccmmpery,na.rm=T))
as.data.frame(veg7)

#plot accretion over time 
ggplot(veg7,aes(x=year,y=meanacc,color=Community.y))+
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=meanacc-seacc, ymax=meanacc+seacc),width=.4)

#plot accretion vs diversity
ggplot(veg7,aes(x=meandiv,y=meanacc,color=Community.y))+
  geom_point() +
  geom_line(stat="smooth",method = "lm",size=.8) +
  facet_wrap(~Community.y,scale="free") 

ggplot(veg6,aes(x=Sagilanc,y=meanaccmmpery,color=Community.y,group=Community.y))+
  geom_point() +
  geom_line(stat="smooth",method = "lm",size=.8) +
  facet_wrap(~Community.y,scale="free") 

Vignlute      Sparalte      Altephil      Coloescu      Polypunc      Echiwalt Phraaust

#ordination
veg7<-veg6%>%
  filter(!is.na(meanaccmmpery)==T)
vegv<-veg7[,5:434]
vegv2<-vegv[,which(colSums(vegv>0)>2)]
m1<-capscale(vegv2~meanaccmmpery+Condition(Community.y),distance="bray",data=veg7)#,na.action=na.omit
summary(m1)
plot(m1,type="n",display="sp",xlim=c(-4,4))
plot(1:10,1:10,type="n",xlim=c(-1,1),ylim=c(-10,5))
text(scores(m1)$species,row.names(scores(m1)$species))
anova(m1)

sort(scores(m1,display="species")[,1])







#old stuff
# acc2<-acc%>%
#   mutate(Sample.date=Sample.Date..mm.dd.yyyy.)%>%
#   separate(Sample.date,into=c("smonth","sday","syear"),by="/")%>%
#   mutate(Establishment.Date=Establishment.Date..mm.dd.yyyy.)%>%
#   separate(Establishment.Date,into=c("emonth","eday","eyear"),by="/")%>%
#   mutate(times=ISOdate(syear,smonth,sday)-ISOdate(eyear,emonth,eday))%>%
#   mutate(timed=as.numeric(times/60/60/24))%>%
#   mutate(accmm=rowMeans(select(acc, starts_with("Accretion")), na.rm = TRUE))%>%
#   mutate(accmmyr=accmm/timed*365)
# 
# hist(acc2$timed)
# ISOdate(2001, 4, 26) - ISOdate(2001, 2, 26)
# ISOdate(acc2$syear[2],acc2$smonth[2],acc2$sday[2])-ISOdate(acc2$eyear[2],acc2$emonth[2],acc2$eday[2])
# 
