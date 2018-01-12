
#This code is a bit messy, it contains code merging the veg (VegECF.R),  accretion (AccretionECF.R), and salinity (SalinityECF.R) data which I used to produce figures for the gulf research grant proposal.

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotrix)
library(vegan)
library(nlme)
library(chron)
library(vegan)

#gethub: https://github.com/PWaryszak/CRMS
#Pawel files: https://sites.google.com/site/phragmitesproject/file-cabinet
#CRMS raw data filse: https://cims.coastal.louisiana.gov/fulltableexports.aspx

save.image("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/CoastalMarsh/CRMS/Figures&Stats/CRMS.Rdata")  

load("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/CoastalMarsh/CRMS/Figures&Stats/CRMS.Rdata")


##### Accretion and veg data at the StationFront level ######
#use: veg7 and acc2a. some accretion will be NA if that plot did not get measured or was lacking enough samples for a regression

dat<-veg7%>%
  left_join(acc2a,by=c("StationFront"))%>%
  select(StationFront:tot,acc,Acerrubr:ZiziMill)
dim(dat)
dat$acc
dat$Community=factor(dat$Community,levels=c("Freshwater","Intermediate","Brackish","Saline"))

#Regression plots with CoverTotal, tot, shannon, richness
pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/Proposals/GulfResearchProgram2017/Figs/accvsdiversity.pdf",width=6,height=4)
ggplot(dat,aes(x=shannon,y=acc,color=Community))+
  labs(x = "Shannon diversity",y="Accretion (mm/yr)",colour="Community type")+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community,scale="free")
dev.off()

# ggplot(env3,aes(x=snow,y=meandist,col=snow)) +
#   theme(legend.position="none",text = element_text(size=20)) +
#   labs(x ="Snow depth",y="Dissimilarity 2007-2015") +
#   geom_point(size=3) +
#   geom_errorbar(aes(ymin=meandist-sedist,ymax=meandist+sedist),size=1,width=.5,stat="identity")

dat2<-dat%>%filter(Community=="Freshwater")
summary(lm(acc~shannon,data=dat2))

dat2<-dat%>%filter(Community=="Intermediate")
summary(lm(acc~shannon,data=dat2))

dat2<-dat%>%filter(Community=="Brackish")
summary(lm(acc~shannon,data=dat2))

dat2<-dat%>%filter(Community=="Saline")
summary(lm(acc~shannon,data=dat2))


dat3<-dat%>%
  filter(!is.na(acc)==T,Community=="Saline")
dat3v<-dat3[,8:437]
dat3v2<-dat3v[,which(colSums(dat3v>0)>2)]
m1<-capscale(dat3v2~acc+Condition(Community),distance="bray",data=dat3)#,na.action=na.omit
m1<-capscale(dat3v2~acc,distance="bray",data=dat3)#,na.action=na.omit
summary(m1)
plot(m1,display=c("cn","species"))
plot(m1,type="n",display="sp",xlim=c(-4,4))
plot(1:10,1:10,type="n",xlim=c(-1,2),ylim=c(-5,5))
text(scores(m1)$species,row.names(scores(m1)$species))
anova(m1)

sort(scores(m1,display="species")[,1])

Sparalte      Vignlute      Schoamer      Coloescu      Altephil 
Polypunc      Sagilati      Schodelt      Lythline      Sagiplat
Sagilanc Sparpate  Phraaust
ggplot(dat3,aes(x=Sparpate,y=acc,color=Community,group=Community))+
  geom_point() +
  geom_line(stat="smooth",method = "lm",size=.8) +
  facet_wrap(~Community,scale="free") 



###### merge the accretion/veg data with the salinity data #####
#env6 (from SalinityECF.R)

dat2<-dat%>%
  left_join(env6,by=c("StationFront"))%>%
  select(StationFront:acc,MeanSalinity,MaxSalinity,Acerrubr:ZiziMill)
dim(dat2)
as.data.frame(dat2[1:15,1:15])
dat2$MeanSalinity
dat2$acc

#Regression plots with CoverTotal, tot, shannon, richness
pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/Proposals/GulfResearchProgram2017/Figs/diversityvssalinity.pdf",width=6,height=4)
ggplot(dat2,aes(x=MeanSalinity,y=shannon,color=Community))+
  labs(x = "Salinity (ppt)",y="Shannon diversity",colour="Community type")+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community,scale="free")
dev.off()


#env7 (from code below including veg porewater and P plots)

dat2<-dat%>%
  left_join(env7,by=c("StationFront"))%>%
  select(StationFront:acc,MeanSalinity,MaxSalinity,Acerrubr:ZiziMill)
dim(dat2)
as.data.frame(dat2[1:15,1:15])
dat2$MeanSalinity
dat2$acc

#Regression plots with CoverTotal, tot, shannon, richness
pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/Proposals/GulfResearchProgram2017/Figs/diversityvssalinity.pdf",width=6,height=4)
ggplot(dat2,aes(x=MeanSalinity,y=shannon,color=Community))+
  labs(x = "Salinity (ppt)",y="Shannon diversity",colour="Community type")+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community,scale="free")
dev.off()











##### Raw data #####

##### Raw env data from Pawel's semi cleaned (?) soil data file #####
#env<-read.csv("CRMS_Soil.csv") #this file is old
env<-read.csv(url("https://sites.google.com/site/phragmitesproject/file-cabinet/CRMS_Soil.csv?attredirects=0"))
#write.csv(env,"/Users/farrer/Dropbox/EmilyComputerBackup/Documents/CoastalMarsh/CRMS/Figures&Stats/CRMS_Soil.csv",row.names = F)

head(env)
length(unique(env$StationID))
#for env data, the unit of measurement is StationID (which is composed of Station and site), even though there are many sites sampled within a station, they are unique measurements

#substitute - for _ so it is the same as the veg data
env$StationID<-as.factor(sub("-","_",env$StationID))

table(env$year)
#Remove year 2001, 2006 and 2017, remove error-looking outliers, keep only 10cm depth measurements:
env2<-env%>%
  filter(year%in%c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016),SoilPorewaterSalinity.ppt<56,SampleDepth.ft==0.328) %>% #all the ones above 56 look incorrect by comparing to conductance, either a decimal is missing or columns were switched around. The value at 54.7 looks correct. I didn't check the other high ones just beneath 56
  mutate(StationID.year=paste(StationID,year,sep="."))
hist(env2$SoilPorewaterSalinity.ppt)
max(env2$SoilPorewaterSalinity.ppt)
length(unique(env2$StationID))

#filter out any plots that had fewer than 4 or whatever (0) samplings per year
counts<-env2%>%
  group_by(StationID.year)%>%
  summarise(n())
counts2<-counts$StationID.year[which(counts$`n()`>0)]
env3<-env2[which(env2$StationID.year%in%counts2),]

#filter out plots sampled in sept-dec (if desired), and summarise means per year
env4<-env3%>%
  #filter(month%in%c(1,2,3,4,5,6,7,8,9))%>%
  group_by(StationID,year)%>%
  summarise(MeanSalinity=mean(SoilPorewaterSalinity.ppt),MaxSalinity=max(SoilPorewaterSalinity.ppt))

#then filter out plots that were missing any year
counts<-env4%>%
  group_by(StationID)%>%
  summarise(n())
counts2<-counts$StationID[which(counts$`n()`>9)]
env5<-env4[which(env4$StationID%in%counts2),]
length(unique(env4$StationID))

#ggplot(env5,aes(x=year,y=MeanSalinity,group=StationID))+#,,color=StationID
  #geom_point() +
  #geom_line()+
  #coord_cartesian(ylim = c(0,6))
  #geom_line(stat="smooth",method = "lm",size=.8)

env6<-env5%>%
  group_by(year)%>%
  summarise(mean=mean(MeanSalinity),max=mean(MaxSalinity))
ggplot(env6,aes(x=year,y=max))+
  geom_point() +
  geom_line(stat="smooth",method = "lm",size=.8)

#add-on, summarize plots at the StationFront level averaging over years
#I could maybe do this sequentially like the veg. first group by stationfront and year. then by stationfront. max would probably mean more in this context.
env7<-env4%>%
  mutate(StationFront=StationID)%>%
  separate(StationFront, into=c("StationFront","StationBack"),sep="_")%>%
  group_by(StationFront)%>%
  summarise(MeanSalinity=mean(MeanSalinity),MaxSalinity=mean(MaxSalinity))















##### Diversity vs. mean salinity by plot: do plots with higher salinity have lower diversity? #####
#use veg5 and env4
dat<-merge(veg5,env4,by=c("StationID","year"))
dim(dat)
dim(veg5)
head(env4)
head(veg5)

dat$Community<-factor(dat$Community,levels=c("Freshwater","Intermediate","Brackish","Saline"))

#intersect(veg4$StationID,env4$StationID)

dat2<-dat%>%
  group_by(Community,year)%>%
  summarise(meansal=mean(MeanSalinity),meandiv=mean(shannon),meanrich=mean(richness),meantot=mean(tot))

ggplot(dat2,aes(x=year,y=meansal,color=Community))+
  geom_point() +
  geom_line() 
  #geom_errorbar(aes(ymin=meanmean-semean, ymax=meanmean+semean),width=.4)

ggplot(dat2,aes(x=year,y=meandiv,color=Community))+
  geom_point() +
  geom_line() 

ggplot(dat2,aes(x=meansal,y=meandiv,color=Community))+
  labs(x = "Soil Salinity (ppt)",y="Plant Diversity",colour="Community type")+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community,scale="free")

options(contrasts=c("contr.helmert","contr.poly")) 
options("contrasts")
m1<-gls(meandiv~meansal*Community2007,data=dat2)
anova(m1,type="marginal")

ggplot(dat,aes(x=MeanSalinity,y=shannon,color=Community))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community,scale="free")


#at the station front level
dat<-merge(veg7,env7,"StationFront")
head(dat)
dim(dat)
dat$Community<-factor(dat$Community,levels=c("Freshwater","Intermediate","Brackish","Saline"))

ggplot(dat,aes(x=MeanSalinity,y=shannon,color=Community))+
  labs(x = "Soil Salinity (ppt)",y="Plant Diversity",colour="Community type")+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community,scale="free")




#####Do plots with lower annual variability in salinity have lower diversity or more variable diversity #####

#filter out plots that had fewer than x measurements across all years
counts<-env3%>%
  group_by(StationID)%>%
  summarise(n())
sort(counts$`n()`)
counts2<-counts$StationID[which(counts$`n()`>8)]
env3a<-env3[which(env3$StationID%in%counts2),]
length(unique(env3a$StationID))

#calculate CV salinity across all measurements
env3b<-env3a%>%
  group_by(StationID)%>%
  summarise(CVSalinity=sd(SoilPorewaterSalinity.ppt)/mean(SoilPorewaterSalinity.ppt))
#env3[which(env3$StationID=="CRMS0118_V01"),]
#some plots have NA for CV salinity b/c they only had one year of measurement


#calculate mean and CV of diversity from veg data
veg4a<-veg4%>%
  group_by(StationID,Community2007)%>%
  #summarise(CVdiv=sd(shannon)/mean(shannon),meandiv=mean(shannon))
  summarise(CVdiv=sd(richness)/mean(richness),meandiv=mean(richness))

#some plots only have one species across all years, so CVshannon=NaN (meandiv=0)
veg4[which(veg4$StationID=="CRMS0003_V03"),][,435:439]

#merge
dat<-merge(veg4a,env3b,by=c("StationID"))
dim(dat)

ggplot(dat,aes(x=CVSalinity,y=meandiv,color=Community2007))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community2007,scale="free")

m1<-gls(CVdiv~CVSalinity*Community2007,data=dat,na.action = na.omit)
summary(m1)
anova(m1,type="marginal")

#result: more variability in salinity means more diversity, richness graphs look better for some reason


#cleaning things up a bit - merge veg and env data (allowing veg data to duplicate itself). then select only stations that had at least years 
#look into bray-curtis instead of diversity
#only take env data from the time point when veg was measured
dat<-merge(env3,veg4,by=c("StationID","year","month","day"))
head(dat)
dim(dat)
#make sure no plot/years are duplicated
dups(StationID.year)




##### Pawel's disimilarity data #####
dis<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/CoastalMarsh/CRMS/Figures&Stats/DissimilarityOutput.csv")
head(dis)

#relationship between dissimilarity of a particular year and mean salinity in that year
CRMSenv<- read.csv(url("https://sites.google.com/site/phragmitesproject/file-cabinet/EnvDataAll.csv?attredirects=0"))

dat<-merge(CRMSenv,dis2)



#relationship between mean plot level dissimilarity and cv salinity
dis2<-dis%>%
  group_by(StationID)%>%
  summarise(meandis=mean(Average_Dissimilarity))
dis2
hist(dis2$meandis)

#to get community2007
dat<-merge(veg4a,dis2)
head(dat)

#from above
dat2<-merge(env3b,dat)
head(dat2)

ggplot(dat2,aes(x=CVSalinity,y=meandis,color=Community2007))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community2007,scale="free")




####Mapping#####
library(rgdal)
shape <- readOGR(dsn = "/Users/farrer/Dropbox/EmilyComputerBackup/Documents/CoastalMarsh/CRMS/Figures&Stats/tl_2013_22_cousub", layer = "tl_2013_22_cousub")
plot(shape)

library(raster)
us1<-getData("GADM",country="USA", level=1)
LA1<-(us1[us1$NAME_1=="Louisiana",])
plot(LA1)

#transform to UTM
LA2 <- spTransform(LA1, CRS("+proj=utm +zone=15 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))

crmssites <- readOGR(dsn = "/Users/farrer/Dropbox/EmilyComputerBackup/Documents/CoastalMarsh/CRMS/Figures&Stats/crms6298/shapefile", layer = "crms6298")

plot(LA2)
points(crmssites,col='orange', pch=20, cex=0.75)

crmssites2<-data.frame(crmssites)
points(crmssites2$X_COORD,crmssites2$Y_COORD,col='blue', pch=20, cex=0.75)
head(crmssites2)
colnames(crmssites2)[6]<-"StationFront"
dat3<-dat2%>%
  mutate(StationFront=StationID)%>%
  separate(StationFront,into=c("StationFront","StationBack"),sep="_")%>%
  group_by(StationFront,Community2007)%>%
  summarise(meandis2=mean(meandis))
head(dat3)

dat4<-merge(dat3,crmssites2,by="StationFront")
dim(dat4)
head(dat4)
range(dat4$meandis2)


range01 <- function(x)(x-min(x))/diff(range(x))
cRamp <- function(x){
  cols <- colorRamp(rev(heat.colors(8)))(range01(x))
  apply(cols, 1, function(xt)rgb(xt[1], xt[2], xt[3], maxColorValue=255))
}  
heat.colors(7)

plot(LA2)
points(dat4$X_COORD,dat4$Y_COORD,bg=cRamp(dat4$meandis2), pch=21, col=1,cex=1.4)
cbind(dat4$meandis2,cRamp(dat4$meandis2))
legend_image <- as.raster(matrix(rev(heat.colors(8)), ncol=1))
plot(c(0,2),c(0,1),type = 'n', axes = F,xlab = '', ylab = '', main = 'Vegetation Variability')
text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
rasterImage(legend_image, 0, 0, 1,1)

#plot(1:4,1:4,pch=21,bg="#FFFF63",col=1)

range01(dat4$meandis2)
cols <- colorRamp(heat.colors(7))(range01(dat4$meandis2))
cols2<-cRamp(dat4$meandis2)

#library(ggmap)
library(maptools)
require(plyr)
library(rgeos)
library(rgdal)

library(rgdal)
library(maptools)
if (!require(gpclib)) install.packages("gpclib", type="source")
gpclibPermit()

LA3<-LA2
LA3@data$id = rownames(LA3@data)
LA3.points = fortify(LA3, region="id")
LA3.df = join(LA3.points, LA3@data, by="id")

ggplot(LA3.df,aes(long,lat,group=group))+
  theme_classic()+
  coord_equal()+
  labs(x = "Longitude",y="Latitude",colour="Plant Community Variability")+
  geom_path(data=LA3.df[LA3.df$id==20,],color="black",size=.3)+
  scale_colour_gradientn(colors=rev(heat.colors(8)))+#"Probability",limits=c(-1,1),low="#00006B",mid="#F2F2F2FF",high="#b2182b",guide="colourbar",midpoint=0,na.value = "black"
  geom_point(data=dat4,aes(x=X_COORD,y=Y_COORD,col=meandis2,group=NA))

pdf("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/CoastalMarsh/CRMS/Figures&Stats/Figs/MapPlantVariability.pdf")
ggplot(LA3.df,aes(long,lat,group=group))+
  theme_classic()+
  coord_equal()+
  labs(x = "Longitude",y="Latitude",fill="Community Variability")+
  geom_path(data=LA3.df[LA3.df$id==20,],color="black",size=.3)+
  scale_fill_gradientn(colors=rev(heat.colors(8)))+#"Probability",limits=c(-1,1),low="#00006B",mid="#F2F2F2FF",high="#b2182b",guide="colourbar",midpoint=0,na.value = "black"
  geom_point(data=dat4,aes(x=X_COORD,y=Y_COORD,group=NA,fill=meandis2),pch=21,col="black",size=2)
dev.off()

#could also look at total coverage in the veg data as a metric of productivity.....












#####take veg data from september-dec and salinity data from january-august#####
#use env3 and veg2. veg2 still has some duplicate years but that should be taken care of with a sept-dec cutoff for veg

env31<-env3%>%
  filter(month%in%c(1,2,3,4,5,6,7,8))%>%
  summarise()
dim(env3)
dim(env31)

veg21<-veg2%>%
  filter(month%in%c(9,10,11,12))
dim(veg2)
dim(veg21)

dat<-merge(env31,veg21,"StationID")
head(env31)
head(veg21)
dim(dat)
which(env$StationID%in%veg21$StationID)
which(env31$StationID=="CRMS0322_V16")

#no overlap in stations from env and veg, arg!






#####Cleaned data, by year#####
CRMSveg<- read.csv(url("https://sites.google.com/site/phragmitesproject/file-cabinet/VegDataAll.csv?attredirects=0"))
CRMSenv<- read.csv(url("https://sites.google.com/site/phragmitesproject/file-cabinet/EnvDataAll.csv?attredirects=0"))

#write.csv(CRMSveg,"/Users/farrer/Dropbox/EmilyComputerBackup/Documents/CoastalMarsh/CRMS/Figures&Stats/VegDataAll.csv",row.names = F)
#write.csv(CRMSenv,"/Users/farrer/Dropbox/EmilyComputerBackup/Documents/CoastalMarsh/CRMS/Figures&Stats/EnvDataAll.csv",row.names = F)

CRMSveg[1:5,1:5]
head(CRMSenv)
dim(CRMSenv)

dat<-cbind(CRMSenv,CRMSveg)

Commtype2007<-dat %>%
  filter(year==2007)%>%
  select(StationID,Community)
dim(Commtype2007)

length(unique(dat$StationID))-dim(Commtype2007)[1]
#60 stations were not sampled in 2007
notsampled2007<-setdiff(dat$StationID,Commtype2007$StationID)

Commtype2008<-dat %>%
  filter(year==2008,StationID %in% as.factor(notsampled2007))%>%
  select(StationID,Community)
Commtype2008

Commtype2007b<-rbind(Commtype2007,Commtype2008);colnames(Commtype2007b)[2]<-"Community2007"
dat2<-merge(dat,Commtype2007b)
dat3<-data.frame(dat2[,1:2],Community2007=dat2[,416],dat2[,3:415])
dat3[1:10,1:10]

hist(dat3$MeanSalinity)
sort(dat3$MeanSalinity,decreasing=T)[1:15] #there are a few salinity values of 1284.412, so I removed them
dat4 <- dat3 %>%
  filter(MeanSalinity<1000)%>%  
  mutate(Community2007=factor(Community2007,levels=c("Freshwater","Intermediate","Brackish","Saline")))%>%
  group_by(Community2007,year)%>%
  summarise(meanmean=mean(MeanSalinity),meanCV=mean(CVSalinity,na.rm=T),semean=std.error(MeanSalinity),seCV=std.error(CVSalinity))

as.data.frame(dat4)
dat4$Community2007

#Mean salinity declines over time in all community types, WTF
ggplot(dat4,aes(x=year,y=meanmean,color=Community2007))+
  geom_point() +
  geom_line() +
  geom_errorbar(aes(ymin=meanmean-semean, ymax=meanmean+semean),width=.4)
  #labs(x = "Cattle density",y="Probability of occurrence")+
  #geom_line(stat="smooth",method = "lm",size=.8)
  #coord_cartesian(ylim = c(0,1))+
  #facet_wrap(~Species)

#CV salinity is large in 2008
ggplot(dat4,aes(x=year,y=meanCV,color=Community2007))+
    geom_point() +
    geom_line() +  
    geom_errorbar(aes(ymin=meanCV-seCV, ymax=meanCV+seCV),width=.4)
  #labs(x = "Cattle density",y="Probability of occurrence")+
  #geom_line(stat="smooth",method = "lm",size=.8)
  #coord_cartesian(ylim = c(0,1))+
  #facet_wrap(~Species)
  








