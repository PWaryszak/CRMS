
##### Continuous bulk hydrologic data from CRMS ####

library(dplyr)
library(tidyr)
library(ggplot2)
library(plotrix)
library(nlme)
library(chron)
library(vegan)

envc<-read.csv("CRMS_Continuous_Hydrographic.csv")#warning!! takes a really long time to read in
#(I think it took hours)!!

head(envc)
#I want the column "Adjusted Water Elevation to Marsh (ft)". I am pretty convinced that this column works for both normal and flotant marshes. There is also a column Adjusted.Marsh.Mat.Elevation.to.Datum..ft. but if you subtract: adjusted water elevation to datum minus the adjusted marsh mat elevation to datum = adjusted water elevation to marsh. and this makes sense. Positive numbers are deep water, negative numbers are water below the surface of the marsh
#I'm not sure if I want to use these salinty data b/c I think they are from water, not soil porewater

#Average over date and station.ID. 
#warning, takes ~6 min to run
envc2<-envc%>%
  group_by(Station.ID,Date..mm.dd.yyyy.,Sensor.Environment)%>%
  summarise(salinity=mean(Adjusted.Salinity..ppt.,na.rm=T),waterdepthft=mean(Adjusted.Water.Elevation.to.Marsh..ft.,na.rm=T))%>%
  mutate(waterdepthcm=waterdepthft/0.0328084)
dim(envc2)
head(envc2)
hist(envc2$waterdepthcm)
envc[which(envc$Station.ID=="CRMS0188-M01"),]
envc[which(envc$Station.ID=="CRMS2041-H01"),]
envc[which(envc$Station.ID=="CRMS0002-H01"&envc$Date..mm.dd.yyyy.=="1/10/2010"),"Adjusted.Water.Elevation.to.Marsh..ft."]
unique(envc2[which(envc2$Sensor.Environment=="Flotant Marsh"),"Station.ID"])
which(veg7$StationFront=="CRMS0058")

#Notes: (weird things about the dataset)
#Stationback = H01 and W01 and M01,
#H = surface water,
#W = it is from a well,
#M = from a flotant marsh.
#However, flotant marshes can have H, W, and M stationbacks. 
#Only three stationfronts have M's (CRMS0115-M01, CRMS0128-M01, and CRMS0058-M01
#and one of these (CRMS0058) has all NAs for water depth from the M plot) 
#so I could just delete those, however we have veg data from CRMS0115 and CRMS0128
#so it would be good to include if possible.
#incidentally CRMS0115 and CRMS0128 only have water depth measurements from the M plots, not the H or W plots, so I will just use those M plots. For CRMS0058, it doesn't matter much b/c we don't have veg data from that plot, and incidentally, it has H01 H02 and W01 plots that have very different water depth measurements
# 74 stationfronts have W01 plots (wells). I checked a bunch of them and it looks like when there is a station with a well, the well was started in 2007 and ran though the early years e.g. 2007-2011, and then there are a few months of overlap but the well was discontinued and an H plot was started e.g. 2011-2017. In the period of overlap, however, the well and the H plots sometimes (but not always) give pretty different water depth numbers (-5 vs. 12cm). I could go either way, maybe right now I'll keep them b/c I don't like throwing away so much data.


#Filter plots with missing data, calculate mean inundation over each day
#(if there are 2 H plots (H01 and H02) average them by day.
#warning!! takes ~ 8 min to run
envc3<-envc2%>%
  filter(is.na(waterdepthcm)==F)%>%
  mutate(year=Date..mm.dd.yyyy.,StationFront=Station.ID)%>%
  separate(year,into=c("month","day","year"),sep="/")%>%
  separate(StationFront,into=c("StationFront","StationBack"),sep=c("-"))%>%
  group_by(StationFront,year,month,day)%>%
  summarise(waterdepthcm=mean(waterdepthcm,na.rm=T))


#Then filter plots/years that at least have 1 measurement in each month (?)
#and calculate the annual average water depth and the proportion days flooded
plotstokeep<-envc3%>%
  unite(StationFront.year,c("StationFront","year"),sep=".")%>%
  group_by(StationFront.year,month)%>%
  summarise(n=n())%>%
  group_by(StationFront.year)%>%
  summarise(n=n())%>%
  filter(n==12)%>%
  select(StationFront.year)

#I should add SD and CV of water depth
envc4<-envc3%>%
  unite(StationFront.year,c("StationFront","year"),sep=".",remove=F)%>%
  filter(StationFront.year%in%as.factor(plotstokeep$StationFront.year))%>%
  mutate(flooded=ifelse(waterdepthcm>0,1,0))%>%#sign(waterdepthcm)
  group_by(StationFront,year)%>%
  summarise(meanwaterdepthcm=mean(waterdepthcm,na.rm=T),floodeddays=sum(flooded),n=n())%>%
  mutate(floodedpercent=floodeddays/n)
head(as.data.frame(envc3))

temp<-subset(envc4,StationFront.year=="CRMS0002.2008")
sum(temp$flooded)/dim(temp)[1]
#for CRMS0002.2008, the percent flooded should be 0.7672414, yes, checks







#Ignore all below, this is from checking the data and figuring out what M, H, and W plots were like
# envc3<-envc2%>%
#   filter(is.na(waterdepthcm)==F)%>%
#   mutate(year=Date..mm.dd.yyyy.,StationFront=Station.ID)%>%
#   separate(year,into=c("month","day","year"),sep="/")%>%
#   separate(StationFront,into=c("StationFront","StationBack"),sep=c("-"))%>%
#   group_by(Station.ID,Sensor.Environment,StationFront,StationBack,year,month)%>%
#   summarise(meanwaterdepthcm=mean(waterdepthcm,na.rm=T))
# 
# envc3[which(envc3$Sensor.Environment=="Marsh Well"),][1:10,1:8]
# unique(envc3[which(envc3$Sensor.Environment=="Marsh Well"),"Station.ID"])[1:10,]
# unique(envc3[which(envc3$StationBack=="W01"),"Station.ID"])[1:10,]
# unique(envc[which(envc$Sensor.Environment=="Marsh Well"),"Station.ID"])
#some of the stations with wells W01 plots
# 1 CRMS0008-W01
# 2 CRMS0058-W01
# 3 CRMS0059-W01
# 4 CRMS0065-W01
# 5 CRMS0089-W01
# 6 CRMS0090-W01
# 7 CRMS0114-W01
# 8 CRMS0117-W01
# 9 CRMS0120-W01
# as.data.frame(envc3[which(envc3$StationFront=="CRMS0030"),])
# 
# envc4<-envc3%>%
#   group_by(Station.ID,Sensor.Environment,StationFront,StationBack,year)%>%
#   summarise(meanwaterdpthcm=mean(meanwaterdpthcm,na.rm=T),n=n())
# as.data.frame(envc4[which(envc4$StationFront=="CRMS0089"),])
# temp<-as.data.frame(envc3[which(envc3$StationFront=="CRMS0008"),])
# spread(temp,StationBack,meanwaterdpthcm)%>%
#   arrange(year,as.numeric(month))
# 
# unique(envc3$Sensor.Environment)
# envc3[which(envc3$Sensor.Environment=="Flotant Marsh"),][1:10,1:8]
# unique(envc3[which(envc3$Sensor.Environment=="Flotant Marsh"),"Station.ID"])[1:10,]
# unique(envc3[which(envc3$StationBack=="M01"),"Station.ID"])[1:10,]

# unique(envc3[which(envc3$StationFront=="CRMS0115"),"Station.ID"])
# hist(as.data.frame(envc3[which(envc3$StationFront=="CRMS0115"&envc3$StationBack=="M01"),"waterdepthcm"])$waterdepthcm)
#M is th only one with numbers

# unique(envc3[which(envc3$StationFront=="CRMS0058"),"Station.ID"])
# hist(as.data.frame(
#   envc3[which(envc3$StationFront=="CRMS0058"&envc3$StationBack=="W01"),"waterdepthcm"]
#   )$waterdepthcm)
#M is the only one without numbers, the other three have very different numbers for water depth

# unique(envc3[which(envc3$StationFront=="CRMS0128"),"Station.ID"])
# hist(as.data.frame(
#   envc3[which(envc3$StationFront=="CRMS0128"&envc3$StationBack=="H01"),"waterdepthcm"]
# )$waterdepthcm)
#M is the only one with numbers

#code for chron
#arrange(chron(as.character(Sample.Date..mm.dd.yyyy.)),.by_group=TRUE)
