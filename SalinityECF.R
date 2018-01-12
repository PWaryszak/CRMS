

##### Raw env data from CRMS website #####
env<-read.csv("/Users/farrer/Dropbox/EmilyComputerBackup/Documents/CoastalMarsh/CRMS/Figures&Stats/CRMS_Discrete_Hydrographic.csv")  #take the file and delete the degree sign in two of the columns

#substitute - for _ so it is the same as the veg data
env$CPRA.Station.ID<-as.factor(sub("-","_",env$CPRA.Station.ID))

head(env)
#for env data, the unit of measurement is StationID (which is composed of Station and site), even though there are many sites sampled within a station, they are unique measurements. I will use only the P stations, not the veg stations

#clean up, make some new columms, select only P stations for "soil porewater" stations that are collected monthly, and select the shallowest depth 10cm depth measurements
#Remove year 2001, 2006 and 2017, remove error-looking outliers
#all the salinity values above 56 look incorrect by comparing to conductance, either a decimal is missing or columns were switched around. The value at 54.7 looks correct. I didn't check the other high ones just beneath 56

env2<-env%>%
  rename(StationID=CPRA.Station.ID)%>%
  mutate(Date=Date..mm.dd.yyyy.,StationFront=StationID)%>%
  separate(Date,into=c("month","day","year"),sep="/")%>%
  mutate(yearadd=20)%>%
  unite(year,yearadd,year,sep="")%>%
  separate(StationFront,into=c("StationFront","StationBack"),sep="_")%>%
  mutate(StationType=StationBack)%>%
  separate(StationType,into=c("StationType","StationBackNum"),sep=1)%>%
  filter(Measurement.Depth..ft.==0.328,StationType=="P",year%in%c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016),Soil.Porewater.Salinity..ppt.<56)%>%
  mutate(StationID.year=paste(StationID,year,sep="."))

env2
hist(env2$Soil.Porewater.Salinity..ppt.)
unique(env2$StationType)
table(env2$year)

#filter out any plots that had fewer than 5 or whatever samplings per year
counts<-env2%>%
  group_by(StationID.year)%>%
  summarise(n())
counts2<-counts$StationID.year[which(counts$`n()`>4)]
env3<-env2[which(env2$StationID.year%in%counts2),]


#filter out plots sampled in sept-dec (if desired), summarise means per site (over the 3 p plots at each site), and summarise means per year, then summarize means over all the years. for max, I could do it either way, below is the max salnity ever recorded at a sampling point (averaged across the three P plots)
env4<-env3%>%
  #filter(month%in%c(1,2,3,4,5,6,7,8,9))%>%
  group_by(StationFront,year, month,day,Date..mm.dd.yyyy.)%>%
  summarise(salinityppt=mean(Soil.Porewater.Salinity..ppt.))%>%
  group_by(StationFront,year)%>%
  summarise(MeanSalinity=mean(salinityppt),MaxSalinity=max(salinityppt))

#then filter out and only keep plots that have at least 7 8 9 or 10 years of data
counts<-env4%>%
  group_by(StationFront)%>%
  summarise(n())
counts2<-counts$StationFront[which(counts$`n()`>6)]
env5<-env4[which(env4$StationFront%in%counts2),]
length(unique(env5$StationFront))
length(unique(env4$StationFront))

#then average across years
env6<-env5%>%
  group_by(StationFront)%>%
  summarize(MeanSalinity=mean(MeanSalinity),MaxSalinity=max(MaxSalinity))





