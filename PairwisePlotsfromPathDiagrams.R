
#Figures for looking at relationships between the different variables in the path analysis

x=Freshwater_Data$Mean_SoilSalinity
y=Freshwater_Data$meanwaterdepthcm
#plot(log(x),y);abline(lm(y~log(x)))
plot(x,y);abline(lm(y~x))

summary(lm(V1~Introduced_Cover,data=Freshwater_Data))
rcorr(log(Freshwater_Data$Mean_SoilSalinity),Freshwater_Data$meanwaterdepthcm)


x=Intermediate_Data$Introduced_Cover
y=Intermediate_Data$Native_Cover
plot(log(x),y);abline(lm(y~log(x+1)))
plot(x,y);abline(lm(y~x))

summary(lm(Native_Cover~Introduced_Cover+Mean_SoilSalinity,data=Intermediate_Data))
rcorr(Intermediate_Data$Mean_SoilSalinity,Intermediate_Data$meanwaterdepthcm)


x=Brackish_Data$Introduced_Cover
y=Brackish_Data$Native_Cover
plot(log(x+1),y);abline(lm(y~log(x+1)))
plot(x,y);abline(lm(y~x))

summary(lm(Native_Cover~Introduced_Cover+Mean_SoilSalinity,data=Intermediate_Data))
rcorr(Brackish_Data$Mean_SoilSalinity,Brackish_Data$meanwaterdepthcm)

rcorr(Saline_Data$Mean_SoilSalinity,Saline_Data$meanwaterdepthcm)


dat2<-Freshwater_Data%>%
  select(V1, Mean_SoilSalinity,meanwaterdepthcm,floodedpercent,Introduced_Cover,Native_Cover,Native_Richness)%>%
  gather(category, values,-Native_Cover)

ggplot(dat2,aes(x=values, y=Native_Cover, group=category, color = category))+
  labs(x = "",y="Native Cover")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 


dat2<-Freshwater_Data%>%
  select(V1, Mean_SoilSalinity,meanwaterdepthcm,floodedpercent,Introduced_Cover,Native_Cover,Native_Richness)%>%
  gather(category, values,-Native_Richness)

ggplot(dat2,aes(x=values, y=Native_Richness, group=category, color = category))+
  labs(x = "",y="Native Richness")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 

rcorr(Saline_Data$richness,Saline_Data$Native_Cover)

dat2<-Freshwater_Data%>%
  select(V1,Introduced_Cover,Native_Cover,Native_Richness)%>%
  gather(category, values,-Introduced_Cover)

ggplot(dat2,aes(x=Introduced_Cover, y=values, group=category, color = category))+
  labs(x = "Introduced Cover",y="")+
  geom_point() + geom_smooth(method = "lm") + #alpha = 0.2
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 

dat2<-Freshwater_Data%>%
  select(V1,Introduced_Cover,Native_Cover,Native_Richness,Mean_SoilSalinity)%>%
  gather(category, values,-Mean_SoilSalinity)

ggplot(dat2,aes(x=log(Mean_SoilSalinity), y=values, group=category, color = category))+
  labs(x = "Salinity",y="")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 

dat2<-Freshwater_Data%>%
  select(V1,Introduced_Cover,Native_Cover,Native_Richness,meanwaterdepthcm)%>%
  gather(category, values,-meanwaterdepthcm)

ggplot(dat2,aes(x=meanwaterdepthcm, y=values, group=category, color = category))+
  labs(x = "Salinity",y="")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 



dat2<-Intermediate_Data%>%
  mutate(Introduced_Cover=log(Introduced_Cover+1))%>%
  select(V1,Introduced_Cover,Native_Cover,Native_Richness)%>%
  gather(category, values,-Introduced_Cover)

ggplot(dat2,aes(x=Introduced_Cover, y=values, group=category, color = category))+
  labs(x = "Introduced Cover",y="")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 

dat2<-Intermediate_Data%>%
  select(V1,Introduced_Cover,Native_Cover,Native_Richness,Mean_SoilSalinity)%>%
  gather(category, values,-Mean_SoilSalinity)

ggplot(dat2,aes(x=Mean_SoilSalinity, y=values, group=category, color = category))+
  labs(x = "Salinity",y="")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 

dat2<-Intermediate_Data%>%
  select(V1,Introduced_Cover,Native_Cover,Native_Richness,meanwaterdepthcm)%>%
  gather(category, values,-meanwaterdepthcm)

ggplot(dat2,aes(x=meanwaterdepthcm, y=values, group=category, color = category))+
  labs(x = "Salinity",y="")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 


summary(lm(Native_Cover~meanwaterdepthcm+Mean_SoilSalinity+Introduced_Cover,data=Intermediate_Data))
plot(Intermediate_Data$meanwaterdepthcm,Intermediate_Data$Mean_SoilSalinity)
summary(lm(meanwaterdepthcm~Mean_SoilSalinity,data=Intermediate_Data))


dat2<-Brackish_Data%>%
  mutate(Introduced_Cover=log(Introduced_Cover+1))%>%
  select(V1,Introduced_Cover,Native_Cover,Native_Richness)%>%
  gather(category, values,-Introduced_Cover)

ggplot(dat2,aes(x=Introduced_Cover, y=values, group=category, color = category))+
  labs(x = "Introduced Cover",y="")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 

dat2<-Brackish_Data%>%
  select(V1,Introduced_Cover,Native_Cover,Native_Richness,meanwaterdepthcm)%>%
  gather(category, values,-meanwaterdepthcm)

ggplot(dat2,aes(x=meanwaterdepthcm, y=values, group=category, color = category))+
  labs(x = "Salinity",y="")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 

dat2<-Brackish_Data%>%
  select(V1,Introduced_Cover,Native_Cover,Native_Richness,Mean_SoilSalinity)%>%
  gather(category, values,-Mean_SoilSalinity)

ggplot(dat2,aes(x=Mean_SoilSalinity, y=values, group=category, color = category))+
  labs(x = "Salinity",y="")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 

dat2<-Brackish_Data%>%
  select(V1,Introduced_Cover,Native_Cover,Native_Richness,meanwaterdepthcm)%>%
  gather(category, values,-meanwaterdepthcm)

ggplot(dat2,aes(x=meanwaterdepthcm, y=values, group=category, color = category))+
  labs(x = "Water depth",y="")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 




dat2<-Saline_Data%>%
  #mutate(Introduced_Cover=log(Introduced_Cover+1))%>%
  select(V1,floodedpercent,Native_Cover,Native_Richness)%>%
  gather(category, values,-floodedpercent)

ggplot(dat2,aes(x=floodedpercent, y=values, group=category, color = category))+
  labs(x = "",y="")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 

plot(Saline_Data$floodedpercent,Saline_Data$meanwaterdepthcm)

dat2<-Saline_Data%>%
  select(V1,Introduced_Cover,Native_Cover,Native_Richness,Mean_SoilSalinity)%>%
  gather(category, values,-Mean_SoilSalinity)

ggplot(dat2,aes(x=Mean_SoilSalinity, y=values, group=category, color = category))+
  labs(x = "Salinity",y="")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 

dat2<-Saline_Data%>%
  select(V1,Introduced_Cover,Native_Cover,Native_Richness,meanwaterdepthcm)%>%
  gather(category, values,-meanwaterdepthcm)

ggplot(dat2,aes(x=meanwaterdepthcm, y=values, group=category, color = category))+
  labs(x = "Water depth",y="")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") 

