#LOAD Libraries and raw data=====
library(tidyverse)
library(vegan)
library(plotrix)
library(nlme)
library(chron)

##### Raw veg data:
#5 => 75 percent cover; 4 = 50-75 percent cover; 3 = 25-50 percent cover; 2 = 5-25 percent cover; 1 = numerous, but less than 5 percent cover, or scattered, with cover up to 5 percent; + = few, with small cover; and r = rare, solitary, with small cover.
#"CRMS_Marsh_Veg.csv" file = clean CRMS veg data as produced in "01_CLEAN_CRMS_Veg.R" file
veg <- read.csv("CRMS_Marsh_Veg.csv")#Data contains cover values for each plant species. Source:
levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline"
veg$Community<-factor(veg$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))

length(unique(veg$StationID))
length(unique(veg$StationFront))#342

#Could filter out plots sampled in month 10 or 11 or 12 if I wanted to try harder to make sure
#that post-hurricanes were not sampled. or I could only look at plots sampled in 10,11,12 
#if I wanted to make sure only post hurricane is sampled, however there aren't many plots like this
#Make data wide
veg2 <- veg %>%
  select(StationID,StationFront,StationBack,day, month, year,Community,Acerrubr:ZiziMill)

#Make sure each plot has a community for every year (or at least 6 of the 10 years) 2007-2016
table(veg2$year)#n of records per year.

counts<-veg2%>%
  group_by(StationID)%>%
  summarise(n())
counts2<-counts$StationID[which(counts$`n()`>5)]
veg3<-veg2[which(veg2$StationID%in%counts2),]
length(unique(veg3$StationID))#3466
table(veg3$year)

#to make sure there are no duplicates:
veg3 <- distinct(veg3)

#Using Pawel's method:
#first define community type by most common type in stationfront
#then average species comp and div and species rich
#then merge accretion data with plant community data (veg4)

#Define the community-type in each StationFront based on n of Commuity-type Counts per station:
StationComm <- group_by(veg3,StationFront,Community) %>% 
  count(Count=StationFront)
StationComm#It gives us count of communities per StationFront
SCwide<- spread(StationComm, key = Community, value = n, fill = 0)#make it wide
SCwide$WhichMax<-colnames(SCwide)[apply(SCwide,1,which.max)]#while wide we can see which Comm is predominant
SCwide
StationCommDefined<-SCwide[, c(1,7)]
colnames(StationCommDefined)[2] <- "Community" #Renaming WhichMAx back to Community
StationCommDefined 


#Join with veg3 to create a plot/year-level (StationID*year level) data set
veg4<-veg3%>%
  rename(Community.yr=Community)%>%
  left_join(StationCommDefined,by="StationFront")
  
veg4$richness<-specnumber(veg4[,8:456])
veg4$shannon<-diversity(veg4[,8:456],index="shannon")
veg4$tot<-rowSums(veg4[,8:456])

#rearrange columns
veg5 <- veg4 %>%
  select(StationID:Community.yr,Community,richness:tot,Acerrubr:ZiziMill)

#Plot Phragaust over time========
m1<-veg5%>%
  group_by(year,Community)%>%
  summarise(mean=mean(Phraaust),se=std.error(Phraaust))

ggplot(m1,aes(x=as.factor(year),y=mean,group=Community, color = Community))+
  labs(x = "Year",y="Mean Cover (%)",colour="Community type")+
  geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(.~Community,scale="fixed")+
  ggtitle("Cover of Phragmites over 2007 - 2017 (Louisiana)")+
  theme_classic() +theme(legend.position = "none",
                    axis.text.x  = element_text(angle = 90),
                    strip.text = element_text(size = 14),
                    plot.title = element_text(hjust= 0.5, size =16))

#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_Phragmites.png")


#Then create a StationFront*year-level dataset
#Summarize means across species/diversity by stationfront and year. 
#there will be an NA for meanaccmmpery when there is veg data but no acc data
#Pawel used the veg6 dataset for merging with envc4 and re-run RDA analysis=======
#Jan 25, 2018:

veg6<-veg5%>%
  group_by(StationFront,Community,year)%>%
  summarise_at(vars(Acerrubr:ZiziMill),mean,na.rm=T)

#then compute richness, shannon, and tot
#on the new averaged stationfront-level species data (~10 survey plots per StationFront)
#(rather than have them be the average across the small plots)
veg6$richness <-specnumber (veg6 [,4:451])
veg6$shannon  <-diversity  (veg6 [,4:451], index="shannon")
veg6$tot      <-rowSums    (veg6 [,4:451])
#write.csv(veg6, file = "CRMS_Veg_Per_Site_03may2018.csv")
str(veg6) # 3606 obs. of  455 variables:


#JOIN VEG6 with ENVC4 & ENV5 DATA=====
#ENVC4 is an object produced in "HydrologicDataECF.R" file off raw CRMS environmental data.
WaterData<-as.data.frame(envc4)#Creating data to merge with veg6, or:
#WaterData <- read_csv("CRMS_MeanWaterDepth_envc4.csv")
dim(WaterData)#data.frame':	3374 obs. of  5 variables:
WaterDataThin <- as.data.frame(select( WaterData, StationFront.year, meanwaterdepthcm, floodedpercent, MeanWaterSalinity))

VegData<-as.data.frame(veg6)#veg6 is produced in above lines or as saved csv file:
#VegData<-read.csv("CRMS_Veg_Per_Site_03may2018.csv.csv")
VegData$StationFront.year<-interaction(VegData$StationFront, VegData$year)#We need that for joining
dim(VegData) #3606  456
VegDataThin <- as.data.frame(select( VegData,  - StationFront))#to ease joining with inner_join:
                             
#Join Veg Data with WaterData by StationFront.year, some levels of "StationFront.year" do not overlap:
VegEnvData<-inner_join(VegDataThin, WaterDataThin , by="StationFront.year" )
dim(VegEnvData) #2849  458
#write.csv(VegEnvData, file = "CRMS_VegEnvData_03may2018.csv", row.names = F)


#Join VegEnvData with soil pore water data (env3):
dim(env5)#2197    8    #USE "env5"  as produced in "SalinityECF.R" file
#env5 <- read.csv(as.data.frame(env5), file = "CRMS_Mean_SoilSalinity_env5_03may2018.csv", row.names = F)
RemoveYear <- env5$year
SoilData <- env5 %>%  select ( -year)

VegAllEnvData <- left_join(VegEnvData, SoilData  , by="StationFront.year" )
dim(VegAllEnvData)#2849  464
#write.csv(VegAllEnvData, file = "VegAllEnvData_03may2018.csv")

#Plot Env Data change over 10 years:======
VegAllEnvData = read.csv("VegAllEnvData_03may2018.csv")
VegAllEnvData$Community <- factor(VegAllEnvData$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))

#Plot Salinity over years over 4 communities:====
salinitySum <- summarySE(VegAllEnvData, measurevar = "MeanWaterSalinity",
                         groupvars = c("Community", "year"), na.rm = T)
  
salinitySum$Community<-factor(salinitySum$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))

ggplot(salinitySum, aes(x=as.factor(year), y=MeanWaterSalinity, shape=Community, color= - MeanWaterSalinity)) + 
   geom_errorbar(aes(ymin=MeanWaterSalinity-ci,
                     ymax=MeanWaterSalinity+ci), width=.35, size=.9)+
  geom_point(size=4) +
  labs(x = "Year",y="Salinity (ppt)",colour="Salinity (ppt)") + 
  facet_grid(.~Community,scale="fixed") +
  ggtitle("Water salinity over 2007 - 2017 (Louisiana)") +
  theme_bw() +theme(legend.position = "none",
                    axis.text.x  = element_text(angle = 90),
                    strip.text   = element_text(size = 14),
                    plot.title   = element_text(hjust= 0.5, size =16))
#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_WaterSalinity.png")

#Plot Richness over time over 4 comms:====
RichnessSum <- summarySE(VegAllEnvData, measurevar = "richness",
                         groupvars = c("Community", "year"), na.rm = T)

RichnessSum$Community<-factor(RichnessSum$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))

ggplot(RichnessSum, aes(x=as.factor(year), y=richness, shape=Community)) + 
  geom_errorbar(aes(ymin=richness-ci,
                    ymax=richness+ci), width=.35, size=.9)+
  geom_point(position=position_dodge(.1),size=4) +
  labs(x = "Year", y="Richness",colour="Salinity (ppt)") + 
  facet_grid(.~Community,scale="fixed") +
  ggtitle("Plant species richness over 2007 - 2017 (Louisiana)") +
  theme_bw() +theme(legend.position = "none",
                    axis.text.x  = element_text(angle = 90),
                    strip.text = element_text(size = 14),
                    plot.title = element_text(hjust= 0.5, size =16))

#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_Richness.png")


#Plot meanwaterdepthcm change over 10 years====
ggplot(VegAllEnvData,aes(x=as.factor(year), y=meanwaterdepthcm, group=Community, color = Community))+
  labs(x = "",y="Water depth (cm)")+
  geom_point(alpha = 0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_grid(~Community) +
  ggtitle("Water depth over 2007 - 2017 (Louisiana)") +
  theme_bw() +theme(legend.position = "none",
                    axis.text.x  = element_text(angle = 90),
                    strip.text = element_text(size = 14),
                    plot.title = element_text(hjust= 0.5, size =16))

#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_WaterDepth.png")


#Plot Mean_SoilSalinity  over 10 years====
ggplot(VegAllEnvData,aes(x=as.factor(year), y=Mean_SoilSalinity, group=Community, color = Community))+
  labs(x = "",y="Soil Salinity (ppt)")+
  geom_point(alpha = 0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_grid(~Community) +
  ggtitle("Soil pore water salinity over 2007 - 2017 (Louisiana)") +
  theme_bw() +theme(legend.position = "none",
                    axis.text.x  = element_text(angle = 90),
                    strip.text = element_text(size = 14),
                    plot.title = element_text(hjust= 0.5, size =16))

#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_SoilSalinity.png")

#Plot % of Flooded days change over 10 years====
ggplot(VegAllEnvData,aes(x=as.factor(year), y=floodedpercent*100, group=Community, color = Community))+
  labs(x = "",y="Days flooded (%)")+
  geom_point(alpha = 0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_grid(~Community) +
  ggtitle("Flood days in years 2007 - 2017 (Louisiana)") +
  theme_bw() +theme(legend.position = "none",
                    axis.text.x  = element_text(angle = 90),
                    strip.text = element_text(size = 14),
                    plot.title = element_text(hjust= 0.5, size =16))

#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_Flooded.png")

#Plot % Phragmites Cover change over 10 years====
VegAllEnvData <- read.csv("VegAllEnvData_03may2018.csv")
VegAllEnvData$Year <- paste0("Year_", VegAllEnvData$year)

VegAllEnvData$Community<-factor(VegAllEnvData$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))
ggplot(VegAllEnvData, aes(x = as.factor(year), y = Phraaust, group = Community, color = Community))+
  labs(x = "Year",y="Phragmites cover (%) annually")+
  geom_point()+
  geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm", size=.8, na.rm = T) 
  #scale_y_continuous(breaks=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017))+
  facet_wrap(~Community,scale="free")+
  ggtitle("Phragmites cover change over 2007 - 2017 (Louisiana)") +
  theme_bw() +theme(legend.position = "none",
                    axis.text.x  = element_text(angle = 90),
                    strip.text = element_text(size = 14),
                    plot.title = element_text(hjust= 0.5, size =16))


#PUT Model info on a plot ======
#Source: https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1],group = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(lm(Phraaust ~ Year,
                             data = VegAllEnvData[VegAllEnvData$Community=="Freshwater",]))+
  theme_bw() + 
  labs(subtitle = "Freshwater")+
  theme(legend.position = "none",
                       axis.text.x   = element_text(angle = 90),
                       plot.subtitle =  element_text(size = 14),
                       plot.title    = element_text(hjust= 0.5, size =16))
  


#Let us remove the sites that had no Phragmites occurance
#over the surveyed decade:

Phrag <- select(VegAllEnvData,StationFront.year, Year, Community, Phraaust) %>%
  spread(key = Year, Phraaust, fill = 0) %>%
  mutate (Total_Cover = Year_2007+Year_2008+Year_2009+Year_2010+Year_2011+Year_2012+Year_2013+Year_2014+Year_2015+Year_2016+Year_2017) %>%
  filter(Total_Cover > 0) %>%
  select ( Year_2007:Year_2017, Community, StationFront.year)%>%
  gather ( Year, Phraaust, Year_2007:Year_2017)


p1less<- ggplotRegression(lm(Phraaust ~ Year,
                         data = Phrag[Phrag$Community=="Freshwater",]))+
      theme_bw() +   theme(legend.position = "none",
                      axis.text.x  = element_text(angle = 90),
                      strip.text = element_text(size = 14),
                      plot.title = element_text(hjust= 0.5, size =16))
p1less

#Create a StationFront-level dataset (average over years)==========
veg7<-veg6 %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(Acerrubr:ZiziMill),mean,na.rm=T)
veg7


#Table 1====
#Summary Table 1:  plots per year that were analyzed:
VegAllEnvData <- read.csv("VegAllEnvData_03may2018.csv")

MyTable <- group_by(VegAllEnvData, year, Community) %>%
  summarise(n()) %>% spread(Community, `n()` ) %>%
  mutate (Total = Brackish + Freshwater+ Intermediate + Saline)

MyTable
#write.csv (MyTable, file = "Table1_23may2018.csv", row.names = F)



