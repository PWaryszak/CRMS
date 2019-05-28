library(ggplot2)
library(Rmisc)
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv") #DATA

#Plot Salinity over years over 4 communities:===========
salinitySum <- summarySE(VegAllEnvData, measurevar = "MeanWaterSalinity",
                         groupvars = c("Community", "year"), na.rm = T)

salinitySum$Community<-factor(salinitySum$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))

ggplot(salinitySum, aes(x=as.factor(year), y=MeanWaterSalinity, shape=Community, color= - MeanWaterSalinity)) + 
  geom_errorbar(aes(ymin=MeanWaterSalinity-ci,
                    ymax=MeanWaterSalinity+ci), width=.35, size=.9)+
  geom_point(size=4) +
  labs(x = "Year",y="Mean Salinity (ppt) +95% CI",colour="Salinity (ppt)") + 
  facet_grid(.~Community,scale="fixed") +
  #ggtitle("Water salinity over 2007 - 2017 (Louisiana)") +
  theme_bw() +
  theme(legend.position = "none",
                    axis.text.x  = element_text(angle = 90),
                    strip.text   = element_text(size = 14),
                    plot.title   = element_text(hjust= 0.5, size =16))
#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_WaterSalinity.pdf")


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
  #ggtitle("Plant species richness over 2007 - 2017 (Louisiana)") +
  theme_bw() +theme(legend.position = "none",
                    axis.text.x  = element_text(angle = 90),
                    strip.text = element_text(size = 14),
                    plot.title = element_text(hjust= 0.5, size =16))

ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_Richness.pdf")


#Plot meanwaterdepthcm change over 10 years====
ggplot(VegAllEnvData,aes(x=as.factor(year), y=meanwaterdepthcm, group=Community, color = Community))+
  labs(x = "",y="Water depth (cm)")+
  geom_point(alpha = 0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_grid(~Community) +
  #ggtitle("Water depth over 2007 - 2017 (Louisiana)") +
  theme_bw() +theme(legend.position = "none",
                    axis.text.x  = element_text(angle = 90),
                    strip.text = element_text(size = 14),
                    plot.title = element_text(hjust= 0.5, size =16))

#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_WaterDepth.pdf")


#Plot Mean_SoilSalinity  over 10 years====
ggplot(VegAllEnvData,aes(x=as.factor(year), y=Mean_SoilSalinity, group=Community, color = Community))+
  labs(x = "",y="Mean Soil Salinity (ppt)")+
  geom_point(alpha = 0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_grid(~Community) +
  #ggtitle("Soil pore water salinity over 2007 - 2017 (Louisiana)") +
  theme_bw() +theme(legend.position = "none",
                    axis.text.x  = element_text(angle = 90),
                    strip.text = element_text(size = 14),
                    plot.title = element_text(hjust= 0.5, size =16))

#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_SoilSalinity.pdf")


#Plot % of Flooded days change over 10 years====
ggplot(VegAllEnvData,aes(x=as.factor(year), y=floodedpercent*100, group=Community, color = Community))+
  labs(x = "",y="Days flooded (%)")+
  geom_point(alpha = 0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_grid(~Community) +
  #ggtitle("Flood days in years 2007 - 2017 (Louisiana)") +
  theme_bw() +theme(legend.position = "none",
                    axis.text.x  = element_text(angle = 90),
                    strip.text = element_text(size = 14),
                    plot.title = element_text(hjust= 0.5, size =16))

#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_Flooded.pdf")


#Summary Table 1:  plots per year that were analyzed:======
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")
library(tidyverse)

MyTable <- select(VegAllEnvData, year, Community) %>%
  group_by (year, Community) %>%
  summarize(n()) %>% 
  spread   (Community, `n()` ) %>%
  mutate   (Total = Brackish + Freshwater+ Intermediate + Saline)

MyTable
#write.csv (MyTable, file = "Table1.csv", row.names = F)

