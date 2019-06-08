#Data +libraries + Apriori Model ========
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
dim(VegAllEnvData) # 3498  473
dim(Plant_Info)#3454 of all species, 747 of introduced ones.

library("tidyverse")
library("vegan")
library("gridExtra")
library("grid")
library(Rmisc)

#Invasive (introduced) ========
Plant_Info_Invasive <- Plant_Info  %>% filter (Plant_Info$nat == "introduced") %>%
                                      select(specCode)

inv_names<-as.character(droplevels((Plant_Info_Invasive$specCode)))
MyCols <- rbind(inv_names, "Community")#To subset later on:

inv_data <- VegAllEnvData [ , colnames(VegAllEnvData) %in% MyCols] #select introduced species columns
dim(inv_data)
inv_data $ richness <- specnumber(inv_data[, 2:44])

#PLOT Richness ===========
RichnessSum <- summarySE(inv_data, measurevar = "richness",
                         groupvars = "Community", na.rm = T)
RichnessSum$Community<-factor(RichnessSum$Community,
                              levels = c( "Freshwater","Intermediate","Brackish","Saline"))

ggplot(RichnessSum, aes(x=Community, y=richness, fill = Community)) + 
  geom_errorbar(aes(ymin=richness-ci,
                    ymax=richness+ci), width=.35, size=.9)+
  scale_fill_manual(values=c(c("#CDB180", "#7E7C22" ,"#8F9727" ,"#65770F")))+
  geom_point(position=position_dodge(.1), size=4) +
  labs(x = "", y="Introduced Plant Richness") + 
  #facet_grid(.~Community,scale="fixed") +
  #ggtitle("Plant species richness over 2007 - 2017 (Louisiana)") +
  #theme_bw() +
  theme(legend.position = "none",
                    axis.text.x  = element_text(size = 14),
                    plot.title = element_text(hjust= 0.5, size =16))

#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_Inv_Richness2.pdf")


#PLOT Cover:==========
inv_data $ Cover <- rowSums(inv_data[, 2:44])
CoverSum <- summarySE(inv_data, measurevar = "Cover",
                         groupvars = "Community", na.rm = T)
#GGPLOT:
ggplot(CoverSum, aes(x=Community, y= Cover, fill = Community)) + 
  geom_errorbar(aes(ymin=Cover-ci,
                    ymax=Cover+ci), width=.35, size=.9)+
  scale_fill_manual(values=c(c("#CDB180", "#7E7C22" ,"#8F9727" ,"#65770F")))+
  geom_point(position=position_dodge(.1), size=4) +
  labs(x = "", y="Introduced Plant Cover")+ 
  #facet_grid(.~Community,scale="fixed") +
  #ggtitle("Plant species Cover over 2007 - 2017 (Louisiana)") +
  #theme_bw() +
  theme(legend.position = "none",
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14),
        plot.title = element_text(hjust= 0.5, size =16))


#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_Inv_Cover.pdf")
