#LOAD RAW DATA:=======
library(corrplot)
library(Hmisc)
library(tidyverse)
library(vegan)
library("gridExtra")

#Figures for looking at relationships between the different variables
#in the path analysis
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")#Our Veg Data. Proccessed as per CRMS file 1,2,3,4 in Github
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
native <- Plant_Info[Plant_Info$nat == "native",5]

#Freshwater=======
Freshwater_Data <-filter (VegAllEnvData, VegAllEnvData$Community=="Freshwater")
#Subset native/introduced Veg matrix where colSums are > 0:
Freshwater_Veg_Matrix <- as.data.frame (subset ( Freshwater_Data, select = c(Acer_rubrum:Ziza_miliacea)))

#Pick out species from Freshwater communities:
colCount1 = colSums(Freshwater_Veg_Matrix) #sum up the abundance column-wise
topID1 = order(colCount1,decreasing=TRUE)[1:length(Freshwater_Veg_Matrix)] # choose all Freshwater plant species
topID1 = names(Freshwater_Veg_Matrix[topID1]) # names of plant species in decreasing order
Freshwater_Plant_Sp <- data.frame( specCode = topID1)
Freshwater_Plant_Sp
#Top 5 most abundant specCode (topID1):
head(topID1)
#1    Sagi_lancifolia
#2     Pani_hemitomon
#3     Poly_punctatum
#4   Alte_philoxeroid
#5      Leer_hexandra

#join Freshwater_Plants & Plant_List to see which species are native/introduced:
Plant_List<- subset(Plant_Info, select = c(specCode, nat))
Freshwater_Plants <- left_join(Freshwater_Plant_Sp,Plant_List, by = "specCode")

#Pick natives only from Freshwater_Data and compute their total cover:
Freshwater_Native.Species <- filter(Freshwater_Plants, nat == "native")#228 native species
Freshwater.native <- subset( Freshwater_Data, select = unique(Freshwater_Native.Species$specCode))
Freshwater_Native_Cover <- rowSums (Freshwater.native) #create extra vector with total cover of all natives
Freshwater_Data$Native_Cover <- Freshwater_Native_Cover#convert vector into column with total cover of all natives
Freshwater_Native_Richness <- specnumber (Freshwater.native) #create extra vector  with richness of all natives
Freshwater_Data$Native_Richness <- Freshwater_Native_Richness#convert vector into column with native richness
#Check the relationship:
summary(lm(Native_Cover ~ Mean_SoilSalinity,data=Freshwater_Data))#1.2e-06 ***
summary(lm(Native_Richness ~ Mean_SoilSalinity,data=Freshwater_Data))#1.57e-07 ***

#Pick introduced only from Freshwater_Data and compute their total cover:
Freshwater_introduced.Species <- filter(Freshwater_Plants, nat == "introduced")#228 native species
Freshwater.introduced <- subset( Freshwater_Data, select = unique(Freshwater_introduced.Species$specCode))
Freshwater_introduced_Cover <- rowSums (Freshwater.introduced) #create extra column with total cover of all introduced
Freshwater_Data$introduced_Cover <- Freshwater_introduced_Cover
#Check the relationship:
summary(lm(introduced_Cover ~ Mean_SoilSalinity,data=Freshwater_Data))#0.00129 **


data1<-Freshwater_Data %>%
  mutate(Soil_Salinity = Mean_SoilSalinity,
         Introduced_Cover = introduced_Cover,
         Water_Depth = meanwaterdepthcm) %>%
  select(Soil_Salinity,Introduced_Cover, Water_Depth, Native_Cover )%>%
  gather(category, values,-Native_Cover) 

#plot1==========
plot1 <- ggplot(data1,aes(x=values, y=Native_Cover, group=category, color = category))+
  labs(x = "",y="Freshwater Native Cover (%)")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none")
plot1

#Run FacetWrapScaleControl.R script first!
p1<-plot1 + facet_wrap_custom(~category, scales = "free", ncol = 3, scale_overrides = list(
    scale_override(1, scale_x_continuous(limits = c(0, 100))),
    scale_override(2, scale_x_continuous(limits = c(0, 20))),
    scale_override(3, scale_x_continuous(limits = c(-40, 60)))))
    

#Intermediate:========
#Figures for looking at relationships between the different variables
#in the path analysis
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")#Our Veg Data. Proccessed as per CRMS file 1,2,3,4 in Github
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
native <- Plant_Info[Plant_Info$nat == "native",5]

Intermediate_Data <-filter (VegAllEnvData, VegAllEnvData$Community=="Intermediate")
#Subset native/introduced Veg matrix where colSums are > 0:
Intermediate_Veg_Matrix <- as.data.frame (subset ( Intermediate_Data, select = c(Acer_rubrum:Ziza_miliacea)))

#Pick out species from Intermediate communities:
colCount1 = colSums(Intermediate_Veg_Matrix) #sum up the abundance column-wise
topID1 = order(colCount1,decreasing=TRUE)[1:length(Intermediate_Veg_Matrix)] # choose all Intermediate plant species
topID1 = names(Intermediate_Veg_Matrix[topID1]) # names of plant species in decreasing order
Intermediate_Plant_Sp <- data.frame( specCode = topID1)
Intermediate_Plant_Sp
#most abundant specCode (topID1):
head(topID1)
#[1] "Spar_patens"     "Vign_luteola"    "Scho_americanus"
#[4] "Sagi_lancifolia" "Poly_punctatum"  "Phra_australis"

#join Intermediate_Plants & Plant_List to see which species are native/introduced:
Plant_List<- subset(Plant_Info, select = c(specCode, nat))
Intermediate_Plants <- left_join(Intermediate_Plant_Sp,Plant_List, by = "specCode")

#Pick natives only from Intermediate_Data and compute their total cover:
Intermediate_Native.Species <- filter(Intermediate_Plants, nat == "native")#228 native species
Intermediate.native <- subset( Intermediate_Data, select = unique(Intermediate_Native.Species$specCode))
Intermediate_Native_Cover <- rowSums (Intermediate.native) #create extra column with total cover of all natives
Intermediate_Data$Native_Cover <- Intermediate_Native_Cover
#Check the relationship:
summary(lm(Native_Cover ~ Mean_SoilSalinity,data=Intermediate_Data))#2e-13 ***

#Pick introduced only from Intermediate_Data and compute their total cover:
Intermediate_introduced.Species <- filter(Intermediate_Plants, nat == "introduced")#228 native species
Intermediate.introduced <- subset( Intermediate_Data, select = unique(Intermediate_introduced.Species$specCode))
Intermediate_introduced_Cover <- rowSums (Intermediate.introduced) #create extra column with total cover of all introduced
Intermediate_Data$introduced_Cover <- Intermediate_introduced_Cover
#Check the relationship:
summary(lm(introduced_Cover ~ Mean_SoilSalinity,data=Intermediate_Data))#1.04e-08 ***


data2<-Intermediate_Data %>%
  mutate(Soil_Salinity = Mean_SoilSalinity,
         Introduced_Cover = introduced_Cover,
         Water_Depth = meanwaterdepthcm) %>%
  select(Soil_Salinity,Introduced_Cover, Water_Depth, Native_Cover )%>%
  gather(category, values,-Native_Cover) 

#plot2=======
plot2 <- ggplot(data2,aes(x=values, y=Native_Cover, group=category, color = category))+
  labs(x = "",y="Intermediate Native Cover (%)")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none")

plot2

#Run FacetWrapScaleControl.R script first!
p2 <- plot2  + facet_wrap_custom(~category, scales = "free", ncol = 3, scale_overrides = list(
  scale_override(1, scale_x_continuous(limits = c(0, 100))),
  scale_override(2, scale_x_continuous(limits = c(0, 20))),
  scale_override(3, scale_x_continuous(limits = c(-40, 60)))))

p2
#Brackish:==========
#Figures for looking at relationships between the different variables
#in the path analysis
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")#Our Veg Data. Proccessed as per CRMS file 1,2,3,4 in Github
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
native <- Plant_Info[Plant_Info$nat == "native",5]

Brackish_Data <-filter (VegAllEnvData, VegAllEnvData$Community=="Brackish")
#Subset native/introduced Veg matrix where colSums are > 0:
Brackish_Veg_Matrix <- as.data.frame (subset ( Brackish_Data, select = c(Acer_rubrum:Ziza_miliacea)))

#Pick out species from Brackish communities:
colCount1 = colSums(Brackish_Veg_Matrix) #sum up the abundance column-wise
topID1 = order(colCount1,decreasing=TRUE)[1:length(Brackish_Veg_Matrix)] # choose all Brackish plant species
topID1 = names(Brackish_Veg_Matrix[topID1]) # names of plant species in decreasing order
Brackish_Plant_Sp <- data.frame( specCode = topID1)
Brackish_Plant_Sp
#most abundant specCode (topID1):
head(topID1)
#[1] "Spar_patens"      "Dist_spicata"     "Scho_americanus" 
#[4] "Bolb_robustus"    "Spar_alterniflor" "Vign_luteola" 

#join Brackish_Plants & Plant_List to see which species are native/introduced:
Plant_List<- subset(Plant_Info, select = c(specCode, nat))
Brackish_Plants <- left_join(Brackish_Plant_Sp,Plant_List, by = "specCode")

#Pick natives only from Brackish_Data and compute their total cover:
Brackish_Native.Species <- filter(Brackish_Plants, nat == "native")#228 native species
Brackish.native <- subset( Brackish_Data, select = unique(Brackish_Native.Species$specCode))
Brackish_Native_Cover <- rowSums (Brackish.native) #create extra column with total cover of all natives
Brackish_Data$Native_Cover <- Brackish_Native_Cover
#Check the relationship:
summary(lm(Native_Cover ~ Mean_SoilSalinity,data=Brackish_Data))#0.00171 ** 

#Pick introduced only from Brackish_Data and compute their total cover:
Brackish_introduced.Species <- filter(Brackish_Plants, nat == "introduced")#228 native species
Brackish.introduced <- subset( Brackish_Data, select = unique(Brackish_introduced.Species$specCode))
Brackish_introduced_Cover <- rowSums (Brackish.introduced) #create extra column with total cover of all introduced
Brackish_Data$introduced_Cover <- Brackish_introduced_Cover
#Check the relationship:
summary(lm(Native_Cover ~ Mean_SoilSalinity,data=Brackish_Data))#0.00171 ** 
summary(lm(Native_Cover ~ meanwaterdepthcm,data=Brackish_Data))#<2e-16 ***

data3<-Brackish_Data %>%
  mutate(Soil_Salinity = Mean_SoilSalinity,
         introduced_Cover = introduced_Cover,
         Water_Depth = meanwaterdepthcm) %>%
  select(Soil_Salinity, Water_Depth, Native_Cover )%>%
  gather(category, values,-Native_Cover) 

#plot3=======
plot3 <- ggplot(data3,aes(x=values, y=Native_Cover, group=category, color = category))+
  labs(x = "",y="Brackish Native Cover (%)")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  scale_color_manual(values =  c("#00BA38", "#619CFF"))+
  geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none")

plot3

#Run FacetWrapScaleControl.R script first!
p3 <- plot3 + facet_wrap_custom(~category, scales = "free", ncol = 2, scale_overrides = list(
  scale_override(1, scale_x_continuous(limits = c(0, 20))),
  scale_override(2, scale_x_continuous(limits = c(-40, 60)))))


#Saline:==========
#Figures for looking at relationships between the different variables
#in the path analysis
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")#Our Veg Data. Proccessed as per CRMS file 1,2,3,4 in Github
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
native <- Plant_Info[Plant_Info$nat == "native",5]

Saline_Data <-filter (VegAllEnvData, VegAllEnvData$Community=="Saline")
#Subset native/introduced Veg matrix where colSums are > 0:
Saline_Veg_Matrix <- as.data.frame (subset ( Saline_Data, select = c(Acer_rubrum:Ziza_miliacea)))

#Pick out species from Saline communities:
colCount1 = colSums(Saline_Veg_Matrix) #sum up the abundance column-wise
topID1 = order(colCount1,decreasing=TRUE)[1:length(Saline_Veg_Matrix)] # choose all Saline plant species
topID1 = names(Saline_Veg_Matrix[topID1]) # names of plant species in decreasing order
Saline_Plant_Sp <- data.frame( specCode = topID1)
Saline_Plant_Sp
#most abundant specCode (topID1):
head(topID1)
#[1] "Spar_patens"      "Dist_spicata"     "Scho_americanus" 
#[4] "Bolb_robustus"    "Spar_alterniflor" "Vign_luteola" 

#join Saline_Plants & Plant_List to see which species are native/introduced:
Plant_List<- subset(Plant_Info, select = c(specCode, nat))
Saline_Plants <- left_join(Saline_Plant_Sp,Plant_List, by = "specCode")

#Pick natives only from Saline_Data and compute their total cover:
Saline_Native.Species <- filter(Saline_Plants, nat == "native")#228 native species
Saline.native <- subset( Saline_Data, select = unique(Saline_Native.Species$specCode))
Saline_Native_Cover <- rowSums (Saline.native) #create extra column with total cover of all natives
Saline_Data$Native_Cover <- Saline_Native_Cover
#Check the relationship:
summary(lm(Native_Cover ~ Mean_SoilSalinity,data=Saline_Data))#<2e-16 ***

#Pick introduced only from Saline_Data and compute their total cover:
Saline_introduced.Species <- filter(Saline_Plants, nat == "introduced")#228 native species
Saline.introduced <- subset( Saline_Data, select = unique(Saline_introduced.Species$specCode))
Saline_introduced_Cover <- rowSums (Saline.introduced) #create extra column with total cover of all introduced
Saline_Data$introduced_Cover <- Saline_introduced_Cover
#Check the relationship:
summary(lm(Native_Cover ~ Mean_SoilSalinity,data=Saline_Data))#<2e-16 ***
summary(lm(Native_Cover ~ meanwaterdepthcm,data=Saline_Data))# 1.61e-09 ***


data4<-Saline_Data %>%
  mutate(Soil_Salinity = Mean_SoilSalinity,
         Introduced_Cover = introduced_Cover,
         Water_Depth = meanwaterdepthcm) %>%
  select(Soil_Salinity, Water_Depth, Native_Cover )%>%
  gather(category, values,-Native_Cover) 

#plot4=====
plot4 <- ggplot(data4,aes(x=values, y=Native_Cover, group=category, color = category))+
  labs(x = "",y="Saline Native Cover (%)")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  scale_color_manual(values =  c("#00BA38", "#619CFF"))+
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none")

plot4

#Run FacetWrapScaleControl.R script first!
p4 <- plot4 + facet_wrap_custom(~category, scales = "free", ncol = 3, scale_overrides = list(
  scale_override(1, scale_x_continuous(limits = c(0, 20))),
  scale_override(2, scale_x_continuous(limits = c(-40, 60)))))


#Arrange all plots together (10 panels):========
library(gridExtra)
library(grid)


grid.arrange(p1, p3,p2,p4, ncol = 2 )#Even scales:

g_even <- arrangeGrob(p1, p3,p2,p4, nrow=2) #generates g

ggsave(g_even, filename = "10Panels_EvenScales_Figure_RawData_FullLabels.jpeg", 
       width = 22, 
       height = 15,
       units = "cm",
       dpi = 600)

ggsave(g_even, filename = "10Panels_EvenScales_Figure_RawData_FullLabels.pdf", 
       width = 22, 
       height = 15,
       units = "cm",
       dpi = 600)

#Introdued_Cover vs Salinity=====
#ADD A FIG with 4 panels showing invasive cover vs salinity 
#and water depth in fresh and intermediate marshes.

#i1, Freshwater=====
#Check the relationship:
summary(lm(introduced_Cover ~ Mean_SoilSalinity,data=Freshwater_Data))#20.00129 ** 
summary(lm(introduced_Cover ~ meanwaterdepthcm,data=Freshwater_Data))# 1.39e-07 ***

#Subset environ variables to plot:
intro_data_Freshwater<-Freshwater_Data %>%
  mutate(Soil_Salinity = Mean_SoilSalinity,
         Introduced_Cover = introduced_Cover,
         Water_Depth = meanwaterdepthcm) %>%
  select(Soil_Salinity, Water_Depth, Introduced_Cover )%>%
  gather(category, values,-Introduced_Cover) 

intro_plot_Freshwater <- ggplot(intro_data_Freshwater,aes(x=values, y=Introduced_Cover, group=category, 
                                      color = category))+
  labs(x = "",y="Freshwater Introduced Cover (%)")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  scale_color_manual(values =  c("#00BA38", "#619CFF"))+
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none")

intro_plot_Freshwater

#Run FacetWrapScaleControl.R script first (to control scale)!
i_Freshwater <- intro_plot_Freshwater + facet_wrap_custom(~category, scales = "free", ncol = 3, scale_overrides = list(
  scale_override(1, scale_x_continuous(limits = c(0, 20))),
  scale_override(2, scale_x_continuous(limits = c(-40, 60)))))
i_Freshwater


#i2, Intermediate=====
#Check the relationship:
summary(lm(introduced_Cover ~ Mean_SoilSalinity,data=Intermediate_Data))#2e-16 ***
summary(lm(introduced_Cover ~ meanwaterdepthcm,data=Intermediate_Data))# 0.473 

#Subset environ variables to plot:
intro_data_Intermediate<-Intermediate_Data %>%
  mutate(Soil_Salinity = Mean_SoilSalinity,
         Introduced_Cover = introduced_Cover,
         Water_Depth = meanwaterdepthcm) %>%
  select(Soil_Salinity, Water_Depth, Introduced_Cover )%>%
  gather(category, values,-Introduced_Cover) 

intro_plot_Intermediate <- ggplot(intro_data_Intermediate,aes(x=values, y=Introduced_Cover, group=category, 
                                      color = category))+
  labs(x = "",y="Intermediate Introduced Cover (%)")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  scale_color_manual(values =  c("#00BA38", "#619CFF"))+
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none")

intro_plot_Intermediate

#Run FacetWrapScaleControl.R script first (to control scale)!
i_Intermediate <- intro_plot_Intermediate + facet_wrap_custom(~category, scales = "free", ncol = 3, scale_overrides = list(
  scale_override(1, scale_x_continuous(limits = c(0, 20))),
  scale_override(2, scale_x_continuous(limits = c(-40, 60)))))
i_Intermediate

#i3, brackish=====
#Check the relationship:
summary(lm(introduced_Cover ~ Mean_SoilSalinity,data=Brackish_Data))#0.114
summary(lm(introduced_Cover ~ meanwaterdepthcm,data=Brackish_Data))#0.482 

#Subset environ variables to plot:
intro_data3<-Brackish_Data %>%
  mutate(Soil_Salinity = Mean_SoilSalinity,
         Introduced_Cover = introduced_Cover,
         Water_Depth = meanwaterdepthcm) %>%
  select(Soil_Salinity, Water_Depth, Introduced_Cover )%>%
  gather(category, values,-Introduced_Cover) 

intro_plot3 <- ggplot(intro_data3,aes(x=values, y=Introduced_Cover, group=category, 
                                      color = category))+
  labs(x = "",y="Brackish Introduced Cover (%)")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  scale_color_manual(values =  c("#00BA38", "#619CFF"))+
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none")

intro_plot4

#Run FacetWrapScaleControl.R script first!
i3 <- intro_plot3 + facet_wrap_custom(~category, scales = "free", ncol = 3, scale_overrides = list(
  scale_override(1, scale_x_continuous(limits = c(0, 20))),
  scale_override(2, scale_x_continuous(limits = c(-40, 60)))))
i3



#i4, saline=====
#Check the relationship:
summary(lm(introduced_Cover ~ Mean_SoilSalinity,data=Saline_Data))#2.60e-06 ***
summary(lm(introduced_Cover ~ meanwaterdepthcm,data=Saline_Data))# 1.14e-08 ***

#Subset environ variables to plot:
intro_data4<-Saline_Data %>%
  mutate(Soil_Salinity = Mean_SoilSalinity,
         Introduced_Cover = introduced_Cover,
         Water_Depth = meanwaterdepthcm) %>%
  select(Soil_Salinity, Water_Depth, Introduced_Cover )%>%
  gather(category, values,-Introduced_Cover) 

intro_plot4 <- ggplot(intro_data4,aes(x=values, y=Introduced_Cover, group=category, 
                                color = category))+
  labs(x = "",y="Saline Introduced Cover (%)")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  scale_color_manual(values =  c("#00BA38", "#619CFF"))+
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none")

intro_plot4

#Run FacetWrapScaleControl.R script first (to control scale)!
i4 <- intro_plot4 + facet_wrap_custom(~category, scales = "free", ncol = 3, scale_overrides = list(
  scale_override(1, scale_x_continuous(limits = c(0, 20))),
  scale_override(2, scale_x_continuous(limits = c(-40, 60)))))
i4

#Arrange introduced_cover plots together:========
library(gridExtra)
library(grid)

grid.arrange(i_Freshwater, i_Intermediate, ncol = 1 )

g <- arrangeGrob(i_Freshwater, i_Intermediate, nrow=2) #generates g

ggsave(g, filename = "4PanelsFigure_RawData_FullLabels_IntroducedCover.jpeg", 
       width = 22, 
       height = 15,
       units = "cm",
       dpi = 600)

ggsave(g, filename = "4PanelsFigure_RawData_FullLabels_IntroducedCover.pdf", 
       width = 22, 
       height = 15,
       units = "cm",
       dpi = 600)

# PLOT Salinity and Water Depth (suppl)=========
#For each marsh type, ie 4 panels side by side (fresh to saline),
#plot  mean soil pore water salinity and mean water depth  
#using the final averaged dataset
#showing the mean salinities and water depths across marsh types.

VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")#Our Veg Data. Proccessed as per CRMS file 1,2,3,4 in Github
names(VegAllEnvData)
VegAllEnvData$Community <- factor(VegAllEnvData$Community, levels = c("Freshwater","Intermediate","Brackish", "Saline"))

sal_plot <- ggplot(VegAllEnvData, aes(x = "", y = Mean_SoilSalinity, shape = Community)) +
  geom_point( alpha = 0.2)+ geom_jitter(alpha = 0.1)+
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1, color = "black") +
  stat_summary(fun.y = "mean", size = 5, geom = "point", color = "black")+
  #geom_boxplot(outlier.shape = NA) +
  facet_grid(.~ Community)+ #geom_jitter( size = 2,alpha = 0.4, aes(color = t))+
  labs(y = "Pore water salinity (ppt)", x="") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))

wat_plot <- ggplot(VegAllEnvData, aes(x = "", y = meanwaterdepthcm, shape = Community)) +
  geom_point( alpha = 0.2)+ geom_jitter(alpha = 0.1)+
  stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1, color = "black") +
  stat_summary(fun.y = "mean", size = 5, geom = "point", color = "black")+
  #geom_boxplot(outlier.shape = NA) +
  facet_grid(.~ Community)+ #geom_jitter( size = 2,alpha = 0.4, aes(color = t))+
  labs(y = "Water depth (cm)", x="") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 20, face = "bold", vjust = 0.5),
        strip.background =  element_rect(fill = "white"))
wat_plot

#Stitch two plots together and save:
g <- arrangeGrob(sal_plot,wat_plot, nrow=2) #generates g
ggsave(g, filename = "SalinityDepth4Communities_Suppl.pdf", 
       width = 22, 
       height = 15,
       units = "cm",
       dpi = 600)

#Plot Native Richness vs salinity, water depth,introduced cover (suppl)=========
#Make a fig with the other interactions 
#native richness vs salinity, water depth and introduced cover
#and native composition vs salinity,
#water depth, and introduced cover and put it in the supplement.
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")#Our Veg Data. Proccessed as per CRMS file 1,2,3,4 in Github
VegAllEnvData$Community <- factor(VegAllEnvData$Community, levels = c("Freshwater","Intermediate","Brackish", "Saline"))
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced

#Subset only veg matrix off whole data:
Veg_Matrix <- as.data.frame (subset (VegAllEnvData, select = c(Acer_rubrum:Ziza_miliacea)))

#Pick natives only from Veg_Matrix and compute their Native_Richness:
Native.Species <- filter(Plant_Info, nat == "native")
vars <- as.character(Native.Species$specCode)#to feed to next line, names of native plant species.
only.native <- select( Veg_Matrix , one_of(vars))
#Calculate Native_Richness:
Native_Richness <- specnumber (only.native) #create extra vector  with richness of all natives
VegAllEnvData$Native_Richness <- Native_Richness#convert vector into column with native richness

#Pick introduced only from Veg_Matrix and compute their total cover:
Introduced.Species <- filter(Plant_Info, nat == "introduced")
vars_introduced <- as.character(Introduced.Species$specCode)#to feed to next line, names of native plant species.
only.introduced <- select( Veg_Matrix , one_of(vars_introduced))
#Calculate total Introduced_Cover:
Introduced_Cover <- rowSums (only.introduced) #create extra vector  with richness of all natives
VegAllEnvData$Introduced_Cover <- Introduced_Cover #convert vector into column with native richness

#Compute Native_Composition in only.native 
Native_distance <- vegdist(only.native, "bray")
Native.PCA <- cmdscale(Native_distance, eig = TRUE)
Native_Composition <-data.frame(Native_Composition = Native.PCA$points[,1]) #get MDS1 (x-axis Comp value)
VegAllEnvData$Native_Composition <- Native_Composition$Native_Composition #convert vector into column with native richness

#PLOTS:
#Native_Richness
r1 <- ggplot(VegAllEnvData,aes(y=Native_Richness, x=Mean_SoilSalinity))+
  labs(x = "Soil salinity (ppt)",y="Native richness")+
  scale_y_continuous(limits = c(0,45))+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community,nrow=1) + 
  theme(legend.position = "none")
r1

#meanwaterdepthcm:
w1 <- ggplot(VegAllEnvData,aes(x=Native_Richness, y=meanwaterdepthcm))+
  labs(x = "Water depth (cm)",y="Native richness")+
  scale_y_continuous(limits = c(0,45))+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community, nrow = 1) + 
  theme(legend.position = "none")
w1
  
#Introduced_Cover:
c1 <- ggplot(VegAllEnvData,aes(x=Native_Richness, y=Introduced_Cover))+
  labs(x = "Introduced cover (%)",y="Native richness")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0,45))+
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community, nrow = 1) + 
  theme(legend.position = "none")
c1

suppl <- arrangeGrob(r1,w1,c1, nrow=3) #generates g

ggsave(suppl, filename = "Suppl_Richness_RawData_Interactions.pdf", 
       width = 22, 
       height = 15,
       units = "cm",
       dpi = 600)


#Plot Native_Composition vs salnity, water depth and introduced cover ==========
#Native_Richness
r2 <- ggplot(VegAllEnvData,aes(y=Native_Composition, x=Mean_SoilSalinity))+
  labs(x = "Soil salinity (ppt)",y="Native composition")+
  scale_y_continuous(limits = c(0,0.6))+
    geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community,nrow=1) + 
  theme(legend.position = "none")
r2

#meanwaterdepthcm:
w2 <- ggplot(VegAllEnvData,aes(x=Native_Composition, y=meanwaterdepthcm))+
  labs(x = "Water depth (cm)",y="Native composition")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0,0.6))+
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community, nrow = 1) + 
  theme(legend.position = "none")
w2

#Introduced_Cover:
c2 <- ggplot(VegAllEnvData,aes(y=Native_Composition, x=Introduced_Cover))+
  labs(x = "Introduced cover (%)",y="Native composition")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0,0.6))+
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community, nrow = 1) + 
  theme(legend.position = "none")
c2

suppl2 <- arrangeGrob(r2,w2,c2, nrow=3) #generates g

ggsave(suppl2, filename = "Suppl_Composition_RawData_Interactions.pdf", 
       width = 22, 
       height = 15,
       units = "cm",
       dpi = 600)

