#LOAD RAW DATA:=======
library(corrplot)
library(Hmisc)
library(tidyverse)
library(vegan)

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
#most abundant specCode (topID1):
head(topID1)
1    Sagi_lancifolia
2     Pani_hemitomon
3     Poly_punctatum
4   Alte_philoxeroid
5      Leer_hexandra

#join Freshwater_Plants & Plant_List to see which species are native/introduced:
Plant_List<- subset(Plant_Info, select = c(specCode, nat))
Freshwater_Plants <- left_join(Freshwater_Plant_Sp,Plant_List, by = "specCode")

#Pick natives only from Freshwater_Data and compute their total cover:
Freshwater_Native.Species <- filter(Freshwater_Plants, nat == "native")#228 native species
Freshwater.native <- subset( Freshwater_Data, select = unique(Freshwater_Native.Species$specCode))
Freshwater_Native_Cover <- rowSums (Freshwater.native) #create extra column with total cover of all natives
Freshwater_Data$Native_Cover <- Freshwater_Native_Cover
#Check the relationship:
summary(lm(Native_Cover ~ Mean_SoilSalinity,data=Freshwater_Data))#1.2e-06 ***

#Pick introduced only from Freshwater_Data and compute their total cover:
Freshwater_introduced.Species <- filter(Freshwater_Plants, nat == "introduced")#228 native species
Freshwater.introduced <- subset( Freshwater_Data, select = unique(Freshwater_introduced.Species$specCode))
Freshwater_introduced_Cover <- rowSums (Freshwater.introduced) #create extra column with total cover of all introduced
Freshwater_Data$introduced_Cover <- Freshwater_introduced_Cover
#Check the relationship:
summary(lm(introduced_Cover ~ Mean_SoilSalinity,data=Freshwater_Data))#0.00129 **


data1<-Freshwater_Data %>%
  mutate(Salinity = Mean_SoilSalinity,
         Introduced = introduced_Cover,
         Water = meanwaterdepthcm) %>%
  select(Salinity,Introduced, Water, Native_Cover )%>%
  gather(category, values,-Native_Cover) 

#plot1==========
plot1 <- ggplot(data1,aes(x=values, y=Native_Cover, group=category, color = category))+
  labs(x = "",y="Freshwater Native Cover (%)")+
  geom_point() + geom_smooth(method = "lm") +
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
  mutate(Salinity = Mean_SoilSalinity,
         Introduced = introduced_Cover,
         Water = meanwaterdepthcm) %>%
  select(Salinity,Introduced, Water, Native_Cover )%>%
  gather(category, values,-Native_Cover) 

#plot2=======
plot2 <- ggplot(data2,aes(x=values, y=Native_Cover, group=category, color = category))+
  labs(x = "",y="Intermediate Native Cover (%)")+
  geom_point() + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none")

plot2

#Run FacetWrapScaleControl.R script first!
p2 <- plot2  + facet_wrap_custom(~category, scales = "free", ncol = 3, scale_overrides = list(
  scale_override(1, scale_x_continuous(limits = c(0, 100))),
  scale_override(2, scale_x_continuous(limits = c(0, 20))),
  scale_override(3, scale_x_continuous(limits = c(-40, 60)))))

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
  mutate(Salinity = Mean_SoilSalinity,
         Introduced = introduced_Cover,
         Water = meanwaterdepthcm) %>%
  select(Salinity, Water, Native_Cover )%>%
  gather(category, values,-Native_Cover) 

#plot3=======
plot3 <- ggplot(data3,aes(x=values, y=Native_Cover, group=category, color = category))+
  labs(x = "",y="Brackish Native Cover (%)")+
  geom_point() + geom_smooth(method = "lm") +
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
  mutate(Salinity = Mean_SoilSalinity,
         Introduced = introduced_Cover,
         Water = meanwaterdepthcm) %>%
  select(Salinity, Water, Native_Cover )%>%
  gather(category, values,-Native_Cover) 

#plot4=====
plot4 <- ggplot(data4,aes(x=values, y=Native_Cover, group=category, color = category))+
  labs(x = "",y="Saline Native Cover (%)")+
  geom_point() + geom_smooth(method = "lm") +
  scale_color_manual(values =  c("#00BA38", "#619CFF"))+
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none")

plot4

#Run FacetWrapScaleControl.R script first!
p4 <- plot4 + facet_wrap_custom(~category, scales = "free", ncol = 3, scale_overrides = list(
  scale_override(1, scale_x_continuous(limits = c(0, 20))),
  scale_override(2, scale_x_continuous(limits = c(-40, 60)))))


#Arrange all plots together:========
library(gridExtra)
library(grid)

grid.arrange(plot1, plot3,plot2,plot4, ncol = 2 )

g <- arrangeGrob(plot1,plot3,plot2,plot4, nrow=2) #generates g

ggsave(g, filename = "10PanelsFigure_RawData.jpeg", 
       width = 21, 
       height = 15,
       units = "cm",
       dpi = 600)

#Even scales:
grid.arrange(p1, p3,p2,p4, ncol = 2 )

g_even <- arrangeGrob(p1, p3,p2,p4, nrow=2) #generates g

ggsave(g_even, filename = "10Panels_EvenScales_Figure_RawData.jpeg", 
       width = 21, 
       height = 15,
       units = "cm",
       dpi = 600)
