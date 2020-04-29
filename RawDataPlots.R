#LOAD LIBRARIES:=======
library(corrplot)
library(Hmisc)
library(tidyverse)
library(vegan)
library(gridExtra)
library(ggpmisc)
library(grid)
#To Draw Figures for looking at relationships between the different variables
#in the path analysis as in Path_SEM7_Summaries.R file

#Freshwater Native_Cover Data =======
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")#Our Veg Data. Proccessed as per CRMS file 1,2,3,4 in Github
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
native <- Plant_Info[Plant_Info$nat == "native",5]

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
summary(lm(Native_Cover ~introduced_Cover ,data=Freshwater_Data))#4.6e-07 ***
summary(lm(Native_Cover ~ Mean_SoilSalinity,data=Freshwater_Data))# 0.00143 ** 
summary(lm(Native_Cover ~ meanwaterdepthcm,data=Freshwater_Data))#P=0.0581 .  

data1<-Freshwater_Data %>%
  mutate(Soil_Salinity = Mean_SoilSalinity,
         Introduced_Cover = introduced_Cover,
         Water_Depth = meanwaterdepthcm) %>%
  select(Soil_Salinity,Introduced_Cover, Water_Depth, Native_Cover )%>%
  gather(category, values,-Native_Cover) 

#plot1a Freshwater NatCov~Introduced_Cover======
plot1a <- ggplot(data1[data1$category == "Introduced_Cover",],aes(x=values, y=Native_Cover, group=category))+
  labs(x = "",y="Freshwater native cover (%)")+
  geom_point( aes(color = "#F8766D")) +
  stat_smooth(method = "lm",color = "#F8766D") +
  scale_color_manual(values =  c("#F8766D"))+
  scale_x_continuous(limits = c(0, 100))+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
plot1a


#plot1b Freshwater NatCov~Soil_Salinity======
plot1b <- ggplot(data1[data1$category == "Soil_Salinity",],aes(x=values, y=Native_Cover, group=category))+
  labs(x = "",y="")+
  geom_point( aes(color = "#00BA38")) +
  stat_smooth(method = "lm",color = "#00BA38") +
  scale_color_manual(values =  c("#00BA38"))+
  scale_x_continuous(limits = c(0, 30))+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
plot1b

#plot1c Freshwater NatCov~Water_Depth ==========
plot1c <- ggplot(data1[data1$category == "Water_Depth",],aes(x=values, y=Native_Cover, group=category, color = category))+
  geom_point(aes(color = "#619CFF"))+
  labs(x = "",y="")+
  scale_color_manual(values =  c("#619CFF","black"))+
  scale_x_continuous(limits = c(-40, 60))+
  stat_smooth(method = "lm",color = "#619CFF") +
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)


plot1c


#Intermediate Native_Cover Data:========
#Figures for looking at relationships between the different variables
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
summary(lm(Native_Cover ~ introduced_Cover,data=Intermediate_Data))#7.42e-08 ***
summary(lm(Native_Cover ~ Mean_SoilSalinity,data=Intermediate_Data))#2e-13 ***
summary(lm(Native_Cover ~ meanwaterdepthcm,data=Intermediate_Data))#<2e-16 ***


data2<-Intermediate_Data %>%
  mutate(Soil_Salinity = Mean_SoilSalinity,
         Introduced_Cover = introduced_Cover,
         Water_Depth = meanwaterdepthcm) %>%
  select(Soil_Salinity,Introduced_Cover, Water_Depth, Native_Cover )%>%
  gather(category, values,-Native_Cover) 

#plot2a Intermediate NatCov~Introduced_Cover======
plot2a <- ggplot(data2[data2$category == "Introduced_Cover",],aes(x=values, y=Native_Cover, group=category))+
  labs(x = "",y="")+
  geom_point( aes(color = "#F8766D")) +
  labs(x = "",y="Intermediate native cover (%)")+
  stat_smooth(method = "lm",color = "#F8766D") +
  scale_color_manual(values =  c("#F8766D"))+
  scale_x_continuous(limits = c(0, 100))+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
plot2a


#plot2b Intermediate  NatCov~Soil_Salinity======
plot2b <- ggplot(data2[data2$category == "Soil_Salinity",],aes(x=values, y=Native_Cover, group=category))+
  geom_point( aes(color = "#00BA38")) +
  stat_smooth(method = "lm",color = "#00BA38") +
  scale_color_manual(values =  c("#00BA38"))+
  scale_x_continuous(limits = c(0, 30))+
  facet_wrap(~category,scales="free") + 
  labs(x="",y="")+
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
plot2b

#plot2c Intermediate NatCov~Water_Depth ==========
plot2c <- ggplot(data2[data2$category == "Water_Depth",],aes(x=values, y=Native_Cover, group=category, color = category))+
  geom_point()+
  labs(x = "",y="")+
    scale_color_manual(values =  c("#619CFF"))+
  scale_x_continuous(limits = c(-40, 60))+
    facet_wrap(~category,scales="free") + 
    theme(legend.position = "none",
        strip.text=element_text(size=16))

plot2c
#Brackish Native_Cover Data:==========
#Figures for looking at relationships between the different variables in the path analysis
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
#Brackish_Data <- Brackish_Data [- which.max(Brackish_Data$Native_Cover), ] #removing outlier keeps it still signif P<0.002
summary(lm(Native_Cover ~ Mean_SoilSalinity,data=Brackish_Data))#0.00171 ** 
summary(lm(Native_Cover ~ meanwaterdepthcm,data=Brackish_Data))#<2e-16 ***

data3<-Brackish_Data %>%
  mutate(Soil_Salinity = Mean_SoilSalinity,
         introduced_Cover = introduced_Cover,
         Water_Depth = meanwaterdepthcm) %>%
  select(Soil_Salinity, Water_Depth, Native_Cover )%>%
  gather(category, values,-Native_Cover) 

#plot3a Brackish NatCov~Soil_Salinity=======
plot3a <- ggplot(data3[data3$category=="Soil_Salinity",],aes(x=values, y=Native_Cover, group = category))+
  labs(x = "",y="Brackish native cover (%)")+
  geom_point( aes(color = "#00BA38")) +
  scale_color_manual(values =  c("#00BA38"))+
  scale_x_continuous(limits = c(0, 30))+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))

plot3a

#plot3b Brackish NatCov~Water_Depth =======

plot3b <- ggplot(data3[data3$category=="Water_Depth",],aes(x=values, y=Native_Cover, group=category))+
  labs(x = "",y="")+
  geom_point( aes(color = "#619CFF")) +
  stat_smooth(method = "lm",color = "#619CFF") +
  scale_color_manual(values =  c("#619CFF"))+
  scale_x_continuous(limits = c(-40, 60))+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)

plot3b

#Saline Native_Cover Data:==========
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

#plot4a NatCov~Soil_Salinity=====
plot4a <- ggplot(data4[data4$category=="Soil_Salinity",],aes(x=values, y=Native_Cover, group=category, color = category))+
  labs(x = "",y="Saline native cover (%)")+
  geom_point()+
  geom_smooth(method = "lm") +
  scale_color_manual(values =  c("black","#00BA38"))+
  scale_x_continuous(limits = c(0, 30))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(color= "black",label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                               stat(r.squared), stat(p.value))),
                  parse = TRUE)+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none" ,
        strip.text=element_text(size=16))

plot4a

#plot4b NatCov~Water_Depth =====
plot4b <- ggplot(data4[data4$category=="Water_Depth",],
                 aes(x=values, y=Native_Cover, group=category, color = category))+
  labs(x = "",y="")+
  geom_point(aes(color = c("#619CFF")))+
  geom_smooth(method = "lm", color = "#619CFF" ) +
  scale_color_manual(values =  c( "#619CFF","black"))+
  scale_x_continuous(limits = c(-40, 60))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(color= "black",label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                               stat(r.squared), stat(p.value))),
                  parse = TRUE)+
  facet_wrap(~category,scales="free") + 
  theme(legend.position = "none", strip.text=element_text(size=16))

plot4b


#Arrange all Native_Cover plots together (10 panels):========
grid.arrange(plot1a,plot1b,plot1c,  plot3a,  plot3b, #Check if all looks good
             plot2a,plot2b, plot2c, plot4a,  plot4b,
             ncol = 5 )#Even scales:

g_even <- arrangeGrob (plot1a,plot1b,plot1c, plot3a,  plot3b, #To produce pdf
                       plot2a,plot2b, plot2c,plot4a,  plot4b,
                      nrow = 2 )#Even scales:

#ggsave(g_even, filename = "10Panels_EvenScales_Figure_RawData_FullLabels_StatsOn.jpeg", 
       width = 32, 
       height = 15,
       units = "cm",
       dpi = 600)

#ggsave(g_even, filename = "10Panels_EvenScales_Figure_RawData_FullLabels_StatsOn_Salinity30ppt.pdf", 
       width = 32, 
       height = 15,
       units = "cm",
       dpi = 600)

#Introdued_Cover vs Salinity=====
#ADD A FIG with 4 panels showing invasive cover vs salinity 
#and water depth in fresh and intermediate marshes.

#Freshwater Introduced_Cover (%)  =====
#Check the relationship:
summary(lm(introduced_Cover ~ Mean_SoilSalinity,data=Freshwater_Data))#20.00129 ** 
summary(lm(introduced_Cover ~ meanwaterdepthcm,data=Freshwater_Data))# 1.39e-07 ***

#Subset environ variables to plot:
Introduced_Cover_Freshwater<-Freshwater_Data %>%
  mutate(Soil_Salinity = Mean_SoilSalinity,
         Introduced_Cover = introduced_Cover,
         Water_Depth = meanwaterdepthcm) %>%
  select(Soil_Salinity, Water_Depth, Introduced_Cover )%>%
  gather(category, values,-Introduced_Cover) 

plot_f1 <- ggplot(Introduced_Cover_Freshwater[Introduced_Cover_Freshwater$category=="Soil_Salinity",],
                  aes(x=values, y=Introduced_Cover, group=category))+
  geom_point( aes(color = "#00BA38")) +
  scale_color_manual(values =  c("#00BA38"))+
  scale_x_continuous(limits = c(0, 30))+
  facet_wrap(~category,scales="free") + 
  labs(x="",y="Freshwater introduced cover (%)")+
  theme(legend.position = "none",
        strip.text=element_text(size=16))
plot_f1


plot_f2 <- ggplot(Introduced_Cover_Freshwater[Introduced_Cover_Freshwater$category=="Water_Depth",],
                  aes(x=values, y=Introduced_Cover, group=category))+
  geom_point( aes(color = "#619CFF")) +
  scale_color_manual(values =  c("#619CFF"))+
  scale_x_continuous(limits = c(-40, 60))+
  facet_wrap(~category,scales="free") + 
  labs(x="",y="")+
  theme(legend.position = "none",
        strip.text=element_text(size=16))
plot_f2


#Intermediate Introduced_Cover (%)=====
#Check the relationship:
summary(lm(introduced_Cover ~ Mean_SoilSalinity,data=Intermediate_Data))#2e-16 ***
summary(lm(introduced_Cover ~ meanwaterdepthcm,data=Intermediate_Data))# 0.473 

#Subset environ variables to plot:
Introduced_Cover_Intermediate <- Intermediate_Data %>%
  mutate(Soil_Salinity = Mean_SoilSalinity,
         Introduced_Cover = introduced_Cover,
         Water_Depth = meanwaterdepthcm) %>%
  select(Soil_Salinity, Water_Depth, Introduced_Cover )%>%
  gather(category, values,-Introduced_Cover) 

#plot_i1 Intermediate Intrd~WaterDepth====
plot_i1 <-  ggplot(Introduced_Cover_Intermediate[Introduced_Cover_Intermediate$category=="Water_Depth",],
                             aes(x=values, y=Introduced_Cover, group=category))+
  geom_point( aes(color = "#619CFF")) +
  scale_color_manual(values =  c("#619CFF"))+
  scale_x_continuous(limits = c(-40, 60))+
  facet_wrap(~category,scales="free") + 
  labs(y="",x="")+
  theme(legend.position = "none",
        strip.text=element_text(size=16))
plot_i1

#plot_i1 Intermediate Intrd~Soil_Salinity====
plot_i2 <-  ggplot(Introduced_Cover_Intermediate[Introduced_Cover_Intermediate$category=="Soil_Salinity",],
                   aes(x=values, y=Introduced_Cover, group=category))+
  geom_point( aes(color = "#00BA38")) +
  stat_smooth(method = "lm",color = "#00BA38") +
  scale_color_manual(values =  c("#00BA38"))+
  scale_x_continuous(limits = c(0, 30))+
  facet_wrap(~category,scales="free") + 
  labs(y="Intermediate introduced cover (%)",x="")+
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
plot_i2 


#Arrange introduced_cover plots together:========
ffii <- arrangeGrob (plot_f1, plot_f2, plot_i2, plot_i1, nrow=2) 

#ggsave(ffii, filename = "4PanelsFigure_RawData_FullLabels_IntroducedCover_StatsOn.jpg", 
       width = 22, 
       height = 15,
       units = "cm",
       dpi = 200)

ggsave(ffii, filename = "4Panels_IntroducedCover_RawData__StatsOn.pdf", 
       width = 22, 
       height = 15,
       units = "cm",
       dpi = 600)

# HISTOGRAM of Salinity and Water Depth (suppl)=========
#For each marsh type, ie 4 panels side by side (fresh to saline),
#plot  mean soil pore water salinity and mean water depth  
#using the final averaged dataset
#showing the mean salinities and water depths across marsh types.

VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")#Our Veg Data. Proccessed as per CRMS file 1,2,3,4 in Github
VegAllEnvData$Community <- factor(VegAllEnvData$Community, levels = c("Freshwater","Intermediate","Brackish", "Saline"))

sal_plot <- ggplot(VegAllEnvData, aes(MeanWaterSalinity, group = Community)) +
  geom_histogram()+
  facet_grid(.~ Community)+ 
  labs(y = "", x="Soil salinity (ppt)") +
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
sal_plot

wat_plot <- ggplot(VegAllEnvData, aes(meanwaterdepthcm)) +
  #geom_point( alpha = 0.2)+ geom_jitter(alpha = 0.1)+
  #stat_summary(fun.data="mean_cl_boot", geom="errorbar", width=0.2,size = 1, color = "black") +
  #stat_summary(fun.y = "mean", size = 5, geom = "point", color = "black")+
  geom_histogram() +
  facet_grid(.~ Community)+ 
  labs(y = "", x="Water depth (cm)") +
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
wat_sal <- arrangeGrob(sal_plot,wat_plot, nrow=2) #generates g
ggsave(wat_sal , filename = "HISTOGRAM_SalinityDepth4Communities.pdf", 
       width = 22, 
       height = 15,
       units = "cm",
       dpi = 600)

#Plot Native Richness/Cover/Composition vs salinity, water depth,introduced cover (suppl)=========
#Make a fig with the other interactions native richness vs salinity, water depth and introduced cover
#and native composition vs salinity, water depth, and introduced cover and put it in the supplement.
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

#PLOTS: Native_Richness~Mean_SoilSalinity all together (suppl):======
r1 <- ggplot(VegAllEnvData,aes(y=Native_Richness, x=Mean_SoilSalinity))+
  labs(x = "Soil salinity (ppt)",y="Native richness")+
  scale_y_continuous(limits = c(0,45))+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community,nrow=1) + 
  theme(legend.position = "none")
r1

#plot r1a, Freshwater Native_Richness ~Soil_Salinity======
r1a <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Freshwater",],
             aes(y=Native_Richness, x=Mean_SoilSalinity))+
  labs(x = "Soil salinity (ppt)",y="Native richness")+
  scale_y_continuous(limits = c(0,45))+ 
  geom_point(aes(alpha=0.2,color= c("#00BA38"))) + 
  stat_smooth(method = "lm",color = "#00BA38") +
  scale_color_manual(values =  c("#00BA38"))+
  scale_x_continuous(limits = c(0, 30))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
            parse = TRUE)
r1a 

#plot r1b, Intermediate Native_Richness ~Soil_Salinity======
r1b <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Intermediate",],
              aes(y=Native_Richness, x=Mean_SoilSalinity))+
  labs(x = "Soil salinity (ppt)",y="Native richness")+
  scale_y_continuous(limits = c(0,45))+ 
  geom_point(aes(alpha=0.2,color="#00BA38")) + 
  stat_smooth(method = "lm",color = "#00BA38") +
  scale_color_manual(values =  c("#00BA38"))+
  scale_x_continuous(limits = c(0, 30))+
    facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
r1b      

#plot r1c, Brackish Native_Richness ~Soil_Salinity======
r1c <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Brackish",],
              aes(y=Native_Richness, x=Mean_SoilSalinity))+
  labs(x = "Soil salinity (ppt)",y="Native richness")+
  scale_y_continuous(limits = c(0,45))+ 
  geom_point(aes(alpha=0.2,color="#00BA38")) + 
  scale_color_manual(values =  c("#00BA38"))+
  scale_x_continuous(limits = c(0, 30))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
r1c      

#plot r1d, Saline Native_Richness ~Soil_Salinity======
r1d <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Saline",],
              aes(y=Native_Richness, x=Mean_SoilSalinity))+
  labs(x = "Soil salinity (ppt)",y="Native richness")+
  scale_y_continuous(limits = c(0,45))+ 
  geom_point(aes(alpha=0.2,color="#00BA38")) + 
  stat_smooth(method = "lm",color = "#00BA38") +
  scale_color_manual(values =  c("#00BA38"))+
  scale_x_continuous(limits = c(0,30))+ #max value of Mean_Salinity is 28.46111
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
r1d  

#PLOTS: Native_Richness~meanwaterdepthcm: all together (suppl):======
w1 <- ggplot(VegAllEnvData,aes(x=Native_Richness, y=meanwaterdepthcm))+
  labs(x = "Water depth (cm)",y="Native richness")+
  scale_y_continuous(limits = c(0,45))+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community, nrow = 1) + 
  theme(legend.position = "none")
w1

#plot w1a, Freshwater Native_Richness ~ meanwaterdepthcm======
w1a <-  ggplot(VegAllEnvData[VegAllEnvData$Community=="Freshwater",],
              aes(y=Native_Richness, x=meanwaterdepthcm))+
  labs(x = "Water depth (cm)",y="Native richness")+
  scale_y_continuous(limits = c(0,45))+ 
  scale_x_continuous(limits = c(-40, 60))+
    geom_point(aes(alpha=0.2,color="#619CFF")) + 
  scale_color_manual(values =  c("#619CFF"))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
w1a

#plot w1b, Intermediate Native_Richness ~meanwaterdepthcm======
w1b <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Intermediate",],
              aes(y=Native_Richness, x = meanwaterdepthcm))+
  labs(x = "Water depth (cm)" ,y="Native richness")+
  scale_y_continuous(limits = c(0,45))+ 
  scale_x_continuous(limits = c(-40, 60))+
    geom_point(aes(alpha=0.2,color="#619CFF")) + 
  scale_color_manual(values =  c("#619CFF"))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
w1b      

#plot w1c, Brackish Native_Richness ~ meanwaterdepthcm======
w1c <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Brackish",],
              aes(y=Native_Richness, x=meanwaterdepthcm))+
  labs(x = "Water depth (cm)" ,y="Native richness")+
  scale_y_continuous(limits = c(0,45))+ 
  scale_x_continuous(limits = c(-40, 60))+
    geom_point(aes(alpha=0.2,color="#619CFF")) + 
  stat_smooth(method = "lm",color = "#619CFF") +
  scale_color_manual(values =  c("#619CFF"))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
w1c  


#plot w1d, Saline Native_Richness ~ meanwaterdepthcm ======
w1d <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Saline",],
              aes(y=Native_Richness, x=meanwaterdepthcm))+
  labs(x = "Water depth (cm)" ,y="Native richness")+
  scale_y_continuous(limits = c(0,45))+ 
  scale_x_continuous(limits = c(-40, 60))+
    geom_point(aes(alpha=0.2,color="#619CFF")) + 
  stat_smooth(method = "lm",color = "#619CFF") +
  scale_color_manual(values =  c("#619CFF"))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
w1d  


#PLOTS Introduced_Cover all together (suppl)======
c1 <- ggplot(VegAllEnvData,aes(x=Native_Richness, y=Introduced_Cover))+
  labs(x = "Introduced cover (%)",y="Native richness")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0,45))+
  scale_x_continuous(limits = c(0, 100))+
    #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community, nrow = 1) + 
  theme(legend.position = "none")
c1


#plot c1a, Freshwater Native_Richness ~Introduced_Cover======
c1a <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Freshwater",],
              aes(y=Native_Richness, x=Introduced_Cover))+
  labs(x = "Introduced cover (%)",y="Native richness")+
  scale_y_continuous(limits = c(0,45))+ 
  scale_x_continuous(limits = c(0, 100))+
    geom_point(aes(alpha=0.2,color="#F8766D")) + 
  scale_color_manual(values =  c("#F8766D"))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
c1a 

#plot c1b, Intermediate Native_Richness ~Introduced_Cover======
c1b <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Intermediate",],
              aes(y=Native_Richness, x=Introduced_Cover))+
  labs(x = "Introduced cover (%)",y="Native richness")+
  scale_y_continuous(limits = c(0,45))+ 
  scale_x_continuous(limits = c(0, 100))+
    geom_point(aes(alpha=0.2,color="#F8766D")) + 
  stat_smooth(method = "lm",color = "#F8766D") +
  scale_color_manual(values =  c("#F8766D"))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.4f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
c1b

#plot c1c, Brackish  Native_Richness ~Introduced_Cover======
c1c <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Brackish",],
              aes(y=Native_Richness, x=Introduced_Cover))+
  labs(x = "Introduced cover (%)",y="Native richness")+
  scale_y_continuous(limits = c(0,45))+ 
  scale_x_continuous(limits = c(0, 100))+
    geom_point(aes(alpha=0.2,color="#F8766D")) + 
  scale_color_manual(values =  c("#F8766D"))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
c1c 

#plot c1d, Saline  Native_Richness ~Introduced_Cover======
c1d <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Saline",],
              aes(y=Native_Richness, x=Introduced_Cover))+
  labs(x = "Introduced cover (%)",y="Native richness")+
  scale_y_continuous(limits = c(0,45))+ 
  scale_x_continuous(limits = c(0, 100))+
    geom_point(aes(alpha=0.2,color="#F8766D")) + 
  scale_color_manual(values =  c("#F8766D"))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
c1d


#Double checking the stats on the plot: (correct!)
summary(lm(Native_Richness~Introduced_Cover, VegAllEnvData[VegAllEnvData$Community=="Intermediate",]))

suppl_rich <- arrangeGrob(c1a,c1b,c1c,c1d,
                          r1a,r1b,r1c,r1d,
                          w1a,w1b,w1c,w1d,
                          
                          nrow=3) 
ggsave(suppl_rich, filename = "12Panel_NativeRichness_RawData_StatsOn.pdf", 
       width = 32, 
       height = 15,
       units = "cm",
       dpi = 600)


#PLOT Native_Composition all together (suppl) ==========
com1 <- ggplot(VegAllEnvData,aes(y=Native_Composition, x=Mean_SoilSalinity))+
  labs(x = "Soil salinity (ppt)",y="Native composition")+
  scale_y_continuous(limits = c(0,0.6))+
    geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community,nrow=1) + 
  theme(legend.position = "none")
com1

#plot com1a, Freshwater Native_Composition ~Soil_Salinity======
com1a <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Freshwater",],
              aes(y=Native_Composition, x=Mean_SoilSalinity))+
  labs(x = "Soil salinity (ppt)",y="Native composition")+
  scale_y_continuous(limits = c(0,0.6))+ 
  geom_point(aes(alpha=0.2,color="#00BA38")) + 
  stat_smooth(method = "lm",color = "#00BA38") +
  scale_color_manual(values =  c("#00BA38"))+
  scale_x_continuous(limits = c(0, 30))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
com1a

#plot com1b, Intermediate Native_Composition ~Soil_Salinity======
com1b <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Intermediate",],
                aes(y=Native_Composition, x=Mean_SoilSalinity))+
  labs(x = "Soil salinity (ppt)",y="Native composition")+
  scale_y_continuous(limits = c(0,0.6))+ 
  geom_point(aes(alpha=0.2,color="#00BA38")) + 
  stat_smooth(method = "lm",color = "#00BA38") +
  scale_color_manual(values =  c("#00BA38"))+
  scale_x_continuous(limits = c(0, 30))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
com1b

#plot com1c, Brackish Native_Composition ~Soil_Salinity======
com1c <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Brackish",],
                aes(y=Native_Composition, x=Mean_SoilSalinity))+
  labs(x = "Soil salinity (ppt)",y="Native composition")+
  scale_y_continuous(limits = c(0,0.6))+ 
  geom_point(aes(alpha=0.2,color="#00BA38")) + 
  scale_color_manual(values =  c("#00BA38"))+
  scale_x_continuous(limits = c(0, 30))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
com1c

#plot com1d, Saline Native_Composition ~Soil_Salinity======
com1d <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Saline",],
                aes(y=Native_Composition, x=Mean_SoilSalinity))+
  labs(x = "Soil salinity (ppt)",y="Native composition")+
  scale_y_continuous(limits = c(0,0.6))+ 
  geom_point(aes(alpha=0.2,color="#00BA38")) + 
  stat_smooth(method = "lm",color = "#00BA38") +
  scale_color_manual(values =  c("#00BA38"))+
  scale_x_continuous(limits = c(0, 30))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
com1d



#PLOT Native_Composition~meanwaterdepthcm all together (suppl):============
w2 <- ggplot(VegAllEnvData,aes(y=Native_Composition, x=meanwaterdepthcm))+
  labs(x = "Water depth (cm)",y="Native composition")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0,0.6))+
  scale_x_continuous(limits = c(-40,60))+
    #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community, nrow = 1) + 
  theme(legend.position = "none")
w2
#plot com2a, Freshwater Native_Composition ~meanwaterdepthcm======
com2a <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Freshwater",],
                aes(y=Native_Composition, x=meanwaterdepthcm))+
  labs(x = "Water depth (cm)", y = "Native composition")+
  scale_y_continuous(limits = c(0,0.6))+ 
  geom_point(aes(alpha=0.2,color="#619CFF")) + 
  scale_color_manual(values =  c("#619CFF"))+
  scale_x_continuous(limits = c(-40, 60))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
com2a

#plot com2b, Intermediate Native_Composition ~meanwaterdepthcm======
com2b <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Intermediate",],
                aes(y=Native_Composition, x=meanwaterdepthcm))+
  labs(x = "Water depth (cm)", y = "Native composition")+
  scale_y_continuous(limits = c(0,0.6))+ 
  geom_point(aes(alpha=0.2,color="#619CFF")) + 
  scale_color_manual(values =  c("#619CFF"))+
  scale_x_continuous(limits = c(-40, 60))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
com2b

#plot com2c, Brackish Native_Composition ~meanwaterdepthcm======
com2c <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Brackish",],
                aes(y=Native_Composition, x=meanwaterdepthcm))+
  labs(x = "Water depth (cm)", y = "Native composition")+
  scale_y_continuous(limits = c(0,0.6))+ 
  geom_point(aes(alpha=0.2,color="#619CFF")) + 
  scale_color_manual(values =  c("#619CFF"))+
  scale_x_continuous(limits = c(-40, 60))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
com2c

#plot com2d, Saline Native_Composition ~meanwaterdepthcm======
com2d <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Saline",],
                aes(y=Native_Composition, x=meanwaterdepthcm))+
  labs(x = "Water depth (cm)", y = "Native composition")+
  scale_y_continuous(limits = c(0,0.6))+ 
  geom_point(aes(alpha=0.2,color="#619CFF")) + 
  scale_color_manual(values =  c("#619CFF"))+
  scale_x_continuous(limits = c(-40, 60))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
com2d

#plot com3 Native_Composition~Introduced_Cover all together (suppl):======
com3 <- ggplot(VegAllEnvData,aes(y=Native_Composition, x=Introduced_Cover))+
  labs(x = "Introduced cover (%)",y="Native composition")+
  geom_point(alpha=0.2) + geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(0,0.6))+
  #geom_line(stat="smooth",method = "lm",size=.8)+
  facet_wrap(~Community, nrow = 1) + 
  theme(legend.position = "none")

com3

#plot com3a, Freshwater Native_Composition ~Introduced_Cover======
com3a <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Freshwater",],
                aes(y=Native_Composition, x=Introduced_Cover))+
  labs(x = "Introduced Cover (%)", y = "Native composition")+
  scale_y_continuous(limits = c(0,0.6))+ 
  geom_point(aes(alpha=0.2,color="#F8766D")) + 
  stat_smooth(method = "lm",color = "#F8766D") +
  scale_color_manual(values =  c("#F8766D"))+
  scale_x_continuous(limits = c(0, 100))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))+
  stat_fit_glance(method = "lm",
                  label.x = c(0.9,0),
                  method.args = list(formula = y ~ x),
                  mapping = aes(label = sprintf('R^2~"="~%.3f~~italic(P)~"="~%.2g',
                                                stat(r.squared), stat(p.value))),
                  parse = TRUE)
com3a

#plot com3b, Intermediate Native_Composition ~Introduced_Cover======
com3b <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Intermediate",],
                aes(y=Native_Composition, x=Introduced_Cover))+
  labs(x = "Introduced Cover (%)", y = "Native composition")+
  scale_y_continuous(limits = c(0,0.6))+ 
  geom_point(aes(alpha=0.2,color="#F8766D")) + 
  scale_color_manual(values =  c("#F8766D"))+
  scale_x_continuous(limits = c(0, 100))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
com3b

#plot com3c, Brackish Native_Composition ~Introduced_Cover======
com3c <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Brackish",],
                aes(y=Native_Composition, x=Introduced_Cover))+
  labs(x = "Introduced Cover (%)", y = "Native composition")+
  scale_y_continuous(limits = c(0,0.6))+ 
  geom_point(aes(alpha=0.2,color="#F8766D")) + 
  scale_color_manual(values =  c("#F8766D"))+
  scale_x_continuous(limits = c(0, 100))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
com3c

#plot com3d, Saline Native_Composition ~Introduced_Cover======
com3d <- ggplot(VegAllEnvData[VegAllEnvData$Community=="Saline",],
                aes(y=Native_Composition, x=Introduced_Cover))+
  labs(x = "Introduced Cover (%)", y = "Native composition")+
  scale_y_continuous(limits = c(0,0.6))+ 
  geom_point(aes(alpha=0.2,color="#F8766D")) + 
  scale_color_manual(values =  c("#F8766D"))+
  scale_x_continuous(limits = c(0, 100))+
  facet_wrap(~Community,scales="free") + 
  theme(legend.position = "none",
        strip.text=element_text(size=16))
com3d

#MERGE ALL COMPOSITION plots=====
suppl_natcomp <- arrangeGrob(com3a,com3b,com3c,com3d,
                             com2a,com2b,com2c,com2d,
                             com1a,com1b,com1c,com1d,
                             nrow=3)

ggsave(suppl_natcomp, filename = "12Panel_Composition_RawData_StatsOn.pdf", 
       width = 32, 
       height = 15,
       units = "cm",
       dpi = 600)

