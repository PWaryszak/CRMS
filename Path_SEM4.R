#Data +libraries + Apriori Model ========
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
dim(VegAllEnvData) # 3498  473

library("lavaan")
library("semPlot")
library("tidyverse")
library("vegan")
library("gridExtra")
library("grid")


#Apriori Path Analysis Model (for latter backward selection):
#Apriori_Model <-
#regressions:
NatRich     ~ Depth + Flood  + Soil + Alien  
Native      ~ Depth + Flood  + Soil + Alien
Alien       ~ Depth + Flood  + Soil 
NatComp ~ Alien + NatRich + Native
#fit_Apriori_Model <- sem(model1,missing="direct",estimator="ML",data=data)

#"Freshwater" Data (AV = average) ========
FreshwaterAV <-
  filter (VegAllEnvData, VegAllEnvData$Community=="Freshwater") %>%
  na.omit() %>%
  group_by(StationFront,Community) %>% #Compute means of all variables per StationFront (site), over all years:
  summarise_at(vars(richness, Mean_SoilSalinity,
                    meanwaterdepthcm, meanwaterdepthcm_SD,
                    floodedpercent,
                    MeanWaterSalinity, MeanWaterSalinity_SD,
                    Acer_rubrum:Ziza_miliacea), mean,na.rm=T)

#Subset native/introduced Veg matrix where colSums are > 0:
Freshwater_Veg_Matrix <- as.data.frame (subset ( FreshwaterAV, select = c(Acer_rubrum:Ziza_miliacea)))
Freshwater_Veg_Matrix <- Freshwater_Veg_Matrix [ , which(colSums(Freshwater_Veg_Matrix) > 0)]
Freshwater_Native_Richness <- specnumber(Freshwater_Veg_Matrix)#Compute Native-Only richness

#Pick out species from Freshwater communities:
colCount1 = colSums(Freshwater_Veg_Matrix) #sum up the abundance column-wise
topID1 = order(colCount1,decreasing=TRUE)[1:length(Freshwater_Veg_Matrix)] # choose all Freshwater plant species
topID1 = names(Freshwater_Veg_Matrix[topID1]) # names of plant species in decreasing order
Freshwater_Plant_Sp <- data.frame( specCode = topID1)

#join Freshwater_Plants & Plant_List to see which species are native/introduced:
Plant_List<- subset(Plant_Info, select = c(specCode, nat))
Freshwater_Plants <- left_join(Freshwater_Plant_Sp,Plant_List, by = "specCode")

#Select natives only from Freshwater.av and compute their total cover:
Freshwater_Native.Species <- filter(Freshwater_Plants, nat == "native")#228 native species
Freshwater.av.veg.native <- subset( FreshwaterAV, select = unique(Freshwater_Native.Species$specCode))
Freshwater.av.veg.native.total <- rowSums (Freshwater.av.veg.native) #create extra column with total cover of all natives

#Subset introduced only from Freshwater.av and compute their total cover::
Freshwater_introduced.Species <- filter(Freshwater_Plants, nat == "introduced")
Freshwater.av.veg.introduced <- subset( FreshwaterAV, select = unique(Freshwater_introduced.Species$specCode))
Freshwater.av.veg.introduced.total <- rowSums (Freshwater.av.veg.introduced)#Extra column = total cover of introduced species per site

#Subset Environ factors:
Freshwater.av.env <-  subset( FreshwaterAV, select = c( Mean_SoilSalinity,
                                                         meanwaterdepthcm,meanwaterdepthcm_SD,
                                                         floodedpercent,
                                                         MeanWaterSalinity,MeanWaterSalinity_SD, 
                                                         richness))
#Add new variables to Freshwater.av.env for SEM as computed above:
Freshwater.av.env$Introduced_Cover <- Freshwater.av.veg.introduced.total
Freshwater.av.env$Native_Cover     <- Freshwater.av.veg.native.total
Freshwater.av.env$Native_Richness  <- Freshwater_Native_Richness 

#Run PCoA on NatComp (composition of native-only plant species):
Freshwater_distance <- vegdist(Freshwater.av.veg.native  , "bray")
Freshwater.pca <- cmdscale(Freshwater_distance , eig = TRUE)

#Combine PCA (NatComp) and env data together:
coordinates<-as.data.frame(Freshwater.pca$points[,1:2]) #get MDS1 (x-axis Comp value)
Freshwater_Data<-cbind(coordinates, Freshwater.av.env)

#Standarize the variables so their effect size are comparable:
Freshwater_Data$TotRich     <- scale (Freshwater_Data$richness)
Freshwater_Data$NatRich     <- scale (Freshwater_Data$Native_Richness)
Freshwater_Data$Soil        <- scale (Freshwater_Data$Mean_SoilSalinity)
Freshwater_Data$Alien       <- scale (Freshwater_Data$Introduced_Cover)
Freshwater_Data$Native      <- scale (Freshwater_Data$Native_Cover)
Freshwater_Data$NatComp <- scale (Freshwater_Data$V1)
Freshwater_Data$Flood       <- scale (Freshwater_Data$floodedpercent)
Freshwater_Data$Depth       <- scale (Freshwater_Data$meanwaterdepthcm)
Freshwater_Data$Depth_SD    <- scale (Freshwater_Data$meanwaterdepthcm_SD)
#write.csv(Freshwater_Data, file = "Freshwater_Data4SEM.csv")

#Freshwater SEM:=========
#All terms significant, backward selection from full model (see "Path_SEM3" R file, line ~200)
#You can load data directly from previosly saved "Freshwater_Data4SEM.csv"
#Freshwater_Data <- read.csv("Freshwater_Data4SEM.csv")

model_Freshwater <- '
#regressions:
NatRich     ~ Depth + Flood     
Native      ~ Depth + Flood  + Soil + Alien
NatComp ~ Soil + Alien 

#covariances:
NatComp ~~ 0*NatRich
Native ~~ 0*NatComp
'
fit_Freshwater <- sem(model_Freshwater,missing="direct",estimator="ML",data=Freshwater_Data)
summary(fit_Freshwater, fit.measures=TRUE, rsquare=T) 
semPaths(fit_Freshwater,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,edge.labels=FALSE,
         edge.label.position = 0.25, nCharNodes=6,
         residuals =  F, exoCov = F, layout = layFreshwater)
title("Freshwater path analysis (2007-2017, P < 0.05)", line = 1)

layFreshwater<-matrix(c(-0.5,  -0.5,
              0.5,  -0.5,
              0,    -0.35,
              0,     0.5,
              0.5,   0.5, 
              -0.5,   0.5,
              0,     0.15),  ncol=2,byrow=TRUE)#Alien Position

#Check by graphing of some of the significant relationships:
ggplot(Freshwater_Data,aes(x=Alien,y=NatComp,size=Alien))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Freshwater_Data")
  
#"Brackish" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced

BrackishAV <-
  filter (VegAllEnvData, VegAllEnvData$Community=="Brackish") %>%
  na.omit() %>%
  group_by(StationFront,Community) %>% #Compute means of all variables per StationFront (site), over all years:
  summarise_at(vars(richness, Mean_SoilSalinity, 
                    meanwaterdepthcm, meanwaterdepthcm_SD,
                    floodedpercent,
                    MeanWaterSalinity, MeanWaterSalinity_SD,
                    Acer_rubrum:Ziza_miliacea), mean,na.rm=T)

#Subset native/introduced Veg matrix where colSums are > 0:
Brackish_Veg_Matrix <- as.data.frame (subset ( BrackishAV, select = c(Acer_rubrum:Ziza_miliacea)))
Brackish_Veg_Matrix <- Brackish_Veg_Matrix [ , which(colSums(Brackish_Veg_Matrix) > 0)]
Brackish_Native_Richness <- specnumber(Brackish_Veg_Matrix)#Compute Native-Only richness

#join Brackish_Plants & Plant_List to see which species are native/introduced:
colCount2 = colSums(Brackish_Veg_Matrix) #sum up the abundance column-wise
topID2 = order(colCount2,decreasing=TRUE)[1:length(Brackish_Veg_Matrix)] # choose all Brackish plant species
topID2 = names(Brackish_Veg_Matrix[topID2]) # names of plant species in decreasing order
Brackish_Plant_Sp <- data.frame( specCode = topID2)

Plant_List<- subset(Plant_Info, select = c(specCode, nat))
Brackish_Plants <- left_join(Brackish_Plant_Sp,Plant_List, by = "specCode")

#Select natives only from BrackishAV and compute their total cover:
Brackish_Native.Species <- filter(Brackish_Plants, nat == "native")# native species
BrackishAV.veg.native <- subset( BrackishAV, select = unique(Brackish_Native.Species$specCode))
BrackishAV.veg.native.total <- rowSums (BrackishAV.veg.native) #create extra column with total cover of all natives

#Subset introduced only from BrackishAV and compute their total cover::
Brackish_introduced.Species <- filter(Brackish_Plants, nat == "introduced")
BrackishAV.veg.introduced <- subset( BrackishAV, select = unique(Brackish_introduced.Species$specCode))
BrackishAV.veg.introduced.total <- rowSums (BrackishAV.veg.introduced)#Extra column = total cover of introduced species per site

#Subset Environ factors:
BrackishAV.env <-  subset( BrackishAV,
                            select = c( Mean_SoilSalinity,
                                        meanwaterdepthcm, meanwaterdepthcm_SD,
                                        floodedpercent,  MeanWaterSalinity, 
                                        richness))

#Add new variables to BrackishAV.env for SEM as computed above:
BrackishAV.env$Introduced_Cover <- BrackishAV.veg.introduced.total
BrackishAV.env$Native_Cover     <- BrackishAV.veg.native.total
BrackishAV.env$Native_Richness  <- Brackish_Native_Richness 

#Run PCoA:
Brackish_distance <- vegdist(BrackishAV.veg.native  , "bray")
Brackish.pca <- cmdscale(Brackish_distance , eig = TRUE)
#Combine PCA (NatComp) and env data together:
coordinates<-as.data.frame(Brackish.pca$points[,1:2]) #get MDS1 (x-axis Comp value)
Brackish_Data<-cbind(coordinates, BrackishAV.env)

#Standarize the variables so their effect size are comparable:
Brackish_Data$TotRich     <- scale (Brackish_Data$richness)
Brackish_Data$NatRich     <- scale (Brackish_Data$Native_Richness)
Brackish_Data$Soil        <- scale (Brackish_Data$Mean_SoilSalinity)
Brackish_Data$Alien       <- scale (Brackish_Data$Introduced_Cover)
Brackish_Data$Native      <- scale (Brackish_Data$Native_Cover)
Brackish_Data$NatComp <- scale (Brackish_Data$V1)
Brackish_Data$Flood       <- scale (Brackish_Data$floodedpercent)
Brackish_Data$Depth       <- scale (Brackish_Data$meanwaterdepthcm)
Brackish_Data$Depth_SD    <- scale (Brackish_Data$meanwaterdepthcm_SD)

#write.csv(Brackish_Data, file = "Brackishr_Data4SEM.csv")
#All terms significant, backward selection from full model (see "Path_SEM3" R file, line ~200)
#You can load data directly from previosly saved "Brackish_Data4SEM.csv"
#Brackish_Data <- read.csv("Brackish_Data4SEM.csv")

#Brackish SEM=========
#best fit model:
model_Brackish <- '
#regressions:
NatRich     ~  Flood    
Native      ~  Flood  
NatComp ~  Soil 

#covariances:
NatComp ~~ 0*NatRich
Native ~~ 0*NatComp
'
fit_Brackish <- sem(model_Brackish,missing="direct",estimator="ML",data=Brackish_Data)
summary(fit_Brackish, fit.measures=TRUE, rsquare=T) 
semPaths(fit_Brackish,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=6,
         residuals =  F, exoCov = F)
title("Brackish path analysis (2007-2017, P<0.05)")

#Check by graphing of some of the significant relationships:
ggplot(Brackish_Data,aes(x=Alien,y=NatComp,size=Alien))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Brackish_Data")+
  labs(caption = "(based on data from: VegAllEnvData_03july2018.csv)")

#"Saline" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced

SalineAV <-
  filter (VegAllEnvData, VegAllEnvData$Community=="Saline") %>%
  na.omit() %>%
  group_by(StationFront,Community) %>% #Compute means of all variables per StationFront (site), over all years:
  summarise_at(vars(richness, Mean_SoilSalinity, 
                    meanwaterdepthcm, meanwaterdepthcm_SD,
                    floodedpercent,
                    MeanWaterSalinity, MeanWaterSalinity_SD,
                    Acer_rubrum:Ziza_miliacea), mean,na.rm=T)

#Subset native/introduced Veg matrix where colSums are > 0:
Saline_Veg_Matrix <- as.data.frame (subset ( SalineAV, select = c(Acer_rubrum:Ziza_miliacea)))
Saline_Veg_Matrix <- Saline_Veg_Matrix [ , which(colSums(Saline_Veg_Matrix) > 0)]
Saline_Native_Richness <- specnumber(Saline_Veg_Matrix)#Compute Native-Only richness

#join Saline_Plants & Plant_List to see which species are native/introduced:
colCount3 = colSums(Saline_Veg_Matrix) #sum up the abundance column-wise
topID3 = order(colCount3,decreasing=TRUE)[1:length(Saline_Veg_Matrix)] # choose all Saline plant species
topID3 = names(Saline_Veg_Matrix[topID3]) # names of plant species in decreasing order
Saline_Plant_Sp <- data.frame( specCode = topID3)

Plant_List<- subset(Plant_Info, select = c(specCode, nat))
Saline_Plants <- left_join(Saline_Plant_Sp,Plant_List, by = "specCode")

#Select natives only from Saline.av and compute their total cover:
Saline_Native.Species <- filter(Saline_Plants, nat == "native")# native species
Saline.av.veg.native <- subset( SalineAV, select = unique(Saline_Native.Species$specCode))
Saline.av.veg.native.total <- rowSums (Saline.av.veg.native) #create extra column with total cover of all natives

#Subset introduced only from Saline.av and compute their total cover::
Saline_introduced.Species <- filter(Saline_Plants, nat == "introduced")
Saline.av.veg.introduced <- subset( SalineAV, select = unique(Saline_introduced.Species$specCode))
Saline.av.veg.introduced.total <- rowSums (Saline.av.veg.introduced)#Extra column = total cover of introduced species per site

#Subset Environ factors:
Saline.av.env <-  subset( SalineAV,
                            select = c( Mean_SoilSalinity,
                                        meanwaterdepthcm, meanwaterdepthcm_SD,
                                        floodedpercent,  MeanWaterSalinity, 
                                        richness))

#Add new variables to Saline.av.env for SEM as computed above:
Saline.av.env$Introduced_Cover <- Saline.av.veg.introduced.total
Saline.av.env$Native_Cover     <- Saline.av.veg.native.total
Saline.av.env$Native_Richness  <- Saline_Native_Richness 

#Run PCoA:
Saline_distance <- vegdist(Saline.av.veg.native , "bray")
Saline.pca <- cmdscale(Saline_distance , eig = TRUE)
#Combine PCA (NatComp) and env data together:
coordinates<-as.data.frame(Saline.pca$points[,1:2]) #get MDS1 (x-axis Comp value)
Saline_Data<-cbind(coordinates, Saline.av.env)

#Standarize the variables so their effect size are comparable:
Saline_Data$TotRich     <- scale (Saline_Data$richness)
Saline_Data$NatRich     <- scale (Saline_Data$Native_Richness)
Saline_Data$Soil        <- scale (Saline_Data$Mean_SoilSalinity)
Saline_Data$Alien       <- scale (Saline_Data$Introduced_Cover)
Saline_Data$Native      <- scale (Saline_Data$Native_Cover)
Saline_Data$NatComp <- scale (Saline_Data$V1)
Saline_Data$Flood       <- scale (Saline_Data$floodedpercent)
Saline_Data$Depth       <- scale (Saline_Data$meanwaterdepthcm)
Saline_Data$Depth_SD    <- scale (Saline_Data$meanwaterdepthcm_SD)

#Saline SEM===========
#Best fit model:
model_Saline <- '
#regressions:
#regressions:
NatRich     ~ Depth + Soil 
Native      ~ Depth + Flood  + Soil 
Alien       ~ Depth + Flood  + Soil 
NatComp ~  NatRich + Native

#covariances:
NatComp ~~ 0*NatRich
Native ~~ 0*NatComp
Alien ~~ 0*NatComp
'
fit_Saline <- sem(model_Saline,missing="direct",estimator="ML",data=Saline_Data)
summary(fit_Saline, fit.measures=TRUE, rsquare=T)

#Design layout of nodes manually for semPaths:
laySaline<-matrix(c(-0.5,  -0.5,
                    0.5,  -0.5,
                    0,     0.10,    #Alien Position
                    0,    -0.35,
                    0,     0.5,
                    0.5,   0.5, 
                    -0.5,   0.5 ),  ncol=2,byrow=TRUE)

semPaths(fit_Saline,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=6, layout = laySaline,
         residuals =  F, exoCov = F)
title("Saline path analysis (2007-2017, P<0.05)")

#Check by graphing of some of the significant relationships:
ggplot(Saline_Data,aes(x=Alien,y=NatComp,size=Alien))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Saline_Data")+
  labs(caption = "(based on data from: VegAllEnvData_03july2018.csv)")

#"Intermediate" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced

IntermediateAV <-
  filter (VegAllEnvData, VegAllEnvData$Community=="Intermediate") %>%
  na.omit() %>%
  group_by(StationFront,Community) %>% #Compute means of all variables per StationFront (site), over all years:
  summarise_at(vars(richness, Mean_SoilSalinity, 
                    meanwaterdepthcm, meanwaterdepthcm_SD,
                    floodedpercent,
                    MeanWaterSalinity, MeanWaterSalinity_SD,
                    Acer_rubrum:Ziza_miliacea), mean,na.rm=T)

#Subset native/introduced Veg matrix where colSums are > 0:
Intermediate_Veg_Matrix <- as.data.frame (subset ( IntermediateAV, select = c(Acer_rubrum:Ziza_miliacea)))
Intermediate_Veg_Matrix <- Intermediate_Veg_Matrix [ , which(colSums(Intermediate_Veg_Matrix) > 0)]
Intermediate_Native_Richness <- specnumber(Intermediate_Veg_Matrix)#Compute Native-Only richness

#join Intermediate_Plants & Plant_List to see which species are native/introduced:
colCount4 = colSums(Intermediate_Veg_Matrix) #sum up the abundance column-wise
topID4 = order(colCount4,decreasing=TRUE)[1:length(Intermediate_Veg_Matrix)] # choose all Intermediate plant species
topID4 = names(Intermediate_Veg_Matrix[topID4]) # names of plant species in decreasing order
Intermediate_Plant_Sp <- data.frame( specCode = topID4)

Plant_List<- subset(Plant_Info, select = c(specCode, nat))
Intermediate_Plants <- left_join(Intermediate_Plant_Sp,Plant_List, by = "specCode")

#Select natives only from Intermediate.av and compute their total cover:
Intermediate_Native.Species <- filter(Intermediate_Plants, nat == "native")# native species
Intermediate.av.veg.native <- subset( IntermediateAV, select = unique(Intermediate_Native.Species$specCode))
Intermediate.av.veg.native.total <- rowSums (Intermediate.av.veg.native) #create extra column with total cover of all natives

#Subset introduced only from Intermediate.av and compute their total cover::
Intermediate_introduced.Species <- filter(Intermediate_Plants, nat == "introduced")
Intermediate.av.veg.introduced <- subset( IntermediateAV, select = unique(Intermediate_introduced.Species$specCode))
Intermediate.av.veg.introduced.total <- rowSums (Intermediate.av.veg.introduced)#Extra column = total cover of introduced species per site

#Subset Environ factors:
Intermediate.av.env <-  subset( IntermediateAV,
                          select = c( Mean_SoilSalinity,
                                      meanwaterdepthcm, meanwaterdepthcm_SD,
                                      floodedpercent,  MeanWaterSalinity, 
                                      richness))

#Add new variables to Intermediate.av.env for SEM as computed above:
Intermediate.av.env$Introduced_Cover <- Intermediate.av.veg.introduced.total
Intermediate.av.env$Native_Cover     <- Intermediate.av.veg.native.total
Intermediate.av.env$Native_Richness  <- Intermediate_Native_Richness 

#Run PCoA:
Intermediate_distance <- vegdist(Intermediate.av.veg.native , "bray")
Intermediate.pca <- cmdscale(Intermediate_distance , eig = TRUE)
#Combine PCA (NatComp) and env data together:
coordinates<-as.data.frame(Intermediate.pca$points[,1:2]) #get MDS1 (x-axis Comp value)
Intermediate_Data<-cbind(coordinates, Intermediate.av.env)

#Standarize the variables so their effect size are comparable:
Intermediate_Data$TotRich     <- scale (Intermediate_Data$richness)
Intermediate_Data$NatRich     <- scale (Intermediate_Data$Native_Richness)
Intermediate_Data$Soil        <- scale (Intermediate_Data$Mean_SoilSalinity)
Intermediate_Data$Alien       <- scale (Intermediate_Data$Introduced_Cover)
Intermediate_Data$Native      <- scale (Intermediate_Data$Native_Cover)
Intermediate_Data$NatComp <- scale (Intermediate_Data$V1)
Intermediate_Data$Flood       <- scale (Intermediate_Data$floodedpercent)
Intermediate_Data$Depth       <- scale (Intermediate_Data$meanwaterdepthcm)
Intermediate_Data$Depth_SD    <- scale (Intermediate_Data$meanwaterdepthcm_SD)

#write.csv(Intermediate_Data, file = "Intermediate_Data4SEM.csv")
#All terms significant, backward selection from full model (see "Path_SEM3" R file, line ~200)
#You can load data directly from previosly saved "Intermediate_Data4SEM.csv"
#Intermediate_Data <- read.csv("Intermediate_Data4SEM.csv")

#Intermediate SEM ========
#best fit model:
model_Intermediate <- '
#regressions:
NatRich     ~ Soil  
Native      ~  Soil + Alien
Alien       ~  Soil 
NatComp ~ Alien + NatRich + Native

#covariances:
NatComp ~~ 0*NatRich
Native ~~ 0*NatComp
'
fit_Intermediate <- sem(model_Intermediate,missing="direct",estimator="ML",data=Intermediate_Data)
summary(fit_Intermediate, fit.measures=TRUE, rsquare=T) 

#Design layout of nodes manually for semPaths:
layIntermediate<-matrix(c(-0.5,  -0.5,
                          0.5,  -0.5,
                          0,     0.10,    #Alien Position
                          0,    -0.35,
                          0,     0.5 ),  ncol=2,byrow=TRUE)


semPaths(fit_Intermediate,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=6, layout = layIntermediate,
         residuals =  F, exoCov = F)
title("Intermediate  path analysis (2007-2017, P<0.05)")

#Check by graphing of some of the significant relationships:
ggplot(Intermediate_Data,aes(x=Alien,y=NatComp,size=Alien))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Intermediate_Data")+
  labs(caption = "(based on data from: VegAllEnvData_03july2018.csv)")

#Combine all ggplots:==========
p1<-ggplot(Freshwater_Data,aes(x=Alien,y=NatComp,size=Alien))+
  geom_point()+  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Freshwater_Data")+
  labs(caption = "(based on data from: VegAllEnvData_03july2018.csv)")

p2<-ggplot(Intermediate_Data,aes(x=Alien,y=NatComp,size=Alien))+
  geom_point()+  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Intermediate_Data")+
  labs(caption = "(based on data from: VegAllEnvData_03july2018.csv)")

p3 <- ggplot(Brackish_Data,aes(x=Alien,y=NatComp,size=Alien))+
  geom_point()+  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Brackish_Data")+
  labs(caption = "(based on data from: VegAllEnvData_03july2018.csv)")

p4 <- ggplot(Saline_Data,aes(x=Alien,y=NatComp,size=Alien))+
  geom_point()+  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Saline_Data")+
  labs(caption = "(based on data from: VegAllEnvData_03july2018.csv)")

grid.arrange(p1,p2,p3,p4, ncol = 2)

pp1<-ggplot(Freshwater_Data,aes(x=Alien,y=Native_Cover,size=Alien))+
  geom_point()+  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Freshwater_Data")+
  labs(caption = "(based on data from: VegAllEnvData_03july2018.csv)")

pp2<-ggplot(Intermediate_Data,aes(x=Alien,y=Native_Cover,size=Alien))+
  geom_point()+  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Intermediate_Data")+
  labs(caption = "(based on data from: VegAllEnvData_03july2018.csv)")

pp3 <- ggplot(Brackish_Data,aes(x=Alien,y=Native_Cover,size=Alien))+
  geom_point()+  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Brackish_Data")+
  labs(caption = "(based on data from: VegAllEnvData_03july2018.csv)")

pp4 <- ggplot(Saline_Data,aes(x=Alien,y=Native_Cover,size=Alien))+
  geom_point()+  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Saline_Data")+
  labs(caption = "(based on data from: VegAllEnvData_03july2018.csv)")

grid.arrange(pp1,pp2,pp3,pp4, ncol = 2)
