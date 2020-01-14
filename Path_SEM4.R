#Data production for SEM models ========
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
dim(VegAllEnvData) # 3498  473

library("lavaan")
library("semPlot")
library("tidyverse")
library("vegan")
library("gridExtra")
library("grid")

#APRIORI MODEL======
#Apriori Path Analysis Model (for latter backward selection):
Apriori_Model <-'
#regressions:
NatRich     ~ Depth + Flood  + Soil + Alien  
Native      ~ Depth + Flood  + Soil + Alien
Alien       ~ Depth + Flood  + Soil 
NatComp     ~ Depth + Flood  + Soil + Alien + NatRich + Native

#covariances:
NatRich ~~ Native
NatComp ~~ NatRich
Native  ~~  NatComp
'
fit_Apriori_Model <- sem(Apriori_Model,missing="direct",estimator="ML",data=data)
summary(fit_Apriori_Model)

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

#Pick natives only from Freshwater.av and compute their total cover:
Freshwater_Native.Species <- filter(Freshwater_Plants, nat == "native")#228 native species
Freshwater.av.veg.native <- subset( FreshwaterAV, select = unique(Freshwater_Native.Species$specCode))
Freshwater.av.veg.native.total <- rowSums (Freshwater.av.veg.native) #create extra column with total cover of all natives

#Pick introduced only from Freshwater.av and compute their total cover::
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


#(Per Emily's Comment, 1/jan/2020) in NMDS ordinations, the axes don’t mean anything 
#(the ordination can be rotated any which way).
#Using PCoA however, axis 1 is the axis that explains most of the
#variation and you can calculate how important it is. 
#If it is not too insane, it might be nice to do PCoA 
#instead of NMDS and report the variance explained by Axis 1 
#so we can argue that one axis is enough to represent changes in
#species comp. you can do this with:

#Remove columns with only one record:
#Emily's comment (Jan2020): The other thing I typically do when I do ordinations is I remove species that are only found in 1 or sometimes even 2 plots. They don't really contribute much to pattens in the data, they just add a lot of noise, (for example, if a species is only present in 1 plot, it is just going to make that plot dissimilar to everything else), so that may be why your percent explained for axis 1 is low, although 23% isn't too bad. You could try taking out those species that are only present in 1 or 2 plots and see if that changes things greatly.
Freshwater.av.veg.native1<- Freshwater.av.veg.native [  , colSums(Freshwater.av.veg.native > 0) > 1]#Remove plots/species with less than 1 record in them
dim(Freshwater.av.veg.native) ; dim(Freshwater.av.veg.native1)
# 42 162
# 42 228 = 162 columns left out of 228

#Run PCoa:
mc_fresh<-capscale(Freshwater.av.veg.native1 ~ 1, distance="bray")#This produces a PCoA, (principle coordinates analysis, otherwise known as metric multidimensional scaling).
#PCA is not great to use with species comp data b/c it uses euclidean distance which essentially means that if two plots both have 0 for a species it calls them similar. Bray dissimilarity fixes this and only uses species that are present in the plots to calculate similarity/dissimilarity. 
plot(mc_fresh,display= "sites", main ="Capscale on Freshwater Communities")
coordinates<-as.data.frame(scores(mc_fresh)$sites) #get MDS1 and 2
plot(coordinates)

#Combine PCoA (NatComp) and env data together:
Freshwater_Data<-cbind(coordinates, Freshwater.av.env)

#Standarize the variables so their effect size are comparable:
Freshwater_Data$TotRich     <- scale (Freshwater_Data$richness)
Freshwater_Data$NatRich     <- scale (Freshwater_Data$Native_Richness)
Freshwater_Data$Soil        <- scale (log(Freshwater_Data$Mean_SoilSalinity))#log to normalize salinity data
Freshwater_Data$Alien       <- scale (Freshwater_Data$Introduced_Cover)
Freshwater_Data$NatCov      <- scale (Freshwater_Data$Native_Cover)
Freshwater_Data$NatComp     <- scale (Freshwater_Data$MDS1)
Freshwater_Data$Flood       <- scale (Freshwater_Data$floodedpercent)
Freshwater_Data$Depth       <- scale (Freshwater_Data$meanwaterdepthcm)
Freshwater_Data$Depth_SD    <- scale (Freshwater_Data$meanwaterdepthcm_SD)
write.csv(Freshwater_Data, file = "Freshwater_Data4SEM_PCoa.csv", row.names = F)

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

#Remove columns with only one record:
Intermediate.av.veg.native1<- Intermediate.av.veg.native [  , colSums(Intermediate.av.veg.native > 0) > 1]#Remove plots/species with less than 1 record in them
dim(Intermediate.av.veg.native) ; dim(Intermediate.av.veg.native1)
#[1]  56 154
#[1]  56 103

#Run PCoA:
mc_Intermediate<-capscale(Intermediate.av.veg.native1 ~ 1, distance="bray")#This produces a PCoA, (principle coordinates analysis, otherwise known as metric multidimensional scaling).
#PCA is not great to use with species comp data b/c it uses euclidean distance which essentially means that if two plots both have 0 for a species it calls them similar. Bray dissimilarity fixes this and only uses species that are present in the plots to calculate similarity/dissimilarity. 
coordinates<-as.data.frame(scores(mc_Intermediate)$sites) #get MDS1 and 2
plot(coordinates)
#Combine PCoA (NatComp) and env data together:
Intermediate_Data<-cbind(coordinates, Intermediate.av.env)

#Standarize the variables so their effect size are comparable:
Intermediate_Data$TotRich     <- scale (Intermediate_Data$richness)
Intermediate_Data$NatRich     <- scale (Intermediate_Data$Native_Richness)
Intermediate_Data$Soil        <- scale (log(Intermediate_Data$Mean_SoilSalinity))#log to normalize salinity data
Intermediate_Data$Alien       <- scale (Intermediate_Data$Introduced_Cover)
Intermediate_Data$NatCov      <- scale (Intermediate_Data$Native_Cover)
Intermediate_Data$NatComp     <- scale (Intermediate_Data$MDS1)
Intermediate_Data$Flood       <- scale (Intermediate_Data$floodedpercent)
Intermediate_Data$Depth       <- scale (Intermediate_Data$meanwaterdepthcm)
Intermediate_Data$Depth_SD    <- scale (Intermediate_Data$meanwaterdepthcm_SD)

write.csv(Intermediate_Data, file = "Intermediate_Data4SEM_PCoA.csv", row.names = F)
#You can load data directly from previosly saved "Intermediate_Data4SEM_PCoA.csv"

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
#Remove columns with only one record:
Brackish.av.veg.native1<- BrackishAV.veg.native [  , colSums(BrackishAV.veg.native > 0) > 1]#Remove plots/species with less than 1 record in them
dim(BrackishAV.veg.native) ; dim(Brackish.av.veg.native1)
#[1] 40 70
#[1] 40 49

#Run PCoA:
mc_Brackish<-capscale(Brackish.av.veg.native1 ~ 1, distance="bray")#This produces a PCoA, (principle coordinates analysis, otherwise known as metric multidimensional scaling).
#PCA is not great to use with species comp data b/c it uses euclidean distance which essentially means that if two plots both have 0 for a species it calls them similar. Bray dissimilarity fixes this and only uses species that are present in the plots to calculate similarity/dissimilarity. 
plot(mc_Brackish,display= "sites", main ="Capscale on Brackish Communities")
coordinates<-as.data.frame(scores(mc_Brackish)$sites) #get MDS1 and 2

#Combine PCoA (NatComp) and env data together:
Brackish_Data<-cbind(coordinates,BrackishAV.env)

#Standarize the variables so their effect size are comparable:
Brackish_Data$TotRich     <- scale (Brackish_Data$richness)
Brackish_Data$NatRich     <- scale (Brackish_Data$Native_Richness)
Brackish_Data$Soil        <- scale (log(Brackish_Data$Mean_SoilSalinity))#log to normalize salinity data
Brackish_Data$Alien       <- scale (Brackish_Data$Introduced_Cover)
Brackish_Data$NatCov      <- scale (Brackish_Data$Native_Cover)
Brackish_Data$NatComp     <- scale (Brackish_Data$MDS1)
Brackish_Data$Flood       <- scale (Brackish_Data$floodedpercent)
Brackish_Data$Depth       <- scale (Brackish_Data$meanwaterdepthcm)
Brackish_Data$Depth_SD    <- scale (Brackish_Data$meanwaterdepthcm_SD)

write.csv(Brackish_Data, file = "Brackish_Data4SEM_PCoA.csv", row.names = F)
#All terms significant, backward selection from full model (see "Path_SEM3" R file, line ~200)
#You can load data directly from previosly saved "Brackish_Data4SEM_PCoA.csv"

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
#Remove columns with only one record:
Saline.av.veg.native1<- Saline.av.veg.native [  , colSums(Saline.av.veg.native > 0) > 1]#Remove plots/species with less than 1 record in them
dim(Saline.av.veg.native) ; dim(Saline.av.veg.native1)
#[1] 64 33
#[1] 64 23

#Run PCoA:
mc_Saline<-capscale(Saline.av.veg.native1 ~ 1, distance="bray")#This produces a PCoA, (principle coordinates analysis, otherwise known as metric multidimensional scaling).
#PCA is not great to use with species comp data b/c it uses euclidean distance which essentially means that if two plots both have 0 for a species it calls them similar. Bray dissimilarity fixes this and only uses species that are present in the plots to calculate similarity/dissimilarity. 
coordinates<-as.data.frame(scores(mc_Saline)$sites) #get MDS1 and 2
#Combine PCoA (NatComp) and env data together:
Saline_Data<-cbind(coordinates, Saline.av.env)

#Standarize the variables so their effect size are comparable:
Saline_Data$TotRich     <- scale (Saline_Data$richness)
Saline_Data$NatRich     <- scale (Saline_Data$Native_Richness)
Saline_Data$Soil        <- scale (log(Saline_Data$Mean_SoilSalinity))#log to normalize salinity data
Saline_Data$Alien       <- scale (Saline_Data$Introduced_Cover)
Saline_Data$NatCov      <- scale (Saline_Data$Native_Cover)
Saline_Data$NatComp <- scale (Saline_Data$MDS1)
Saline_Data$Flood       <- scale (Saline_Data$floodedpercent)
Saline_Data$Depth       <- scale (Saline_Data$meanwaterdepthcm)
Saline_Data$Depth_SD    <- scale (Saline_Data$meanwaterdepthcm_SD)

write.csv(Saline_Data, file = "Saline_Data4SEM_PCoA.csv", row.names = F)
#You can load data directly from previosly saved "Saline_Data4SEM_PCoA.csv"

#Checking relationships in Data========
#Check by graphing of some of the significant relationships:
ggplot(Saline_Data,aes(x=Alien,y=NatComp,size=Alien))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Saline_Data")+
  labs(caption = "(based on data from: VegAllEnvData_03july2018.csv)")

f4<-ggplot(Saline_Data,aes(x=Flood,y=NatRich))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Saline_Data")

 ggplot(Saline_Data,aes(x=Alien,y=Soil))+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  ggtitle ("Saline_Data")

grid.arrange(f1,f2,f3,f4, ncol = 2)
 
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
