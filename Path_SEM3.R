#LOAD LIBRARIES & DATA:
#22june2018 - we give up top weed and look at the impact of all weeds:
#New Veg  data = VegAllEnvData_22june2018
VegAllEnvData <- read.csv("VegAllEnvData_22june2018.csv")
str(VegAllEnvData) #3464 obs. of  471 variables:
library("lavaan")
library("semPlot")
library("tidyverse")
library("vegan")
library("scatterplot3d")
library("gridExtra")
library("grid")

#See 04_MERGE_VegHydroSoil for details on how "VegAllEnvData_22june2018.csv" was produced.
#"Freshwater" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_22june2018.csv")
VegAllEnvData <- rename(VegAllEnvData, StationFront = StationFront.x)
#Joinining veg and env data created two extra columns for StationFront.
#Rename Stationfront.x to StationFront

Freshwater_Only <-  filter (VegAllEnvData, VegAllEnvData$Community=="Freshwater") %>%
  na.omit() %>% rename(StationFront = StationFront.x)
dim(Freshwater_Only)#336 471

#Compute means of all variables per StationFront (site), over all years:
Freshwater.av <- Freshwater_Only %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness,Mean_SoilSalinity,
                    meanwaterdepthcm,floodedpercent,
                    MeanWaterSalinity, Acer_rubrum:Ziza_miliacea),mean,na.rm=T)
dim(Freshwater.av)#  42 460

#Subset native/introduced Veg matrix where colSums are > 0:
Freshwater_Veg_Matrix <- as.data.frame (subset ( Freshwater.av, select = c(Acer_rubrum:Ziza_miliacea)))
dim(Freshwater_Veg_Matrix)#   42 453
Freshwater_Veg_Matrix <- Freshwater_Veg_Matrix [ , which(colSums(Freshwater_Veg_Matrix) > 0)]
dim(Freshwater_Veg_Matrix)#   42 283

colCount1 = colSums(Freshwater_Veg_Matrix) #sum up the abundance column-wise
topID1 = order(colCount1,decreasing=TRUE)[1:length(Freshwater_Veg_Matrix)] # choose all Freshwater plant species
topID1 = names(Freshwater_Veg_Matrix[topID1]) # names of plant species in decreasing order
Freshwater_Plant_Sp <- data.frame( specCode = topID1)
Freshwater_Plant_Sp#283 species (both native and introduced)
Plant_List <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
str(Plant_List)#data.frame':	3454 obs. of  6 variables

#join Freshwater_Plants & Plant_List to see which species are native/introduced:
Plant_List<- subset(Plant_List, select = c(specCode, nat))
Freshwater_Plants <- left_join(Freshwater_Plant_Sp,Plant_List, by = "specCode")
dim(Freshwater_Plants)#  283  2

#Subset native only
Freshwater_Native.Species <- filter(Freshwater_Plants, nat == "native")
Freshwater_Native.Species #228 native species

#Select natives only from Freshwater.av:
Freshwater.av.veg.native <- subset( Freshwater.av, select = unique(Freshwater_Native.Species$specCode))
dim(Freshwater.av.veg.native)# 42 228
Freshwater.av.veg.native.total <- rowSums (Freshwater.av.veg.native) #create extra column with total cover of all natives
range( Freshwater.av.veg.native.total ) # 55.52063 136.75000

#Compute Richness : 
Freshwater_Native_Richness <- specnumber(Freshwater_Veg_Matrix)#Compute Native-Only richness
range(Freshwater_Native_Richness)#   14 93

#Subset introduced only:
Freshwater_introduced.Species <- filter(Freshwater_Plants, nat == "introduced")
Freshwater_introduced.Species #22
########specCode        nat
1  Alte_philoxeroid introduced
2    Colo_esculenta introduced
3  Ludw_grandiflora introduced
4    Phra_australis introduced
5    Luzi_peruviana introduced
6  Hydr_bonariensis introduced
7    Sphe_zeylanica introduced
8       Pani_repens introduced
9       Salv_minima introduced
10 Verb_brasiliensi introduced
11   Sorg_halepense introduced
12  Alte_caracasana introduced
13 Alte_paronychioi introduced
14      Sacc_indica introduced
15  Hype_perforatum introduced
16   Cype_difformis introduced
17 Cype_sanguinolen introduced
18   Eich_crassipes introduced
19    Pasp_urvillei introduced
20     Penn_glaucum introduced
21       Sonc_asper introduced
22   Sonc_oleraceus introduced

Freshwater.av.veg.introduced <- subset( Freshwater.av, select = unique(Freshwater_introduced.Species$specCode))
dim(Freshwater.av.veg.introduced)#42 22
Freshwater.av.veg.introduced.total <- rowSums (Freshwater.av.veg.introduced)#Extra column = total cover of introduced species per site
TotalCover_Freshwater_Weeds <- colSums(Freshwater.av.veg.introduced)
range(TotalCover_Freshwater_Weeds)# 0.0125 222.3352

#WEED Species in Freshwater:
names(sort(TotalCover_Freshwater_Weeds, decreasing = TRUE))#22 & 
#"Alte_philoxeroid"  first = most abundant

Freshwater_Weed_Sp_barchart <- barchart(sort(TotalCover_Freshwater_Weeds),col = "green", main = "Freshwater Alien Plants (colSums)")
Freshwater_Weed_Sp_barchart
Freshwater_Weed_histogram <- qplot(Freshwater.av.veg.introduced.total, main = "Freshwater Alien Plants (RowSums)", colour = I("green"))
Freshwater_Weed_histogram
grid.arrange(Freshwater_Weed_Sp_barchart, Freshwater_Weed_histogram, ncol = 2)

#Subset Environ factors:
Freshwater.av.env <-  subset( Freshwater.av, select = c( Mean_SoilSalinity,meanwaterdepthcm,
                                                 MeanWaterSalinity,floodedpercent,richness))
#Add new factors for SEM:
Freshwater.av.env$Introduced_Cover <- Freshwater.av.veg.introduced.total
Freshwater.av.env$Native_Cover     <- Freshwater.av.veg.native.total
Freshwater.av.env$Native_Richness  <-  Freshwater_Native_Richness #Computed above

#Freshwater the most dominant species:======
colCount_Freshwater = colSums(Freshwater_Veg_Matrix) #sum up the abundance column-wise
topSp_Freshwater = order(colCount_Freshwater,decreasing=TRUE)[1] # 
topSp_Freshwater = names(Freshwater_Veg_Matrix[topSp_Freshwater]) # subset name of that one most dominant
topSp_Freshwater #  "Pani_hemitomon" is the most dominant species

#Run PCoA:
#  WEB >>>  https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_examples
#use a PCoA  (principal coordinates analysis) rather than a PCA (Principal components analysis). With PCoA you can
#use bray curtis dissimilarity (rather than only euclidean distance which is what PCA uses)
Freshwater_distance <- vegdist(Freshwater_Veg_Matrix , "bray")
Freshwater.pca <- cmdscale(Freshwater_distance , eig = TRUE)
names(Freshwater.pca)#"points" "eig"    "x"      "ac"     "GOF" 
ordiplot(Freshwater.pca, display = 'sites', type = 'points',
         cex = 2,bg="yellow")

#Draw Ordination points:
#Combine MDS PC and env data together:
coordinates<-as.data.frame(Freshwater.pca$points[,1:2]) #get MDS1 (x-axis Comp value)
Freshwater_Data<-cbind(coordinates, Freshwater.av.env)
dim(Freshwater_Data)#only 41 10

#Standarize the variables so their effect size are comparable:
Freshwater_Data$TotRich     <- scale (Freshwater_Data$richness)
Freshwater_Data$NatRich     <- scale (Freshwater_Data$Native_Richness)
Freshwater_Data$Soil        <- scale (Freshwater_Data$Mean_SoilSalinity)
Freshwater_Data$Water       <- scale (Freshwater_Data$MeanWaterSalinity)
Freshwater_Data$Alien       <- scale (Freshwater_Data$Introduced_Cover)
Freshwater_Data$Native      <- scale (Freshwater_Data$Native_Cover)
Freshwater_Data$Composition <- scale (Freshwater_Data$V1)
Freshwater_Data$Flood       <- scale (Freshwater_Data$floodedpercent)
Freshwater_Data$Depth       <- scale (Freshwater_Data$meanwaterdepthcm)

#Vegan MDS in GGPLOT to see where weeds are most abundant:
ggplot(data = Freshwater_Data, aes(V1,V2,size = Introduced_Cover)) + geom_point() +
       ggtitle("PCoA of Freshwater Communities",subtitle = "averaged across 10 years, TopWeed = altephil") +
       xlab("X coordinate of PCoA")+ylab("Y coordinate of PCoA")+
       theme(legend.position = "bottom")
     
#Freshwater SEM  with SOIL salinity =======
model1 <- '
#regressions:

NatRich     ~ Depth + Flood  + Soil + Alien  
Native      ~ Depth + Flood  + Soil + Alien

Alien       ~ Depth + Flood  + Soil 
Composition ~ Alien + NatRich + Native

#covariances:
NatRich ~~ Native
Composition ~~ NatRich
#Composition ~~ Alien
Native ~~ Composition
#Depth ~~ Flood
'
fit1 <- sem(model1,missing="direct",estimator="ML",data=Freshwater_Data)
summary(fit1, fit.measures=TRUE, rsquare=T) 

#Design layout for our SemPath Diagram boxes:
lay<-matrix(c(-0.5,  -0.5,
              0.5,  -0.5,
              0,     0.15, #Alien Position
              0,    -0.35,
              0,     0.5,
              0.5,   0.5, 
             -0.5,   0.5), ncol=2,byrow=TRUE)

#Extrat p-values to control edge.label.bg & edge.label.font in semPaths:
extractPvalues    <- parameterEstimates(fit1)
FreshwaterSigData <- data.frame(Pvalue =extractPvalues[1:20, "pvalue"])#P values are for our regressions
FreshwaterSigData
Bold_Freshwater_Sig <- as.integer(ifelse(FreshwaterSigData$Pvalue < 0.05 , 2,1))#Set bold(2) if P< 0.05, otherwise = 1
Bold_Freshwater_Sig #values #for edge.label.font, 11 models to define which effects are significant (2=bold):
Freshwater_Label_bg <- ifelse(FreshwaterSigData$Pvalue < 0.05 ,"yellow","white")# coding background labels

#Run Freshwater semPaths:======
semPaths(fit1,"est", intercepts = F, fade = F, 
              title = T, edge.label.cex = 1.1,sizeMan = 8,
              edge.label.position = 0.25, nCharNodes=6,
              residuals =  F, exoCov = F,layout = lay,
             edge.label.font = Bold_Freshwater_Sig,
              edge.label.bg = Freshwater_Label_bg )#,filetype = "jpg",filename = "SEM_Freshwater2018")
title("Freshwater path analysis (2007-2017)", line = 1)




#"Brackish" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_22june2018.csv")
VegAllEnvData <- rename(VegAllEnvData, StationFront = StationFront.x)

Brackish_Only <-  filter (VegAllEnvData, VegAllEnvData$Community=="Brackish") %>% na.omit()
dim(Brackish_Only)#293 471

#Compute means of all variables per StationFront (site), over all years:
Brackish.av <- Brackish_Only %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness,Mean_SoilSalinity,
                    meanwaterdepthcm,floodedpercent,
                    MeanWaterSalinity, Acer_rubrum:Ziza_miliacea),mean,na.rm=T)
dim(Brackish.av)#  42 460

#Subset native/introduced Veg matrix where colSums are > 0:
Brackish_Veg_Matrix <- as.data.frame (subset ( Brackish.av, select = c(Acer_rubrum:Ziza_miliacea)))
dim(Brackish_Veg_Matrix)#   42 453
Brackish_Veg_Matrix <- Brackish_Veg_Matrix [ , which(colSums(Brackish_Veg_Matrix) > 0)]
dim(Brackish_Veg_Matrix)#   42 87 

colCount2 = colSums(Brackish_Veg_Matrix) #sum up the abundance column-wise
topID2 = order(colCount2,decreasing=TRUE)[1:length(Brackish_Veg_Matrix)] # choose all Brackish plant species
topID2 = names(Brackish_Veg_Matrix[topID2]) # names of plant species in decreasing order
Brackish_Plant_Sp <- data.frame( specCode = topID2)
Brackish_Plant_Sp#90 species (both native and introduced)
Plant_List <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
str(Plant_List)#data.frame':	3454 obs. of  6 variables

#join Brackish_Plants & Plant_List to see which species are native/introduced:
Plant_List<- subset(Plant_List, select = c(specCode, nat))
Brackish_Plants <- left_join(Brackish_Plant_Sp,Plant_List, by = "specCode")
dim(Brackish_Plants)#  87  2

#Subset native only
Brackish_Native.Species <- filter(Brackish_Plants, nat == "native")
Brackish_Native.Species #70 native species

#Select natives only from Brackish.av:
Brackish.av.veg.native <- subset( Brackish.av, select = unique(Brackish_Native.Species$specCode))
dim(Brackish.av.veg.native)# 42 70
Brackish.av.veg.native.total <- rowSums (Brackish.av.veg.native) #create extra column with total cover of all natives
range( Brackish.av.veg.native.total ) #40.80952 118.73840

#Compute Richness : 
Brackish_Native_Richness <- specnumber(Brackish_Veg_Matrix)#Compute Native-Only richness
range(Brackish_Native_Richness)#  3 30
#Subset introduced only:
Brackish_introduced.Species <- filter(Brackish_Plants, nat == "introduced")
Brackish_introduced.Species #4
####### specCode        nat
1   Phra_australis introduced
2 Alte_philoxeroid introduced
3   Echi_crusgalli introduced
4           Amar_L introduced

Brackish.av.veg.introduced <- subset( Brackish.av, select = unique(Brackish_introduced.Species$specCode))
dim(Brackish.av.veg.introduced)#42  4
Brackish.av.veg.introduced.total <- rowSums (Brackish.av.veg.introduced)#Extra column = total cover of introduced species per site
TotalCover_Brackish_Weeds <- colSums(Brackish.av.veg.introduced)
range(TotalCover_Brackish_Weeds)# 0.1111111 21.6052579

#WEED Species in Brackish:
names(sort(TotalCover_Brackish_Weeds, decreasing = TRUE))
Brackish_Weed_Sp_barchart <- barchart(sort(TotalCover_Brackish_Weeds),col = "green", main = "Brackish Alien Plants (colSums)")
Brackish_Weed_Sp_barchart
Brackish_Weed_histogram <- qplot(Brackish.av.veg.introduced.total, main = "Brackish Alien Plants (RowSums)", colour = I("green"))
Brackish_Weed_histogram
grid.arrange(Brackish_Weed_Sp_barchart, Brackish_Weed_histogram, ncol = 2)

#Subset Environ factors:
Brackish.av.env <-  subset( Brackish.av, select = c( Mean_SoilSalinity,meanwaterdepthcm,
                                                         MeanWaterSalinity,floodedpercent,richness))
#Add new factors for SEM:
Brackish.av.env$Introduced_Cover <- Brackish.av.veg.introduced.total
Brackish.av.env$Native_Cover     <- Brackish.av.veg.native.total
Brackish.av.env$Native_Richness  <-  Brackish_Native_Richness #Computed above

#Brackish the most dominant species:======
colCount_Brackish = colSums(Brackish_Veg_Matrix) #sum up the abundance column-wise
topSp_Brackish = order(colCount_Brackish,decreasing=TRUE)[1] # 
topSp_Brackish = names(Brackish_Veg_Matrix[topSp_Brackish]) # subset name of that one most dominant
topSp_Brackish # "Spar_patens" is the most dominant species

#Run PCoA:
#  WEB >>>  https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_examples
#use a PCoA  (principal coordinates analysis) rather than a PCA (Principal components analysis). With PCoA you can
#use bray curtis dissimilarity (rather than only euclidean distance which is what PCA uses)
Brackish_distance <- vegdist(Brackish_Veg_Matrix , "bray")
Brackish.pca <- cmdscale(Brackish_distance , eig = TRUE)
names(Brackish.pca)#"points" "eig"    "x"      "ac"     "GOF" 
ordiplot(Brackish.pca, display = 'sites', type = 'points',
         cex = 2,bg="yellow")

#Draw Ordination points:
#Combine MDS PC and env data together:
coordinates<-as.data.frame(Brackish.pca$points[,1:2]) #get MDS1 (x-axis Comp value)
Brackish_Data<-cbind(coordinates, Brackish.av.env)
dim(Brackish_Data)#42 10

#Standarize the variables so their effect size are comparable:
Brackish_Data$TotRich     <- scale (Brackish_Data$richness)
Brackish_Data$NatRich     <- scale (Brackish_Data$Native_Richness)
Brackish_Data$Soil        <- scale (Brackish_Data$Mean_SoilSalinity)
Brackish_Data$Water       <- scale (Brackish_Data$MeanWaterSalinity)
Brackish_Data$Alien       <- scale (Brackish_Data$Introduced_Cover)
Brackish_Data$Native      <- scale (Brackish_Data$Native_Cover)
Brackish_Data$Composition <- scale (Brackish_Data$V1)
Brackish_Data$Flood       <- scale (Brackish_Data$floodedpercent)
Brackish_Data$Depth       <- scale (Brackish_Data$meanwaterdepthcm)

#Vegan MDS in GGPLOT to see where weeds are most abundant:
ggplot(data = Brackish_Data, aes(V1,V2,size = Introduced_Cover)) + geom_point() +
  ggtitle("PCoA of Brackish Communities",subtitle = "averaged across 10 years, TopWeed = altephil") +
  xlab("X coordinate of PCoA")+ylab("Y coordinate of PCoA")+
  theme(legend.position = "bottom")

#Brackish SEM  with SOIL salinity =======
model_Brackish <- '
#regressions:

NatRich     ~ Depth + Flood  + Soil + Alien  
Native      ~ Depth + Flood  + Soil + Alien

Alien       ~ Depth + Flood  + Soil 
Composition ~ Alien + NatRich + Native

#covariances:
NatRich ~~ Native
Composition ~~ NatRich
#Composition ~~ Alien
Native ~~ Composition
#Depth ~~ Flood
'
fit_Brackish <- sem(model_Brackish,missing="direct",estimator="ML",data=Brackish_Data)
summary(fit_Brackish, fit.measures=TRUE, rsquare=T) 

#Design layout for our SemPath Diagram boxes:
lay<-matrix(c(-0.5,  -0.5,
              0.5,  -0.5,
              0,     0.15, #Alien Position
              0,    -0.35,
              0,     0.5,
              0.5,   0.5, 
             -0.5,   0.5), ncol=2,byrow=TRUE)

#Extrat p-values to control edge.label.bg & edge.label.font in semPaths:
extractPvalues    <- parameterEstimates(fit_Brackish)
BrackishSigData <- data.frame(Pvalue =extractPvalues[1:20, "pvalue"])#P values are for our regressions
BrackishSigData
Bold_Brackish_Sig <- as.integer(ifelse(BrackishSigData$Pvalue < 0.05 , 2,1))#Set bold(2) if P< 0.05, otherwise = 1
Bold_Brackish_Sig #values #for edge.label.font, 11 models to define which effects are significant (2=bold):
Brackish_Label_bg <- ifelse(BrackishSigData$Pvalue < 0.05 ,"yellow","white")# coding background labels

#Run Brackish semPaths:======
semPaths(fit1,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=6,
         residuals =  F, exoCov = F,layout = lay,
         edge.label.font = Bold_Brackish_Sig,
         edge.label.bg = Brackish_Label_bg )#,filetype = "jpg",filename = "SEM_Brackish2018")
title("Brackish path analysis (2007-2017)", line = 1)




#"Intermediate" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_22june2018.csv")
Intermediate_Only <-  filter (VegAllEnvData, VegAllEnvData$Community=="Intermediate") %>% na.omit()
dim(Intermediate_Only)#358 471

#Compute means of all variables per StationFront (site), over all years:
Intermediate.av <- Intermediate_Only %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness,Mean_SoilSalinity,
                    meanwaterdepthcm,floodedpercent,
                    MeanWaterSalinity, Acer_rubrum:Ziza_miliacea),mean,na.rm=T)
dim(Intermediate.av)#  55 460

#Subset native/introduced Veg matrix where colSums are > 0:
Intermediate_Veg_Matrix <- as.data.frame (subset ( Intermediate.av, select = c(Acer_rubrum:Ziza_miliacea)))
dim(Intermediate_Veg_Matrix)#   55 453
Intermediate_Veg_Matrix <- Intermediate_Veg_Matrix [ , which(colSums(Intermediate_Veg_Matrix) > 0)]
dim(Intermediate_Veg_Matrix)#    55 194

colCount3 = colSums(Intermediate_Veg_Matrix) #sum up the abundance column-wise
topID3 = order(colCount3,decreasing=TRUE)[1:length(Intermediate_Veg_Matrix)] # choose all Intermediate plant species
topID3 = names(Intermediate_Veg_Matrix[topID3]) # names of plant species in decreasing order
Intermediate_Plant_Sp <- data.frame( specCode = topID3)
Intermediate_Plant_Sp#207 species (both native and introduced)
Plant_List <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced

#join Intermediate_Plants & Plant_List to see which species are native/introduced:
Plant_List<- subset(Plant_List, select = c(specCode, nat))
Intermediate_Plants <- left_join(Intermediate_Plant_Sp,Plant_List, by = "specCode")
dim(Intermediate_Plants)#  194   2

#Subset native only
Intermediate_Native.Species <- filter(Intermediate_Plants, nat == "native")
Intermediate_Native.Species #152 native species

#Select natives only from Intermediate.av:
Intermediate.av.veg.native <- subset( Intermediate.av, select = unique(Intermediate_Native.Species$specCode))
dim(Intermediate.av.veg.native)#  55 152
Intermediate.av.veg.native.total <- rowSums (Intermediate.av.veg.native) #create extra column with total cover of all natives
range( Intermediate.av.veg.native.total ) #   60.79143 170.80400

#Compute Richness : 
Intermediate_Native_Richness <- specnumber(Intermediate_Veg_Matrix)#Compute Native-Only richness
range(Intermediate_Native_Richness)# 6 65
#Subset introduced only:
Intermediate_introduced.Species <- filter(Intermediate_Plants, nat == "introduced")
Intermediate_introduced.Species #4
####### specCode        nat
1    Phra_australis introduced
2  Alte_philoxeroid introduced
3       Pani_repens introduced
4    Colo_esculenta introduced
5    Sphe_zeylanica introduced
6  Ludw_grandiflora introduced
7    Echi_crusgalli introduced
8  Hydr_bonariensis introduced
9     Cyno_dactylon introduced
10 Echi_cruspavonis introduced
11   Eich_crassipes introduced
12     Rume_crispus introduced
13  Poly_hydropiper introduced
14    Pasp_urvillei introduced
15      Sacc_indica introduced
16           Amar_L introduced

Intermediate.av.veg.introduced <- subset( Intermediate.av, select = unique(Intermediate_introduced.Species$specCode))
dim(Intermediate.av.veg.introduced)#55 16
Intermediate.av.veg.introduced.total <- rowSums (Intermediate.av.veg.introduced)#Extra column = total cover of introduced species per site
range(Intermediate.av.veg.introduced.total)# 0.00000 56.78286
TotalCover_Intermediate_Weeds <- colSums(Intermediate.av.veg.introduced)
range(TotalCover_Intermediate_Weeds)#  0.01909091 181.01777778

#WEED Species in Intermediate:
names(sort(TotalCover_Intermediate_Weeds, decreasing = TRUE))
Intermediate_Weed_Sp_barchart <- barchart(sort(TotalCover_Intermediate_Weeds),col = "green", main = "Intermediate Alien Plants (colSums)")
Intermediate_Weed_Sp_barchart
Intermediate_Weed_histogram <- qplot(Intermediate.av.veg.introduced.total, main = "Intermediate Alien Plants (RowSums)", colour = I("green"))
Intermediate_Weed_histogram
grid.arrange(Intermediate_Weed_Sp_barchart, Intermediate_Weed_histogram, ncol = 2)

#Subset Environ factors:
Intermediate.av.env <-  subset( Intermediate.av, select = c( Mean_SoilSalinity,meanwaterdepthcm,
                                                     MeanWaterSalinity,floodedpercent,richness))
#Add new factors for SEM:
Intermediate.av.env$Introduced_Cover <- Intermediate.av.veg.introduced.total
Intermediate.av.env$Native_Cover     <- Intermediate.av.veg.native.total
Intermediate.av.env$Native_Richness  <-  Intermediate_Native_Richness #Computed above

#Intermediate the most dominant species:======
colCount_Intermediate = colSums(Intermediate_Veg_Matrix) #sum up the abundance column-wise
topSp_Intermediate = order(colCount_Intermediate,decreasing=TRUE)[1] # 
topSp_Intermediate = names(Intermediate_Veg_Matrix[topSp_Intermediate]) # subset name of that one most dominant
topSp_Intermediate # "Spar_patens" is the most dominant species

#Run PCoA:
#  WEB >>>  https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_examples
#use a PCoA  (principal coordinates analysis) rather than a PCA (Principal components analysis). With PCoA you can
#use bray curtis dissimilarity (rather than only euclidean distance which is what PCA uses)
Intermediate_distance <- vegdist(Intermediate_Veg_Matrix , "bray")
Intermediate.pca <- cmdscale(Intermediate_distance , eig = TRUE)
names(Intermediate.pca)#"points" "eig"    "x"      "ac"     "GOF" 
ordiplot(Intermediate.pca, display = 'sites', type = 'points',
         cex = 2,bg="yellow")

#Draw Ordination points:
#Combine MDS PC and env data together:
coordinates<-as.data.frame(Intermediate.pca$points[,1:2]) #get MDS1 (x-axis Comp value)
Intermediate_Data<-cbind(coordinates, Intermediate.av.env)
dim(Intermediate_Data)#59 10

#Standarize the variables so their effect size are comparable:
Intermediate_Data$TotRich     <- scale (Intermediate_Data$richness)
Intermediate_Data$NatRich     <- scale (Intermediate_Data$Native_Richness)
Intermediate_Data$Soil        <- scale (Intermediate_Data$Mean_SoilSalinity)
Intermediate_Data$Water       <- scale (Intermediate_Data$MeanWaterSalinity)
Intermediate_Data$Alien       <- scale (Intermediate_Data$Introduced_Cover)
Intermediate_Data$Native      <- scale (Intermediate_Data$Native_Cover)
Intermediate_Data$Composition <- scale (Intermediate_Data$V1)
Intermediate_Data$Flood       <- scale (Intermediate_Data$floodedpercent)
Intermediate_Data$Depth       <- scale (Intermediate_Data$meanwaterdepthcm)

#Vegan MDS in GGPLOT to see where weeds are most abundant:
ggplot(data = Intermediate_Data, aes(V1,V2,size = Introduced_Cover)) + geom_point() +
  ggtitle("PCoA of Intermediate Communities",subtitle = "averaged across 10 years, TopWeed = altephil") +
  xlab("X coordinate of PCoA")+ylab("Y coordinate of PCoA")+
  theme(legend.position = "bottom")

#Intermediate SEM  with SOIL salinity =======
model_Intermediate <- '
#regressions:

NatRich     ~ Depth + Flood  + Soil + Alien  
Native      ~ Depth + Flood  + Soil + Alien

Alien       ~ Depth + Flood  + Soil 
Composition ~ Alien + NatRich + Native

#covariances:
NatRich ~~ Native
Composition ~~ NatRich
#Composition ~~ Alien
Native ~~ Composition
#Depth ~~ Flood
'
fit_Intermediate <- sem(model_Intermediate,missing="direct",estimator="ML",data=Intermediate_Data)
summary(fit_Intermediate, fit.measures=TRUE, rsquare=T) 

#Design layout for our SemPath Diagram boxes:
lay<-matrix(c(-0.5,  -0.5,
              0.5,  -0.5,
              0,     0.15, #Alien Position
              0,    -0.35,
              0,     0.5,
              0.5,   0.5, 
             -0.5,   0.5), ncol=2,byrow=TRUE)

#Extrat p-values to control edge.label.bg & edge.label.font in semPaths:
extractPvalues    <- parameterEstimates(fit_Intermediate)
IntermediateSigData <- data.frame(Pvalue =extractPvalues[1:20, "pvalue"])#P values are for our regressions
IntermediateSigData
Bold_Intermediate_Sig <- as.integer(ifelse(IntermediateSigData$Pvalue < 0.05 , 2,1))#Set bold(2) if P< 0.05, otherwise = 1
Bold_Intermediate_Sig #values #for edge.label.font, 11 models to define which effects are significant (2=bold):
Intermediate_Label_bg <- ifelse(IntermediateSigData$Pvalue < 0.05 ,"yellow","white")# coding background labels

#Run Intermediate semPaths:======
semPaths(fit_Intermediate,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=6,
         residuals =  F, exoCov = F,layout = lay,
         edge.label.font = Bold_Intermediate_Sig,
         edge.label.bg = Intermediate_Label_bg )#,filetype = "jpg",filename = "SEM_Intermediate2018")
title("Intermediate path analysis (2007-2017)", line = 1)


#"Saline" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_22june2018.csv")
Saline_Only <-  filter (VegAllEnvData, VegAllEnvData$Community=="Saline") %>% na.omit()
dim(Saline_Only)#now: 478 470

#Compute means of all variables per StationFront (site), over all years:
Saline.av <- Saline_Only %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness,Mean_SoilSalinity,
                    meanwaterdepthcm,floodedpercent,
                    MeanWaterSalinity, Acer_rubrum:Ziza_miliacea),mean,na.rm=T)
dim(Saline.av)#  63 460


#Subset native/introduced Veg matrix where colSums are > 0:
Saline_Veg_Matrix <- as.data.frame (subset ( Saline.av, select = c(Acer_rubrum:Ziza_miliacea)))
dim(Saline_Veg_Matrix)#   63 453
Saline_Veg_Matrix <- Saline_Veg_Matrix [ , which(colSums(Saline_Veg_Matrix) > 0)]
dim(Saline_Veg_Matrix)#   63 37

colCount4 = colSums(Saline_Veg_Matrix) #sum up the abundance column-wise
topID4 = order(colCount4,decreasing=TRUE)[1:length(Saline_Veg_Matrix)] # choose all Saline plant species
topID4 = names(Saline_Veg_Matrix[topID4]) # names of plant species in decreasing order
Saline_Plant_Sp <- data.frame( specCode = topID4)
Saline_Plant_Sp#37 species (both native and introduced)
Plant_List <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
str(Plant_List)#data.frame':	3454 obs. of  6 variables

#join Saline_Plants & Plant_List to see which species are native/introduced:
Plant_List<- subset(Plant_List, select = c(specCode, nat))
Saline_Plants <- left_join(Saline_Plant_Sp,Plant_List, by = "specCode")
dim(Saline_Plants)# ] 37  2

#Subset native only
Saline_Native.Species <- filter(Saline_Plants, nat == "native")
Saline_Native.Species #32 native species
#Select natives only from Saline.av:
Saline.av.veg.native <- subset( Saline.av, select = unique(Saline_Native.Species$specCode))
dim(Saline.av.veg.native)# 63 32
Saline.av.veg.native.total <- rowSums (Saline.av.veg.native) #create extra column with total cover of all natives
range( Saline.av.veg.native.total ) # 21.65515 79.57386

#Compute Richness : 
Saline_Native_Richness <- specnumber(Saline_Veg_Matrix)#Compute Native-Only richness
range(Saline_Native_Richness)#   1 14

#Subset introduced only:
Saline_introduced.Species <- filter(Saline_Plants, nat == "introduced")
Saline_introduced.Species #2
##     specCode        nat
1    Pani_repens introduced
2 Phra_australis introduced

Saline.av.veg.introduced <- subset( Saline.av, select = unique(Saline_introduced.Species$specCode))
dim(Saline.av.veg.introduced)#63 2
Saline.av.veg.introduced.total <- rowSums (Saline.av.veg.introduced)#Extra column = total cover of introduced species per site
range(Saline.av.veg.introduced.total)#0.00000 2.11296
TotalCover_Saline_Weeds <- colSums(Saline.av.veg.introduced)

#WEED Species in Saline:
names(sort(TotalCover_Saline_Weeds, decreasing = TRUE))#2

Saline_Weed_Sp_barchart <- barchart(sort(TotalCover_Saline_Weeds),col = "green", main = "Saline Alien Plants (colSums)")
Saline_Weed_Sp_barchart
Saline_Weed_histogram <- qplot(Saline.av.veg.introduced.total, main = "Saline Alien Plants (RowSums)", colour = I("green"))
Saline_Weed_histogram
grid.arrange(Saline_Weed_Sp_barchart, Saline_Weed_histogram, ncol = 2)

#Subset Environ factors:
Saline.av.env <-  subset( Saline.av, select = c( Mean_SoilSalinity,meanwaterdepthcm,
                                                 MeanWaterSalinity,floodedpercent,richness))
Saline.av.env$Introduced_Cover <- Saline.av.veg.introduced.total
Saline.av.env$Native_Cover     <- Saline.av.veg.native.total
Saline.av.env$Native_Richness  <- Saline_Native_Richness

#Saline the most dominant species:======
colCount_Saline = colSums(Saline_Veg_Matrix) #sum up the abundance column-wise
topSp_Saline = order(colCount_Saline,decreasing=TRUE)[1] # 
topSp_Saline = names(Saline_Veg_Matrix[topSp_Saline]) # subset name of that one most dominant
topSp_Saline # "Spar_alterniflor" is the most dominant

#Run PCoA:
#  WEB >>>  https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_examples
#use a PCoA  (principal coordinates analysis) rather than a PCA (Principal components analysis). With PCoA you can
#use bray curtis dissimilarity (rather than only euclidean distance which is what PCA uses)
Saline_distance <- vegdist(Saline_Veg_Matrix , "bray")
Saline.pca <- cmdscale(Saline_distance , eig = TRUE)
names(Saline.pca)#"points" "eig"    "x"      "ac"     "GOF" 
ordiplot(Saline.pca, display = 'sites', type = 'points',
         cex = 2,bg="yellow")

#Draw Ordination points:
#Combine MDS PC and env data together:
coordinates<-as.data.frame(Saline.pca$points[,1:2]) #get MDS1 (x-axis Comp value)
Saline_Data<-cbind(coordinates, Saline.av.env)
dim(Saline_Data)#only 63 10

#Standarize the variables so their effect size are comparable:
Saline_Data$Richness   <- scale (Saline_Data$richness)
Saline_Data$NatRich   <- scale (Saline_Data$Native_Richness)
Saline_Data$Soil       <- scale (Saline_Data$Mean_SoilSalinity)
Saline_Data$Water      <- scale (Saline_Data$MeanWaterSalinity)
Saline_Data$Alien      <- scale (Saline_Data$Introduced_Cover)
Saline_Data$Native     <- scale (Saline_Data$Native_Cover)
Saline_Data$Composition <- scale (Saline_Data$V1)
Saline_Data$Flood       <- scale (Saline_Data$floodedpercent)
Saline_Data$Depth       <- scale (Saline_Data$meanwaterdepthcm)

#Vegan MDS in GGPLOT to see where weeds are most abundant:
ggplot(data = Saline_Data, aes(V1,V2,size = Introduced_Cover)) + geom_point() +
  ggtitle("PCoA of Saline Communities",subtitle = paste("averaged across 10 years, Top Alien = ", topSp_Saline, sep = "")) +
  xlab("X coordinate of PCoA")+ylab("Y coordinate of PCoA")+
  theme(legend.position = "bottom")

#Saline SEM  with SOIL salinity =======
model_Saline <- '
#regressions:

NatRich     ~ Depth + Flood  + Soil + Alien  
Native      ~ Depth + Flood  + Soil + Alien
Alien       ~ Depth + Flood  + Soil 
Composition ~ Alien + NatRich + Native

#covariances:
NatRich ~~ Native
Composition ~~ NatRich
#Composition ~~ Alien
#Native ~~ Composition
#Depth ~~ Flood
'
fit_Saline <- sem(model_Saline,missing="direct",estimator="ML",data=Saline_Data)
summary(fit_Saline, fit.measures=TRUE, rsquare=T) 

#Design layout for our SemPath Diagram boxes:
lay<-matrix(c(-0.5,  -0.5,
              0.5,  -0.5,
              0,     0.15, #Alien Position
              0,    -0.35,
              0,     0.5,
              0.5,   0.5, 
              -0.5,   0.5), ncol=2,byrow=TRUE)

#Extrat p-values to control edge.label.bg & edge.label.font in semPaths:
extractPvalues    <- parameterEstimates(fit_Saline)
SalineSigData <- data.frame(Pvalue =extractPvalues[1:18, "pvalue"])#P values are for our regressions
SalineSigData
Bold_Saline_Sig <- as.integer(ifelse(SalineSigData$Pvalue < 0.05 , 2,1))#Set bold(2) if P< 0.05, otherwise = 1
Bold_Saline_Sig #values #for edge.label.font, 11 models to define which effects are significant (2=bold):
Saline_Label_bg <- ifelse(SalineSigData$Pvalue < 0.05 ,"yellow","white")# coding background labels

#Run Saline semPaths:======
semPaths(fit_Saline,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=6,
         residuals =  F, exoCov = F,layout = lay,
         edge.label.font = Bold_Saline_Sig,
         edge.label.bg = Saline_Label_bg )#,filetype = "jpg",filename = "SEM_Saline2018")
title("Saline path analysis (2007-2017)", line = 1)