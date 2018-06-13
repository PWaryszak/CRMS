#LOAD LIBRARIES & DATA:
#11june2018 - we give up top weed and look at the impact of all weeds:
#New Veg  data = VegAllEnvData_11june2018
VegAllEnvData <- read.csv("VegAllEnvData_11june2018.csv")
str(VegAllEnvData) #2849 obs. of  470 variables:
library("lavaan")
library("semPlot")
library("tidyverse")
library("vegan")
library("scatterplot3d")
library("gridExtra")
library("grid")

#See 04_MERGE_VegHydroSoil for details on how "VegAllEnvData_11june2018.csv" was produced.
#"Freshwater" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_11june2018.csv")
FreshwaterOnly <- VegAllEnvData[ VegAllEnvData$Community=="Freshwater",]
FreshwaterOnly_Clean <- na.omit(FreshwaterOnly)#rows with NA-s that need removing
FreshwaterVeg_Cover<-subset(FreshwaterOnly_Clean, select = Acer_rubrum:Ziza_miliacea)  #Freshwater veg cover data only
dim(FreshwaterVeg_Cover)# 336 453

dim(FreshwaterOnly)#now: 603 470

#Create a StationFront-level Freshwater dataset (average over years)
Freshwater.soil <- subset(FreshwaterOnly_Clean,
                     select = c(StationFront,Community,richness,Mean_SoilSalinity,
                                meanwaterdepthcm,floodedpercent,
                                MeanWaterSalinity, Acer_rubrum:Ziza_miliacea))

#Compute mean covers per Station, all years:
Freshwater.av <-Freshwater.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:Ziza_miliacea),mean,na.rm=T)# %>% na.omit()
dim(Freshwater.av)# 41 460

#Subset native/introduced Veg matrix:
#ID and remove the most dominant weed from veg matrix:
colCount = colSums(FreshwaterVeg_Cover) #sum up the abundance column-wise
topID = order(colCount,decreasing=TRUE)[1:length(FreshwaterVeg_Cover)] # choose all Freshwater plant species
topID = names(FreshwaterVeg_Cover[topID]) # names of plant species in decreasing order
Freshwater_Plant_Sp <- data.frame( specCode = topID)
Freshwater_Plant_Sp
Plant_List <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018
str(Plant_List)#data.frame':	3454 obs. of  6 variables

#join Freshwater_Plants & Plant_List to see which species are invasive:
Plant_List<- subset(Plant_List, select = c(specCode, nat))
Freshwater_Plants <- left_join(Freshwater_Plant_Sp,Plant_List, by = "specCode")
head(Freshwater_Plants)# most abundant weed is Altephil 

#Subset native only
Freshwater_Native.Species <- filter(Freshwater_Plants, nat == "native")
Freshwater_Native.Species #346
#Select natives only from Freshwater.av:
Freshwater.av.veg.native <- subset( Freshwater.av, select = unique(Freshwater_Native.Species$specCode))
dim(Freshwater.av.veg.native)#41 346
range (colSums(Freshwater.av.veg.native))#0.00000 98.65938
Freshwater.av.veg.native.total <- rowSums (Freshwater.av.veg.native) #create extra column with total cover of all natives

#Zero colums need to be removed prior ordinations: 
Freshwater.av.veg.native.good<- Freshwater.av.veg.native [ , colSums(Freshwater.av.veg.native) > 0]
dim(Freshwater.av.veg.native.good)#  41 216
Freshwater_Native_Richness <- specnumber(Freshwater.av.veg.native.good)#Compute Native-Only richness
range(Freshwater_Native_Richness)#13 83

#Subset introduced only:
Freshwater_introduced.Species <- filter(Freshwater_Plants, nat == "introduced")
Freshwater_introduced.Species #346
Freshwater.av.veg.introduced <- subset( Freshwater.av, select = unique(Freshwater_introduced.Species$specCode))
dim(Freshwater.av.veg.introduced)#41 43
range(Freshwater.av.veg.introduced.total)#0.000000 7.167448
Freshwater.av.veg.introduced.total <- rowSums (Freshwater.av.veg.introduced)#Extra column = total cover of introduced species per site

#Subset Environ factors:
Freshwater.av.env <-  subset( Freshwater.av, select = c( Mean_SoilSalinity,meanwaterdepthcm,
                                               MeanWaterSalinity,floodedpercent,richness))
Freshwater.av.env$Introduced_Cover <- Freshwater.av.veg.introduced.total
Freshwater.av.env$Native_Cover <- Freshwater.av.veg.native.total
Freshwater.av.env$Native_Richness <- Freshwater_Native_Richness 

#Freshwater most dominant = "Pani_hemitomon":======
colCount_Freshwater = colSums(FreshwaterVeg_Cover) #sum up the abundance column-wise
topSp_Freshwater = order(colCount_Freshwater,decreasing=TRUE)[1] # 
topSp_Freshwater = names(FreshwaterVeg_Cover[topSp_Freshwater]) # 
topSp_Freshwater #"Pani_hemitomon"

#Run PCoA:
#  WEB >>>  https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_examples
#use a PCoA  (principal coordinates analysis) rather than a PCA (Principal components analysis). With PCoA you can
#use bray curtis dissimilarity (rather than only euclidean distance which is what PCA uses)
Freshwater_distance <- vegdist(Freshwater.av.veg.native.good, "bray")
Freshwater.pca <- cmdscale(Freshwater_distance , eig = TRUE)
names(Freshwater.pca)#"points" "eig"    "x"      "ac"     "GOF" 
ordiplot(Freshwater.pca, display = 'sites', type = 'points',
         cex = 2,bg="yellow")

#Draw Ordination points:
#Combine MDS PC and env data together:
coordinates<-as.data.frame(Freshwater.pca$points[,1:2]) #get MDS1 (x-axis Comp value)
veg.nmds<-cbind(coordinates, Freshwater.av.env)
dim(veg.nmds)#only 41 10

#Standarize the variables so their effect size are comparable:
veg.nmds$TotRich <- scale (veg.nmds$richness)
veg.nmds$NatRich <- scale (veg.nmds$Native_Richness)
veg.nmds$Soil <- scale (veg.nmds$Mean_SoilSalinity)
veg.nmds$Water <- scale (veg.nmds$MeanWaterSalinity)
veg.nmds$Alien <- scale (veg.nmds$Introduced_Cover)
veg.nmds$Native <- scale (veg.nmds$Native_Cover)
veg.nmds$Composition <- scale (veg.nmds$V1)
veg.nmds$Flood<- scale (veg.nmds$floodedpercent)
veg.nmds$Depth<- scale (veg.nmds$meanwaterdepthcm)

#Vegan MDS in GGPLOT to see where weeds are most abundant:
ggplot(data = veg.nmds, aes(V1,V2,size = Introduced_Cover)) + geom_point() +
       ggtitle("PCoA of Freshwater Communities",subtitle = "averaged across 10 years, TopWeed = altephil") +
       xlab("X coordinate of PCoA")+ylab("Y coordinate of PCoA")+
       theme(legend.position = "bottom")
     
#Freshwater SEM  with SOIL salinity =======
model1 <- '
#regressions:

NatRich  ~ Depth +Soil +Flood +Alien
Composition ~ Depth + Flood + Soil + Alien
Alien ~ Depth + Flood + Soil 


#covariances:
NatRich  ~~ Composition
     '
fit1 <- sem(model1,missing="direct",estimator="ML",data=veg.nmds)
summary(fit1, fit.measures=TRUE, rsquare=T) 

#Design layout for our SemPath Diagram boxes:
lay<-matrix(c(-0.5,  -0.5,
              0.5,  -0.5,
              0,    -0.3, #Alien Position
              -0.5,   0.5,
              0,     0.5,
              0.5,   0.5), ncol=2,byrow=TRUE)

#Extrat p-values to control edge.label.bg & edge.label.font in semPaths:
extractPvalues<-parameterEstimates(fit1)#Thansk to WEB: http://lavaan.ugent.be/tutorial/inspect.html
extractPvalues[1:13, "pvalue"]#First 13 values are for our regressions
FreshwaterSigData <- data.frame(Pvalue =extractPvalues[1:13, "pvalue"])#First 11 values are for our regressions
FreshwaterSigData
Bold_Freshwater_Sig <- as.integer(ifelse(FreshwaterSigData$Pvalue < 0.05 ,2,1))#Set bold(2) if P< 0.05, otherwise = 1
Bold_Freshwater_Sig #values #for edge.label.font, 11 models to define which effects are significant (2=bold):
Freshwater_Label_bg <- ifelse(FreshwaterSigData$Pvalue < 0.05 ,"yellow","white")# coding background labels

#Run semPaths:
par(mfrow = c(1,1))
semPaths(fit1,"est", intercepts = F, fade = F, 
              title = T, edge.label.cex = 1.1,sizeMan = 8,
              edge.label.position = 0.25, nCharNodes=6,
              residuals =  F, exoCov = F,layout = lay,
              edge.label.font = Bold_Freshwater_Sig,
              edge.label.bg = Freshwater_Label_bg ,filetype = "jpg",filename = "SEM_Freshwater2018")
title("Freshwater path analysis (2007-2017)", line = 1)




#"Brackish" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_11june2018.csv")
Brackish_Only <- VegAllEnvData[ VegAllEnvData$Community=="Brackish",]
dim(Brackish_Only)#now: 608 470
Brackish_Only_Clean <- na.omit(Brackish_Only)#rows with NA-s that need removing
BrackishVeg_Cover<-subset(Brackish_Only_Clean, select = Acer_rubrum:Ziza_miliacea)  #Brackish veg cover data only
dim(BrackishVeg_Cover)# 284 453

#Create a StationFront-level Brackish dataset (average over years)
Brackish.soil <- subset(Brackish_Only_Clean,
                     select = c(StationFront,Community,richness,Mean_SoilSalinity,
                                meanwaterdepthcm,floodedpercent,
                                MeanWaterSalinity, Acer_rubrum:Ziza_miliacea))

#Compute mean covers per Station, all years:
Brackish.av <-Brackish.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:Ziza_miliacea),mean,na.rm=T)# %>% na.omit()
dim(Brackish.av)# 40 460

#Subset native/introduced Veg matrix:
#ID and remove the most dominant weed from veg matrix:
#join Brackish_Plants & Plant_List to see which species are invasive:
Plant_List <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018
str(Plant_List)#data.frame':	3454 obs. of  6 variables:
Plant_List<- subset(Plant_List, select = c(specCode, nat))

colCount = colSums(BrackishVeg_Cover) #sum up the abundance column-wise
topID = order(colCount,decreasing=TRUE)[1:length(BrackishVeg_Cover)] # choose all Brackish plant species
topID = names(BrackishVeg_Cover[topID]) # names of plant species in decreasing order
Brackish_Plant_Sp <- data.frame( specCode = topID)
Brackish_Plant_Sp
Brackish_Plants <- left_join(Brackish_Plant_Sp,Plant_List, by = "specCode")
head(Brackish_Plants)

#Subset native only
Brackish_Native.Species <- filter(Brackish_Plants, nat == "native")
Brackish_Native.Species #346
#Select natives only from Brackish.av:
Brackish.av.veg.native <- subset( Brackish.av, select = unique(Brackish_Native.Species$specCode))
dim(Brackish.av.veg.native)#40 346
range (colSums(Brackish.av.veg.native))# 0.0000 711.6422
Brackish.av.veg.native.total <- rowSums (Brackish.av.veg.native) #create extra column with total cover of all natives

#Zero colums need to be removed prior ordinations: 
Brackish.av.veg.native.good<- Brackish.av.veg.native [ , colSums(Brackish.av.veg.native) > 0]
dim(Brackish.av.veg.native.good)#  41 73

#Subset introduced only:
Brackish_introduced.Species <- filter(Brackish_Plants, nat == "introduced")
Brackish_introduced.Species #43
Brackish.av.veg.introduced <- subset( Brackish.av, select = unique(Brackish_introduced.Species$specCode))
dim(Brackish.av.veg.introduced)#40 43
range(Brackish.av.veg.introduced)#0.000000 3.071408
Brackish.av.veg.introduced.total <- rowSums (Brackish.av.veg.introduced)#Extra column = total cover of introduced species per site

#Subset Environ factors:
Brackish.av.env <-  subset( Brackish.av, select = c( Mean_SoilSalinity,meanwaterdepthcm,
                                               MeanWaterSalinity,floodedpercent,richness))
Brackish.av.env$Introduced_Cover <- Brackish.av.veg.introduced.total
Brackish.av.env$Native_Cover <- Brackish.av.veg.native.total
Brackish_Native_Richness <- specnumber(Brackish.av.veg.native.good)#Compute Native-Only richness
range(Brackish_Native_Richness)#4 26
Brackish.av.env$Native_Richness <- Brackish_Native_Richness

#Brackish the most dominant species:======
colCount_Brackish = colSums(BrackishVeg_Cover) #sum up the abundance column-wise
topSp_Brackish = order(colCount_Brackish,decreasing=TRUE)[1] # 
topSp_Brackish = names(BrackishVeg_Cover[topSp_Brackish]) # 
topSp_Brackish #"Spar_patens"

#Run PCoA:
#  WEB >>>  https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_examples
#use a PCoA  (principal coordinates analysis) rather than a PCA (Principal components analysis). With PCoA you can
#use bray curtis dissimilarity (rather than only euclidean distance which is what PCA uses)
Brackish_distance <- vegdist(Brackish.av.veg.native.good, "bray")#native veg matrix
Brackish.pca <- cmdscale(Brackish_distance , eig = TRUE)
names(Brackish.pca)#"points" "eig"    "x"      "ac"     "GOF" 
ordiplot(Brackish.pca, display = 'sites', type = 'points',
         cex = 2,bg="yellow")

#Draw Ordination points:
#Combine MDS PC and env data together:
coordinates<-as.data.frame(Brackish.pca$points[,1:2]) #get MDS1 (x-axis Comp value)
Brackish_Data<-cbind(coordinates, Brackish.av.env)
dim(Brackish_Data)#only 40 10

#Standarize the variables so their effect size are comparable:
Brackish_Data$TotRich <- scale (Brackish_Data$richness)
Brackish_Data$NatRich <- scale (Brackish_Data$Native_Richness)
Brackish_Data$Soil     <- scale (Brackish_Data$Mean_SoilSalinity)
Brackish_Data$Water    <- scale (Brackish_Data$MeanWaterSalinity)
Brackish_Data$Alien    <- scale (Brackish_Data$Introduced_Cover)
Brackish_Data$Native   <- scale (Brackish_Data$Native_Cover)
Brackish_Data$Composition <- scale (Brackish_Data$V1)
Brackish_Data$Flood   <- scale (Brackish_Data$floodedpercent)
Brackish_Data$Depth   <- scale (Brackish_Data$meanwaterdepthcm)

#Vegan MDS in GGPLOT to see where weeds are most abundant:
ggplot(data = Brackish_Data, aes(V1,V2,size = Introduced_Cover)) + geom_point() +
  ggtitle("PCoA of Brackish Communities",subtitle = paste("averaged across 10 years, Top Alien = ", topSp_Brackish, sep = "")) +
  xlab("X coordinate of PCoA")+ylab("Y coordinate of PCoA")+
  theme(legend.position = "bottom")

#Brackish SEM  with SOIL salinity =======
model_Brackish  <- '
#regressions:

NatRich ~ Depth +Soil +Flood +Alien
Composition ~ Depth + Flood + Soil + Alien
Alien ~ Depth + Flood + Soil 


#covariances:
NatRich ~~ Composition
'
fit_Brackish <- sem(model_Brackish ,missing="direct",estimator="ML",data=Brackish_Data)
summary(fit_Brackish , fit.measures=TRUE, rsquare=T) 

#Design layout for our SemPath Diagram boxes:
lay<-matrix(c(-0.5,  -0.5,
              0.5,  -0.5,
              0,    -0.3, #Alien Position
             -0.5,   0.5,
              0,     0.5,
              0.5,   0.5), ncol=2,byrow=TRUE)

#Extrat p-values to control edge.label.bg & edge.label.font in semPaths:
Brackish_Pvalues<-parameterEstimates(fit_Brackish)#Thansk to WEB: http://lavaan.ugent.be/tutorial/inspect.html
Brackish_Pvalues[1:13, "pvalue"]#First 13 values are for our regressions
Brackish_SigData <- data.frame(Pvalue = Brackish_Pvalues[1:13, "pvalue"])#First 11 values are for our regressions
Brackish_SigData
Bold_Brackish_Sig <- as.integer(ifelse(Brackish_SigData$Pvalue < 0.05 ,2,1))#Set bold(2) if P< 0.05, otherwise = 1
Bold_Brackish_Sig #values #for edge.label.font, 11 models to define which effects are significant (2=bold):
Brackish_Label_bg <- ifelse(Brackish_SigData$Pvalue < 0.05 ,"yellow","white")# coding background labels

#Run semPaths:
par(mfrow = c(1,1))
semPaths(fit_Brackish,"est", intercepts = F, fade = F, 
              title = T, edge.label.cex = 1.1,sizeMan = 8,
              edge.label.position = 0.25, nCharNodes=6,
              residuals =  F, exoCov = F,
              layout = lay,
              edge.label.font = Bold_Brackish_Sig,
              edge.label.bg = Brackish_Label_bg,
              label.font = 2 )#nodes font, 2 = bold
title("Brackish path analysis (2007-2017)", line = 1) # To save use: filetype = "jpg",filename = "SEM_Brackish2018")





#"Intermediate" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_11june2018.csv")
Intermediate_Only <- VegAllEnvData[ VegAllEnvData$Community=="Intermediate",]
dim(Intermediate_Only)#now: 978 470
Intermediate_Only_Clean <- na.omit(Intermediate_Only)#rows with NA-s that need removing
IntermediateVeg_Cover<-subset(Intermediate_Only_Clean, select = Acer_rubrum:Ziza_miliacea)  #Intermediate veg cover data only
dim(IntermediateVeg_Cover)# 388 453

#Create a StationFront-level Intermediate dataset (average over years)
Intermediate.soil <- subset(Intermediate_Only_Clean,
                        select = c(StationFront,Community,richness,Mean_SoilSalinity,
                                   meanwaterdepthcm,floodedpercent,
                                   MeanWaterSalinity, Acer_rubrum:Ziza_miliacea))

#Compute mean covers per Station, all years:
Intermediate.av <-Intermediate.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:Ziza_miliacea),mean,na.rm=T)# %>% na.omit()
dim(Intermediate.av)# 59 460

#Subset native/introduced Veg matrix:
#ID and remove the most dominant weed from veg matrix:
#join Intermediate_Plants & Plant_List to see which species are invasive:
Plant_List <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018
str(Plant_List)#data.frame':	3454 obs. of  6 variables:
Plant_List<- subset(Plant_List, select = c(specCode, nat))

colCount_Intermediate = colSums(IntermediateVeg_Cover) #sum up the abundance column-wise
topID_Intermediate = order(colCount_Intermediate,decreasing=TRUE)[1:length(IntermediateVeg_Cover)] # choose all Intermediate plant species
topID_Intermediate = names(IntermediateVeg_Cover[topID]) # names of plant species in decreasing order
Intermediate_Plant_Sp <- data.frame( specCode = topID_Intermediate)
Intermediate_Plants <- left_join(Intermediate_Plant_Sp,Plant_List, by = "specCode")
head(Intermediate_Plants)

#Subset native only
Intermediate_Native.Species <- filter(Intermediate_Plants, nat == "native")
Intermediate_Native.Species #346
#Select natives only from Intermediate.av:
Intermediate.av.veg.native <- subset( Intermediate.av, select = unique(Intermediate_Native.Species$specCode))
dim(Intermediate.av.veg.native)#59 346
range (colSums(Intermediate.av.veg.native))# 0.0000 711.6422
Intermediate.av.veg.native.total <- rowSums (Intermediate.av.veg.native) #create extra column with total cover of all natives

#Zero colums need to be removed prior ordinations: 
Intermediate.av.veg.native.good<- Intermediate.av.veg.native [ , colSums(Intermediate.av.veg.native) > 0]
dim(Intermediate.av.veg.native.good)#  41 73

#Subset introduced only:
Intermediate_introduced.Species <- filter(Intermediate_Plants, nat == "introduced")
Intermediate_introduced.Species #43
Intermediate.av.veg.introduced <- subset( Intermediate.av, select = unique(Intermediate_introduced.Species$specCode))
dim(Intermediate.av.veg.introduced)#40 43
range(Intermediate.av.veg.introduced)#0.000000 3.071408
Intermediate.av.veg.introduced.total <- rowSums (Intermediate.av.veg.introduced)#Extra column = total cover of introduced species per site

#Subset Environ factors:
Intermediate.av.env <-  subset( Intermediate.av, select = c( Mean_SoilSalinity,meanwaterdepthcm,
                                                     MeanWaterSalinity,floodedpercent,richness))
Intermediate.av.env$Introduced_Cover <- Intermediate.av.veg.introduced.total
Intermediate.av.env$Native_Cover <- Intermediate.av.veg.native.total


#Intermediate the most dominant species:======
colCount_Intermediate = colSums(IntermediateVeg_Cover) #sum up the abundance column-wise
topSp_Intermediate = order(colCount_Intermediate,decreasing=TRUE)[1] # 
topSp_Intermediate = names(IntermediateVeg_Cover[topSp_Intermediate]) # 
topSp_Intermediate #"Spar_patens"

#Run PCoA:
#  WEB >>>  https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_examples
#use a PCoA  (principal coordinates analysis) rather than a PCA (Principal components analysis). With PCoA you can
#use bray curtis dissimilarity (rather than only euclidean distance which is what PCA uses)
Intermediate_distance <- vegdist(Intermediate.av.veg.native.good, "bray")
Intermediate.pca <- cmdscale(Intermediate_distance , eig = TRUE)
names(Intermediate.pca)#"points" "eig"    "x"      "ac"     "GOF" 
ordiplot(Intermediate.pca, display = 'sites', type = 'points',
         cex = 2,bg="yellow")

#Draw Ordination points:
#Combine MDS PC and env data together:
coordinates<-as.data.frame(Intermediate.pca$points[,1:2]) #get MDS1 (x-axis Comp value)
Intermediate_Data<-cbind(coordinates, Intermediate.av.env)
dim(Intermediate_Data)#only 41 9

#Standarize the variables so their effect size are comparable:
Intermediate_Data$Richness <- scale (Intermediate_Data$richness)
Intermediate_Data$Soil <- scale (Intermediate_Data$Mean_SoilSalinity)
Intermediate_Data$Water <- scale (Intermediate_Data$MeanWaterSalinity)
Intermediate_Data$Alien <- scale (Intermediate_Data$Introduced_Cover)
Intermediate_Data$Native <- scale (Intermediate_Data$Native_Cover)
Intermediate_Data$Composition <- scale (Intermediate_Data$V1)
Intermediate_Data$Flood<- scale (Intermediate_Data$floodedpercent)
Intermediate_Data$Depth<- scale (Intermediate_Data$meanwaterdepthcm)

#Vegan MDS in GGPLOT to see where weeds are most abundant:
ggplot(data = Intermediate_Data, aes(V1,V2,size = Introduced_Cover)) + geom_point() +
  ggtitle("PCoA of Intermediate Communities",subtitle = paste("averaged across 10 years, Top Alien = ", topSp_Intermediate, sep = "")) +
  xlab("X coordinate of PCoA")+ylab("Y coordinate of PCoA")+
  theme(legend.position = "bottom")

#Intermediate SEM  with SOIL salinity =======
model_Intermediate  <- '
#regressions:

Richness ~ Depth +Soil +Flood +Alien
Composition ~ Depth + Flood + Soil + Alien
Alien ~ Depth + Flood + Soil 


#covariances:
Richness ~~ Composition
'
fit_Intermediate <- sem(model_Intermediate ,missing="direct",estimator="ML",data=Intermediate_Data)
summary(fit_Intermediate , fit.measures=TRUE, rsquare=T) 

#Design layout for our SemPath Diagram boxes:
lay<-matrix(c(-0.5,  -0.5,
              0.5,  -0.5,
              0,    -0.3, #Alien Position
              -0.5,   0.5,
              0,     0.5,
              0.5,   0.5), ncol=2,byrow=TRUE)

#Extrat p-values to control edge.label.bg & edge.label.font in semPaths:
Intermediate_Pvalues<-parameterEstimates(fit_Intermediate)#Thansk to WEB: http://lavaan.ugent.be/tutorial/inspect.html
Intermediate_Pvalues[1:13, "pvalue"]#First 13 values are for our regressions
Intermediate_SigData <- data.frame(Pvalue = Intermediate_Pvalues[1:13, "pvalue"])#First 11 values are for our regressions
Intermediate_SigData
Bold_Intermediate_Sig <- as.integer(ifelse(Intermediate_SigData$Pvalue < 0.05 ,2,1))#Set bold(2) if P< 0.05, otherwise = 1
Bold_Intermediate_Sig #values #for edge.label.font, 11 models to define which effects are significant (2=bold):
Intermediate_Label_bg <- ifelse(Intermediate_SigData$Pvalue < 0.05 ,"yellow","white")# coding background labels

#Run semPaths:
par(mfrow = c(1,1))
semPaths(fit_Intermediate,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=6,
         residuals =  F, exoCov = F,
         layout = lay,
         edge.label.font = Bold_Intermediate_Sig,
         edge.label.bg = Intermediate_Label_bg,
         label.font = 2 ) #nodes font, 2 = bold
title("Intermediate path analysis (2007-2017)", line = 1) # To save use: filetype = "jpg",filename = "SEM_Intermediate2018")

#"Saline" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_11june2018.csv")
Saline_Only <- VegAllEnvData[ VegAllEnvData$Community=="Saline",]
dim(Saline_Only)#now: 608 470
Saline_Only_Clean <- na.omit(Saline_Only)#rows with NA-s that need removing
SalineVeg_Cover<-subset(Saline_Only_Clean, select = Acer_rubrum:Ziza_miliacea)  #Saline veg cover data only
dim(SalineVeg_Cover)# 284 453

#Create a StationFront-level Saline dataset (average over years)
Saline.soil <- subset(Saline_Only_Clean,
                      select = c(StationFront,Community,richness,Mean_SoilSalinity,
                                 meanwaterdepthcm,floodedpercent,
                                 MeanWaterSalinity, Acer_rubrum:Ziza_miliacea))

#Compute mean covers per Station, all years:
Saline.av <-Saline.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:Ziza_miliacea),mean,na.rm=T)# %>% na.omit()
dim(Saline.av)# 40 460

#Subset native/introduced Veg matrix:
#ID and remove the most dominant weed from veg matrix:
#join Saline_Plants & Plant_List to see which species are invasive:
Plant_List <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018
str(Plant_List)#data.frame':	3454 obs. of  6 variables:
Plant_List<- subset(Plant_List, select = c(specCode, nat))

colCount = colSums(SalineVeg_Cover) #sum up the abundance column-wise
topID = order(colCount,decreasing=TRUE)[1:length(SalineVeg_Cover)] # choose all Saline plant species
topID = names(SalineVeg_Cover[topID]) # names of plant species in decreasing order
Saline_Plant_Sp <- data.frame( specCode = topID)
Saline_Plant_Sp
Saline_Plants <- left_join(Saline_Plant_Sp,Plant_List, by = "specCode")
head(Saline_Plants)

#Subset native only
Saline_Native.Species <- filter(Saline_Plants, nat == "native")
Saline_Native.Species #346
#Select natives only from Saline.av:
Saline.av.veg.native <- subset( Saline.av, select = unique(Saline_Native.Species$specCode))
dim(Saline.av.veg.native)#40 346
range (colSums(Saline.av.veg.native))# 0.0000 711.6422
Saline.av.veg.native.total <- rowSums (Saline.av.veg.native) #create extra column with total cover of all natives

#Zero colums need to be removed prior ordinations: 
Saline.av.veg.native.good<- Saline.av.veg.native [ , colSums(Saline.av.veg.native) > 0]
dim(Saline.av.veg.native.good)#  41 73

#Subset introduced only:
Saline_introduced.Species <- filter(Saline_Plants, nat == "introduced")
Saline_introduced.Species #43
Saline.av.veg.introduced <- subset( Saline.av, select = unique(Saline_introduced.Species$specCode))
dim(Saline.av.veg.introduced)#40 43
range(Saline.av.veg.introduced)#0.000000 3.071408
Saline.av.veg.introduced.total <- rowSums (Saline.av.veg.introduced)#Extra column = total cover of introduced species per site

#Subset Environ factors:
Saline.av.env <-  subset( Saline.av, select = c( Mean_SoilSalinity,meanwaterdepthcm,
                                                 MeanWaterSalinity,floodedpercent,richness))
Saline.av.env$Introduced_Cover <- Saline.av.veg.introduced.total
Saline.av.env$Native_Cover <- Saline.av.veg.native.total


#Saline the most dominant species:======
colCount_Saline = colSums(SalineVeg_Cover) #sum up the abundance column-wise
topSp_Saline = order(colCount_Saline,decreasing=TRUE)[1] # 
topSp_Saline = names(SalineVeg_Cover[topSp_Saline]) # 
topSp_Saline #"Spar_patens"

#Run PCoA:
#  WEB >>>  https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_examples
#use a PCoA  (principal coordinates analysis) rather than a PCA (Principal components analysis). With PCoA you can
#use bray curtis dissimilarity (rather than only euclidean distance which is what PCA uses)
Saline_distance <- vegdist(Saline.av.veg.native.good, "bray")
Saline.pca <- cmdscale(Saline_distance , eig = TRUE)
names(Saline.pca)#"points" "eig"    "x"      "ac"     "GOF" 
ordiplot(Saline.pca, display = 'sites', type = 'points',
         cex = 2,bg="yellow")

#Draw Ordination points:
#Combine MDS PC and env data together:
coordinates<-as.data.frame(Saline.pca$points[,1:2]) #get MDS1 (x-axis Comp value)
Saline_Data<-cbind(coordinates, Saline.av.env)
dim(Saline_Data)#only 41 9

#Standarize the variables so their effect size are comparable:
Saline_Data$Richness <- scale (Saline_Data$richness)
Saline_Data$Soil <- scale (Saline_Data$Mean_SoilSalinity)
Saline_Data$Water <- scale (Saline_Data$MeanWaterSalinity)
Saline_Data$Alien <- scale (Saline_Data$Introduced_Cover)
Saline_Data$Native <- scale (Saline_Data$Native_Cover)
Saline_Data$Composition <- scale (Saline_Data$V1)
Saline_Data$Flood<- scale (Saline_Data$floodedpercent)
Saline_Data$Depth<- scale (Saline_Data$meanwaterdepthcm)

#Vegan MDS in GGPLOT to see where weeds are most abundant:
ggplot(data = Saline_Data, aes(V1,V2,size = Introduced_Cover)) + geom_point() +
  ggtitle("PCoA of Saline Communities",subtitle = paste("averaged across 10 years, Top Alien = ", topSp_Saline, sep = "")) +
  xlab("X coordinate of PCoA")+ylab("Y coordinate of PCoA")+
  theme(legend.position = "bottom")

#Saline SEM  with SOIL salinity =======
model_Saline  <- '
#regressions:

Richness ~ Depth +Soil +Flood +Alien
Composition ~ Depth + Flood + Soil + Alien
Alien ~ Depth + Flood + Soil 


#covariances:
Richness ~~ Composition
'
fit_Saline <- sem(model_Saline ,missing="direct",estimator="ML",data=Saline_Data)
summary(fit_Saline , fit.measures=TRUE, rsquare=T) 

#Design layout for our SemPath Diagram boxes:
lay<-matrix(c(-0.5,  -0.5,
              0.5,  -0.5,
              0,    -0.3, #Alien Position
              -0.5,   0.5,
              0,     0.5,
              0.5,   0.5), ncol=2,byrow=TRUE)

#Extract p-values to control edge.label.bg & edge.label.font in semPaths:
Saline_Pvalues<-parameterEstimates(fit_Saline)#Thansk to WEB: http://lavaan.ugent.be/tutorial/inspect.html
Saline_Pvalues[1:13, "pvalue"]#First 13 values are for our regressions
Saline_SigData <- data.frame(Pvalue = Saline_Pvalues[1:13, "pvalue"])#First 11 values are for our regressions
Saline_SigData
Bold_Saline_Sig <- as.integer(ifelse(Saline_SigData$Pvalue < 0.05 ,2,1))#Set bold(2) if P< 0.05, otherwise = 1
Bold_Saline_Sig #values #for edge.label.font, 11 models to define which effects are significant (2=bold):
Saline_Label_bg <- ifelse(Saline_SigData$Pvalue < 0.05 ,"yellow","white")# coding background labels

#Run semPaths:
par(mfrow = c(1,1))
semPaths(fit_Saline,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=6,
         residuals =  F, exoCov = F,
         layout = lay,
         edge.label.font = Bold_Saline_Sig,
         edge.label.bg = Saline_Label_bg,
         label.font = 2 ) #nodes font, 2 = bold
title("Saline path analysis (2007-2017)", line = 1) # To save use: filetype = "jpg",filename = "SEM_Saline2018")









         
        
         
         
         
