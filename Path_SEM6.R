#Analysis of CRMS data with year as random effect in new piecewise SEM appraoch
#using piecewise SEM-Modified from: Jon Lefcheck, jslefche@vims.edu 
#Install required libraries:=========
install.packages("ape") #Version 3.3
install.packages("caper") # Vresion 0.5.2
install.packages("nlme") # Version 3.1.122
install.packages("lavaan") # Version 0.5.19
install.packages("devtools")
install.packages("vegan")
# Load required libraries
library(ape) #Version 3.3
library(caper) # Vresion 0.5.2
library(nlme) # Version 3.1.122
library(lavaan) # Version 0.5.19
library(devtools) #to load development versions of packages.
library(vegan)

# Load piecewiseSEM from CRAN (# install.packages("piecewiseSEM")
# Install development branch from github
install_github("jslefche/piecewiseSEM@devel", build_vignette = TRUE)
library(piecewiseSEM) # Version 1.0.0
library(dplyr)
library(tidyr)

# Read in data=========
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")
str(VegAllEnvData)
sum(is.na(VegAllEnvData))# 13497
# Remove rows where response or predictors are NA
VegAllEnvData = na.omit(VegAllEnvData)

#Compute Native indices========
#separate natives from introduced:
#Load dataset on native-ness of Lousiana plants:
Plant_List <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
str(Plant_List)#data.frame':	3454 obs. of  6 variables

#Select Veg matrix only from our CRMS data ("VegAllEnvData"):
Veg_Matrix <- as.data.frame (subset (VegAllEnvData, select = c(Acer_rubrum:Ziza_miliacea)))
dim(Veg_Matrix)#  1474 by 453
#Check if all columns are above 0:
Veg_Matrix <- Veg_Matrix [ , which(colSums(Veg_Matrix) > 0)]
dim(Veg_Matrix)#   1474  349

colCount1 = colSums(Veg_Matrix) #sum up the abundance column-wise
topID1 = order(colCount1,decreasing=TRUE)[1:length(Veg_Matrix)] # order plant species by colSums
topID1 = names(Veg_Matrix[topID1]) # names of plant species in decreasing order
Plant_Sp <- data.frame( specCode = topID1)
Plant_Sp #List of 349 species (both native and introduced)

#join Plants_Sp & Plant_List to see which species are native/introduced:
Plant_List_Nat<- subset(Plant_List, select = c(specCode, nat))
All_Plants <- left_join(Plant_Sp,Plant_List_Nat, by = "specCode")
dim(All_Plants)#  349   2

#Subset native only
Native.Species <- filter(All_Plants, nat == "native")
Native.Species #277 native species

#Select natives only from Freshwater.av:
Native_Veg_Matrix <- subset(VegAllEnvData, select =unique(Native.Species$specCode))
dim(Native_Veg_Matrix)# 1474  277

#Compute Native_Richness in Native_Veg_Matrix: 
Native_Richness <- data.frame(Native_Richness = specnumber(Native_Veg_Matrix))#Compute Native-Only richness
range(Native_Richness)#   1 43
dim(Native_Richness)#   1474    1

#Compute Native_Cover in Native_Veg_Matrix: 
Native_Cover <- data.frame(Native_Cover = rowSums(Native_Veg_Matrix))#Compute Native-Only richness
range(Native_Cover)#   7 215
dim(Native_Cover)#   1474    1

#Compute Native_Composition in Native_Veg_Matrix:
Native_distance <- vegdist(Native_Veg_Matrix , "bray")
Native.PCA <- cmdscale(Native_distance , eig = TRUE)
Native_Composition<-data.frame(Native_Composition = Native.PCA$points[,1]) #get MDS1 (x-axis Comp value)


#Compute Alien indices========
#separate natives from introduced:
#Load dataset on native-ness of Lousiana plants:
Plant_List <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
str(Plant_List)#data.frame':	3454 obs. of  6 variables

#Select Veg matrix only from our CRMS data ("VegAllEnvData"):
Veg_Matrix <- as.data.frame (subset (VegAllEnvData, select = c(Acer_rubrum:Ziza_miliacea)))
dim(Veg_Matrix)#  1474 by 453
#Check if all columns are above 0:
Veg_Matrix <- Veg_Matrix [ , which(colSums(Veg_Matrix) > 0)]
dim(Veg_Matrix)#   1474  349

colCount2 = colSums(Veg_Matrix) #sum up the abundance column-wise
topID1 = order(colCount2,decreasing=TRUE)[1:length(Veg_Matrix)] # order plant species by colSums
topID1 = names(Veg_Matrix[topID1]) # names of plant species in decreasing order
Plant_Sp <- data.frame( specCode = topID1)
Plant_Sp #List of 349 species (both native and introduced)

#join Plants_Sp & Plant_List to see which species are native/introduced:
Plant_List_Nat<- subset(Plant_List, select = c(specCode, nat))
All_Plants <- left_join(Plant_Sp,Plant_List_Nat, by = "specCode")
dim(All_Plants)#  349   2

#Subset Alien only
Alien.Species <- filter(All_Plants, nat == "introduced")
Alien.Species #28 Alien species

#Select Aliens only from Freshwater.av:
Alien_Veg_Matrix <- subset(VegAllEnvData, select = unique(Alien.Species$specCode))
dim(Alien_Veg_Matrix)#  1474   28

#Compute Alien_Richness in Alien_Veg_Matrix: 
Alien_Richness <- data.frame(Alien_Richness = specnumber(Alien_Veg_Matrix))#Compute Alien-Only richness
range(Alien_Richness)#   0 5
dim(Alien_Richness)#   1474    1

#Compute Alien_Cover in Alien_Veg_Matrix: 
Alien_Cover <- data.frame(Alien_Cover = rowSums(Alien_Veg_Matrix))#Compute Alien-Only richness
range(Alien_Cover)#   0 92.1
dim(Alien_Cover)#   1474    1

#Join Alien/NAtive indices with VegAllEnvData:============
SEM_Data <- cbind(VegAllEnvData, Native_Richness,Native_Cover, Native_Composition,
                  Alien_Cover, Alien_Richness)
dim(SEM_Data)# 1474  478
#write.csv(SEM_Data, file = "SEM_Data.csv")


#LOAD SEM DATA:
SEM_Data <- read.csv("SEM_Data.csv")

#Standarize SEM_Data variables =====
#These are used in following SEM (so their effect size are comparable):
SEM_Data$NatRich     <- scale (SEM_Data$Native_Richness)
SEM_Data$Soil        <- scale (SEM_Data$Mean_SoilSalinity)
SEM_Data$Water       <- scale (SEM_Data$MeanWaterSalinity)
SEM_Data$WaterSD     <- scale (SEM_Data$MeanWaterSalinity_SD)
SEM_Data$Alien       <- scale (SEM_Data$Alien_Cover)
SEM_Data$Native      <- scale (SEM_Data$Native_Cover)
SEM_Data$NatComp     <- scale (SEM_Data$Native_Composition)
SEM_Data$Flood       <- scale (SEM_Data$floodedpercent)
SEM_Data$Depth       <- scale (SEM_Data$meanwaterdepthcm)
SEM_Data$Depth_SD    <- scale (SEM_Data$meanwaterdepthcm_SD)


# Run analysis for freshwater communities:
SEM_Data_Freshwater <- SEM_Data[SEM_Data$Community == "Freshwater",]
dim(SEM_Data_Freshwater)#345 486

# Now fit piecewise model with random effect for freshwater communities
# Create component models and store in list
SEM_Data_Freshwater_pSEM_randomList = list(
  
  NativeRichness = lme(NatRich ~Depth , random = ~ 1 | year, data = SEM_Data_Freshwater), 
  
  NativeCover =   lme(Native ~Depth + Soil + Alien, random = ~ 1 | year, data = SEM_Data_Freshwater),
  
  NativeComposition = lme(NatComp ~ Soil + Alien, random = ~ 1 | year, data = SEM_Data_Freshwater))

# Run goodness-of-fit tests
sem.fit(SEM_Data_Freshwater_pSEM_randomList, SEM_Data_Freshwater)
#OUTPUT:
$`missing.paths`
missing.path estimate std.error  df crit.value p.value    
1 NatComp ~ Depth + Soil + Alien  -0.0010    0.0065 331    -0.1498  0.8810    
2 NatRich ~ Soil + Depth  -1.6561    0.4045 332    -4.0942  0.0001 ***
3 NatRich ~ Alien + Depth  -0.0663    0.0411 332    -1.6128  0.1077    
4 Native ~ NatRich + Depth + Soil + Alien   0.3174    0.0471 330     6.7350  0.0000 ***
5 NatComp ~ NatRich + Depth + Soil + Alien  -0.0035    0.0062 330    -0.5731  0.5670    
6 NatComp ~ Native + Depth + Soil + Alien  -0.0136    0.0066 330    -2.0668  0.0395   *

  
# Evaluate path significance using unstandardized coefficients
sem.coefs(SEM_Data_Freshwater_pSEM_randomList, SEM_Data_Freshwater, standardize = "none")
#OUTPUT:
response predictor     estimate  std.error p.value    
1 NatRich     Depth -0.236423381 0.05867678  0.0001 ***
2 Native     Alien -0.244767148 0.03745998  0.0000 ***
3 Native      Soil -2.256067830 0.37640329  0.0000 ***
4 Native     Depth -0.226096473 0.05369604  0.0000 ***
5 NatComp      Soil -0.088754382 0.04549804  0.0519    
6 NatComp     Alien -0.004344838 0.00461328  0.3470

# Obtain standardized regression coefficients
sem.coefs(SEM_Data_Freshwater_pSEM_randomList, SEM_Data_Freshwater, standardize = "scale")

# Explore individual model fits
rsquared(SEM_Data_Freshwater_pSEM_randomList)
#OUTPUT:
1  NatRich gaussian identity   none 0.04475739  0.09564938
2   Native gaussian identity   none 0.19728425  0.24007564
3  NatComp gaussian identity   none 0.01220502  0.01220502