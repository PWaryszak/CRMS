library("lavaan")
library("semPlot")
library("tidyverse")
library("vegan")
#See 04_MERGE_VegHydroSoil for details on how "VegAllEnvData_03may2018.csv" was produced.

#"Freshwater" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_03may2018.csv")
freshOnly <- VegAllEnvData[ VegAllEnvData$Community=="Freshwater",]
dim(freshOnly)#now: 603 465
freshOnly_Clean <- na.omit(freshOnly)#rows with NA-s that need removing
freshVeg_Cover<-subset(freshOnly_Clean, select = Acerrubr:ZiziMill)  #Freshwater veg cover data only

#Create a StationFront-level Freshwater dataset (average over years)
fresh.soil <- subset(freshOnly_Clean,
                     select = c(StationFront,Community,richness,Mean_SoilSalinity,
                                meanwaterdepthcm,floodedpercent,
                                MeanWaterSalinity, Acerrubr:ZiziMill))

fresh.av <-fresh.soil %>% #na.omit() %>%
  group_by(StationFront,Community)%>% #over all years
  summarise_at(vars(richness:ZiziMill),mean,na.rm=T)# 

dim(fresh.av)
#Subset Veg matrix with no Phragmites:
fresh.av.veg <- subset( fresh.av, select =  Acerrubr:ZiziMill)
fresh.av.veg.No.Phrag <- subset( fresh.av.veg, select = - Phraaust)# 41 448
fresh.av.veg.No.Phrag2 <- fresh.av.veg.No.Phrag [ , colSums(fresh.av.veg.No.Phrag) > 0] #remove zero columns prior PCA, 41 x 268 now
#Subset Environ factors:
fresh.av.env <-  subset( fresh.av, select = c(Phraaust, Mean_SoilSalinity,meanwaterdepthcm,floodedpercent,richness))
fresh.av.env$Phragmites <- ifelse(fresh.av.env$Phraaust == 0, "Absent","Present")

#Compute PCA x - values of the most dominant:
#WEB: https://rpubs.com/njvijay/27823
fresh.pca <- prcomp(fresh.av.veg.No.Phrag2,center = TRUE,scale. = TRUE)
names(fresh.pca)#"sdev","rotation", "center","scale","x" 
summary(fresh.pca)
biplot(fresh.pca,scale=0, cex=.7)

#Freshwater dominant plant species:======
colCount = colSums(fresh.av.veg.No.Phrag2)
topID = order(colCount,decreasing=TRUE)[1] # 
topID = names(fresh.av.veg.No.Phrag2[topID]) # 
topID #"Panihemi"

colCount = colSums(fresh.av.veg.No.Phrag2)
topID = order(colCount,decreasing=TRUE)[1:268] # 
topID = names(fresh.av.veg.No.Phrag2[topID]) # 
topID #"Panihemi"
Freshwater_Plants <- data.frame( specCode = topID)
Plant_List <- read.csv("Louisiana_InvasivePlantList.csv")
str(Plant_List)#data.frame':	745 obs. of  11 variables:
#join Freshwater_Plants & Plant_List to see which species is invasive:
Plant_List_Inv <- subset(Plant_List, select = c(specCode, nat))
Freshwater_Plants_Inv <- left_join(Freshwater_Plants,Plant_List_Inv, by = "specCode")
head(Freshwater_Plants_Inv)
##specCode      nat
# 1 Panihemi     <NA>
# 2 Sagilanc     <NA>
# 3 Polypunc     <NA>
# 4 Leerhexa     <NA>
# 5 Altephil invasive = First invasive (Alternanthera philoxeroides, aligator weed)
# 6 Typhlati     <NA>



coordinateX <- as.data.frame(fresh.pca$rotation["Panihemi",1:41]) #get MDS1 (x-axis Comp value)
coordinateX #-0.02192527
as.data.frame(fresh.pca$x["Panihemi",1:41])




#As coordinateX is negative we need to flip the pca chart to positive:
pca.out <- fresh.pca
pca.out$rotation <- -pca.out$rotation
pca.out$x <- -pca.out$x
biplot(pca.out,scale=0, cex=.7)

#Finds dominant plant species's positive x-coordinate
#Compute MDS:
MDS <- metaMDS(fresh.av.veg.No.Phrag, distance = "bray")#computing distances for Path analysis
MDS$stress * 100 # =  21.8%.
plot(MDS$points[,1:2])

coordinates<-as.data.frame(MDS$points[,1:2]) #get MDS1 (x-axis Comp value)
veg.nmds<-cbind(coordinates, fresh.av.env)
dim(veg.nmds)#41  8 = only 41 rows a soil pore water data (Mean_SoilSalinity) is relatively small

names(veg.nmds)#"MDS1","MDS2", "Phraaust","Mean_SoilSalinity","meanwaterdepthcm","floodedpercent","richness","Phragmites" 
#Standarize the variables so their effect size are comparable:
veg.nmds$Rich <- scale (veg.nmds$richness)
veg.nmds$Salt <- scale (veg.nmds$Mean_SoilSalinity)
veg.nmds$Phra <- scale (veg.nmds$Phraaust)
veg.nmds$Comp <- scale (veg.nmds$MDS1)
veg.nmds$Flood<- scale (veg.nmds$floodedpercent)
veg.nmds$Depth<- scale (veg.nmds$meanwaterdepthcm)

#Standarize the values:
#Standardize (subtract mean and divide by sd) 
#all columns of your dataframe prior to path analysis, 
#then your coefficients will be standardized and you can compare their magnitudes as importance.
Fresh1 <- lm (Rich ~ Comp,   data = veg.nmds)
Fresh2 <- lm (Comp ~ Rich,    data = veg.nmds)
Fresh3 <- lm (Phra ~ Depth,  data = veg.nmds)
Fresh4 <- lm (Phra ~ Flood,  data = veg.nmds)
Fresh5 <- lm (Phra ~ Salt ,  data = veg.nmds)
Fresh6 <- lm (Comp ~ Phra,   data = veg.nmds)
Fresh7 <- lm (Rich ~ Phra,    data = veg.nmds)
Fresh8 <- lm (Comp ~ Depth,  data = veg.nmds)
Fresh9 <- lm (Comp ~ Flood,  data = veg.nmds)
Fresh10<- lm (Comp ~ Salt ,  data = veg.nmds)

#Check residuals:
par(mfrow = c(2,5))
plot(Fresh1, which = 1, main = "Fresh1 = lm (Rich ~ Comp)" )
plot(Fresh2, which = 1, main = "Fresh2 = lm (Comp ~ Rich)" )
plot(Fresh3, which = 1, main = "Fresh3 = lm (Phra ~ Depth)" )
plot(Fresh4, which = 1, main = "Fresh4 = lm (Phra ~ Flood)" )
plot(Fresh5, which = 1, main = "Fresh5 = lm (Phra ~ Salt)" )
plot(Fresh6, which = 1, main = "Fresh6 = lm (Comp ~ Phra)" )
plot(Fresh7, which = 1, main = "Fresh7 = lm (Rich ~ Phra)" )
plot(Fresh8, which = 1, main = "Fresh8 = lm (Comp ~ Depth)" )
plot(Fresh9, which = 1, main = "Fresh9 = lm (Comp ~ Flood)" )
plot(Fresh10, which =1, main = "Fresh10 = lm (Comp ~ Salt)" )


FreshSigData <- data.frame(Pvalue = c(summary(Fresh1)$coefficients[2,4],
                                      summary(Fresh2)$coefficients[2,4],
                                      summary(Fresh3)$coefficients[2,4],
                                      summary(Fresh4)$coefficients[2,4],
                                      summary(Fresh5)$coefficients[2,4],
                                      summary(Fresh6)$coefficients[2,4],
                                      summary(Fresh7)$coefficients[2,4],
                                      summary(Fresh8)$coefficients[2,4],
                                      summary(Fresh9)$coefficients[2,4],
                                      summary(Fresh10)$coefficients[2,4]))

                              
FreshSigData 
Bold_Fresh_Sig <- as.integer(ifelse(FreshSigData$Pvalue < 0.05 ,2,1))#Set bold(2) if P< 0.05, otherwise = 1
Bold_Fresh_Sig #values #for edge.label.font, 11 models to define which effects are significant (2=bold):

#ly is a pre-designed layout for our SemPath Diagram boxes:
lay<-matrix(c(-0.5,  -0.5,
              0.5,  -0.5,
             -0.5,   0.5,
                0,  -0.3,
                0,   0.5,
              0.5,   0.5), ncol=2,byrow=TRUE)
#Set groups for coloring, if legend = TRUE it will be displayed on the right:
grps<-list(Abiotic=c("Depth","Salt","Flood"),Invasive_Phragmites=c("Phra"), Composition = c("Comp","Rich"))

#ALL IN:
par(mfrow = c(1,1))
semPaths(Fresh1 +Fresh2 +Fresh3+ Fresh4 +Fresh5 +Fresh6+
           Fresh7  +Fresh8 + Fresh9 + Fresh10 ,
         "est", intercepts = F, fade = F,  edge.label.font = Bold_Fresh_Sig,
         title = T, edge.label.cex = 1.4,layout = lay,
         color=c("lightblue","green","lightgreen"),groups=grps,
         sizeMan = 12, nCharNodes=0, asize = 5,legend=FALSE,
         edge.label.position = 0.3)
title("Freshwater (2007-2017)", line = 2)


#Vegan MDS in GGPLOT:
ggplot(data = veg.nmds, aes(MDS1, MDS2,color = Phragmites)) + geom_point(size=4) +
  ggtitle("NMDS of Freshwater Communities" ,subtitle = "averaged across 10 years")



#"Brackish" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_03may2018.csv")
BrackishOnly <- VegAllEnvData[ VegAllEnvData$Community=="Brackish",]
dim(BrackishOnly)#now: 608 465
BrackishOnly_Clean <- na.omit(BrackishOnly)#rows with NA-s that need removing
BrackishVeg_Cover<-subset(BrackishOnly_Clean, select = Acerrubr:ZiziMill)  #Brackish veg cover data only

#Create a StationFront-level Brackish dataset (average over years)
Brackish.soil <- subset(BrackishOnly_Clean,
                     select = c(StationFront,Community,richness,
                                meanwaterdepthcm,floodedpercent,Mean_SoilSalinity, Acerrubr:ZiziMill))

#compute mean cover per site (station);
Brackish.av <-Brackish.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:ZiziMill),mean,na.rm=T) %>% na.omit()

#Subset Veg matrix with no Phragmites:
Brackish.av.veg <- subset( Brackish.av, select =  Acerrubr:ZiziMill)
Brackish.av.veg.No.Phrag <- subset( Brackish.av.veg, select = - Phraaust)

#Brackish dominant plant species:======
colCount_Brackish = colSums(Brackish.av.veg)
topID_Brackish = order(colCount_Brackish, decreasing=TRUE)[1:449] # 
topID_Brackish = names( Brackish.av.veg [topID_Brackish]) # 
topID_Brackish

Brackish_Plants <- data.frame( specCode = topID_Brackish)
Plant_List <- read.csv("Louisiana_InvasivePlantList.csv")#list of invasive species in LA

#join Brackish_Plants & Plant_List to see which species is invasive:
Plant_List_Inv <- subset(Plant_List, select = c(specCode, nat))
Brackish_Plants_Inv <- left_join(Brackish_Plants,Plant_List_Inv, by = "specCode")
head(Brackish_Plants_Inv, n=12)
#  #specCode      nat
#  1  Sparpate     <NA>
#  2  Distspic     <NA>
#  3  Sparalte     <NA>
#  4  Bolbrobu     <NA>
#  5  Schoamer     <NA>
#  6  Juncroem     <NA>
#  7  Lythline     <NA>
#  8  Vignlute     <NA>
#  9  Sparcyno     <NA>
#  10 Typhlati     <NA>
#  11  Ivafrut     <NA>
#  12 Phraaust invasive = Dominant invasive (Phragmites australis)

#Subset Environ factors:
Brackish.av.env <-  subset( Brackish.av, select = c(Phraaust,meanwaterdepthcm,floodedpercent, Mean_SoilSalinity,richness))
Brackish.av.env$Phragmites <- ifelse(Brackish.av.env$Phraaust == 0, "Absent","Present")

#Compute MDS:
MDS_Brackish <- metaMDS(Brackish.av.veg.No.Phrag, distance = "bray")#computing distances for Path analysis
MDS_Brackish$stress * 100 # =  21.57%.
plot(MDS_Brackish$points[,1:2])

coordinates_Brackish<-as.data.frame(MDS_Brackish$points[,1:2]) #get MDS1 (x-axis Comp value)
veg.nmds_Brackish<-cbind(coordinates_Brackish, Brackish.av.env)
dim(veg.nmds_Brackish)#40  6 = only 41 rows a soil pore water data (Mean_SoilSalinity) is relatively small

#Standardize all variables using scale function:
names(veg.nmds_Brackish)#"MDS1","MDS2", "Phraaust","Mean_SoilSalinity","richness","Phragmites" 
veg.nmds_Brackish$Rich <- scale (veg.nmds_Brackish$richness)
veg.nmds_Brackish$Salt <- scale (veg.nmds_Brackish$Mean_SoilSalinity)
veg.nmds_Brackish$Phra <- scale (veg.nmds_Brackish$Phraaust)
veg.nmds_Brackish$Comp <- scale (veg.nmds_Brackish$MDS1)
veg.nmds_Brackish$Flood<- scale (veg.nmds_Brackish$floodedpercen)
veg.nmds_Brackish$Depth<- scale (veg.nmds_Brackish$meanwaterdepthcm)


#Compute coefficients for path analysis diagram:
Brackish1 <- lm (Rich ~ Comp,   data = veg.nmds_Brackish)
Brackish2 <- lm (Comp~ Rich,    data = veg.nmds_Brackish)
Brackish3 <- lm (Phra ~ Depth,  data = veg.nmds_Brackish)
Brackish4 <- lm (Phra ~ Flood,  data = veg.nmds_Brackish)
Brackish5 <- lm (Phra ~ Salt ,  data = veg.nmds_Brackish)
Brackish6 <- lm (Comp ~ Phra,   data = veg.nmds_Brackish)
Brackish7 <- lm (Rich~ Phra,    data = veg.nmds_Brackish)
Brackish8 <- lm (Comp ~ Depth,  data = veg.nmds_Brackish)
Brackish9 <- lm (Comp ~ Flood,  data = veg.nmds_Brackish)
Brackish10<- lm (Comp ~ Salt ,  data = veg.nmds_Brackish)

BrackishSigData <- data.frame(Pvalue = c(summary(Brackish1)$coefficients[2,4],
                                      summary(Brackish2)$coefficients[2,4],
                                      summary(Brackish3)$coefficients[2,4],
                                      summary(Brackish4)$coefficients[2,4],
                                      summary(Brackish5)$coefficients[2,4],
                                      summary(Brackish6)$coefficients[2,4],
                                      summary(Brackish7)$coefficients[2,4],
                                      summary(Brackish8)$coefficients[2,4],
                                      summary(Brackish9)$coefficients[2,4],
                                      summary(Brackish10)$coefficients[2,4]))


BrackishSigData 
Bold_Brackish_Sig <- as.integer(ifelse(BrackishSigData$Pvalue < 0.05 ,2,1))#Set bold(2) if P< 0.05, otherwise = 1
Bold_Brackish_Sig #values #for edge.label.font, 11 models to define which effects are significant (2=bold):

#ly is a pre-designed layout for our SemPath Diagram boxes:
lay<-matrix(c(-0.5,  -0.5,
              0.5,  -0.5,
              -0.5,   0.5,
              0,  -0.3,
              0,   0.5,
              0.5,   0.5), ncol=2,byrow=TRUE)
#Set groups for coloring, if legend = TRUE it will be displayed on the right:
grps<-list(Abiotic=c("Depth","Salt","Flood"),Invasive_Phragmites=c("Phra"), Composition = c("Comp","Rich"))

#ALL IN:
semPaths(Brackish1 +Brackish2 +Brackish3+ Brackish4 +Brackish5 +Brackish6+
           Brackish7  +Brackish8 + Brackish9 + Brackish10 ,
         "est", intercepts = F, fade = F,  edge.label.font = Bold_Brackish_Sig,
         title = T, edge.label.cex = 1.4,layout = lay,
         color=c("lightblue","green","lightgreen"),groups=grps,
         sizeMan = 12, nCharNodes=0, asize = 5,legend=FALSE,
         edge.label.position = 0.3)
title("Brackish (2007-2017)", line = 2)

#Check the distribution of residuals:
par(mfrow = c(2,5))
plot(Brackish1, which = 1, main = "Brackish1 = lm (Rich ~ Comp)" )
plot(Brackish2, which = 1, main = "Brackish2 = lm (Comp ~ Rich)" )
plot(Brackish3, which = 1, main = "Brackish3 = lm (Phra ~ Depth)" )
plot(Brackish4, which = 1, main = "Brackish4 = lm (Phra ~ Flood)" )
plot(Brackish5, which = 1, main = "Brackish5 = lm (Phra ~ Salt)" )
plot(Brackish6, which = 1, main = "Brackish6 = lm (Comp ~ Phra)" )
plot(Brackish7, which = 1, main = "Brackish7 = lm (Rich ~ Phra)" )
plot(Brackish8, which = 1, main = "Brackish8 = lm (Comp ~ Depth)" )
plot(Brackish9, which = 1, main = "Brackish9 = lm (Comp ~ Flood)" )
plot(Brackish10, which = 1, main = "Brackish10 = lm (Comp ~ Salt)" )

#Vegan MDS in GGPLOT:
ggplot(data = veg.nmds_Brackish, aes(MDS1, MDS2,color = Phragmites)) + geom_point(size=4) +
  ggtitle("NMDS of Brackish Communities",subtitle = "averaged across 10 years")





#"Intermediate" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_03may2018.csv")
IntermediateOnly <- VegAllEnvData[ VegAllEnvData$Community=="Intermediate",]
dim(IntermediateOnly)#now: 978 465
IntermediateOnly_Clean <- na.omit(IntermediateOnly)#rows with NA-s that need removing
IntermediateVeg_Cover<-subset(IntermediateOnly_Clean, select = Acerrubr:ZiziMill)  #Intermediate veg cover data only

#Create a StationFront-level Intermediate dataset (average over years)
Intermediate.soil <- subset(IntermediateOnly_Clean,
                        select = c(StationFront,Community,richness,Mean_SoilSalinity,
                                   meanwaterdepthcm,floodedpercent,Acerrubr:ZiziMill))

Intermediate.av <-Intermediate.soil %>% #na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:ZiziMill),mean,na.rm=T)# %>% na.omit()

#Subset Veg matrix with no Phragmites:
Intermediate.av.veg <- subset(Intermediate.av, select =  Acerrubr:ZiziMill)
names(Intermediate.av.veg)#59 449
Intermediate.av.veg.No.Phrag <- subset( Intermediate.av.veg, select = - Phraaust)

#Intermediate dominant plant species:======
colCount_Intermediate = colSums(Intermediate.av.veg)
topID_Intermediate = order(colCount_Intermediate, decreasing=TRUE)[1:449] # 
topID_Intermediate = names( Intermediate.av.veg [topID_Intermediate]) # 
topID_Intermediate

Intermediate_Plants <- data.frame( specCode = topID_Intermediate)
Plant_List <- read.csv("Louisiana_InvasivePlantList.csv")#list of invasive species in LA

#join Intermediate_Plants & Plant_List to see which species is invasive:
Plant_List_Inv <- subset(Plant_List, select = c(specCode, nat))
Intermediate_Plants_Inv <- left_join(Intermediate_Plants,Plant_List_Inv, by = "specCode")
head(Intermediate_Plants_Inv)
#specCode      nat
# Sparpate     <NA>
# 2 Phraaust invasive #= Dominant invasive (Phragmites australis)
# 3 Schoamer     <NA>
# 4 Vignlute     <NA>
# 5 Polypunc     <NA>
# 6 Paspvagi     <NA>


#Subset Environ factors:
Intermediate.av.env <-  subset( Intermediate.av, select = c(Phraaust, Mean_SoilSalinity,
                                                            meanwaterdepthcm,floodedpercent,richness))
Intermediate.av.env$Phragmites <- ifelse(Intermediate.av.env$Phraaust == 0, "Absent","Present")

#Compute MDS:
MDS_Intermediate <- metaMDS(Intermediate.av.veg.No.Phrag, distance = "bray")#computing distances for Path analysis
MDS_Intermediate$stress * 100 # =  20.41%.
plot(MDS_Intermediate$points[,1:2])

coordinates_Intermediate<-as.data.frame(MDS_Intermediate$points[,1:2]) #get MDS1 (x-axis Comp value)
veg.nmds_Intermediate<-cbind(coordinates_Intermediate, Intermediate.av.env)
dim(veg.nmds_Intermediate)#59  rows of soil pore water data (Mean_SoilSalinity) is relatively small

names(veg.nmds_Intermediate)#"MDS1","MDS2", "Phraaust","Mean_SoilSalinity","richness","Phragmites" 
veg.nmds_Intermediate$Rich <- scale (veg.nmds_Intermediate$richness)
veg.nmds_Intermediate$Salt <- scale (veg.nmds_Intermediate$Mean_SoilSalinity)
veg.nmds_Intermediate$Phra <- scale (veg.nmds_Intermediate$Phraaust)
veg.nmds_Intermediate$Comp <- scale (veg.nmds_Intermediate$MDS1)
veg.nmds_Intermediate$Flood<- scale (veg.nmds_Intermediate$floodedpercent)
veg.nmds_Intermediate$Depth<- scale (veg.nmds_Intermediate$meanwaterdepthcm)


#Compute coefficients for path analysis diagram:
Intermediate1 <- lm (Rich ~ Comp,   data = veg.nmds_Intermediate)
Intermediate2 <- lm (Comp~ Rich,    data = veg.nmds_Intermediate)
Intermediate3 <- lm (Phra ~ Depth,  data = veg.nmds_Intermediate)
Intermediate4 <- lm (Phra ~ Flood,  data = veg.nmds_Intermediate)
Intermediate5 <- lm (Phra ~ Salt ,  data = veg.nmds_Intermediate)
Intermediate6 <- lm (Comp ~ Phra,   data = veg.nmds_Intermediate)
Intermediate7 <- lm (Rich~ Phra,    data = veg.nmds_Intermediate)
Intermediate8 <- lm (Comp ~ Depth,  data = veg.nmds_Intermediate)
Intermediate9 <- lm (Comp ~ Flood,  data = veg.nmds_Intermediate)
Intermediate10<- lm (Comp ~ Salt ,  data = veg.nmds_Intermediate)

IntermediateSigData <- data.frame(Pvalue = c(summary(Intermediate1)$coefficients[2,4],
                                         summary(Intermediate2)$coefficients[2,4],
                                         summary(Intermediate3)$coefficients[2,4],
                                         summary(Intermediate4)$coefficients[2,4],
                                         summary(Intermediate5)$coefficients[2,4],
                                         summary(Intermediate6)$coefficients[2,4],
                                         summary(Intermediate7)$coefficients[2,4],
                                         summary(Intermediate8)$coefficients[2,4],
                                         summary(Intermediate9)$coefficients[2,4],
                                         summary(Intermediate10)$coefficients[2,4]))


IntermediateSigData 
Bold_Intermediate_Sig <- as.integer(ifelse(IntermediateSigData$Pvalue < 0.05 ,2,1))#Set bold(2) if P< 0.05, otherwise = 1
Bold_Intermediate_Sig #values #for edge.label.font, 11 models to define which effects are significant (2=bold):

#ly is a pre-designed layout for our SemPath Diagram boxes:
lay<-matrix(c(-0.5,  -0.5,
              0.5,  -0.5,
              -0.5,   0.5,
              0,  -0.3,
              0,   0.5,
              0.5,   0.5), ncol=2,byrow=TRUE)
#Set groups for coloring, if legend = TRUE it will be displayed on the right:
grps<-list(Abiotic=c("Depth","Salt","Flood"),Invasive_Phragmites=c("Phra"), Composition = c("Comp","Rich"))

#ALL IN:
par(mfrow = c(1,1))
semPaths(Intermediate1 +Intermediate2 +Intermediate3+ Intermediate4 +Intermediate5 +Intermediate6+
           Intermediate7  +Intermediate8 + Intermediate9 + Intermediate10 ,
         "est", intercepts = F, fade = F,  edge.label.font = Bold_Intermediate_Sig,
         title = T, edge.label.cex = 1.4,layout = lay,
         color=c("lightblue","green","lightgreen"),groups=grps,
         sizeMan = 12, nCharNodes=0, asize = 5,legend=FALSE,
         edge.label.position = 0.3)
title("Intermediate (2007-2017)", line = 2)

#Check the distribution of residuals:
par(mfrow = c(2,5))
plot(Intermediate1, which = 1, main = "Intermediate1 = lm (Rich ~ Comp)" )
plot(Intermediate2, which = 1, main = "Intermediate2 = lm (Comp ~ Rich)" )
plot(Intermediate3, which = 1, main = "Intermediate3 = lm (Phra ~ Depth)" )
plot(Intermediate4, which = 1, main = "Intermediate4 = lm (Phra ~ Flood)" )
plot(Intermediate5, which = 1, main = "Intermediate5 = lm (Phra ~ Salt)" )
plot(Intermediate6, which = 1, main = "Intermediate6 = lm (Comp ~ Phra)" )
plot(Intermediate7, which = 1, main = "Intermediate7 = lm (Rich ~ Phra)" )
plot(Intermediate8, which = 1, main = "Intermediate8 = lm (Comp ~ Depth)" )
plot(Intermediate9, which = 1, main = "Intermediate9 = lm (Comp ~ Flood)" )
plot(Intermediate10, which = 1, main = "Intermediate10 = lm (Comp ~ Salt)" )

#Vegan MDS in GGPLOT:
ggplot(data = veg.nmds_Intermediate, aes(MDS1, MDS2,color = Phragmites)) + geom_point(size=4) +
  ggtitle("NMDS of Intermediate Communities",subtitle = "averaged across 10 years")



#"Saline" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_03may2018.csv")
SalineOnly <- VegAllEnvData[ VegAllEnvData$Community=="Saline",]
dim(SalineOnly)#now: 660 465
SalineOnly_Clean <- na.omit(SalineOnly)#rows with NA-s that need removing
SalineVeg_Cover<-subset(SalineOnly_Clean, select = Acerrubr:ZiziMill)  #Saline veg cover data only

#Create a StationFront-level Saline dataset (average over years)
Saline.soil <- subset(SalineOnly_Clean,
                        select = c(StationFront,Community,richness,Mean_SoilSalinity,
                                   meanwaterdepthcm,floodedpercent,Acerrubr:ZiziMill))

Saline.av <-Saline.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:ZiziMill),mean,na.rm=T) %>% na.omit()

#Subset Veg matrix with no Phragmites:
Saline.av.veg <- subset( Saline.av, select =  Acerrubr:ZiziMill)
Saline.av.veg.No.Phrag <- subset( Saline.av.veg, select = - Phraaust)

#Saline dominant plant species:======
colCount_Saline = colSums(Saline.av.veg)
topID_Saline = order(colCount_Saline, decreasing=TRUE)[1:50] # 
topID_Saline = names( Saline.av.veg [topID_Saline]) # 
topID_Saline

Saline_Plants <- data.frame( specCode = topID_Saline)
Plant_List <- read.csv("LA_Plants.csv")#list of invasive species in LA

#join Saline_Plants & Plant_List to see which species is invasive:
Plant_List_Inv <- subset(Plant_List, select = c(specCode, nat))
Saline_Plants_Inv <- left_join(Saline_Plants,Plant_List_Inv, by = "specCode")
head(Saline_Plants_Inv, n=17)
#  #  specCode      nat 
#  1  Sparalte     <NA>
#  2  Juncroem     <NA>
#  3  Sparpate     <NA>
#  4  Distspic     <NA>
#  5  Avicgerm     <NA>
#  6  Bolbrobu     <NA>
#  7  Salidepr     <NA>
#  8  Sparcyno     <NA>
#  9   Ivafrut     <NA>
#  10 Batimari     <NA>
#  11 Symptenu     <NA>
#  12 Schoamer     <NA>
#  13 Amaraust     <NA>
#  14 Sesuport     <NA>
#  15 Blutverm     <NA>
#  16 Paspvagi     <NA>
#  17 Panirepe   invasive #most dominant invasive = Panicum repens L.


#Subset Environ factors:
Saline.av.env <-  subset( Saline.av, select = c(Phraaust, Mean_SoilSalinity,
                                                meanwaterdepthcm,floodedpercent,richness))
Saline.av.env$Phragmites <- ifelse(Saline.av.env$Phraaust == 0, "Absent","Present")

#Compute MDS:
MDS_Saline <- metaMDS(Saline.av.veg.No.Phrag, distance = "bray")#computing distances for Path analysis
MDS_Saline$stress * 100 # =  16.08%.
plot(MDS_Saline$points[,1:2])

coordinates_Saline<-as.data.frame(MDS_Saline$points[,1:2]) #get MDS1 (x-axis Comp value)
veg.nmds_Saline<-cbind(coordinates_Saline, Saline.av.env)
dim(veg.nmds_Saline)#63  6 = only 41 rows a soil pore water data (Mean_SoilSalinity) is relatively small

names(veg.nmds_Saline)#"MDS1","MDS2", "Phraaust","Mean_SoilSalinity","richness","Phragmites" 
#Standardize all variables using scale function:
veg.nmds_Saline$Rich <- scale (veg.nmds_Saline$richness)
veg.nmds_Saline$Salt <- scale (veg.nmds_Saline$Mean_SoilSalinity)
veg.nmds_Saline$Phra <- scale (veg.nmds_Saline$Phraaust)
veg.nmds_Saline$Comp <- scale (veg.nmds_Saline$MDS1)
veg.nmds_Saline$Flood<- scale (veg.nmds_Saline$floodedpercen)
veg.nmds_Saline$Depth<- scale (veg.nmds_Saline$meanwaterdepthcm)


#Compute coefficients for path analysis diagram:
Saline1 <- lm (Rich ~ Comp,   data = veg.nmds_Saline)
Saline2 <- lm (Comp~ Rich,    data = veg.nmds_Saline)
Saline3 <- lm (Phra ~ Depth,  data = veg.nmds_Saline)
Saline4 <- lm (Phra ~ Flood,  data = veg.nmds_Saline)
Saline5 <- lm (Phra ~ Salt ,  data = veg.nmds_Saline)
Saline6 <- lm (Comp ~ Phra,   data = veg.nmds_Saline)
Saline7 <- lm (Rich~ Phra,    data = veg.nmds_Saline)
Saline8 <- lm (Comp ~ Depth,  data = veg.nmds_Saline)
Saline9 <- lm (Comp ~ Flood,  data = veg.nmds_Saline)
Saline10<- lm (Comp ~ Salt ,  data = veg.nmds_Saline)

SalineSigData <- data.frame(Pvalue = c(summary(Saline1)$coefficients[2,4],
                                         summary(Saline2)$coefficients[2,4],
                                         summary(Saline3)$coefficients[2,4],
                                         summary(Saline4)$coefficients[2,4],
                                         summary(Saline5)$coefficients[2,4],
                                         summary(Saline6)$coefficients[2,4],
                                         summary(Saline7)$coefficients[2,4],
                                         summary(Saline8)$coefficients[2,4],
                                         summary(Saline9)$coefficients[2,4],
                                         summary(Saline10)$coefficients[2,4]))


SalineSigData 
Bold_Saline_Sig <- as.integer(ifelse(SalineSigData$Pvalue < 0.05 ,2,1))#Set bold(2) if P< 0.05, otherwise = 1
Bold_Saline_Sig #values #for edge.label.font, 11 models to define which effects are significant (2=bold):

#ly is a pre-designed layout for our SemPath Diagram boxes:
lay<-matrix(c(-0.5,  -0.5,
              0.5,  -0.5,
              -0.5,   0.5,
              0,  -0.3,
              0,   0.5,
              0.5,   0.5), ncol=2,byrow=TRUE)
plot(lay)
#Set groups for coloring, if legend = TRUE it will be displayed on the right:
grps<-list(Abiotic=c("Depth","Salt","Flood"),Invasive_Phragmites=c("Phra"), Composition = c("Comp","Rich"))

#ALL IN:
par(mfrow = c(1,1))
semPaths(Saline1 +Saline2 +Saline3+ Saline4 +Saline5 +Saline6+
           Saline7  +Saline8 + Saline9 + Saline10 ,
         "est", intercepts = F, fade = F,  edge.label.font = Bold_Saline_Sig,
         title = T, edge.label.cex = 1.4,layout = lay,
         color=c("lightblue","green","lightgreen"),groups=grps,
         sizeMan = 12, nCharNodes=0, asize = 5,legend=FALSE,
         edge.label.position = 0.3)
title("Saline (2007-2017)", line = 2)


#Check the distribution of residuals:
par(mfrow = c(2,5))
plot(Saline1, which = 1, main = "Saline1 = lm (Rich ~ Comp)" )
plot(Saline2, which = 1, main = "Saline2 = lm (Comp ~ Rich)" )
plot(Saline3, which = 1, main = "Saline3 = lm (Phra ~ Depth)" )
plot(Saline4, which = 1, main = "Saline4 = lm (Phra ~ Flood)" )
plot(Saline5, which = 1, main = "Saline5 = lm (Phra ~ Salt)" )
plot(Saline6, which = 1, main = "Saline6 = lm (Comp ~ Phra)" )
plot(Saline7, which = 1, main = "Saline7 = lm (Rich ~ Phra)" )
plot(Saline8, which = 1, main = "Saline8 = lm (Comp ~ Depth)" )
plot(Saline9, which = 1, main = "Saline9 = lm (Comp ~ Flood)" )
plot(Saline10, which = 1, main = "Saline10 = lm (Comp ~ Salt)" )

#Vegan MDS in GGPLOT:
ggplot(data = veg.nmds_Saline, aes(MDS1, MDS2,color = Phragmites)) + geom_point(size=4) +
  ggtitle("NMDS of Saline Communities",subtitle = "averaged across 10 years")






#Source: http://lavaan.ugent.be/tutorial/index.html
library("lavaan")
library("semPlot")

#SEM FRESHWATER=======
  model1 <- '
#regressions
Phra ~ Depth + Salt + Flood
Rich ~ Phra + Depth + Salt + Flood
Comp ~ Phra + Depth + Salt + Flood

#covariances
Rich~~Comp
'
#Removing non-significant terms:
model1a <- '
#regressions
Rich ~  Depth  + Flood
Comp ~  Depth  + Salt + Flood

#covariances
Rich~~Comp
'

fit1a <- sem(model1a,missing="direct",estimator="ML",data=veg.nmds)
summary(fit1a, fit.measures=TRUE,rsquare=T) 

par(mfrow = c(1,1))
semPaths(fit1a,
         "est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=0,
         residuals =  F)
title("Freshwater SEM (2007-2017), All p-values < 0.05", line = 2)


#SEM BRACKSIH=========
fit2 <- sem(model2,missing="direct",estimator="ML",data=veg.nmds)
summary(fit1, fit.measures=TRUE,rsquare=T) 

par(mfrow = c(1,1))
semPaths(fit2,
         "est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=0,
         residuals =  F)
title("Freshwater SEM (2007-2017)", line = 2)


model2a <- '
#regressions
Phra ~ Depth + Salt + Flood
Rich ~ Phra + Depth + Salt + Flood
Comp ~ Phra + Depth + Salt + Flood

#covariances
Rich~~Comp
'

fit2a <- sem(model2a,missing="direct",estimator="ML",data= veg.nmds_Brackish)
summary(fit2, fit.measures=TRUE,rsquare=T) 

par(mfrow = c(1,1))
semPaths(fit2,
         "est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=0,
         residuals =  F)
title("Brackish SEM (2007-2017)", line = 2)


#SEM INTERMEDIATE========
model3 <- '
#regressions
Phra ~ Depth + Salt + Flood
Rich ~ Phra + Depth + Salt + Flood
Comp ~ Phra + Depth + Salt + Flood

#covariances
Rich~~Comp
'

fit3 <- sem(model3,missing="direct",estimator="ML",data= veg.nmds_Intermediate)
summary(fit3, fit.measures=TRUE,rsquare=T) 

par(mfrow = c(1,1))
semPaths(fit3,
         "est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=0,
         residuals =  F)
title("Intermediate SEM (2007-2017)", line = 2)

#Removing non-significant variabels:
model3a <- '
#regressions
Rich ~ Phra  + Salt 
Comp ~ Phra  + Salt

#covariances
Rich~~Comp
'

fit3a <- sem(model3a,missing="direct",estimator="ML",data= veg.nmds_Intermediate)
summary(fit3a, fit.measures=TRUE,rsquare=T) 

par(mfrow = c(1,1))
semPaths(fit3a,
         "est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=0,
         residuals =  F)
title("Intermediate SEM (2007-2017), All p-values < 0.05", line = 2)

#to check:
#covariances - sometimes lavaan automatically adds covariances, make sure there aren't any listed that you don't want. if there are unwanted covariances add for example Plant_Dens~~0*X16_PD_whole_tree to the covariances section of the model above. Right now I'm leaving all the covariances in, b/c otherwise the model does not have good fit.
#check that your degrees of freedom under "Estmiator ML" is not 0, if it is you have specified, for example, all arrows between all boxes and you don't have enough information to test significance of the model. Drop some terms to solve this problem.

#to report: 
#under "estimator ML" (not "model test baseline model") report the "Minimum Function test statistic" and df and p value - this p value should be non-significant!! that means that observed and expected covariance matrices are not different)
#RMSEA value should be <0.05

#Delete explanatory variables that aren't significant. I'm just doing this in bulk here to make sure it works. You should so this one by one and assess overall model fit by AIC (when you delete a variable make sure AIC decreases, stop deleting variables when AIC stops decreasing). Variables that are non significant (<0.05) should be retained in the model, but not shown on the path diagram.



#Practice Path analysis ##########
model <- '
#regressions
Plant_Shannon ~ snow
Plant_Dens ~ snow
X16_PD_whole_tree ~ snow + Plant_Shannon
X.C ~  Plant_Dens 
X.N ~ Plant_Dens
NAG ~  Plant_Dens
BG ~ Plant_Dens + X16_PD_whole_tree
PHOS ~ Plant_Dens 
#covariances
Plant_Shannon~~Plant_Dens
#X.C ~~ 0*NAG+0*BG+0*PHOS
#X.N ~~ 0*NAG+0*BG+0*PHOS
'
fit2 <- sem(model,missing="direct",estimator="ML",data=datstand)
summary(fit2, fit.measures=TRUE,rsquare=T) 
# A silly dataset:
A <- rnorm(100)
B <- A + rnorm(100)
C <- B + rnorm(100)
DF <- data.frame(A, B, C)
# Two regressions:
res1 <- lm(B ~ C, data = DF)
summary(res1)
res2 <- lm(A ~ B + C, data = DF)
# Plot both in the same path diagram in two ways:
semPaths(res1 + res2, "model", "est", intercepts = F)
semPaths(list(res1, res2), "model", "est", intercepts = FALSE)
#More examples on the web: http://sachaepskamp.com/semPlot/examples

