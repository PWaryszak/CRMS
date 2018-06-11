#LOAD LIBRARIES & DATA:
#11june2018 - we give up top weed and look at the impact of all weeds:
#New Veg  data = VegAllEnvData_11june2018
VegAllEnvData <- read.csv("VegAllEnvData_11june2018.csv")

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
freshOnly <- VegAllEnvData[ VegAllEnvData$Community=="Freshwater",]
dim(freshOnly)#now: 603 470
freshOnly_Clean <- na.omit(freshOnly)#rows with NA-s that need removing
FreshwaterVeg_Cover<-subset(freshOnly_Clean, select = Acer_rubrum:Ziza_miliacea)  #Freshwater veg cover data only
dim(FreshwaterVeg_Cover)# 336 453

#ID and remove the most dominant weed from veg matrix:
colCount = colSums(FreshwaterVeg_Cover) #sum up the abundance column-wise
topID = order(colCount,decreasing=TRUE)[1:length(FreshwaterVeg_Cover)] # choose all Freshwater plant species
topID = names(FreshwaterVeg_Cover[topID]) # names of plant species in decreasing order
Freshwater_Plant_Sp <- data.frame( specCode = topID)
Freshwater_Plant_Sp
Plant_List <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018
str(Plant_List)#data.frame':	3454 obs. of  6 variables:

#join Freshwater_Plants & Plant_List to see which species are invasive:
Plant_List<- subset(Plant_List, select = c(specCode, nat))
Freshwater_Plants <- left_join(Freshwater_Plant_Sp,Plant_List, by = "specCode")
head(Freshwater_Plants)# most abundant weed is Altephil 

#Create a StationFront-level Freshwater dataset (average over years)
fresh.soil <- subset(freshOnly_Clean,
                     select = c(StationFront,Community,richness,Mean_SoilSalinity,
                                meanwaterdepthcm,floodedpercent,
                                MeanWaterSalinity, Acer_rubrum:Ziza_miliacea))

#Compute mean covers per Station, all years:
fresh.av <-fresh.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:Ziza_miliacea),mean,na.rm=T)# %>% na.omit()
dim(fresh.av)# 41 460

#Subset Native Veg matrix (No introduced plants):
Freshwater_Native.Species <- filter(Freshwater_Plants, nat == "native")
Freshwater_Native.Species #346
#Select only native species from fresh.av:
fresh.av.veg.native <- subset( fresh.av, select = unique(Freshwater_Native.Species$specCode))
dim(fresh.av.veg.native)#41 346
range (colSums(fresh.av.veg.native))#0.00000 98.65938
#Zero colums need to be removed prior ordinations: 
fresh.av.veg.native.good<- fresh.av.veg.native [ , colSums(fresh.av.veg.native) > 0]
dim(fresh.av.veg.native.good)#  41 216

#Compute total cover of introduced species per site:
Freshwater_introduced.Species <- filter(Freshwater_Plants, nat == "introduced")
Freshwater_introduced.Species #346
fresh.av.veg.introduced <- subset( fresh.av, select = unique(Freshwater_introduced.Species$specCode))
dim(fresh.av.veg.introduced)#41 43
fresh.av.veg.introduced.total <- rowSums (fresh.av.veg.introduced)
range(fresh.av.veg.introduced.total)#0.000000 7.167448

#Subset Environ factors:
fresh.av.env <-  subset( fresh.av, select = c( Mean_SoilSalinity,meanwaterdepthcm,
                                               floodedpercent,richness))
fresh.av.env$Introduced_Species <- fresh.av.veg.introduced.total


#Compute Freshwater PCoA x - values of the most dominant native plant "Panihemi":======
#  WEB >>>  https://www.davidzeleny.net/anadat-r/doku.php/en:pcoa_examples
#use a PCoA  (principal coordinates analysis) rather than a PCA (Principal components analysis). With PCoA you can
#use bray curtis dissimilarity (rather than only euclidean distance which is what PCA uses)
veg.D <- vegdist(fresh.av.veg.native.good, "bray")
fresh.pca <- cmdscale(veg.D , eig = TRUE)
names(fresh.pca)#"points" "eig"    "x"      "ac"     "GOF" 
str(fresh.pca)
ordiplot(fresh.pca, display = 'sites', type = 'points',
         cex = 2,bg="yellow")


#Draw Ordination points:
#Combine MDS PC and env data together:
coordinates<-as.data.frame(fresh.pca$points[,1:2]) #get MDS1 (x-axis Comp value)
veg.nmds<-cbind(coordinates, fresh.av.env)
dim(veg.nmds)#only 41 7
names(veg.nmds)#""V1"    "V2"   "Mean_SoilSalinity"  "meanwaterdepthcm"   "floodedpercent" "richness",Introduced_Species
     
#Standarize the variables so their effect size are comparable:
veg.nmds$Rich <- scale (veg.nmds$richness)
veg.nmds$Salt <- scale (veg.nmds$Mean_SoilSalinity)
veg.nmds$Weed <- scale (veg.nmds$Introduced_Species)
veg.nmds$Comp <- scale (veg.nmds$V1)
veg.nmds$Flood<- scale (veg.nmds$floodedpercent)
veg.nmds$Depth<- scale (veg.nmds$meanwaterdepthcm)

#Vegan MDS in GGPLOT to see where weeds are present:
ggplot(data = veg.nmds, aes(V1,V2,size = Introduced_Species)) + geom_point() +
       ggtitle("PCoA of Freshwater Communities",subtitle = "averaged across 10 years, TopWeed = altephil") +
       xlab("X coordinate of PCoA")+ylab("Y coordinate of PCoA")+
       theme(legend.position = "bottom")
     
#SEM FRESHWATER=======
     model1 <- '
     #regressions
     
     Rich ~ Depth + Flood
     NatX ~  Depth + Flood 
     
     #covariances
     Rich ~~ Comp
     NatX ~~ Rich
     '
     fit1 <- sem(model1,missing="direct",estimator="ML",data=veg.nmds)
     summary(fit1, fit.measures=TRUE, rsquare=T) 
     
     par(mfrow = c(1,1))
     semPaths(fit1,
              "est", intercepts = F, fade = F, 
              title = T, edge.label.cex = 1.1,sizeMan = 8,
              edge.label.position = 0.25, nCharNodes=0,
              residuals =  F, exoCov = F)
     title("Freshwater SEM (2007-2017), All p-values < 0.05", line = 2)
     
     
     
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
     
     #Old Freshwater Path Analysis====
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
     
     #PCA Biplot Freshwater====
     #As coordinateX is negative we can flip the pca chart to positive:
     pca.out <- fresh.pca
     pca.out$rotation <- -pca.out$rotation
     pca.out$x <- -pca.out$x
     biplot(pca.out,scale=0, cex=.6)
     coordinateX2 <- as.data.frame(pca.out$rotation["Panihemi",1:41]) #get MDS1 (x-axis Comp value)
     coord2 <- cbind (coordinateX,coordinateX2)
     coord2 #after flipping positive values = abs(negative value)
     
     PC1=fresh.pca$rotation["Panihemi",1]
     PC2=fresh.pca$rotation["Panihemi",2]
     coordPeniHemi <- cbind(PC1,PC2)
     coordPeniHemi #0.02192527 -0.100972
     par(mfrow = c(1,2))
     biplot(fresh.pca,scale=0, cex=.6, main = "Freshwater PCA")
     plot(0,0, xlim = c(-0.5,0.5), ylim = c(-0.5,0.5),main = "Panihemi", col="red" )
     arrows(0,0, coordPeniHemi[1,1], coordPeniHemi[1,2])
     
     #"Brackish" Data ========
     VegAllEnvData <- read.csv("VegAllEnvData_11june2018.csv")
     BrackishOnly <- VegAllEnvData[ VegAllEnvData$Community=="Brackish",]
     dim(BrackishOnly)#now: 608 465
     BrackishOnly_Clean <- na.omit(BrackishOnly)#rows with NA-s that need removing
     BrackishVeg_Cover<-subset(BrackishOnly_Clean, select = Acer_rubrum:Ziza_miliacea)  #Brackish veg cover data only
     
     #Create a StationFront-level Brackish dataset (average over years)
     Brackish.soil <- subset(BrackishOnly_Clean,
                             select = c(StationFront,Community,richness,
                                        meanwaterdepthcm,floodedpercent,Mean_SoilSalinity, Acer_rubrum:Ziza_miliacea))
     
     #compute mean cover per site (station);
     Brackish.av <-Brackish.soil %>% na.omit() %>%
       group_by(StationFront,Community)%>%
       summarise_at(vars(richness:Ziza_miliacea),mean,na.rm=T) %>% na.omit()
     
     #Subset Veg matrix with no Phragmites:
     Brackish.av.veg <- subset( Brackish.av, select =  Acer_rubrum:Ziza_miliacea)
     Brackish.av.veg.No.Phrag <- subset( Brackish.av.veg, select = - Phraaust)
     
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
     
     #Old Freshwater Path Analysis====
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
     
     
     #SEM BRACKSIH=========
     
     model2 <- '
     #regressions
     Phra ~ Depth + Salt + Flood
     Rich ~ Phra + Depth + Salt + Flood
     Comp ~ Phra + Depth + Salt + Flood
     
     #covariances
     Rich~~Comp
     '
     
     fit2 <- sem(model2,missing="direct",estimator="ML",data=veg.nmds_Brackish)
     summary(fit2, fit.measures=TRUE,rsquare=T) 
     
     par(mfrow = c(1,1))
     semPaths(fit2,
              "est", intercepts = F, fade = F, 
              title = T, edge.label.cex = 1.1,sizeMan = 8,
              edge.label.position = 0.25, nCharNodes=0,
              residuals =  F)
     title("Brackish SEM (2007-2017)", line = 2)
     
     
     model2a <- '
     #regressions
     Rich ~  Depth + Salt 
     Comp ~  Depth + Salt 
     
     #covariances
     Rich~~Comp
     '
     
     fit2a <- sem(model2a,missing="direct",estimator="ML",data= veg.nmds_Brackish)
     summary(fit2a, fit.measures=TRUE,rsquare=T) 
     
     par(mfrow = c(1,1))
     semPaths(fit2a,
              "est", intercepts = F, fade = F, 
              title = T, edge.label.cex = 1.1,sizeMan = 8,
              edge.label.position = 0.25, nCharNodes=0,
              residuals =  F)
     title("Brackish SEM (2007-2017), All p-values < 0.05", line = 2)
     
     #"Intermediate" Data ========
     VegAllEnvData <- read.csv("VegAllEnvData_11june2018.csv")
     IntermediateOnly <- VegAllEnvData[ VegAllEnvData$Community=="Intermediate",]
     dim(IntermediateOnly)#now: 978 465
     IntermediateOnly_Clean <- na.omit(IntermediateOnly)#rows with NA-s that need removing
     IntermediateVeg_Cover<-subset(IntermediateOnly_Clean, select = Acer_rubrum:Ziza_miliacea)  #Intermediate veg cover data only
     
     #Create a StationFront-level Intermediate dataset (average over years)
     Intermediate.soil <- subset(IntermediateOnly_Clean,
                                 select = c(StationFront,Community,richness,Mean_SoilSalinity,
                                            meanwaterdepthcm,floodedpercent,Acer_rubrum:Ziza_miliacea))
     
     Intermediate.av <-Intermediate.soil %>% na.omit() %>%
       group_by(StationFront,Community)%>%
       summarise_at(vars(richness:Ziza_miliacea),mean,na.rm=T) %>% na.omit()
     
     #Subset Veg matrix with no Phragmites:
     Intermediate.av.veg <- subset( Intermediate.av, select =  Acer_rubrum:Ziza_miliacea)
     Intermediate.av.veg.No.Phrag <- subset( Intermediate.av.veg, select = - Phraaust)
     
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
     
     #SEM Intermediate=========
     model3 <- '
     #regressions
     Phra ~ Depth + Salt + Flood
     Rich ~ Phra + Depth + Salt + Flood
     Comp ~ Phra + Depth + Salt + Flood
     
     #covariances
     Rich~~Comp
     '
     
     fit3 <- sem(model3,missing="direct",estimator="ML",data=veg.nmds_Intermediate)
     summary(fit3, fit.measures=TRUE,rsquare=T) 
     
     par(mfrow = c(1,1))
     semPaths(fit3,
              "est", intercepts = F, fade = F, 
              title = T, edge.label.cex = 1.1,sizeMan = 8,
              edge.label.position = 0.25, nCharNodes=0,
              residuals =  F)
     title("Intermediate SEM (2007-2017)", line = 2)
     
     
     model3a <- '
     #regressions
     Rich ~ Phra  + Salt 
     Comp ~ Phra + Salt 
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
     
     
     
     #"Saline" Data ========
     VegAllEnvData <- read.csv("VegAllEnvData_11june2018.csv")
     SalineOnly <- VegAllEnvData[ VegAllEnvData$Community=="Saline",]
     dim(SalineOnly)#now: 660 465
     SalineOnly_Clean <- na.omit(SalineOnly)#rows with NA-s that need removing
     SalineVeg_Cover<-subset(SalineOnly_Clean, select = Acer_rubrum:Ziza_miliacea)  #Saline veg cover data only
     
     #Create a StationFront-level Saline dataset (average over years)
     Saline.soil <- subset(SalineOnly_Clean,
                           select = c(StationFront,Community,richness,Mean_SoilSalinity,
                                      meanwaterdepthcm,floodedpercent,Acer_rubrum:Ziza_miliacea))
     
     Saline.av <-Saline.soil %>% na.omit() %>%
       group_by(StationFront,Community)%>%
       summarise_at(vars(richness:Ziza_miliacea),mean,na.rm=T) %>% na.omit()
     
     #Subset Veg matrix with no Phragmites:
     Saline.av.veg <- subset( Saline.av, select =  Acer_rubrum:Ziza_miliacea)
     Saline.av.veg.No.Phrag <- subset( Saline.av.veg, select = - Phraaust)
     
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
     
     
     #SEM Saline=========
     model4 <- '
     #regressions
     Phra ~ Depth + Salt + Flood
     Rich ~ Phra + Depth + Salt + Flood
     Comp ~ Phra + Depth + Salt + Flood
     
     #covariances
     Rich~~Comp
     '
     
     fit4 <- sem(model4,missing="direct",estimator="ML",data=veg.nmds_Saline)
     summary(fit4, fit.measures=TRUE,rsquare=T) 
     
     par(mfrow = c(1,1))
     semPaths(fit4,
              "est", intercepts = F, fade = F, 
              title = T, edge.label.cex = 1.1,sizeMan = 8,
              edge.label.position = 0.25, nCharNodes=0,
              residuals =  F)
     title("Saline SEM (2007-2017)", line = 2)
     
     
     model4a <- '
     #regressions
     Rich ~  Depth + Salt 
     Comp ~   Salt 
     
     #covariances
     Rich~~Comp
     '
     
     fit4a <- sem(model4a, missing="direct",estimator="ML",data= veg.nmds_Saline)
     summary(fit4a, fit.measures=TRUE,rsquare=T) 
     
     par(mfrow = c(1,1))
     semPaths(fit4a,
              "est", intercepts = F, fade = F, 
              title = T, edge.label.cex = 1.1,sizeMan = 8,
              edge.label.position = 0.25, nCharNodes=0,
              residuals =  F)
     title("Saline SEM (2007-2017), All p-values < 0.05", line = 2)
     
     
     
     
     
     
     