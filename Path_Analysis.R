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
                                meanwaterdepthcm,floodedpercent, Acerrubr:ZiziMill))

fresh.av <-fresh.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:ZiziMill),mean,na.rm=T) %>% na.omit()

#Subset Veg matrix with no Phragmites:
fresh.av.veg <- subset( fresh.av, select =  Acerrubr:ZiziMill)
fresh.av.veg.No.Phrag <- subset( fresh.av.veg, select = - Phraaust)

#Subset Environ factors:
fresh.av.env <-  subset( fresh.av, select = c(Phraaust, Mean_SoilSalinity,meanwaterdepthcm,floodedpercent,richness))
fresh.av.env$Phragmites <- ifelse(fresh.av.env$Phraaust == 0, "Absent","Present")

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
Fresh2 <- lm (Comp~ Rich,    data = veg.nmds)
Fresh3 <- lm (Phra ~ Depth,  data = veg.nmds)
Fresh4 <- lm (Phra ~ Flood,  data = veg.nmds)
Fresh5 <- lm (Phra ~ Salt ,  data = veg.nmds)
Fresh6 <- lm (Comp ~ Phra,   data = veg.nmds)
Fresh7 <- lm (Rich~ Phra,    data = veg.nmds)
Fresh8 <- lm (Comp ~ Depth,  data = veg.nmds)
Fresh9 <- lm (Comp ~ Flood,  data = veg.nmds)
Fresh10<- lm (Comp ~ Salt ,  data = veg.nmds)

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
plot(lay)
#Set groups for coloring, if legend = TRUE it will be displayed on the right:
grps<-list(Abiotic=c("Depth","Salt","Flood"),Invasive_Phragmites=c("Phra"), Composition = c("Comp","Rich"))

#ALL IN:
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
plot(lay)
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

Intermediate.av <-Intermediate.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:ZiziMill),mean,na.rm=T) %>% na.omit()

#Subset Veg matrix with no Phragmites:
Intermediate.av.veg <- subset( Intermediate.av, select =  Acerrubr:ZiziMill)
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
plot(lay)
#Set groups for coloring, if legend = TRUE it will be displayed on the right:
grps<-list(Abiotic=c("Depth","Salt","Flood"),Invasive_Phragmites=c("Phra"), Composition = c("Comp","Rich"))

#ALL IN:
semPaths(Intermediate1 +Intermediate2 +Intermediate3+ Intermediate4 +Intermediate5 +Intermediate6+
           Intermediate7  +Intermediate8 + Intermediate9 + Intermediate10 ,
         "est", intercepts = F, fade = F,  edge.label.font = Bold_Intermediate_Sig,
         title = T, edge.label.cex = 1.4,layout = lay,
         color=c("lightblue","green","lightgreen"),groups=grps,
         sizeMan = 12, nCharNodes=0, asize = 5,legend=FALSE,
         edge.label.position = 0.3)
title("Intermediate (2007-2017)", line = 2)

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
semPaths(Saline1 +Saline2 +Saline3+ Saline4 +Saline5 +Saline6+
           Saline7  +Saline8 + Saline9 + Saline10 ,
         "est", intercepts = F, fade = F,  edge.label.font = Bold_Saline_Sig,
         title = T, edge.label.cex = 1.4,layout = lay,
         color=c("lightblue","green","lightgreen"),groups=grps,
         sizeMan = 12, nCharNodes=0, asize = 5,legend=FALSE,
         edge.label.position = 0.3)
title("Saline (2007-2017)", line = 2)

#Vegan MDS in GGPLOT:
ggplot(data = veg.nmds_Saline, aes(MDS1, MDS2,color = Phragmites)) + geom_point(size=4) +
  ggtitle("NMDS of Saline Communities",subtitle = "averaged across 10 years")






#Practice Path analysis ##########
#Source: http://lavaan.ugent.be/tutorial/index.html
library("lavaan")
library("semPlot")

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
