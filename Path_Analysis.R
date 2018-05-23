library("lavaan")
library("semPlot")
library("nlme")
library("lme4")
library("tidyverse")
library("vegan")

#"Freshwater" Data ========
VegAllEnvData <- read.csv("VegAllEnvData_03may2018.csv")
freshOnly <- VegAllEnvData[ VegAllEnvData$Community=="Freshwater",]
dim(freshOnly)#now: 603 465
freshOnly_Clean <- na.omit(freshOnly)#rows with NA-s that need removing
freshVeg_Cover<-subset(freshOnly_Clean, select = Acerrubr:ZiziMill)  #Freshwater veg cover data only

#Create a StationFront-level Freshwater dataset (average over years)
fresh.soil <- subset(freshOnly_Clean,
                     select = c(StationFront,Community,richness,Mean_SoilSalinity, Acerrubr:ZiziMill))

fresh.av <-fresh.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:ZiziMill),mean,na.rm=T) %>% na.omit()

#Subset Veg matrix with no Phragmites:
fresh.av.veg <- subset( fresh.av, select =  Acerrubr:ZiziMill)
fresh.av.veg.No.Phrag <- subset( fresh.av.veg, select = - Phraaust)

#Subset Environ factors:
fresh.av.env <-  subset( fresh.av, select = c(Phraaust, Mean_SoilSalinity,richness))
fresh.av.env$Phragmites <- ifelse(fresh.av.env$Phraaust == 0, "Absent","Present")

#Compute MDS:
MDS <- metaMDS(fresh.av.veg.No.Phrag, distance = "bray")#computing distances for Path analysis
MDS$stress * 100 # =  21.8%.
plot(MDS$points[,1:2])

coordinates<-as.data.frame(MDS$points[,1:2]) #get MDS1 (x-axis Comp value)
veg.nmds<-cbind(coordinates, fresh.av.env)
dim(veg.nmds)#41  6 = only 41 rows a soil pore water data (Mean_SoilSalinity) is relatively small

names(veg.nmds)#"MDS1","MDS2", "Phraaust","Mean_SoilSalinity","richness","Phragmites" 
veg.nmds$Rich <- scale (veg.nmds$richness)
veg.nmds$Salt <- scale (veg.nmds$Mean_SoilSalinity)
veg.nmds$Phra <- scale (veg.nmds$Phraaust)
veg.nmds$Comp<- scale (veg.nmds$MDS1)

#Standarize the values:
#Standardize (subtract mean and divide by sd) 
#all columns of your dataframe prior to path analysis, 
#then your coefficients will be standardized and you can compare their magnitudes as importance.

Fresh1 <- lm (Phra ~ Salt , data = veg.nmds)
summary(Fresh1)# P = 0.67
Fresh2 <- lm (Rich ~ Salt , data = veg.nmds)
summary(Fresh2)#P = 0.032 
Fresh3 <- lm (Rich ~ Phra, data = veg.nmds)
summary(Fresh3)#P = 0.554 
Fresh4 <- lm (Comp ~ Phra,  data = veg.nmds)
summary(Fresh4)#P = 0.999 
Fresh5 <- lm (Comp ~ Rich,  data = veg.nmds)
summary(Fresh5)#P < 0.001 

semPaths(Fresh1 + Fresh2 + Fresh3 + Fresh4 + Fresh5,
         "col", "est", intercepts = F,
         title = T, edge.label.cex = 1.9,layout = "tree2",
         sizeMan = 14, nCharNodes = 0)

title("Freshwater Path Analysis (CRMS, 2007-2017)", line = 3)

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
                     select = c(StationFront,Community,richness,Mean_SoilSalinity, Acerrubr:ZiziMill))

Brackish.av <-Brackish.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:ZiziMill),mean,na.rm=T) %>% na.omit()

#Subset Veg matrix with no Phragmites:
Brackish.av.veg <- subset( Brackish.av, select =  Acerrubr:ZiziMill)
Brackish.av.veg.No.Phrag <- subset( Brackish.av.veg, select = - Phraaust)

#Subset Environ factors:
Brackish.av.env <-  subset( Brackish.av, select = c(Phraaust, Mean_SoilSalinity,richness))
Brackish.av.env$Phragmites <- ifelse(Brackish.av.env$Phraaust == 0, "Absent","Present")

#Compute MDS:
MDS_Brackish <- metaMDS(Brackish.av.veg.No.Phrag, distance = "bray")#computing distances for Path analysis
MDS_Brackish$stress * 100 # =  21.57%.
plot(MDS_Brackish$points[,1:2])

coordinates_Brackish<-as.data.frame(MDS_Brackish$points[,1:2]) #get MDS1 (x-axis Comp value)
veg.nmds_Brackish<-cbind(coordinates_Brackish, Brackish.av.env)
dim(veg.nmds_Brackish)#40  6 = only 41 rows a soil pore water data (Mean_SoilSalinity) is relatively small

names(veg.nmds_Brackish)#"MDS1","MDS2", "Phraaust","Mean_SoilSalinity","richness","Phragmites" 
veg.nmds_Brackish$Rich <- scale (veg.nmds_Brackish$richness)
veg.nmds_Brackish$Salt <- scale (veg.nmds_Brackish$Mean_SoilSalinity)
veg.nmds_Brackish$Phra <- scale (veg.nmds_Brackish$Phraaust)
veg.nmds_Brackish$Comp<- scale (veg.nmds_Brackish$MDS1)

#Standarize the values:
#Standardize (subtract mean and divide by sd) 
#all columns of your dataframe prior to path analysis, 
#then your coefficients will be standardized and you can compare their magnitudes as importance.

Brackish1 <- lm (Phra ~ Salt , data = veg.nmds_Brackish)
summary(Brackish1)# P = 0.262
Brackish2 <- lm (Rich ~ Salt , data = veg.nmds_Brackish)
summary(Brackish2)#P = 0.0.0261 * 
Brackish3 <- lm (Rich ~ Phra, data = veg.nmds_Brackish)
summary(Brackish3)#P = 0.791 
Brackish4 <- lm (Comp ~ Phra,  data = veg.nmds_Brackish)
summary(Brackish4)#P = 0.885
Brackish5 <- lm (Comp ~ Rich,  data = veg.nmds_Brackish)
summary(Brackish5)#P < 0.001 

semPaths(Brackish1 + Brackish2 + Brackish3 + Brackish4 + Brackish5,
         "col", "est", intercepts = F,
         title = T, edge.label.cex = 1.9,layout = "tree2",
         sizeMan = 14, nCharNodes = 0)

title("Brackish Path Analysis (CRMS, 2007-2017)", line = 3)

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
                        select = c(StationFront,Community,richness,Mean_SoilSalinity, Acerrubr:ZiziMill))

Intermediate.av <-Intermediate.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:ZiziMill),mean,na.rm=T) %>% na.omit()

#Subset Veg matrix with no Phragmites:
Intermediate.av.veg <- subset( Intermediate.av, select =  Acerrubr:ZiziMill)
Intermediate.av.veg.No.Phrag <- subset( Intermediate.av.veg, select = - Phraaust)

#Subset Environ factors:
Intermediate.av.env <-  subset( Intermediate.av, select = c(Phraaust, Mean_SoilSalinity,richness))
Intermediate.av.env$Phragmites <- ifelse(Intermediate.av.env$Phraaust == 0, "Absent","Present")

#Compute MDS:
MDS_Intermediate <- metaMDS(Intermediate.av.veg.No.Phrag, distance = "bray")#computing distances for Path analysis
MDS_Intermediate$stress * 100 # =  20.41%.
plot(MDS_Intermediate$points[,1:2])

coordinates_Intermediate<-as.data.frame(MDS_Intermediate$points[,1:2]) #get MDS1 (x-axis Comp value)
veg.nmds_Intermediate<-cbind(coordinates_Intermediate, Intermediate.av.env)
dim(veg.nmds_Intermediate)#40  6 = only 41 rows a soil pore water data (Mean_SoilSalinity) is relatively small

names(veg.nmds_Intermediate)#"MDS1","MDS2", "Phraaust","Mean_SoilSalinity","richness","Phragmites" 
veg.nmds_Intermediate$Rich <- scale (veg.nmds_Intermediate$richness)
veg.nmds_Intermediate$Salt <- scale (veg.nmds_Intermediate$Mean_SoilSalinity)
veg.nmds_Intermediate$Phra <- scale (veg.nmds_Intermediate$Phraaust)
veg.nmds_Intermediate$Comp <- scale (veg.nmds_Intermediate$MDS1)

#Standarize the values:
#Standardize (subtract mean and divide by sd) 
#all columns of your dataframe prior to path analysis, 
#then your coefficients will be standardized and you can compare their magnitudes as importance.

Intermediate1 <- lm (Phra ~ Salt , data = veg.nmds_Intermediate)
summary(Intermediate1)# P = 0.415
Intermediate2 <- lm (Rich ~ Salt , data = veg.nmds_Intermediate)
summary(Intermediate2)# P < 0.001 
Intermediate3 <- lm (Rich ~ Phra, data = veg.nmds_Intermediate)
summary(Intermediate3)# P = 0.054  
Intermediate4 <- lm (Comp ~ Phra,  data = veg.nmds_Intermediate)
summary(Intermediate4)# P = 0.00924
Intermediate5 <- lm (Comp ~ Rich,  data = veg.nmds_Intermediate)
summary(Intermediate5)# P < 0.001 

semPaths(Intermediate1 + Intermediate2 + Intermediate3 + Intermediate4 + Intermediate5,
         "col", "est", intercepts = F,
         title = T, edge.label.cex = 1.9,layout = "tree2",
         sizeMan = 14, nCharNodes = 0)

title("Intermediate Path Analysis (CRMS, 2007-2017)", line = 3)

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
                        select = c(StationFront,Community,richness,Mean_SoilSalinity, Acerrubr:ZiziMill))

Saline.av <-Saline.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(richness:ZiziMill),mean,na.rm=T) %>% na.omit()

#Subset Veg matrix with no Phragmites:
Saline.av.veg <- subset( Saline.av, select =  Acerrubr:ZiziMill)
Saline.av.veg.No.Phrag <- subset( Saline.av.veg, select = - Phraaust)

#Subset Environ factors:
Saline.av.env <-  subset( Saline.av, select = c(Phraaust, Mean_SoilSalinity,richness))
Saline.av.env$Phragmites <- ifelse(Saline.av.env$Phraaust == 0, "Absent","Present")

#Compute MDS:
MDS_Saline <- metaMDS(Saline.av.veg.No.Phrag, distance = "bray")#computing distances for Path analysis
MDS_Saline$stress * 100 # =  16.08%.
plot(MDS_Saline$points[,1:2])

coordinates_Saline<-as.data.frame(MDS_Saline$points[,1:2]) #get MDS1 (x-axis Comp value)
veg.nmds_Saline<-cbind(coordinates_Saline, Saline.av.env)
dim(veg.nmds_Saline)#63  6 = only 41 rows a soil pore water data (Mean_SoilSalinity) is relatively small

names(veg.nmds_Saline)#"MDS1","MDS2", "Phraaust","Mean_SoilSalinity","richness","Phragmites" 
veg.nmds_Saline$Rich <- scale (veg.nmds_Saline$richness)
veg.nmds_Saline$Salt <- scale (veg.nmds_Saline$Mean_SoilSalinity)
veg.nmds_Saline$Phra <- scale (veg.nmds_Saline$Phraaust)
veg.nmds_Saline$Comp<- scale (veg.nmds_Saline$MDS1)

#Standarize the values:
#Standardize (subtract mean and divide by sd) 
#all columns of your dataframe prior to path analysis, 
#then your coefficients will be standardized and you can compare their magnitudes as importance.

Saline1 <- lm (Phra ~ Salt , data = veg.nmds_Saline)
summary(Saline1)# P = 0.744
Saline2 <- lm (Rich ~ Salt , data = veg.nmds_Saline)
summary(Saline2)#P = 0.00577 **
Saline3 <- lm (Rich ~ Phra, data = veg.nmds_Saline)
summary(Saline3)#P = 0.011 *
Saline4 <- lm (Comp ~ Phra,  data = veg.nmds_Saline)
summary(Saline4)#P = 0.0771 .
Saline5 <- lm (Comp ~ Rich,  data = veg.nmds_Saline)
summary(Saline5)#P < 0.001 

semPaths(Saline1 + Saline2 + Saline3 + Saline4 + Saline5,
         "col", "est", intercepts = F,
         title = T, edge.label.cex = 1.9,layout = "tree2",
         sizeMan = 14, nCharNodes = 0)

title("Saline Path Analysis (CRMS, 2007-2017)", line = 3)

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