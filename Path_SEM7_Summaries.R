#Data +libraries + Apriori Model ========
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")#Our Veg Data. Proccessed as per CRMS file 1,2,3,4 in Github
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
dim(VegAllEnvData) # 3498  473
#Install packages if not done so yet:
#Load packages if installed on our comp:
library("lavaan")
library("OpenMx")
library("semPlot")
library("tidyverse")
library("vegan")
library("gridExtra")
library("grid")

#METADATA:
#All variables were abbreviated and scaled (so their effect size are comparable): 
#See details here: https://github.com/PWaryszak/CRMS/blob/master/Path_SEM6.R
#These are used in following SEM :
#NatRich     <- scale (SEM_Data$Native_Richness)
#Soil        <- scale (SEM_Data$Mean_SoilSalinity)
#Water       <- scale (SEM_Data$MeanWaterSalinity)
#Alien       <- scale (SEM_Data$Alien_Cover)
#Native      <- scale (SEM_Data$Native_Cover)
#NatComp     <- scale (SEM_Data$Native_Composition)
#Flood       <- scale (SEM_Data$floodedpercent)
#Depth       <- scale (SEM_Data$meanwaterdepthcm)

#APRIORI MODEL======
#This is our Apriori Path Analysis Model (for latter backward selection)
#Tested against CRMS data (Floodpercent removed as srongly correlated with water depth):

Apriori_Model <-'
#regressions:
NatRich     ~ Depth   + Soil + Alien  
Native      ~ Depth   + Soil + Alien
Alien       ~ Depth   + Soil 
NatComp     ~ Depth   + Soil + Alien + NatRich + Native

#covariances:
NatRich ~~ Native
NatComp ~~ NatRich
Native  ~~  NatComp
'
fit_Apriori_Model <- sem(Apriori_Model,missing="direct",estimator="ML",data=data)
summary(fit_Apriori_Model)

#Freshwater SEM:=========
#Load data directly from previosly saved "Freshwater_Data4SEM.csv" (See Path_SEM4 for details)
Freshwater_Data <- read.csv("Freshwater_Data4SEM_PCoa.csv")
#Best fit model follwoing backward selection on Apriori Model (see line 15):


model_Freshwater <- '
#regressions:
NatCov      ~  Soil + Alien
NatComp     ~  Soil + Alien
NatRich     ~    Soil

#covariances:
NatComp ~~ 0*NatRich
NatCov  ~~ 0*NatComp
#NatCov  ~~ 0*NatRich #Turned on for plotting.
'

fit_Freshwater <- sem(model_Freshwater,missing="direct",estimator="ML",data=Freshwater_Data)
summary(fit_Freshwater, fit.measures=TRUE, rsquare=T) 

#Emily's plot with new layout
#order: NatCov, NatComp,Depth, Soil, Alien
x1 = c(-1, 0,  1, -1, 1)
y1 = c(-1,-1, -1, 1, 1)
ly1 = matrix(c(x1, y1), ncol=2)
semPaths(fit_Freshwater ,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,edge.labels=FALSE,
         edge.label.position = 0.2, nCharNodes=6,
         residuals =  F, exoCov = F, 
         edge.label.bg = "lightyellow",
         legend = F, layout=ly1)# ,edge.color = c(1,1,1,1,1,1,2,2,2) #I can't really figure this out, I would import the figure into illustrator or something to change arrow color so that arrows pointing to or from native comp are black so that they don't impart a good/bad connotation to the reviewer
title("Fresh", line =2)

summary(fit_Freshwater, fit.measures=TRUE, rsquare=T) 

#Intermediate SEM ========
#You can load data directly from previosly saved "Intermediate_Data4SEM.csv" (See Path_SEM4 for details)
Intermediate_Data <- read.csv("Intermediate_Data4SEM_PCoA.csv")
#Pick Best fit model following backward selection on Apriori Model (see line 15):

model_Intermediate2 <- '
#regressions:
NatRich     ~  Soil  + Alien
NatCov      ~  Soil  + Alien
Alien       ~  Soil
NatComp     ~  Soil  

#covariances:
#NatComp ~~ 0* NatRich #Turned on for plotting
#NatCov ~~ 0* NatComp  #Turned on for plotting
#NatCov ~~ 0* NatRich  #Turned on for plotting
'

fit_Intermediate2 <- sem(model_Intermediate2,missing="direct",estimator="ML",data=Intermediate_Data)
summary(fit_Intermediate2)

x2 = c( 1, -1, 1, 0, -1)
y2 = c(-1,-1, 1, -1, 1)
ly2 = matrix(c(x2, y2), ncol=2)

semPaths(fit_Intermediate2,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,
         edge.label.position = 0.15, nCharNodes=6,
         residuals =  F, exoCov = F,edge.label.bg = "lightyellow",
         legend = F, layout = ly2)
title("Intermediate ")

summary(fit_Intermediate2, fit.measures=TRUE, rsquare=T) 

#Brackish SEM=========
#All terms significant, backward selection from full model (see "Path_SEM3" R file, line ~200)
#Best fit model follwoing backward selection on Apriori Model (see line 15):
Brackish_Data <- read.csv("Brackish_Data4SEM_PCoA.csv")

x2 = c( 1, -1, 0, 0, -1)
y2 = c(-1,-1, 1, -1, 1)
ly2 = matrix(c(x2, y2), ncol=2)

model_Brackish3 <- '
#regressions:
NatRich     ~  Depth  + Soil
NatCov      ~  Depth
#NatComp     ~    Soil

#covariances:
#NatComp ~~ 0*NatRich
#NatCov ~~ 0*NatComp
NatRich ~~ 0*NatCov
'

fit_Brackish3 <- sem(model_Brackish3,missing="direct",estimator="ML",data=Brackish_Data)
summary(fit_Brackish3)

#Plot Brackish SEM:
x3 = c( -1,  1,1, -1)
y3 = c( -1, -1,1,  1)
ly3 = matrix(c(x3, y3), ncol=2)

semPaths(fit_Brackish3,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,
         edge.label.position = 0.15, nCharNodes=6,
         residuals =  F, exoCov = F, edge.label.bg = "lightyellow",
         legend = F, layout = ly3)
title("Brackish")

#Saline SEM===========
#You can load data directly from previosly saved "Saline_Data4SEM.csv"
Saline_Data <- read.csv("Saline_Data4SEM_PCoA.csv")
#emily trying this, i don't know why you treated this differently
#than the others, having natrich and NatCov affct NatCov comp, 
#also I took alien out since there are only like 2 plots with aliens in them??
#The RMSEA is 0.08 which isn't awesome, and the model includes a nonsig parameter. 
#I would probalby include nonsig parameters in order to make the RMSEA better
#(you can also use AIC to justify including a non sig parameter),
#just don't include them in the path diagrams.

model_Saline <- '
#regressions:
NatRich     ~    Soil
NatCov      ~  Depth +Soil
NatComp ~ Soil

#covariances:
#NatComp ~~ 0*NatRich #Turned on for plotting
#NatCov ~~ 0*NatComp  #Turned on for plotting
#NatRich ~~ 0*NatCov  #Turned on for plotting
'

fit_Saline <- sem(model_Saline,missing="direct",estimator="ML",data=Saline_Data)
summary(fit_Saline,fit.measures=T,rsquare=T)

x4 = c( 1, 0, -1, -1, 1)
y4 = c(-1,-1, -1, 1, 1)
ly4 = matrix(c(x4, y4), ncol=2)

semPaths(fit_Saline,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,
         edge.label.position = 0.25, nCharNodes=6,
         residuals =  F, exoCov = F,edge.label.bg = "lightyellow",
         legend = F, layout = ly4)
title("Saline")


#Bind all Figures together=========
layout.matrix <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2)

layout(mat = layout.matrix,
       heights = c(2, 2), # Heights of the two rows
       widths = c(2, 2)) # Widths of the two columns
#Plot1 fresh:
x1 = c(-1, 0,  1, -1, 1)
y1 = c(-1,-1, -1, 1, 1)
ly1 = matrix(c(x1, y1), ncol=2)
semPaths(fit_Freshwater ,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,edge.labels=FALSE,
         edge.label.position = 0.2, nCharNodes=6,
         residuals =  F, exoCov = F, 
         edge.label.bg = "lightyellow",
         legend = F, layout=ly1)# ,edge.color = c(1,1,1,1,1,1,2,2,2) #I can't really figure this out, I would import the figure into illustrator or something to change arrow color so that arrows pointing to or from native comp are black so that they don't impart a good/bad connotation to the reviewer
title("Fresh", line =2)

#Plot2 Intermediate:
x2 = c( 1, -1, 1, 0, -1)
y2 = c(-1,-1, 1, -1, 1)
ly2 = matrix(c(x2, y2), ncol=2)

semPaths(fit_Intermediate2,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,
         edge.label.position = 0.15, nCharNodes=6,
         residuals =  F, exoCov = F,edge.label.bg = "lightyellow",
         legend = F, layout = ly2)
title("Intermediate ")

#Plot3 Brackish:
x3 = c( -1,  1,1, -1)
y3 = c( -1, -1,1,  1)
ly3 = matrix(c(x3, y3), ncol=2)

semPaths(fit_Brackish3,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,
         edge.label.position = 0.15, nCharNodes=6,
         residuals =  F, exoCov = F, edge.label.bg = "lightyellow",
         legend = F, layout = ly3)
title("Brackish")

#Plot4
x4 = c( 1, 0, -1, -1, 1)
y4 = c(-1,-1, -1, 1, 1)
ly4 = matrix(c(x4, y4), ncol=2)

semPaths(fit_Saline,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,
         edge.label.position = 0.25, nCharNodes=6,
         residuals =  F, exoCov = F,edge.label.bg = "lightyellow",
         legend = F, layout = ly4)
title("Saline")
