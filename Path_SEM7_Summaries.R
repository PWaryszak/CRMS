#Data +libraries + Apriori Model ========
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")#Our Veg Data. Proccessed as per CRMS file 1,2,3,4 in Github
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
dim(VegAllEnvData) # 3498  473
#Install packages if not done so yet:
install.packages("lavaan")
install.packages("semPlot")
install.packages("tidyverse")
install.packages("vegan")
install.packages("gridExtra")
install.packages("grid")

#Load packages if installed on our comp:
library("lavaan")
library("semPlot")
library("tidyverse")
library("vegan")
library("gridExtra")
library("grid")

#APRIORI MODEL======
#This is our Apriori Path Analysis Model (for latter backward selection)
#Tested agaisnt CRMS data:
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

#Freshwater SEM:=========
#Load data directly from previosly saved "Freshwater_Data4SEM.csv" (See Path_SEM4 for details)
Freshwater_Data <- read.csv("Freshwater_Data4SEM.csv")
#Best fit model follwoing backward selection on Apriori Model (see line 15):
model_Freshwater <- '
#regressions:
NatRich     ~ Depth + Flood     
Native      ~ Depth + Flood  + Soil + Alien
NatComp     ~ Soil + Alien 

#covariances:
NatComp ~~ 0*NatRich
Native  ~~ 0*NatComp
'
fit_Freshwater <- sem(model_Freshwater,missing="direct",estimator="ML",data=Freshwater_Data)
summary(fit_Freshwater, fit.measures=TRUE, rsquare=T) 

#Produce SemPaths FIGURE with Nodes:
semPaths(fit_Freshwater ,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,edge.labels=FALSE,
         edge.label.position = 0.2, nCharNodes=6,
         residuals =  F, exoCov = F, 
         edge.label.bg = "lightyellow",
          legend = F)
title("Freshwater path analysis (2007-2017, P < 0.05)", line =2)



#"Intermediate" Data ========

#Intermediate SEM ========
#You can load data directly from previosly saved "Intermediate_Data4SEM.csv" (See Path_SEM4 for details)
Intermediate_Data <- read.csv("Intermediate_Data4SEM.csv")
#Pick Best fit model following backward selection on Apriori Model (see line 15):
model_Intermediate2 <- '
#regressions:
NatRich     ~  Soil 
Native      ~  Soil + Alien
Alien       ~  Soil 
NatComp     ~  Soil + NatRich 

#covariances:
NatComp ~~ NatRich
Native ~~ NatComp
Native ~~ NatRich
'
fit_Intermediate2 <- sem(model_Intermediate2,missing="direct",estimator="ML",data=Intermediate_Data)
summary(fit_Intermediate2, fit.measures=TRUE, rsquare=T) 


semPaths(fit_Intermediate2,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,
         edge.label.position = 0.25, nCharNodes=6,
         residuals =  F, exoCov = F,edge.label.bg = "lightyellow",
         legend = F)
title("Intermediate  path analysis (2007-2017, P<0.05)")




#write.csv(Brackish_Data, file = "Brackishr_Data4SEM.csv")
#All terms significant, backward selection from full model (see "Path_SEM3" R file, line ~200)
#You can load data directly from previosly saved "Brackish_Data4SEM.csv"
#Brackish_Data <- read.csv("Brackish_Data4SEM.csv")

#Brackish SEM=========
#Best fit model follwoing backward selection on Apriori Model (see line 15):
Brackish_Data <- read.csv("Brackish_Data4SEM.csv")
model_Brackish3 <- '
#regressions:
NatRich     ~  Depth  + Soil
Native      ~  Depth 
NatComp     ~  Soil

#covariances:
NatComp ~~ 0*NatRich
Native ~~ 0*NatComp
NatRich ~~ 0*Native
'
fit_Brackish3 <- sem(model_Brackish3,missing="direct",estimator="ML",data=Brackish_Data)
summary(fit_Brackish3, fit.measures=TRUE, rsquare=T) 

#Plot Brackish SEM:
semPaths(fit_Brackish3,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.3,sizeMan = 12,
         edge.label.position = 0.25, nCharNodes=6,
         legend = F)
title("Brackish path analysis (2007-2017, P<0.05)")


#Saline SEM===========
#You can load data directly from previosly saved "Saline_Data4SEM.csv"
Saline_Data <- read.csv("Saline_Data4SEM.csv")
#Compare models to find best fit: (Best fit model follwoing backward selection on Apriori Model (see line 15))
model_Saline <- '
#regressions:
#regressions:
NatRich     ~ Depth + Soil 
Native      ~ Depth + Soil 
Alien       ~ Depth + Soil 
NatComp     ~  NatRich + Native

#covariances:
NatComp ~~ 0*NatRich
Native ~~ 0*NatComp
'
fit_Saline <- sem(model_Saline,missing="direct",estimator="ML",data=Saline_Data)
summary(fit_Saline, fit.measures=TRUE, rsquare=T)

semPaths(fit_Saline,"est", intercepts = F, fade = F, 
         title = T, edge.label.cex = 1.1,sizeMan = 8,
         edge.label.position = 0.25, nCharNodes=6,
         residuals =  F, exoCov = F)


