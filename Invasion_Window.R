VegEnvData <- read.csv("VegEnvDataNew2018.csv")
library(tidyverse)
library(vegan)
library(scatterplot3d) 

#Phragmites + Spartina 3D Invasion Window visualization:
PS <- VegEnvData[ , c("Community","floodedpercent","Phraaust" ,"Sparalte", "MeanSalinity")]

par(mfrow=c(1,2))
Phrag <- PS[PS$Phraaust > 0, ]
attach(Phrag) 
scatterplot3d(MeanSalinity,floodedpercent,Phraaust,
              pch=15, highlight.3d=TRUE, type= "p",
              main="Invasion Window: Phragmites") 

Spart <- PS[PS$Sparalte > 0, ]
attach(Spart) 
scatterplot3d(MeanSalinity,floodedpercent,Sparalte,
              pch=15, highlight.3d=TRUE, type= "p",
              main="Invasion Window:Spartina") 

#Spartina==========
Spartina <- VegEnvData[ VegEnvData$Sparalte > 0 , c("Community","floodedpercent","Sparalte")]
summary(Spartina)
sum(VegEnvData$Sparalte == 0)#1570 with no Spartina
sum(VegEnvData$Sparalte > 0) #840 with spartina = 54%

library(Rmisc)
flooded <- summarySE(data = Spartina, measurevar = "floodedpercent", groupvars = "Community")
flooded
qplot(Community, floodedpercent, data = flooded, geom = c("boxplot"))

#####Community   N floodedpercent        sd          se         ci
1     Brackish 223      0.4524025 0.2503541 0.016764949 0.03303881
2   Freshwater   9      0.5565832 0.2250190 0.075006346 0.17296494
3 Intermediate  60      0.5078588 0.2530630 0.032670299 0.06537312
4       Saline 548      0.4804940 0.2153011 0.009197208 0.01806617

SpartinaCover <- summarySE(data = Spartina, measurevar = "Sparalte", groupvars = "Community")
SpartinaCover
qplot(Community, Sparalte, data = SpartinaCover, geom = c("boxplot"))

####Community   N  Sparalte        sd        se       ci
1     Brackish 223 11.611095 13.073603 0.8754732 1.725302
2   Freshwater   9  5.302469  3.836193 1.2787310 2.948759
3 Intermediate  60  9.832553 11.954417 1.5433086 3.088153
4       Saline 548 49.951953 23.328199 0.9965313 1.957497


