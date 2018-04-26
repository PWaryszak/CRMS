library("lavaan")
library("semPlot")
library("nlme")
library("lme4")

#"Freshwater" Data ========
VegEnvData <- read.csv("VegEnvDataNew2018.csv")
freshOnly <- VegEnvData[ VegEnvData$Community=="Freshwater",]
sum(is.na(freshOnly$MeanSalinity)) # rows with NA-s that need removing
sum(is.na(freshOnly$Phraaust)) #0 rows with NA - yay!
FreshNArows<- is.na(freshOnly$MeanSalinity) #object = NA rows in freshOnly
freshOnly_Cover <- freshOnly[ ! FreshNArows,]#remove NA rows
dim(freshOnly_Cover)#504 442
freshVeg_Cover<-freshOnly_Cover[,9:437] #Freshwater veg cover data only
names(freshVeg_Cover)# from "Acerrubr" to "ZiziMill" = check if true
freshEnv<-freshOnly_Cover [ , c(1:8, 287, 439:442)] # Env data for Freshwater Community +"Phraust"

#Build regressions for path analysis:
Freshwater1 <- lmer (Phraaust ~ MeanSalinity +1|year, data = freshEnv)
(summary(Freshwater1)$coefficients)
Freshwater2 <- lm (richness ~ MeanSalinity, data = freshEnv)
(summary(Freshwater2)$coefficients)
Freshwater3 <- lm (richness ~ Phraaust, data = freshEnv)
(summary(Freshwater3)$coefficients)
Freshwater4 <- lm (CoverTotal ~ richness, data = freshEnv)
(summary(Freshwater4)$coefficients)
Freshwater5 <- lm (CoverTotal ~ Phraaust, data = freshEnv)
(summary(Freshwater5)$coefficients)

table(ifelse(freshEnv$Phraaust == 0, "No","Yes"))
#No  Yes 
#470 34
#34/504*100 = 6.7% of plots contain Phraaust
Freshwater1 <- lm (Phraaust ~ MeanSalinity, data = freshEnv)
Freshwater2 <- lm (richness ~ MeanSalinity, data = freshEnv)
Freshwater3 <- lm (richness ~ Phraaust, data = freshEnv)
Freshwater4 <- lm (CoverTotal ~ richness, data = freshEnv)
Freshwater5 <- lm (CoverTotal ~ Phraaust, data = freshEnv)

# Plot path diagram:
semPaths(Freshwater1 + Freshwater2 + Freshwater3 + Freshwater4 +
           Freshwater5, "model", "est", intercepts = F,
         title = T, edge.label.cex = 0.9,layout = "tree2",
         sizeMan = 12, nCharNodes = 0)

title("Freshwater Community Path Analysis (CRMS, 2007-2016)", line = 3)


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