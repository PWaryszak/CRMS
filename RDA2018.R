#HOW "VegEnvDataNew2018.csv" was produced?====
#New data was produced by Emily (efarrer@tulane.edu) in Jan 2018 as shown in 2 following files:
#"HydrologicDataECF.R" and VegECF.R"
#then...

#Pawel (pwaryszak@tulane.edu) JOINED VEG6 with ENVC4 data objects as per below:
#ENVC4 is an object produced in "HydrologicDataECF.R" file off raw CRMS environmental data.
#WaterData<-as.data.frame(envc4)#Creating data to merge with veg6, or:
WaterData <- read_csv("CRMS_MeanWaterDepth_Salinity_envc4.csv")
dim(WaterData)#2994 obs. of  14 variables:
WaterDataThin <- as.data.frame(select( WaterData, StationFront.year, meanwaterdepthcm, MeanSalinity,floodedpercent))

#Veg6 is an object produced in "VegECF.R":
#VegData<-as.data.frame(veg6)#veg6 is produced in above lines or as saved csv file:
VegData<-read.csv("CRMS_Veg2018.csv")
VegData$StationFront.year<-interaction(VegData$StationFront, VegData$year)#We need that for joining
dim(VegData)#3090 obs. of  439 variables:

#Join VegData with WaterData
#by StationFront.year, some levels of "StationFront.year" do not overlap:
library(tidyverse)
library(vegan)
VegEnvData<-inner_join(VegData,WaterDataThin , by="StationFront.year" )
dim(VegEnvData)# 2410  442
#write.csv(VegEnvData, file = "VegEnvDataNew2018.csv", row.names = F)

#"Freshwater" RDA========
VegEnvData <- read.csv("VegEnvDataNew2018.csv")
freshOnly <- VegEnvData[ VegEnvData$Community=="Freshwater",]
dim(freshOnly)#541 442
sum(is.na(freshOnly$MeanSalinity)) # rows with NA-s that need removing
sum(is.na(freshOnly$Phraaust)) #0 rows with NA - yay!
FreshNArows<- is.na(freshOnly$MeanSalinity) #object = NA rows in freshOnly
freshOnly_Cover <- freshOnly[ ! FreshNArows,]#remove NA rows
dim(freshOnly_Cover)#504 442

freshVeg_Cover<-freshOnly_Cover[,9:437] #Freshwater veg cover data only
names(freshVeg_Cover)# from "Acerrubr" to "ZiziMill" = check if true
#freshVeg_0and1<-ifelse ( freshVeg_Cover == 0 , 0 , 1)##turning cover to presence/absence data if needed.
freshEnv<-freshOnly_Cover [ , c(1:8, 287, 439:442)] # Env data for Freshwater Community +"Phraust"
dim(freshEnv)#504  13

##BRAY Vegetation distance matrix construction [Cover Values]:
#?vegdist = The function computes dissimilarity indices
BRAYfresh <- vegdist(freshVeg_Cover, distance = "bray")
df.response1 <- decostand( BRAYfresh, method = 'hellinger' )#?decostand# standardization method  for community data
fresh_rda <- rda(df.response1 ~ MeanSalinity + Phraaust, freshEnv)
#ANOVA:
Fresh_Anova_Cover<-anova.cca(fresh_rda, by = "margin")
Fresh_Anova_Cover
#STATS OUTPUT:
#Model: rda(formula = df.response1 ~ MeanSalinity + Phraaust, data = freshEnv)
##############Df  Variance       F Pr(>F)    
#MeanSalinity   1 0.0002679 16.3952  0.001 ***
#Phraaust       1 0.0001383  8.4615  0.001 ***
#Residual     501 0.0081877  

# Compute % Variance explained by RDA1 and RDA2 ==
#== adj.r.squared * Cum. constr. eigenvalues prop. explained
a <- summary(fresh_rda)
a$concont$importance[2,1]#Cum. constr. eigenvalues prop. explained for RDA1
a$concont$importance[3,2]#Cum. constr. eigenvalues prop. explained for RDA2
#######################RDA1      RDA2
#Cumulative Proportion 0.6610200 1.0000000
R2 <- RsquareAdj(fresh_rda)$adj.r.squared # R2 of the variance explained
R2#0.04338864

#RDAs explained = Proportion explained *R2 *100%
RDA1_fresh <- round(100 * RsquareAdj(fresh_rda)$adj.r.squared * summary(fresh_rda)$concont$importance[2,1], digits = 1)
RDA1_fresh #2.9%
RDA2_fresh <- round(100 * RsquareAdj(fresh_rda)$adj.r.squared * summary(fresh_rda)$concont$importance[3,2], digits = 1)
RDA2_fresh #4.3%

# PLOTS (quick ones to see)
plot(fresh_rda, display=c("lc","cn"), main="Lousiana Freshwhater Communities")
# GRAPHING Freshwater RDA with ggplot:
# Use the "scores" function, then use the elements of it, casting them to data frames:
df.sites <- as.data.frame( scores(fresh_rda)$sites )
# The environment variables are in another element: $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( fresh_rda$CCA$biplot[, 1:2] )
df.env$var <- rownames( fresh_rda$CCA$biplot )
df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows
FreshPlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab('RDA1 (2.9 % of variation)') + 
  ylab('RDA2 (4.3 % of variation)') +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  annotate("text", x = 0.75, y = -0.18, label = c("Mean Salinity ***"), size=8, color="darkgreen") +
  annotate("text", x = 0.3, y = 1.07, label = c("Phragmites Cover ***"), size=8, color="darkblue") + theme_bw()
FreshPlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                 axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                 axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                 axis.title.y = element_text(size=22),
                 legend.title = element_text(size=22),
                 plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +
  ggtitle("Lousiana Freshwater Plant Communities (2007 - 2016)")
#ggsave('2018Freshwater_RDA_Plot4b.jpeg', dpi=300, height=5, width=9)

#"Intermediate" RDA========
VegEnvData <- read.csv("VegEnvDataNew2018.csv")
IntermediateOnly <- VegEnvData[ VegEnvData$Community=="Intermediate",]
dim(IntermediateOnly)#767 442
sum(is.na(IntermediateOnly$MeanSalinity)) # rows with NA-s that need removing
sum(is.na(IntermediateOnly$Phraaust)) #0 rows with NA - yay!
IntermediateNArows<- is.na(IntermediateOnly$MeanSalinity) #object = NA rows in IntermediateOnly
IntermediateOnly_Cover <- IntermediateOnly[ ! IntermediateNArows,]#remove NA rows
dim(IntermediateOnly_Cover)#714 442

IntermediateVeg_Cover<-IntermediateOnly_Cover[,9:437] #Intermediatewater veg cover data only
names(IntermediateVeg_Cover)# from "Acerrubr" to "ZiziMill"
#IntermediateVeg_0and1<-ifelse ( IntermediateVeg_Cover == 0 , 0 , 1)##turning cover to presence/absence data
IntermediateEnv<-IntermediateOnly_Cover [ , c(1:8,287, 439:442)] # Env data for Intermediatewater Community +"Phraust"
dim(IntermediateEnv)#714  13

##BRAY Vegetation distance matrix construction [Cover Values]:
BRAYIntermediate <- vegdist(IntermediateVeg_Cover, distance = "bray")
df.response1 <- decostand( BRAYIntermediate, method = 'hellinger' )#?decostand# standardization method  for community data
Intermediate_rda <- rda(df.response1 ~ MeanSalinity + Phraaust, IntermediateEnv)
#ANOVA:
Intermediate_Anova_Cover<-anova.cca(Intermediate_rda, by = "margin")
Intermediate_Anova_Cover
#STATS OUTPUT:
#Model: rda(formula = df.response1 ~ MeanSalinity + Phraaust, data = IntermediateEnv)
##############Df  Variance       F Pr(>F)    
#MeanSalinity   1 0.0016722 83.7449  0.001 ***
#  Phraaust     1 0.0001524  7.6333  0.003 ** 
#  Residual   711 0.0141969                   


# Compute % Variance explained by RDA1 and RDA2 ==
#== adj.r.squared * Cum. constr. eigenvalues prop. explained
a <- summary(Intermediate_rda)
a$concont$importance[2,1]#Cum. constr. eigenvalues prop. explained for RDA1
a$concont$importance[3,2]#Cum. constr. eigenvalues prop. explained for RDA2
#######################RDA1      RDA2
#Cumulative Proportion 0.6610200 1.0000000
R2 <- RsquareAdj(Intermediate_rda)$adj.r.squared # R2 of the variance explained
R2#0.1111971

#RDAs explained = Proportion explained *R2 *100%
RDA1_Intermediate <- round(100 * RsquareAdj(Intermediate_rda)$adj.r.squared * summary(Intermediate_rda)$concont$importance[2,1], digits = 1)
RDA1_Intermediate #10.3%
RDA2_Intermediate <- round(100 * RsquareAdj(Intermediate_rda)$adj.r.squared * summary(Intermediate_rda)$concont$importance[3,2], digits = 1)
RDA2_Intermediate # 11.1%

# PLOTS (quick ones to see)
plot(Intermediate_rda, display=c("lc","cn"), main="Lousiana Intermediatewhater Communities")
# GRAPHING Intermediate water RDA with ggplot
# Use the "scores" function, then use the elements of it, casting them to data frames, e.g.:
df.sites <- as.data.frame( scores(Intermediate_rda)$sites )
# The environment variables are in another element, e.g.: $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( Intermediate_rda$CCA$biplot[, 1:2] )
df.env$var <- rownames( Intermediate_rda$CCA$biplot )
df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows
IntermediatePlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab('RDA1 (10.3 % of variation)') + 
  ylab('RDA2 (11.1 % of variation)') +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  annotate("text", x = 0.75, y = -0.18, label = c("Mean Salinity ***"), size=8, color="darkgreen") +
  annotate("text", x = 0.3, y = 1.07, label = c("Phragmites Cover **"), size=8, color="darkblue") + theme_bw()
IntermediatePlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                        axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                        axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                        axis.title.y = element_text(size=22),
                        legend.title = element_text(size=22),
                        plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +
  ggtitle("Lousiana Intermediate Plant Communities (2007 - 2016)")
#ggsave('2018Intermediatewater_RDA_Plot2.jpeg', dpi=300, height=5, width=9)


#" Brackish" RDA========
VegEnvData <- read.csv("VegEnvDataNew2018.csv")
BrackishOnly <- VegEnvData[ VegEnvData$Community=="Brackish",]
dim(BrackishOnly)#543 442
sum(is.na(BrackishOnly$MeanSalinity)) # rows with NA-s that need removing
sum(is.na(BrackishOnly$Phraaust)) #0 rows with NA - yay!
BrackishNArows<- is.na(BrackishOnly$MeanSalinity) #object = NA rows in BrackishOnly
BrackishOnly_Cover <- BrackishOnly[ ! BrackishNArows,]#remove NA rows
dim(BrackishOnly_Cover)#491 442
BrackishVeg_Cover<-BrackishOnly_Cover[,9:437] #Brackishwater veg cover data only
names(BrackishVeg_Cover)# from "Acerrubr" to "ZiziMill"
#BrackishVeg_0and1<-ifelse ( BrackishVeg_Cover == 0 , 0 , 1)##turning cover to presence/absence data
BrackishEnv<-BrackishOnly_Cover [ , c(1:8,287, 439:442)] # Env data for Brackishwater Community +"Phraust"
dim(BrackishEnv)#491  13

##BRAY Vegetation distance matrix construction [Cover Values]:
BRAYBrackish <- vegdist(BrackishVeg_Cover, distance = "bray")
df.response1 <- decostand( BRAYBrackish, method = 'hellinger' )#?decostand# standardization method  for community data
Brackish_rda <- rda(df.response1 ~ MeanSalinity + Phraaust, BrackishEnv)
#ANOVA:
Brackish_Anova_Cover<-anova.cca(Brackish_rda, by = "margin")
Brackish_Anova_Cover
#STATS OUTPUT:
#Model: rda(formula = df.response1 ~ MeanSalinity + Phraaust, data = BrackishEnv)
##############Df  Variance       F Pr(>F)    
#MeanSalinity   1 0.000396 8.6377  0.002 **
#Phraaust       1 0.000260 5.6702  0.002 ** 
#Residual     488 0.022373                   


# Compute % Variance explained by RDA1 and RDA2:
#== adj.r.squared * Cum. constr. eigenvalues prop. explained
a <- summary(Brackish_rda)
a$concont$importance[2,1]#Cum. constr. eigenvalues prop. explained for RDA1
a$concont$importance[3,2]#Cum. constr. eigenvalues prop. explained for RDA2
#######################RDA1      RDA2
#Cumulative Proportion 0.6610200 1.0000000
R2 <- RsquareAdj(Brackish_rda)$adj.r.squared # R2 of the variance explained
R2#0.0251852

#RDAs explained = Proportion explained *R2 *100%
RDA1_Brackish <- round(100 * RsquareAdj(Brackish_rda)$adj.r.squared * summary(Brackish_rda)$concont$importance[2,1], digits = 1)
RDA1_Brackish #1.9 %
RDA2_Brackish <- round(100 * RsquareAdj(Brackish_rda)$adj.r.squared * summary(Brackish_rda)$concont$importance[3,2], digits = 1)
RDA2_Brackish # 2.5 %

# PLOTS (quick ones to see)
plot(Brackish_rda, display=c("lc","cn"), main="Lousiana Brackishwhater Communities (2007-2016)")
# GRAPHING Brackish water RDA with ggplot
# Use the "scores" function, then use the elements of it, casting them to data frames, e.g.:
df.sites <- as.data.frame( scores(Brackish_rda)$sites )
# The environment variables are in another element, e.g.: $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( Brackish_rda$CCA$biplot[, 1:2] )
df.env$var <- rownames( Brackish_rda$CCA$biplot )
df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows
BrackishPlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab('RDA1 (1.9 % of variation)') + 
  ylab('RDA2 (2.5 % of variation)') +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  annotate("text", x = 0.55, y = -0.88, label = c("Mean Salinity **"), size=8, color="darkgreen") +
  annotate("text", x = 0.4, y = 1.07, label = c("Phragmites Cover **"), size=8, color="darkblue") + theme_bw()
BrackishPlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                    axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                    axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                    axis.title.y = element_text(size=22),
                    legend.title = element_text(size=22),
                    plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +
  ggtitle("Lousiana Brackish Plant Communities (2007 - 2016)")
ggsave('2018Brackishwater_RDA_Plot2.jpeg', dpi=300, height=5, width=9)


#" Saline" RDA========
VegEnvData <- read.csv("VegEnvDataNew2018.csv")
SalineOnly <- VegEnvData[ VegEnvData$Community=="Saline",]
dim(SalineOnly)#559 441
sum(is.na(SalineOnly$MeanSalinity)) # rows with NA-s that need removing
sum(is.na(SalineOnly$Phraaust)) #0 rows with NA - yay!
SalineNArows<- is.na(SalineOnly$MeanSalinity) #object = NA rows in SalineOnly
SalineOnly_Cover <- SalineOnly[ ! SalineNArows,]#remove NA rows
dim(SalineOnly_Cover)#546 442

SalineVeg_Cover<-SalineOnly_Cover[,9:437] #Salinewater veg cover data only
names(SalineVeg_Cover)# from "Acerrubr" to "ZiziMill"
#SalineVeg_0and1<-ifelse ( SalineVeg_Cover == 0 , 0 , 1)##turning cover to presence/absence data
SalineEnv<-SalineOnly_Cover [ , c(1:8,287, 439:442)] # Env data for Salinewater Community +"Phraust"
dim(SalineEnv)#546  13

##BRAY Vegetation distance matrix construction [Cover Values]:
BRAYSaline <- vegdist(SalineVeg_Cover, distance = "bray")
df.response1 <- decostand( BRAYSaline, method = 'hellinger' )#?decostand# standardization method  for community data
Saline_rda <- rda(df.response1 ~ MeanSalinity + Phraaust, SalineEnv)
#ANOVA:
Saline_Anova_Cover<-anova.cca(Saline_rda, by = "margin")
Saline_Anova_Cover
#STATS OUTPUT:
#Model: rda(formula = df.response1 ~ MeanSalinity + Phraaust, data = SalineEnv)
##############Df  Variance       F Pr(>F)    
#MeanSalinity   1 0.006762 88.2046  0.001 ***
#Phraaust       1 0.000144  1.8844  0.147    
#Residual     543 0.041629 

# Compute % Variance explained by RDA1 and RDA2:
#== adj.r.squared * Cum. constr. eigenvalues prop. explained
a <- summary(Saline_rda)
a$concont$importance[2,1]#Cum. constr. eigenvalues prop. explained for RDA1
a$concont$importance[3,2]#Cum. constr. eigenvalues prop. explained for RDA2
#######################RDA1      RDA2
#Cumulative Proportion 0.6610200 1.0000000
R2 <- RsquareAdj(Saline_rda)$adj.r.squared # R2 of the variance explained
R2#0.1468727

#RDAs explained = Proportion explained *R2 *100%
RDA1_Saline <- round(100 * RsquareAdj(Saline_rda)$adj.r.squared * summary(Saline_rda)$concont$importance[2,1], digits = 1)
RDA1_Saline #14.6 %
RDA2_Saline <- round(100 * RsquareAdj(Saline_rda)$adj.r.squared * summary(Saline_rda)$concont$importance[3,2], digits = 1)
RDA2_Saline # 14.7 %

# PLOTS (quick ones to see)
plot(Saline_rda, display=c("lc","cn"), main="Lousiana Saline Communities (2007-2016)")
# GRAPHING Saline water RDA with ggplot
# Use the "scores" function, then use the elements of it, casting them to data frames, e.g.:
df.sites <- as.data.frame( scores(Saline_rda)$sites )
# The environment variables are in another element, e.g.: $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( Saline_rda$CCA$biplot[, 1:2] )
df.env$var <- rownames( Saline_rda$CCA$biplot )
df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows
SalinePlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab('RDA1 (14.6 % of variation)') + 
  ylab('RDA2 (14.7 % of variation)') +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  annotate("text", x =- 0.8, y = 0.1, label = c("Mean Salinity ***"), size=8, color="darkgreen") +
  annotate("text", x =  0.1, y = -1.26, label = c("Phragmites Cover"), size=8, color="darkblue") + theme_bw()
SalinePlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                  axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                  axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                  axis.title.y = element_text(size=22),
                  legend.title = element_text(size=22),
                  plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +
  ggtitle("Lousiana Saline Plant Communities (2007 - 2016)")
#ggsave('2018Saline_RDA_Plot2.jpeg', dpi=300, height=5, width=9)


