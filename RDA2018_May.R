library(tidyverse)
library(vegan)

#LOAD NEW May 2018 Data:
#Produced as per 04_MERGE_VegHydroSoil.R file
VegAllEnvData = read.csv("VegAllEnvData_03may2018.csv")
VegAllEnvData$Community <- factor(VegAllEnvData$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))

#RDA FRESHWATER ~ Mean_SoilSalinity======
freshOnly <- VegAllEnvData[ VegAllEnvData$Community=="Freshwater",]
dim(freshOnly)#now: 603 465
sum(is.na(freshOnly$MeanWaterSalinity)) #42 rows with NA-s that need removing
sum(is.na(freshOnly$Phraaust)) #0 rows with NA - yay!

FreshNArows<- is.na(freshOnly$MeanWaterSalinity) #object = NA rows in freshOnly
freshOnly_Clean <- freshOnly[ ! FreshNArows,]#remove NA rows

freshOnly_Clean <- na.omit(freshOnly)
freshVeg_Cover<-subset(freshOnly_Clean, select = Acerrubr:ZiziMill)  #Freshwater veg cover data only

freshEnv<-subset(freshOnly_Clean,
                 select = c(StationFront,Community, year, Phraaust, MeanWaterSalinity, Mean_SoilSalinity)) # Env data for Freshwater Community +"Phraust"
dim(freshEnv)#336   6

##BRAY Vegetation distance matrix construction [Cover Values]:
BRAYfresh2 <- vegdist(freshVeg_Cover, distance = "bray")#compute dissimilarity indices between comm-s
df.response2 <- decostand( BRAYfresh2, method = 'hellinger' )#standardization method  for community data
Freshwater_rda <- rda(df.response2 ~ Mean_SoilSalinity + Phraaust, freshEnv)
#ANOVA:
Fresh_Anova_Cover_soil<-anova.cca(Freshwater_rda, by = "margin")
Fresh_Anova_Cover_soil
Fresh_Anova_Cover_soil####################Df  Variance      F Pr(>F)    
#Mean_SoilSalinity   1 0.0003896 11.6643  0.001 ***
#Phraaust            1 0.0001398  4.1863  0.002 ** 
#printCoefmat(as.data.frame(Fresh_Anova_Cover_soil), na.print="", signif.legend=FALSE) 
fresh.soil.summary <- as.data.frame(Fresh_Anova_Cover_soil)
fresh.soil.summary$com <- "Freshwater"

#ADD YEAR as factor to reduce effect of temporal autocorrelation:
Freshwater_rda_year <- rda(df.response2 ~ Mean_SoilSalinity + Phraaust + year, freshEnv)
#ANOVA:
Fresh_Anova_Cover_soil_year<-anova.cca(Freshwater_rda_year, by = "margin")
Fresh_Anova_Cover_soil_year#Df  Variance      F Pr(>F)    
############################Mean_SoilSalinity   1 0.0003767 11.3217  0.001 ***
############################Phraaust            1 0.0001401  4.2101  0.003 ** 
############################year                1 0.0000757  2.2739  0.03
#PLOT:
plot(Freshwater_rda , display=c("lc","cn"), main="Lousiana Freshwhater Communities")
Freshwater.summary <- summary(Freshwater_rda)
Freshwater.summary$concont$importance[2,1]#Cum. constr. eigenvalues prop. explained for RDA1
Freshwater.summary$concont$importance[3,2]#Cum. constr. eigenvalues prop. explained for RDA2

#######################RDA1      RDA2
#Cumulative Proportion 0.6610200 1.0000000
R2 <- RsquareAdj(Freshwater_rda)$adj.r.squared # R2 of the variance explained
R2#0.04

#RDAs explained = Proportion explained *R2 *100%
RDA1_Freshwater <- round(100 * RsquareAdj(Freshwater_rda)$adj.r.squared * summary(Freshwater_rda)$concont$importance[2,1], digits = 1)
RDA1_Freshwater 
RDA2_Freshwater <- round(100 * RsquareAdj(Freshwater_rda)$adj.r.squared * summary(Freshwater_rda)$concont$importance[3,2], digits = 1)
RDA2_Freshwater 

# PLOTS (quick ones to see)
plot(Freshwater_rda, display=c("lc","cn"), main="Lousiana Freshwater Communities")
# GRAPHING Freshwater water RDA with ggplot
# Use the "scores" function, then use the elements of it, casting them to data frames, e.g.:
df.sites <- as.data.frame( scores(Freshwater_rda)$sites )
# The environment variables are in another element, e.g.: $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( Freshwater_rda$CCA$biplot[, 1:2] )
df.env$var <- rownames( Freshwater_rda$CCA$biplot )
df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows
FreshwaterPlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab(paste0(as.character(RDA1_Freshwater) ,' % of variation (RDA1)')) + 
  ylab(paste0(as.character(RDA2_Freshwater) ,' % of variation (RDA2)')) +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  annotate("text", x = 0.8, y = 0.1, label = c("Soil Salinity ***"), size=8, color="darkgreen") +
  annotate("text", x = 0, y = 0.97, label = c("Phragmites australis **"), size=8, color="darkblue") + 
  theme_bw()+
  theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +
  ggtitle("Freshwater Plant Communities (2007 - 2017, LA)")

FreshwaterPlot

#"Intermediate" RDA ~ Mean_SoilSalinity========
VegEnvData <- read.csv("VegAllEnvData_03may2018.csv")
IntermediateOnly <- VegEnvData[ VegEnvData$Community=="Intermediate",]
IntermediateOnly_Clean <- na.omit(IntermediateOnly)#remove NA rows
dim(IntermediateOnly_Clean)#388 465

IntermediateVeg_Cover<- subset(IntermediateOnly_Clean, select = Acerrubr:ZiziMill) #Intermediatewater veg cover data only

IntermediateEnv<- subset(IntermediateOnly_Clean, select = c(StationFront,Community, year, Phraaust, MeanWaterSalinity, Mean_SoilSalinity)) # Env data for Intermediatewater Community +"Phraust"

##BRAY Vegetation distance matrix construction [Cover Values]:
BRAYIntermediate <- vegdist(IntermediateVeg_Cover, distance = "bray")
Intermediate.response <- decostand( BRAYIntermediate, method = 'hellinger' )#?decostand# standardization method  for community data
Intermediate_rda <- rda(Intermediate.response ~ Mean_SoilSalinity + Phraaust, IntermediateEnv)

#ANOVA:
Intermediate_Anova_Cover<-anova.cca(Intermediate_rda, by = "margin")
Intermediate_Anova_Cover
#STATS OUTPUT:
#Model: rda(formula = df.response1 ~ MeanSalinity + Phraaust, data = IntermediateEnv)
##############Df  Variance       F Pr(>F)    
#Mean_SoilSalinity   1 0.0018185 53.4083  0.001 ***
#Phraaust            1 0.0003315  9.7357  0.001 ***
intermediate.soil.summary <- as.data.frame(Intermediate_Anova_Cover)
intermediate.soil.summary$com <- "Intermediate"
intermediate.soil.summary

# Compute % Variance explained by RDA1 and RDA2 ==
#== adj.r.squared * Cum. constr. eigenvalues prop. explained
Intermediate.summary <- summary(Intermediate_rda)
Intermediate.summary$concont$importance[2,1]#Cum. constr. eigenvalues prop. explained for RDA1
Intermediate.summary$concont$importance[3,2]#Cum. constr. eigenvalues prop. explained for RDA2
#######################RDA1      RDA2
#Cumulative Proportion 0.6610200 1.0000000
R2 <- RsquareAdj(Intermediate_rda)$adj.r.squared # R2 of the variance explained
R2#0.1111971

#RDAs explained = Proportion explained *R2 *100%
RDA1_Intermediate <- round(100 * RsquareAdj(Intermediate_rda)$adj.r.squared * summary(Intermediate_rda)$concont$importance[2,1], digits = 1)
RDA1_Intermediate #11.8 %
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
  xlab(paste0(as.character(RDA1_Intermediate) ,' % of variation (RDA1)')) + 
  ylab(paste0(as.character(RDA2_Intermediate) ,' % of variation (RDA2)')) +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  annotate("text", x = 0.75, y = 0.28, label = c("Soil Salinity ***"), size=8, color="darkgreen") +
  annotate("text", x = 0.3, y = 0.97, label = c("Phragmites australis ***"), size=8, color="darkblue") + 
  theme_bw()+
  theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                        axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                        axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                        axis.title.y = element_text(size=22),
                        legend.title = element_text(size=22),
                        plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +
  ggtitle("Intermediate Plant Communities (2007 - 2017, LA)")
IntermediatePlot
#ggsave('2018Intermediatewater_RDA_Plot2.jpeg', dpi=300, height=5, width=9)



#"Brackish" RDA ~ Mean_SoilSalinity========
VegEnvData <- read.csv("VegAllEnvData_03may2018.csv")
BrackishOnly <- VegEnvData[ VegEnvData$Community=="Brackish",]
BrackishOnly_Clean <- na.omit(BrackishOnly)#remove NA rows
dim(BrackishOnly_Clean)#284 465

BrackishVeg_Cover<- subset(BrackishOnly_Clean, select = Acerrubr:ZiziMill) #Brackishwater veg cover data only

BrackishEnv<- subset(BrackishOnly_Clean, select = c(StationFront,Community, year, Phraaust, MeanWaterSalinity, Mean_SoilSalinity)) # Env data for Brackishwater Community +"Phraust"

##BRAY Vegetation distance matrix construction [Cover Values]:
BRAYBrackish <- vegdist(BrackishVeg_Cover, distance = "bray")
Brackish.response <- decostand( BRAYBrackish, method = 'hellinger' )#?decostand# standardization method  for community data
Brackish_rda <- rda(Brackish.response ~ Mean_SoilSalinity + Phraaust, BrackishEnv)
#ANOVA:
Brackish_Anova_Cover<-anova.cca(Brackish_rda, by = "margin")
Brackish_Anova_Cover

#STATS OUTPUT:
#Model: rda(formula = df.response1 ~ MeanSalinity + Phraaust, data = BrackishEnv)
##############Df  Variance       F Pr(>F)    
#Mean_SoilSalinity   1 0.0008607 10.2997  0.001 ***
#Phraaust            1 0.0003563  4.2635  0.010 ** 
Brackish.soil.summary <- as.data.frame(Brackish_Anova_Cover)
Brackish.soil.summary$com <- "Brackish"
Brackish.soil.summary

# Compute % Variance explained by RDA1 and RDA2 ==
#== adj.r.squared * Cum. constr. eigenvalues prop. explained
Brackish.summary <- summary(Brackish_rda)
Brackish.summary$concont$importance[2,1]#Cum. constr. eigenvalues prop. explained for RDA1
Brackish.summary$concont$importance[3,2]#Cum. constr. eigenvalues prop. explained for RDA2
#######################RDA1      RDA2
#Cumulative Proportion 0.6610200 1.0000000
R2 <- RsquareAdj(Brackish_rda)$adj.r.squared # R2 of the variance explained
R2#0.04

#RDAs explained = Proportion explained *R2 *100%
RDA1_Brackish <- round(100 * RsquareAdj(Brackish_rda)$adj.r.squared * summary(Brackish_rda)$concont$importance[2,1], digits = 1)
RDA1_Brackish 
RDA2_Brackish <- round(100 * RsquareAdj(Brackish_rda)$adj.r.squared * summary(Brackish_rda)$concont$importance[3,2], digits = 1)
RDA2_Brackish 

# PLOTS (quick ones to see)
plot(Brackish_rda, display=c("lc","cn"), main="Lousiana Brackish Communities")
# GRAPHING Brackish water RDA with ggplot
# Use the "scores" function, then use the elements of it, casting them to data frames, e.g.:
df.sites <- as.data.frame( scores(Brackish_rda)$sites )
# The environment variables are in another element, e.g.: $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( Brackish_rda$CCA$biplot[, 1:2] )
df.env$var <- rownames( Brackish_rda$CCA$biplot )
df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows
BrackishPlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab(paste0(as.character(RDA1_Brackish) ,' % of variation (RDA1)')) + 
  ylab(paste0(as.character(RDA2_Brackish) ,' % of variation (RDA2)')) +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  annotate("text", x = 0.85, y = 0.1, label = c("Soil Salinity ***"), size=8, color="darkgreen") +
  annotate("text", x = 0.55, y = 0.97, label = c("Phragmites australis **"), size=8, color="darkblue") + 
  theme_bw()+
  theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +
  ggtitle("Brackish Plant Communities (2007 - 2017, LA)")

BrackishPlot
#ggsave('2018Brackishwater, dpi=300, height=5, width=9)





#"Saline" RDA ~ Mean_SoilSalinity========
VegEnvData <- read.csv("VegAllEnvData_03may2018.csv")
SalineOnly <- VegEnvData[ VegEnvData$Community=="Saline",]
SalineOnly_Clean <- na.omit(SalineOnly)#remove NA rows
dim(SalineOnly_Clean)#284 465

SalineVeg_Cover<- subset(SalineOnly_Clean, select = Acerrubr:ZiziMill) #Saline water veg cover data only

SalineEnv<- subset(SalineOnly_Clean, select = c(StationFront,Community, year, Phraaust, MeanWaterSalinity, Mean_SoilSalinity)) # Env data for Saline water Community +"Phraust"

##BRAY Vegetation distance matrix construction [Cover Values]:
BRAYSaline <- vegdist(SalineVeg_Cover, distance = "bray")
Saline.response <- decostand( BRAYSaline, method = 'hellinger' )#?decostand# standardization method  for community data
Saline_rda <- rda(Saline.response ~ Mean_SoilSalinity + Phraaust, SalineEnv)
#ANOVA:
Saline_Anova_Cover<-anova.cca(Saline_rda, by = "margin")
Saline_Anova_Cover
#STATS OUTPUT:
#Model: rda(formula = df.response1 ~ MeanSalinity + Phraaust, data = SalineEnv)
##############Df  Variance       F Pr(>F)    
#Mean_SoilSalinity   1 0.007086 71.9055  0.001 ***
#Phraaust            1 0.000318  3.2307  0.021 *  
Saline.soil.summary <- as.data.frame(Saline_Anova_Cover)
Saline.soil.summary$com <- "Saline"
Saline.soil.summary

# Compute % Variance explained by RDA1 and RDA2 ==
#== adj.r.squared * Cum. constr. eigenvalues prop. explained
Saline.summary <- summary(Saline_rda)
Saline.summary$concont$importance[2,1]#Cum. constr. eigenvalues prop. explained for RDA1
Saline.summary$concont$importance[3,2]#Cum. constr. eigenvalues prop. explained for RDA2
#######################RDA1      RDA2
#Cumulative Proportion 0.6610200 1.0000000
R2 <- RsquareAdj(Saline_rda)$adj.r.squared # R2 of the variance explained
R2#0.04

#RDAs explained = Proportion explained *R2 *100%
RDA1_Saline <- round(100 * RsquareAdj(Saline_rda)$adj.r.squared * summary(Saline_rda)$concont$importance[2,1], digits = 1)
RDA1_Saline 
RDA2_Saline <- round(100 * RsquareAdj(Saline_rda)$adj.r.squared * summary(Saline_rda)$concont$importance[3,2], digits = 1)
RDA2_Saline 

# PLOTS (quick ones to see)
plot(Saline_rda, display=c("lc","cn"), main="Lousiana Saline Communities")
# GRAPHING Saline water RDA with ggplot
# Use the "scores" function, then use the elements of it, casting them to data frames, e.g.:
df.sites <- as.data.frame( scores(Saline_rda)$sites )
# The environment variables are in another element, e.g.: $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( Saline_rda$CCA$biplot[, 1:2] )
df.env$var <- rownames( Saline_rda$CCA$biplot )
df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows
SalinePlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab(paste0(as.character(RDA1_Saline) ,' % of variation (RDA1)')) + 
  ylab(paste0(as.character(RDA2_Saline) ,' % of variation (RDA2)')) +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  annotate("text", x = -0.1, y = -1, label = c("Soil Salinity ***"), size=8, color="darkgreen") +
  annotate("text", x = -0.65, y = 0.07, label = c("Phragmites australis *"), size=8, color="darkblue") + 
  theme_bw()+
  theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
        axis.title.y = element_text(size=22),
        legend.title = element_text(size=22),
        plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +
  ggtitle("Saline Plant Communities (2007 - 2017, LA)")

SalinePlot
#ggsave('2018Salinewater, dpi=300, height=5, width=9)





#Summary of All RDA-s=======
RDA.all.summary <- rbind(fresh.soil.summary,
                          intermediate.soil.summary,
                          Brackish.soil.summary,
                          Saline.soil.summary)
RDA.all.summary
#write.csv(RDA.all.summary , file =  "RDA.all.summary_09may2018.csv")                    
#ggsave(FreshwaterPlot, filename = "RDA_Plot_Freshwater2007to2017.jpg", dpi=300, height=5, width=9)
#ggsave(SalinePlot, filename = "RDA_Plot_Salinereshwater2007to2017.jpg", dpi=300, height=5, width=9)
#ggsave(BrackishPlot, filename = "RDA_Plot_Brackish2007to2017.jpg", dpi=300, height=5, width=9)
#ggsave(IntermediatePlot, filename = "RDA_Plot_Intermediate2007to2017.jpg", dpi=300, height=5, width=9)


#Create a StationFront-level Freshwater dataset (average over years)==========
fresh.soil <- subset(freshOnly_Clean,
                     select = c(StationFront,Community,Mean_SoilSalinity, Acerrubr:ZiziMill))

fresh.av <-fresh.soil %>% na.omit() %>%
  group_by(StationFront,Community)%>%
  summarise_at(vars(Mean_SoilSalinity:ZiziMill),mean,na.rm=T) %>% na.omit()

fresh.av.veg <- subset( fresh.av, select = Acerrubr:ZiziMill)
fresh.av.env <-  subset( fresh.av, select = c(Phraaust, Mean_SoilSalinity))
BRAYfresh <- vegdist(fresh.av.veg, distance = "bray")
df.response1 <- decostand( BRAYfresh, method = 'hellinger' )#standardization method  for community data
fresh_rda <- rda(df.response1 ~ Mean_SoilSalinity + Phraaust, fresh.av.env )
#ANOVA:
Fresh_Anova_av<-anova.cca(fresh_rda, by = "margin")
Fresh_Anova_av
####################Df  Variance      F Pr(>F)   
#Mean_SoilSalinity  1 0.0015462 1.9813  0.005 **
#Phraaust           1 0.0006917 0.8863  0.792  


