##### Raw env data from CRMS website #####
#The data is cleaned to be added to veg data of the same period off the same CRMS website.
#DATA ("CRMS_Discrete_Hydrographic.csv") accessed on 03May2018 from CRMS web:
#CRMS ONLY: https://cims.coastal.louisiana.gov/FullTableExports.aspx

library(tidyverse)
library(plotrix)
library(nlme)
library(chron)#install.packages("chron")
library(vegan)#install.packages("vegan")

env<-read.csv("CRMS_Discrete_Hydrographic.csv") #take the file and delete the degree sign in two of the columns
#This is Soil Pore Water Data, measured on average once a month with the sippers,
dim(env)#	 228991 rows of  35 variables:
head(env)
names(env)
#for env data, the unit of measurement is StationID (which is composed of Station and site),
#even though there are many sites sampled within a station, they are unique measurements.
#I will use only the P stations, not the veg stations
#clean up, make some new columms, select only P stations for "soil porewater" stations 
#that are collected monthly, and select the shallowest depth 10cm depth measurements
#Remove year 2001, 2006 and 2017, remove error-looking outliers
#all the salinity values above 56 look incorrect by comparing to conductance,
#either a decimal is missing or columns were switched around. The value at 54.7 looks correct. 
#I didn't check the other high ones just beneath 56 
#substitute - for _ so it is the same as the veg data

env2<-env %>%
  rename(StationID = CPRA.Station.ID)%>%
  mutate(Date=Date..mm.dd.yyyy. , StationFront=StationID)%>%
  separate(Date,into=c("month","day","year"),sep="/")%>%
  #mutate(yearadd=20)%>%
  #unite(year,yearadd,year,sep="")%>%
  separate(StationFront,into=c("StationFront","StationBack"),sep="-")%>%
  mutate(StationType=StationBack)%>%
  separate(StationType,into=c("StationType","StationBackNum"),sep=1)%>%
  filter(Measurement.Depth..ft.==0.328, StationType=="P",
         year %in% c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017),
         Soil.Porewater.Salinity..ppt. < 56) %>%
  mutate(StationID.year=paste(StationID, year ,sep="."))

dim(env2)#73078    43
hist(env2$Soil.Porewater.Salinity..ppt.)
unique(env2$StationType)#"P"
table(env2$year)

#filter out any plots that had fewer than 5 or whatever samplings per year
counts<-env2%>%
  group_by(StationID.year)%>%
  summarise(n())
counts2<-counts$StationID.year[which(counts$`n()`>4)]
env3<-env2[which(env2$StationID.year%in%counts2),]
dim(env3)#62531    43

#filter out plots sampled in sept-dec (if desired, as veg surveys were done ~sep), 
#summarise means per site (over the 3 p plots at each site),
#and summarise means per year, then summarize means over all the years.
#for max, I could do it either way, below is the max salnity ever recorded at a sampling point
#(averaged across the three P plots)
env4<-env3%>%
  filter(month %in% c(1,2,3,4,5,6,7,8,9)) %>%
  group_by(StationFront, year, month, day, Date..mm.dd.yyyy.) %>%
  summarise(salinityppt = mean(Soil.Porewater.Salinity..ppt.)) %>%
  group_by(StationFront,year) %>%
  summarise(Mean_SoilSalinity=mean(salinityppt),Max_SoilSalinity=max(salinityppt),Min_SoilSalinity=min(salinityppt),
            SD_SoilSalinity = sd(salinityppt), CV_SoilSalinity = SD_SoilSalinity/Mean_SoilSalinity) %>%
  mutate(StationFront.year=paste(StationFront,year,sep="."))
dim(env4)# 2781    8

#then filter out and only keep plots that have at least 7 8 9 or 10 years of data
counts<-env4%>%
  group_by(StationFront)%>%
  summarise(n())
counts2<-counts$StationFront[ which(counts$`n()` > 6 )]

env5<-env4[ which(env4$StationFront %in% counts2 ),] #to be merged with veg data, at least of 6 years of continous data off env4
length(unique(env5$StationFront))#247 sites have at least 7 years-long string of data in them.
dim(env5)#2197    8    #USE env5 for merging with veg data.
#write.csv(as.data.frame(env5), file = "CRMS_Mean_SoilSalinity_env5_03may2018.csv", row.names = F)

#MERGE env5 with VEgEnvData===
str(env5) #'data.frame':	2197 obs. of  8 variables:

veg6<- read.csv("CRMS_Veg_Per_Site_03may2018.csv") #As produced in "VegECF.R" file
veg6$StationFront.year <- interaction( veg6$StationFront, veg6$year)
dim(veg6)#3606  457

data5 <- left_join(veg6, env5, by = "StationFront.year") %>% na.omit()
dim(data5) # 1918  464


#Table 1 of plots per year analyzed========
MyTable <- group_by(data5, year.x, Community) %>%
  summarise(n()) %>% spread(Community, `n()` )
#write.csv (MyTable, file = "PlotNumberTable.csv", row.names = F)

MyTable2 <- group_by(veg6, year, Community) %>%
  summarise(n()) %>% spread(Community, `n()` ) 
MyTable2



#PATH ANALYSIS########
year07 <- data5[ data5$year.x=="2007",]
dim(year07)#69 449

year08 <- data5[ data5$year.x=="2008",]
dim(year08)# 110 449
#Build regressions for path analysis:
F08a <- lm (ifelse(year08$Phraaust == 0,0,1) ~ Mean_SoilSalinity, data = year08 )
summary(F08a) #NS
F08b <- lm (richness ~ Mean_SoilSalinity, data = year08)
summary(F08b) #Significant
F08c<- lm (richness ~ ifelse(year08$Phraaust == 0,0,1), data = year08)
summary(F08c)#NS
F08d <- lm (CoverTotal ~ richness, data = year08)
summary(F08d) #NS
F08e <- lm (CoverTotal ~ Phraaust, data = year08)
summary(F08e) #NS

year09 <- data5[ data5$year.x=="2009",]
dim(year09)# 133 449
#Build regressions for path analysis:
F09a <- lm (ifelse(year09$Phraaust == 0,0,1) ~ Mean_SoilSalinity, data = year09)
summary(F09a) #NS
F09b <- lm (richness ~ Mean_SoilSalinity, data = year09)
summary(F09b) #Significant
F09c<- lm (richness ~ ifelse(year09$Phraaust == 0,0,1), data = year09)
summary(F09c)#NS
F09d <- lm (CoverTotal ~ richness, data = year09)
summary(F09d) #Significant
F09e <- lm (CoverTotal ~ Phraaust, data = year09)
summary(F09e) #NS

semPaths(F09a + F09b + F09c + F09d +F09e ,
          "model", "est", intercepts = F,
         title = T, edge.label.cex = 0.9,layout = "tree2",
         sizeMan = 12, nCharNodes = 0)

title("LA Community Path Analysis (CRMS, 2009)", line = 3)

# Soil "Freshwater" RDA========
freshOnly <- data5[ data5$Community=="Freshwater",]
dim(freshOnly)#

freshVeg_Cover<-freshOnly[,9:438] #Freshwater veg cover data only
names(freshVeg_Cover)# from "Acerrubr" to "ZiziMill" = check if true
#freshVeg_0and1<-ifelse ( freshVeg_Cover == 0 , 0 , 1)##turning cover to presence/absence data if needed.
freshEnv<-freshOnly [ , c("Mean_SoilSalinity","Phraaust")] # Env data for Freshwater Community +"Phraust"
names(freshEnv)#504  19

##BRAY Vegetation distance matrix construction [Cover Values]:
#?vegdist = The function computes dissimilarity indices
BRAYfresh <- vegdist(freshVeg_Cover, distance = "bray")
df.response1 <- decostand( BRAYfresh, method = 'hellinger' )#?decostand# standardization method  for community data
fresh_rda <- rda(df.response1 ~ Mean_SoilSalinity + Phraaust, freshEnv)
#ANOVA:
Fresh_Anova_Cover<-anova.cca(fresh_rda, by = "margin")
Fresh_Anova_Cover
#STATS OUTPUT:
#Model: rda(formula = df.response1 ~ MeanSalinity + Phraaust, data = freshEnv)
##############Df  Variance       F        Pr(>F)    
#Mean_SoilSalinity   1 0.0005095 11.9679  0.001 ***
#Phraaust            1 0.0000381  0.8955  0.445    
#Residual          293 0.0124746 


# Compute % Variance explained by RDA1 and RDA2 ==
#== adj.r.squared * Cum. constr. eigenvalues prop. explained
R2 <- RsquareAdj(fresh_rda)$adj.r.squared # R2 of the variance explained
R2#0.03555682

#RDAs explained = Proportion explained *R2 *100%
RDA1_fresh <- round(100 * RsquareAdj(fresh_rda)$adj.r.squared * summary(fresh_rda)$concont$importance[2,1], digits = 1)
RDA1_fresh # 3.3%
RDA2_fresh <- round(100 * RsquareAdj(fresh_rda)$adj.r.squared * summary(fresh_rda)$concont$importance[3,2], digits = 1)
RDA2_fresh #3.6%

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
  xlab('RDA1 (3.3 % of variation)') + 
  ylab('RDA2 (3.6 % of variation)') +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  annotate("text", x = 0.75, y = -0.18, label = c("Mean Soil Salinity ***"), size=8, color="darkgreen") +
  annotate("text", x = 0.3, y = 1.07, label = c("Phragmites Cover"), size=8, color="darkblue") + theme_bw()
FreshPlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                 axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                 axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                 axis.title.y = element_text(size=22),
                 legend.title = element_text(size=22),
                 plot.title = element_text(size=22, face="bold", hjust = 0.5)) +
  ggtitle("Freshwater Plant Communities (2007 - 2016)")
#ggsave('2018Freshwater_RDA_Plot4b.jpeg', dpi=300, height=5, width=9)

# Soil "Brackish" RDA========
BrackishOnly <- data5[ data5$Community=="Brackish",]
dim(BrackishOnly)#239 449

BrackishVeg_Cover<-BrackishOnly[,9:438] #Brackish veg cover data only
names(BrackishVeg_Cover)# from "Acerrubr" to "ZiziMill" = check if true
#BrackishVeg_0and1<-ifelse ( BrackishVeg_Cover == 0 , 0 , 1)##turning cover to presence/absence data if needed.
BrackishEnv<-BrackishOnly [ , c("Mean_SoilSalinity","Phraaust")] # Env data for Brackish Community +"Phraust"
names(BrackishEnv)#check

##BRAY Vegetation distance matrix construction [Cover Values]:
#?vegdist = The function computes dissimilarity indices
BRAYBrackish <- vegdist(BrackishVeg_Cover, distance = "bray")
df.response1 <- decostand( BRAYBrackish, method = 'hellinger' )#?decostand# standardization method  for community data
Brackish_rda <- rda(df.response1 ~ Mean_SoilSalinity + Phraaust, BrackishEnv)
#ANOVA:
Brackish_Anova_Cover<-anova.cca(Brackish_rda, by = "margin")
Brackish_Anova_Cover
#STATS OUTPUT:
#Model: rda(formula = df.response1 ~ MeanSalinity + Phraaust, data = BrackishEnv)
#####################Df  Variance   F        Pr(>F)    
#Mean_SoilSalinity   1 0.0011026 10.629  0.001 ***
#Phraaust            1 0.0003534  3.407  0.023 *  
#Residual          236 0.0244829      

# Compute % Variance explained by RDA1 and RDA2 ==
#== adj.r.squared * Cum. constr. eigenvalues prop. explained
R2_Brackish<- RsquareAdj(Brackish_rda)$adj.r.squared # R2 of the variance explained
R2_Brackish#  0.05250234

#RDAs explained = Proportion explained *R2 *100%
RDA1_Brackish <- round(100 * RsquareAdj(Brackish_rda)$adj.r.squared * summary(Brackish_rda)$concont$importance[2,1], digits = 1)
RDA1_Brackish # 4.6 %
RDA2_Brackish <- round(100 * RsquareAdj(Brackish_rda)$adj.r.squared * summary(Brackish_rda)$concont$importance[3,2], digits = 1)
RDA2_Brackish # 5.3 %

# PLOTS (quick ones to see)
plot(Brackish_rda, display=c("lc","cn"), main="Lousiana Brackish Communities")
# GRAPHING Brackish RDA with ggplot:
# Use the "scores" function, then use the elements of it, casting them to data frames:
df.sites <- as.data.frame( scores(Brackish_rda)$sites )
# The environment variables are in another element: $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( Brackish_rda$CCA$biplot[, 1:2] )
df.env$var <- rownames( Brackish_rda$CCA$biplot )
df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows
BrackishPlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab('RDA1 (4.6  % of variation)') + 
  ylab('RDA2 (5.3 % of variation)') +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  annotate("text", x = 0.75, y = -0.58, label = c("Mean Soil Salinity ***"), size=8, color="darkgreen") +
  annotate("text", x = 0.3, y = 1.07, label = c("Phragmites Cover *"), size=8, color="darkblue") + theme_bw()
BrackishPlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                 axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                 axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                 axis.title.y = element_text(size=22),
                 legend.title = element_text(size=22),
                 plot.title = element_text(size=22, face="bold", hjust = 0.5)) +
  ggtitle("Brackish Plant Communities (2007 - 2016)")
ggsave('2018Brackish_SOIL_RDA_Plot4b.jpeg', dpi=300, height=5, width=9)


# Soil "Intermediate" RDA========
IntermediateOnly <- data5[ data5$Community=="Intermediate",]
dim(IntermediateOnly)#288 449

IntermediateVeg_Cover<-IntermediateOnly[,9:438] #Intermediate veg cover data only
names(IntermediateVeg_Cover)# from "Acerrubr" to "ZiziMill" = check if true
#IntermediateVeg_0and1<-ifelse ( IntermediateVeg_Cover == 0 , 0 , 1)##turning cover to presence/absence data if needed.
IntermediateEnv<-IntermediateOnly [ , c("Mean_SoilSalinity","Phraaust")] # Env data for Intermediate Community +"Phraust"
names(IntermediateEnv)#check

##BRAY Vegetation distance matrix construction [Cover Values]:
#?vegdist = The function computes dissimilarity indices
BRAYIntermediate <- vegdist(IntermediateVeg_Cover, distance = "bray")
df.response1 <- decostand( BRAYIntermediate, method = 'hellinger' )#?decostand# standardization method  for community data
Intermediate_rda <- rda(df.response1 ~ Mean_SoilSalinity + Phraaust, IntermediateEnv)
#ANOVA:
Intermediate_Anova_Cover<-anova.cca(Intermediate_rda, by = "margin")
Intermediate_Anova_Cover
#STATS OUTPUT:
#Model: rda(formula = df.response1 ~ MeanSalinity + Phraaust, data = IntermediateEnv)
#####################Df  Variance   F        Pr(>F)    
#Mean_SoilSalinity   1 0.0015913 26.327  0.001 ***
#Phraaust            1 0.0001298  2.147  0.091 .  
#Residual          285 0.0172262   

# Compute % Variance explained by RDA1 and RDA2 ==
#== adj.r.squared * Cum. constr. eigenvalues prop. explained
R2_Intermediate<- RsquareAdj(Intermediate_rda)$adj.r.squared # R2 of the variance explained
R2_Intermediate#  0.08433308

#RDAs explained = Proportion explained *R2 *100%
RDA1_Intermediate <- round(100 * RsquareAdj(Intermediate_rda)$adj.r.squared * summary(Intermediate_rda)$concont$importance[2,1], digits = 1)
RDA1_Intermediate # 7.9 %
RDA2_Intermediate <- round(100 * RsquareAdj(Intermediate_rda)$adj.r.squared * summary(Intermediate_rda)$concont$importance[3,2], digits = 1)
RDA2_Intermediate #  8.4 %

# PLOTS (quick ones to see)
plot(Intermediate_rda, display=c("lc","cn"), main="Lousiana Intermediate Communities")
# GRAPHING Intermediate RDA with ggplot:
# Use the "scores" function, then use the elements of it, casting them to data frames:
df.sites <- as.data.frame( scores(Intermediate_rda)$sites )
# The environment variables are in another element: $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( Intermediate_rda$CCA$biplot[, 1:2] )
df.env$var <- rownames( Intermediate_rda$CCA$biplot )
df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows
IntermediatePlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab('RDA1 (7.9 % of variation)') + 
  ylab('RDA2 (8.4 % of variation)') +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  annotate("text", x = 0.75, y = -0.28, label = c("Mean Soil Salinity ***"), size=8, color="darkgreen") +
  annotate("text", x = 0.3, y = 1.07, label = c("Phragmites Cover"), size=8, color="darkblue") + theme_bw()
IntermediatePlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                    axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                    axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                    axis.title.y = element_text(size=22),
                    legend.title = element_text(size=22),
                    plot.title = element_text(size=22, face="bold", hjust = 0.5)) +
  ggtitle("Intermediate Plant Communities (2007 - 2016)")
#ggsave('2018Intermediate_SOIL_RDA_Plot.jpeg', dpi=300, height=5, width=9)



# Soil "Saline" RDA========
SalineOnly <- data5[ data5$Community=="Saline",]
dim(SalineOnly)#380 449

SalineVeg_Cover<-SalineOnly[,9:438] #Saline veg cover data only
names(SalineVeg_Cover)# from "Acerrubr" to "ZiziMill" = check if true
#SalineVeg_0and1<-ifelse ( SalineVeg_Cover == 0 , 0 , 1)##turning cover to presence/absence data if needed.
SalineEnv<-SalineOnly [ , c("Mean_SoilSalinity","Phraaust")] # Env data for Saline Community +"Phraust"
names(SalineEnv)#check

##BRAY Vegetation distance matrix construction [Cover Values]:
#?vegdist = The function computes dissimilarity indices
BRAYSaline <- vegdist(SalineVeg_Cover, distance = "bray")
df.response1 <- decostand( BRAYSaline, method = 'hellinger' )#?decostand# standardization method  for community data
Saline_rda <- rda(df.response1 ~ Mean_SoilSalinity + Phraaust, SalineEnv)
#ANOVA:
Saline_Anova_Cover<-anova.cca(Saline_rda, by = "margin")
Saline_Anova_Cover
#STATS OUTPUT:
#Model: rda(formula = df.response1 ~ MeanSalinity + Phraaust, data = SalineEnv)
#####################Df  Variance   F        Pr(>F)    

# Compute % Variance explained by RDA1 and RDA2 ==
#== adj.r.squared * Cum. constr. eigenvalues prop. explained
R2_Saline<- RsquareAdj(Saline_rda)$adj.r.squared # R2 of the variance explained
R2_Saline#  

#RDAs explained = Proportion explained *R2 *100%
RDA1_Saline <- round(100 * RsquareAdj(Saline_rda)$adj.r.squared * summary(Saline_rda)$concont$importance[2,1], digits = 1)
RDA1_Saline #  %
RDA2_Saline <- round(100 * RsquareAdj(Saline_rda)$adj.r.squared * summary(Saline_rda)$concont$importance[3,2], digits = 1)
RDA2_Saline #  %

# PLOTS (quick ones to see)
plot(Saline_rda, display=c("lc","cn"), main="Lousiana Saline Communities")
# GRAPHING Saline RDA with ggplot:
# Use the "scores" function, then use the elements of it, casting them to data frames:
df.sites <- as.data.frame( scores(Saline_rda)$sites )
# The environment variables are in another element: $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( Saline_rda$CCA$biplot[, 1:2] )
df.env$var <- rownames( Saline_rda$CCA$biplot )
df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows
SalinePlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab('RDA1 (7.9 % of variation)') + 
  ylab('RDA2 (8.4 % of variation)') +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  annotate("text", x = 0.75, y = -0.28, label = c("Mean Soil Salinity ***"), size=8, color="darkgreen") +
  annotate("text", x = 0.3, y = 1.07, label = c("Phragmites Cover"), size=8, color="darkblue") + theme_bw()
SalinePlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                        axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                        axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                        axis.title.y = element_text(size=22),
                        legend.title = element_text(size=22),
                        plot.title = element_text(size=22, face="bold", hjust = 0.5)) +
  ggtitle("Saline Plant Communities (2007 - 2016)")
#ggsave('2018Saline_SOIL_RDA_Plot.jpeg', dpi=300, height=5, width=9)




#Cor Water & Soil Salinity=====
cor.test(data5$Mean_SoilSalinity, data5$MeanSalinity)
#t = 122.45, df = 1201, p-value < 2.2e-16
# cor = 0.962206 = Super HIGH correlation.






#to average across years=====
#env6<-env5%>% group_by(StationFront)%>%summarize(Mean_SoilSalinity=mean(MeanSalinity),Max_SoilSalinity=max(MaxSalinity))



