#New VegEnv data produced in 2018 in "VegECF.R" and "HydrologicDataECF.R" file
#files were usingraw CRMS environmental data. ECF stands for Emily C. Farrer who
#introduced changes and re-produced new cleaner data.
#My name is Pawel Waryszak, email: pwaryszak@tulane.edu
#Below I re-run the RDA analysis we have done in 2017 (as per below) with new vegEnvData:
#2018 Re-run of RDA=======
library(vegan)
library(tidyverse)
VegEnvData <- read.csv("VegEnvData2018.csv")#Data contains cover values for all plant species
str(VegEnvData)#3090 obs. of  451 variables:

#FRESHWATER RDA========
freshOnly <- VegEnvData[ VegEnvData$Community=="Freshwater",]
dim(freshOnly)#699 451
sum(is.na(freshOnly$MeanSalinity)) #195 rows with na need removing
sum(is.na(freshOnly$Phraaust)) #0 rows with NA - yay!
FreshNArows<- is.na(freshOnly$MeanSalinity) #object = NA rows in freshOnly
freshOnly_Cover <- freshOnly[ ! FreshNArows,]#remove NA rows
dim(freshOnly_Cover)#504 451

freshVeg_Cover<-freshOnly_Cover[,8:437] #Freshwater veg cover data only
names(freshVeg_Cover)# from "Acerrubr" to "ZiziMill"
freshVeg_0and1<-ifelse ( freshVeg_Cover == 0 , 0 , 1)##turning cover to presence/absence data

freshEnv<-freshVeg_Cover [ , c(1:7,286, 438:451)] # Env data of Freshwater +"Phraust"
dim(freshEnv)#504  22, colum 286 = "Phraaust"

##BRAY Vegetation distance matrix construction [Cover Values]:
?vegdist#The function computes dissimilarity indices
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
# GRAPHING Freshwater RDA with ggplot
# Use the "scores" function, then use the elements of it, casting them to data frames, e.g.:
df.sites <- as.data.frame( scores(fresh_rda)$sites )
# The environment variables are in another element, e.g.: $CCA$biplot gives the biplot coords for the env variables 
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
  annotate("text", x = 0.75, y = -0.48, label = c("Mean Salinity"), size=8, color="darkgreen") +
  annotate("text", x = 0.3, y = 1.07, label = c("Phrag Cover"), size=8, color="darkblue") + theme_bw()
FreshPlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                 axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                 axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                 axis.title.y = element_text(size=22),
                 legend.title = element_text(size=22),
                 plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +ggtitle("Lousiana Freshwater Plant Communities")

#ggsave('2018Freshwater_RDA_Plot.jpeg', dpi=300, height=5, width=8)

##BRAY Vegetation distance matrix construction [Cover Values]:
?vegdist#The function computes dissimilarity indices
BRAYfresh <- vegdist(freshVeg, distance = "bray")
df.response1 <- decostand( BRAYfresh, method = 'hellinger' )#?decostand# standardization method  for community data
fresh_rda <- rda(df.response1 ~ MeanSalinity + Phraaust, freshEnv)
#ANOVA:
Fresh_Anova_Cover<-anova.cca(fresh_rda, by = "margin")
Fresh_Anova_Cover



#Produce veg data and matching env data for RDA ANALYSIS ==========
#Done in 2017
#Create veg matrices for each year:
#Let us subset the plots (stationID) present in year 2007 from all data.
#We also remove all records from Swamp station as they contain no records our target species Phragmites:

veg <- read.csv("CRMS_Marsh_Veg.csv")#From cleaned the CRMS_Marsh_Vegetation.csv to suit R.
str(veg)#133612 obs. of  24 variables:
samples2007<-veg[veg$year ==2007,] 
length(levels(droplevels(samples2007$StationID)))#2558 = number of good plots in 2007 
length(levels(droplevels(samples2007$StationFront)))#282 = number of stations (sites) in 2007 

#Subset only these 2007 station from entire data set to have a
#consistent set of plot across years:

DF2007to2016<- veg[ which (veg$StationID  %in%  samples2007$StationID), c("year", "StationID", "StationFront","StationBack", "SpecCode", "Cover", "Community", "CoverTotal")]
str(DF2007to2016)#118570 obs. of  8 variables:
table(DF2007to2016$year)# a well balanced sampling!!!!
#2007  2008  2009  2010  2011  2012  2013  2014  2015  2016 
#11757 11225 11624 11692 11527 11573 11936 12473 12504 12259 

#Reshape to wide format to compute plant composition indices:
DF2007to2016$Cover <- ifelse(DF2007to2016$Cover ==0,0,1) #turning cover to presence/absence data
v<-DF2007to2016[,c("StationID","StationFront","Community","SpecCode","Cover","CoverTotal","year")] #select most important variables

v.wide<-spread(v,key = SpecCode, value = Cover, fill = 0)#species indices can be computed in a wide format only= each species has its own column.
#write.csv(v.wide, file = "VegConsistentPlotsData.csv", row.names = FALSE)

#Veg Dissimilarity Analysis across years, with For Loop ===========
#TRAIL:
#Subset one plot for trial RDA run for Christina:
CRMS0002_V54<- v.wide[v.wide$StationID=="CRMS0002_V54",]
dim(CRMS0002_V54)#10 by 409
CRMS0002_V54[, 1:5] #years not in asc order
#Put years in order first
trail<-CRMS0002_V54[ order(CRMS0002_V54$year),]
trail[,1:5]
dim(trail)#10 409
#Compute Disimilarity distances:
DisTrail <- vegdist(trail[6:409], distance = "bray", binary = TRUE, upper=TRUE, diag=FALSE)
class(DisTrail)
#Convert Dis to matrix"
m<-as.matrix(DisTrail)
colnames(m) <- c( "Year2007", "Year2008", "Year2009","Year2010","Year2011","Year2012","Year2013","Year2014","Year2015","Year2016")
rownames(m) <- c( "Year2007", "Year2008", "Year2009","Year2010","Year2011","Year2012","Year2013","Year2014","Year2015","Year2016")
m[ row(m) == col(m) ] <- NA #replacing diagonal with NA
m
m2<-as.data.frame(m)
#Averaging by column:
#This will givethe average dissimilarity of each year to all other years:
m3<- colMeans(m2, na.rm = TRUE)
m3


#FOR LOOP to compute dissimilarity across all years===================

Output2 <- NULL #we need to set an empty shelf for data called Output

for ( i in unique(v.wide$StationID) ){
  #create a subset data per each plot (StationID)
  subset_plot <- subset(v.wide, StationID== i)
  
  order_plot<- subset_plot[ order(subset_plot$year), ]
  
  subset_veg <- order_plot[ ,6:409] #subsetting veg matrix only
  
  Dissimilarity_Matrix <- vegdist(subset_veg, distance = "bray", binary = TRUE, upper=TRUE, diag=FALSE)
  
  m <- as.matrix(Dissimilarity_Matrix) #turning "dist" into "matrix to allow further computations
  
  m[ row(m) == col(m) ] <- NA #replacing diagonal with NA

  m <- as.matrix(Dissimilarity_Matrix)
  
  m[ row(m) == col(m) ] <- NA #replacing diagonal with NA
  
  m_data_average<- as.data.frame( colMeans(m, na.rm = TRUE)) #Averaging by column:
  
  rownames(m_data_average)[1] <- c( "Year2007", "Year2008", "Year2009","Year2010","Year2011","Year2012","Year2013","Year2014","Year2015","Year2016")
  colnames(m_data_average)[1] <- "Average_Dissimilarity"
  
  namePlot <- i
  saveoutput <- data.frame(m_data_average, StationID = namePlot)
  Output2 <- rbind(Output, saveoutput)
}

head(Output2, n = 30)
Output2$year2rest<- substr(rownames(Output2), 1, 8)#fixing the row names
Output2$StationFront<- substr(Output2$StationID, 1,8)
str(Output2)#25590 obs. of  4 variables:
#write.csv(Output2, file="DissimilarityOutput.csv")

#Dissimilarity Analysis per community types:==========
#Get veg data and define StationsFront (Site) prevelent community:
str(veg)#133612 obs. of  24 variables
levels(veg$Community)#"Brackish" "Freshwater" "Intermediate" "Saline"
veg$Community<-factor(veg$Community, levels = c( "Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels

#Define the community-type in each StationFront based on  n of Commuity-typ Counts per station:
StationComm<- group_by(veg,StationFront,Community ) %>% count(Count=StationFront)
StationComm#It gives us count of communities per StationFront (740)
SCwide<- spread(StationComm, key = Community, value = n, fill = 0)#make it wide
SCwide$WhichMax<-colnames(SCwide)[apply(SCwide,1,which.max)]#while wide we can see which Comm is predominant
SCwide
StationCommDefined<-SCwide[, c(1,7)]
colnames(StationCommDefined)[2] <- "Community" #Renaming WhichMAx back to Community
StationCommDefined #320 stationFronts

#GGPLOT of Dissimilarity over time=========

#Join Dissimilarity Output2 with Community-type variables:
DisOutput<-read.csv("DissimilarityOutput.csv")#running For loop takes times so I saved output in this file
DissCommunity <- left_join(DisOutput, StationCommDefined, by = "StationFront")
sum(is.na(DissCommunity))#0 YAY! no NA!
head(DissCommunity, n= 1)
#Average_Dissimilarity    StationID year2rest StationFront Community
#    0.3544974          CRMS0002_V54  Year2007     CRMS0002  Brackish
length(levels(DissCommunity$StationID))#2558 YAY! as many as before for loop.
length(unique(DissCommunity$StationFront))#282 YAY! as many as before for loop.
unique(DissCommunity$year2rest)#10 levels

#GGPLOT:
overtime<-summarize(group_by(DissCommunity,year2rest,Community), MeanDiss = mean(Average_Dissimilarity))
overtime$Community<-factor(overtime$Community, levels = c("Freshwater","Intermediate","Brackish","Saline"))#re-arranging levels ac to salinity levels
VariabilityPlot <- ggplot(overtime, aes(x = Community, y = MeanDiss,color=year2rest,shape =year2rest))+ geom_point(position = "jitter", size=4)
VariabilityPlot1<-VariabilityPlot + scale_shape_manual(values = c(15,15,16,16,18,18,24,24,13,13)) +theme_classic() +ggtitle("Mean community variability over 2007-2016 period")
VariabilityPlot1 +ylab("Mean Dissimilarity of One Year to All Other Years")

#Run RDA by Community==============
library(vegan);library(ggplot2);library(tidyr);library(dplyr);require(grid)
###The Bray-Curtis dissimilarity is bounded between 0 and 1, 
###where 0 means the two sites have the same composition (that is they share all the species), 
###and 1 means the two sites do not share any species
#LOAD VEG DATA:
vegdata <- read.csv(url("https://sites.google.com/site/phragmitesproject/file-cabinet/VegDataAll.csv?attredirects=0"))
dim(vegdata) #to see how many rows and columns are there = 25580 rows by  404 cols
#scanning for zero columns (singletons)
occur.col<-apply(vegdata,2,sum)#function that compute sums per colums
zerosCOL<-vegdata[occur.col <= 0 ,]#less or equal zero
nrow(zerosCOL)#number of zero rows = 0 YAY!
#scanning for zero rows
occur.row<-apply(vegdata,1,sum)#function that compute sums per colums
zerosROW<-vegdata[occur.row <= 0 ,]#less or equal zero
nrow(zerosROW)#number of zero rows = 1 = needs removing!
cutROWS<-rownames(vegdata[occur.row <= 0 ,])
cutROWS#"5374" this row needs to be cut out of both veg and env data

#LOAD ENV DATA:
envdata<- read.csv(url("https://sites.google.com/site/phragmitesproject/file-cabinet/EnvDataAll.csv?attredirects=0"))
dim(envdata)#25580    11
vegenv<-cbind(vegdata,envdata)# Binding veg matrix and env data together
dim(vegenv)# 25580   415
vegenv2<- vegenv[-5374,]
dim(vegenv2)#25579   415

#SUBSET COMMUNITIES:
#FRESHWATER:
freshOnly <- vegenv2[ vegenv2$Community=="Freshwater",]
head(freshOnly)
sum(is.na(freshOnly$MeanSalinity)) #51 rows with na need removing
sum(is.na(freshOnly$PhragCover)) #0 rows with na need removing
dim(freshOnly)#5442  415
OurNArows<- is.na(freshOnly$MeanSalinity) #51 rows with na need removing
5442 - 51 # = 5391
freshOnly1 <- freshOnly[ ! OurNArows,]
dim(freshOnly1 )#5391  415
freshVeg<-freshOnly1[,1:404] #Freshwater  veg community data only
head(freshVeg)
freshEnv<-freshOnly1 [ , -c(1:404)] #Freshwater env data only
head(freshEnv)

##BRAY Vegetation distance matrix construction
#FRESHWATER:
BRAYfresh <- vegdist(freshVeg, distance = "bray")
str(BRAYfresh)#Class 'dist'  atomic [1:14528745]

df.response1 <- decostand( BRAYfresh, method = 'hellinger' )
str(df.response1)# num [1:5391, 1:5391]

fresh_rda <- rda(df.response1 ~ MeanSalinity + PhragCover, freshEnv)
str(fresh_rda)

# PLOTS (quick ones to see)
plot(fresh_rda, display=c("cn", "sp", "lc"), main="Lousiana Freshwhater Communities")

plot(fresh_rda, display=c("lc","cn"), main="Lousiana Freshwhater Communities")

# GRAPHING Freshwater RDA with ggplot2==========
# Use the "scores" function, then use the elements of it, casting them to data frames, e.g.:
df.sites <- as.data.frame( scores(fresh_rda)$sites )

# The environment variables are in another element, e.g.:
# $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( fresh_rda$CCA$biplot[, 1:2] )
df.env
df.env$var <- rownames( fresh_rda$CCA$biplot )

df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows

summary(fresh_rda)
#Accumulated constrained eigenvalue, Importance of components:
#                        RDA1      RDA2
#Eigenvalue            1.881e-05 2.088e-06
#Proportion Explained  9.001e-01 9.991e-02
#Cumulative Proportion 9.001e-01 1.000e+00

#             RDA1     RDA2 PC1 PC2 PC3 PC4
#MeanSalinity 0.1317  0.99128   0   0   0   0
#PhragCover   0.9979 -0.06419   0   0   0   0

# R2 of the variance explained
R2 <- RsquareAdj(fresh_rda)$adj.r.squared
R2#0.002309445
RsquareAdj(fresh_rda)

#RDAs explained = Proportion explained *R2 *100%
9.001e-01*0.002309445*100 #RDA1 = 0.2%
9.991e-02*0.002309445*100 #RDA2 = 0.02


FreshPlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab('RDA1 (0.2 % of variation)') + 
ylab('RDA2 (0.02 % of variation)') +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  #geom_text(data=df.env, aes(x=RDA1, y=RDA2, label=var), size=8, colour="darkblue", parse=T) 
   
  annotate("text", x = 0.75, y = -0.48, label = c("Phrag Cover"), size=8, color="darkgreen") +
  annotate("text", x = 0.3, y = 1.07, label = c("Mean Salinity"), size=8, color="darkblue") + theme_bw()
FreshPlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                axis.title.y = element_text(size=22),
                legend.title = element_text(size=22),
                plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +ggtitle("Lousiana Freshwater Plant Communities")

#ggsave('FreshPlot.jpeg', graf, dpi=300, height=5, width=8)
#ggsave('FreshPlot.pdf', graf, dpi=300, height=35, width=35, units='cm')
#ggsave('FreshPlot.eps', graf, dpi=300, height=34, width=34, units='cm')


#Statistics on fit of fresh_rda:
anova.cca(fresh_rda, step=100)
#Permutation test for rda under reduced model,Number of permutations: 999
#Model: rda(formula = df.response1 ~ MeanSalinity + PhragCover, data = freshEnv)
#            Df  Variance      F Pr(>F)    
#Model       2 0.0000209 7.2384  0.001 *** ///  Residual 5388 0.0077784 


# GRAPHING Brackish RDA with ggplot2==========
# Use the "scores" function, then use the elements of it, casting them to data frames, e.g.:
df.sites <- as.data.frame( scores(brack_rda)$sites )

# The environment variables are in another element, e.g.:
# $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( brack_rda$CCA$biplot[, 1:2] )
df.env
df.env$var <- rownames( brack_rda$CCA$biplot )

df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows

summary(brack_rda)
#Accumulated constrained eigenvalue, Importance of components:
                        #RDA1      RDA2
#Eigenvalue            0.002984 4.355e-05
#Proportion Explained  0.985620 1.438e-02
#Cumulative Proportion 0.985620 1.000e+00

#Biplot scores for constraining variables

#RDA1    RDA2 PC1 PC2 PC3 PC4
#MeanSalinity  0.99960 0.02816   0   0   0   0
#PhragCover   -0.02523 0.99968   0   0   0   0


# R2 of the variance explained
R2 <- RsquareAdj(brack_rda)$adj.r.squared
R2#0.06896562
RsquareAdj(brack_rda)

#RDAs explained = Proportion explained *R2 *100%
0.985620*0.06896562*100 #RDA1 = 6.79%
1.438e-02*0.06896562*100 #RDA2 = 0.09%


brackPlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab('RDA1 (6.79 % of variation)') + 
  ylab('RDA2 (0.09 % of variation)') +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  #geom_text(data=df.env, aes(x=RDA1, y=RDA2, label=var), size=8, colour="darkblue", parse=T) 
  
  annotate("text", x = 0.9, y = -0.15, label = c("Mean Salinity"), size=8, color="darkgreen") +
  annotate("text", x = 0, y = 1.1, label = c("Phrag Cover"), size=8, color="darkblue") + theme_bw()
brackPlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                 axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                 axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                 axis.title.y = element_text(size=22),
                 legend.title = element_text(size=22),
                 plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +ggtitle("Brackish Plant Community")

#ggsave('brackPlot.jpeg', graf, dpi=300, height=5, width=8)
#ggsave('brackPlot.pdf', graf, dpi=300, height=35, width=35, units='cm')
#ggsave('brackPlot.eps', graf, dpi=300, height=34, width=34, units='cm')

# GRAPHING Intermediate RDA with ggplot2==========
# Use the "scores" function, then use the elements of it, casting them to data frames, e.g.:
df.sites <- as.data.frame( scores(inter_rda)$sites )

# The environment variables are in another element, e.g.:
# $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( inter_rda$CCA$biplot[, 1:2] )
df.env
df.env$var <- rownames( inter_rda$CCA$biplot )

df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows

summary(inter_rda)
#Accumulated constrained eigenvalue, Importance of components:
#RDA1      RDA2
#                        RDA1      RDA2
#Eigenvalue            0.0001519 1.741e-06
#Proportion Explained  0.9886700 1.133e-02
#Cumulative Proportion 0.9886700 1.000e+00

#Biplot scores for constraining variables
    #          RDA1      RDA2 PC1 PC2 PC3 PC4
#MeanSalinity 0.008098  0.999967   0   0   0   0
#PhragCover   0.999980 -0.006314   0   0   0   0

# R2 of the variance explained
R2 <- RsquareAdj(inter_rda)$adj.r.squared
R2#0.008729309
RsquareAdj(inter_rda)

#RDAs explained = Proportion explained *R2 *100%
0.9886700*0.008729309*100 #RDA1 = 0.86%
1.133e-02*0.008729309*100 #RDA2 = 0.01%


interPlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab('RDA1 (0.86 % of variation)') + 
  ylab('RDA2 (0.01 % of variation)') +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  #geom_text(data=df.env, aes(x=RDA1, y=RDA2, label=var), size=8, colour="darkblue", parse=T) 
  
  annotate("text", x = 0.9, y = -0.1, label = c("Phrag Cover"), size=8, color="darkgreen") +
  annotate("text", x = 0, y = 1.5, label = c("Mean Salinity"), size=8, color="darkblue") + theme_bw()
interPlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                 axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                 axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                 axis.title.y = element_text(size=22),
                 legend.title = element_text(size=22),
                 plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +ggtitle("Intermediate Plant Community")

#ggsave('interPlot.jpeg', graf, dpi=300, height=5, width=8)
#ggsave('interPlot.pdf', graf, dpi=300, height=35, width=35, units='cm')
#ggsave('interPlot.eps', graf, dpi=300, height=34, width=34, units='cm')

# GRAPHING Saline RDA with ggplot2==========
# Use the "scores" function, then use the elements of it, casting them to data frames, e.g.:
df.sites <- as.data.frame( scores(sal_rda)$sites )

# The environment variables are in another element, e.g.:
# $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( sal_rda$CCA$biplot[, 1:2] )
df.env
df.env$var <- rownames( sal_rda$CCA$biplot )

df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows

summary(sal_rda)
#Accumulated constrained eigenvalue, Importance of components:
                        #RDA1      RDA2
#Eigenvalue            0.01135 7.042e-06
#Proportion Explained  0.99938 6.200e-04
#Cumulative Proportion 0.99938 1.000e+00

#Biplot scores for constraining variables
#                  RDA1    RDA2 PC1 PC2 PC3 PC4
#MeanSalinity  0.99957 0.02942   0   0   0   0
#PhragCover   -0.06417 0.99794   0   0   0   0


# R2 of the variance explained
R2 <- RsquareAdj(sal_rda)$adj.r.squared
R2#0.07688748
RsquareAdj(sal_rda)

#RDAs explained = Proportion explained *R2 *100%
0.07688748*0.99938*100 #RDA1 = 7.68%
0.07688748*6.200e-04*100 #RDA2 = 0.01%


salPlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab('RDA1 (7.68 % of variation)') + 
  ylab('RDA2 (0.01 % of variation)') +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  #geom_text(data=df.env, aes(x=RDA1, y=RDA2, label=var), size=8, colour="darkblue", parse=T) 
  
  annotate("text", x = 0.9, y = -0.1, label = c("Mean Salinity"), size=8, color="darkgreen") +
  annotate("text", x = 0.1, y = 1.3, label = c("Phrag Cover"), size=8, color="darkblue") + theme_bw()
salPlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                 axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                 axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                 axis.title.y = element_text(size=22),
                 legend.title = element_text(size=22),
                 plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +ggtitle("Saline Plant Community")

#ggsave('salPlot.jpeg', graf, dpi=300, height=5, width=8)
#ggsave('salPlot.pdf', graf, dpi=300, height=35, width=35, units='cm')
#ggsave('salPlot.eps', graf, dpi=300, height=34, width=34, units='cm')


#Saving RDA outputs in a seperate files=========
str(fresh_rda)#List of 12
save(fresh_rda, file = "fresh_rda.rda")
getwd()#"C:/Users/cbirnbaum/Documents/crms_data_Christina"
load("fresh_rda.rda")#load the rda object back to R

str(sal_rda)#List of 12
save(sal_rda, file = "sal_rda.rda")
load("sal_rda.rda") #load the rda object back to R

str(brack_rda)#List of 12
save(brack_rda, file = "brack_rda.rda")
load("brack_rda.rda") #load the rda object back to R

str(inter_rda)#List of 12
save(inter_rda, file = "inter_rda.rda")
load("inter_rda.rda") #load the rda object back to R




