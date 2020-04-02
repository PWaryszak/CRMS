#Data +libraries + Apriori Model ========
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/native
dim(VegAllEnvData) # 3498  473
dim(Plant_Info)#3454 of all species, 747 of native ones.

library("tidyverse")
library("vegan")
library("gridExtra")
library("grid")
library("Rmisc")

#native (native) ========
Plant_Info_native <- Plant_Info  %>% filter (Plant_Info$nat == "native") %>%
  select(specCode)

nat_names<-as.character(droplevels((Plant_Info_native$specCode)))
MyCols <- rbind(nat_names, "Community", "StationFront") #To subset/group_by later on:

nat_data <- VegAllEnvData [ , colnames(VegAllEnvData) %in% MyCols ] #select native species columns & group_by cols
nat_data $ richness <- specnumber(nat_data[, 2:347])

#PLOT Richness ===========
RichnessSum <- summarySE(nat_data, measurevar = "richness",
                         groupvars = "Community", na.rm = T)
RichnessSum$Community<-factor(RichnessSum$Community,
                              levels = c( "Freshwater","Intermediate","Brackish","Saline"))
n <- RichnessSum #to merge with alien summary data
n$nat <- "Native"

ggplot(RichnessSum, aes(x=Community, y=richness, fill = Community)) + 
  geom_errorbar(aes(ymin=richness-ci,
                    ymax=richness+ci), width=.35, size=.9)+
  scale_fill_manual(values=c(c("#CDB180", "#7E7C22" ,"#8F9727" ,"#65770F")))+
  geom_point(position=position_dodge(.1), size=4) +
  labs(x = "", y="Native Plant Richness") + 
  #facet_grid(.~Community,scale="fixed") +
  #ggtitle("Plant species richness over 2007 - 2017 (Louisiana)") +
  #theme_bw() +
  theme(legend.position = "none",
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14),
        axis.title.y  = element_text(size = 14),
        plot.title = element_text(hjust= 0.5, size =16))

#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_nat_Richness2.pdf")

#Aov Native_Richness:========
data_aov <- 
  group_by(nat_data ,StationFront,Community) %>% #Compute means of all variables per StationFront (site), over all years:
  na.omit() %>%
  summarise_at(vars(richness,
                    Acer_rubrum:Ziza_miliacea), mean,na.rm=T)

( RichnessSum.aov <- aov(richness ~ Community, data_aov ) )
summary(RichnessSum.aov)
TukeyHSD(RichnessSum.aov)

#AOV native cover=====
nat_data $ Native_Cover <- rowSums(nat_data[, 2:347])

data_aov <- 
  group_by(nat_data ,StationFront,Community) %>% #Compute means of all variables per StationFront (site), over all years:
  na.omit() %>%
  summarise_at(vars(Native_Cover,
                    Acer_rubrum:Ziza_miliacea), mean,na.rm=T)

( Cover.aov <- aov(Native_Cover ~ Community, data_aov ) )
summary(Cover.aov)
TukeyHSD(Cover.aov)

#OUTPUT:
#Call:  aov(formula = Native_Cover ~ Community, data = data_aov)

Terms:
  Community Residuals
Sum of Squares   13999.00  85156.53
Deg. of Freedom         3       211

Residual standard error: 20.08944
Estimated effects may be unbalanced
> summary(Cover.aov)
Df Sum Sq Mean Sq F value   Pr(>F)    
Community     3  13999    4666   11.56 4.78e-07 ***
  Residuals   211  85157     404                     
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> TukeyHSD(Cover.aov)
Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = Native_Cover ~ Community, data = data_aov)

$Community
diff        lwr        upr     p adj
Freshwater-Brackish      14.509151   3.445804  25.572497 0.0045048
Intermediate-Brackish     3.989323  -6.516917  14.495563 0.7591217
Saline-Brackish          -7.674056 -18.049541   2.701429 0.2246908
Intermediate-Freshwater -10.519828 -20.557472  -0.482184 0.0359504
Saline-Freshwater       -22.183206 -32.083909 -12.282504 0.0000001
Saline-Intermediate     -11.663378 -20.937392  -2.389364 0.0071346


#Native and Alien PLot toegether===========
an <- rbind(a,n)
an

ggplot(an, aes(x=nat, y=richness, fill = Community, shape = nat)) + 
  geom_errorbar(aes(ymin=richness-ci,
                    ymax=richness+ci), width=.35, size=.9)+
  scale_fill_manual(values=c(c("#CDB180", "#7E7C22" ,"#8F9727" ,"#65770F")))+
  geom_point(position=position_dodge(.1), size=3) +
  labs(x = "", y="Native Plant Richness") + 
  facet_grid(.~Community,scale="fixed") +
  #ggtitle("Plant species richness over 2007 - 2017 (Louisiana)") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14),
        axis.title.y  = element_text(size = 14),
        plot.title = element_text(hjust= 0.5, size =16))

#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_nat_Richness2.pdf")





#PLOT Cover:==========
nat_data $ Cover <- rowSums(nat_data[, 2:44])
CoverSum <- summarySE(nat_data, measurevar = "Cover",
                      groupvars = "Community", na.rm = T)
#GGPLOT:
ggplot(CoverSum, aes(x=Community, y= Cover, fill = Community)) + 
  geom_errorbar(aes(ymin=Cover-ci,
                    ymax=Cover+ci), width=.35, size=.9)+
  scale_fill_manual(values=c(c("#CDB180", "#7E7C22" ,"#8F9727" ,"#65770F")))+
  geom_point(position=position_dodge(.1), size=4) +
  labs(x = "", y="native Plant Cover")+ 
  #facet_grid(.~Community,scale="fixed") +
  #ggtitle("Plant species Cover over 2007 - 2017 (Louisiana)") +
  #theme_bw() +
  theme(legend.position = "none",
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14),
        plot.title = element_text(hjust= 0.5, size =16))


#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_nat_Cover.pdf")
