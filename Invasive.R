#Data +libraries + Apriori Model ========
VegAllEnvData <- read.csv("VegAllEnvData_03july2018.csv")
Plant_Info <- read.csv("LA_Plants_Clean.csv")#cleaned on 11 june 2018, has info on what specCode is native/introduced
dim(VegAllEnvData) # 3498  473
dim(Plant_Info)#3454 of all species, 747 of introduced ones.

library("tidyverse")
library("vegan")
library("gridExtra")
library("grid")
library(Rmisc)

#Invasive (introduced) ========
Plant_Info_Invasive <- Plant_Info  %>% filter (Plant_Info$nat == "introduced") %>%
                                      select(specCode)

inv_names<-as.character(droplevels((Plant_Info_Invasive$specCode)))
MyCols <- rbind(inv_names, "Community","StationFront")#To subset later on:

inv_data <- VegAllEnvData [ , colnames(VegAllEnvData) %in% MyCols] #select introduced species columns
dim(inv_data)#3498   45
inv_data $ richness <- specnumber(inv_data[, 2:44])

#PLOT Richness:========
RichnessSum <- summarySE(inv_data, measurevar = "richness",
                         groupvars = "Community", na.rm = T)
RichnessSum$Community<-factor(RichnessSum$Community,
                              levels = c( "Freshwater","Intermediate","Brackish","Saline"))

ggplot(RichnessSum, aes(x=Community, y=richness, fill = Community)) + 
  geom_errorbar(aes(ymin=richness-ci,
                    ymax=richness+ci), width=.35, size=.9)+
  scale_fill_manual(values=c(c("#CDB180", "#7E7C22" ,"#8F9727" ,"#65770F")))+
  geom_point(position=position_dodge(.1), size=4) +
  labs(x = "", y="Introduced Plant Richness") + 
  #facet_grid(.~Community,scale="fixed") +
  #ggtitle("Plant species richness over 2007 - 2017 (Louisiana)") +
  #theme_bw() +
  theme(legend.position = "none",
                    axis.text.x  = element_text(size = 14),
                    plot.title = element_text(hjust= 0.5, size =16))

#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_Inv_Richness2.pdf")


#PLOT Cover:==========
inv_data $ Cover <- rowSums(inv_data[, 2:44])
CoverSum <- summarySE(inv_data, measurevar = "Cover",
                         groupvars = "Community", na.rm = T)
#GGPLOT:
ggplot(CoverSum, aes(x=Community, y= Cover, fill = Community)) + 
  geom_errorbar(aes(ymin=Cover-ci,
                    ymax=Cover+ci), width=.35, size=.9)+
  scale_fill_manual(values=c(c("#CDB180", "#7E7C22" ,"#8F9727" ,"#65770F")))+
  geom_point(position=position_dodge(.1), size=4) +
  labs(x = "", y="Introduced Plant Cover")+ 
  #facet_grid(.~Community,scale="fixed") +
  #ggtitle("Plant species Cover over 2007 - 2017 (Louisiana)") +
  #theme_bw() +
  theme(legend.position = "none",
        axis.text.x  = element_text(size = 14),
        axis.text.y  = element_text(size = 14),
        plot.title = element_text(hjust= 0.5, size =16))

#ggsave(dpi=600, width = 7, height = 5, filename = "Rplot_CRMS_Inv_Cover.pdf")

#AOV on Introduced_Cover======
inv_data $Introduced_Cover<- rowSums(inv_data[, 2:44])

data_aov <- 
  group_by(inv_data ,StationFront,Community) %>% #Compute means of all variables per StationFront (site), over all years:
  na.omit() %>%
  summarise_at(vars(Introduced_Cover,
                    Alte_caracasana:Vign_angularis), mean,na.rm=T)

( Cover.aov <- aov(Introduced_Cover ~ Community, data_aov ) )
summary(Cover.aov)
TukeyHSD(Cover.aov)

#OUTPUT:
#Call:  aov(formula = Introduced_Cover ~ Community, data = data_aov)

Terms:          Community Residuals
Sum of Squares   5206.192 14831.110
Deg. of Freedom         3       211

Residual standard error: 8.38389
Estimated effects may be unbalanced

> summary(Cover.aov)
Df Sum Sq Mean Sq F value   Pr(>F)    
Community     3   5206  1735.4   24.69 9.85e-14 ***
Residuals   211  14831    70.3                     

> TukeyHSD(Cover.aov)
Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = Introduced_Cover ~ Community, data = data_aov)

$Community
diff                                     lwr       upr     p adj
Freshwater-Brackish      12.041501   7.424453 16.658549 0.0000000
Intermediate-Brackish     5.634021   1.249470 10.018572 0.0056613
Saline-Brackish          -0.451822  -4.781806  3.878162 0.9930821
Intermediate-Freshwater  -6.407480 -10.596473 -2.218487 0.0005873
Saline-Freshwater       -12.493323 -16.625166 -8.361480 0.0000000
Saline-Intermediate      -6.085843  -9.956152 -2.215534 0.0003815


#Most common species========
most <- VegAllEnvData %>%
  na.omit() %>%
  group_by(Community) %>% #Compute means of all variables per StationFront (site), over all years:
  summarise_at(vars(Acer_rubrum:Ziza_miliacea), sum ,na.rm=T) %>%
  gather(specCode,cover, - Community)%>%
  arrange(desc(cover,Community))

most_nat <- left_join(most, Plant_Info, by = "specCode")

write.csv(most_nat, file = "TopSpecies_SUM_Nat_Inv_CRMS.csv", row.names = F)

#Subset native/introduced Veg matrix where colSums are > 0:
Freshwater_Veg_Matrix <- as.data.frame (subset ( FreshwaterAV, select = c(Acer_rubrum:Ziza_miliacea)))
Freshwater_Veg_Sums <- colSums(Freshwater_Veg_Matrix)
which.max(Freshwater_Veg_Sums)#Pani_hemitomon
