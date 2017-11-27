#Load DATA & packages:=======
library(Rmisc)
library(ggplot2)
library(dplyr)

#LOAD DATA off Internet if not on your comp (Clean Data, quite big):
#CRMSveg<- read.csv(url("https://sites.google.com/site/phragmitesproject/file-cabinet/VegDataAll.csv?attredirects=0"))
#dim(CRMSveg)#25580 rows & 404 = 404 cols of plant species
CRMSveg<-read.csv("VegDataAll.csv")#it loads quicker from your own computer.
dim(CRMSveg)#25580   404 - just check if the same dimensions of internet file vs local file

#CRMSenv<- read.csv(url("https://sites.google.com/site/phragmitesproject/file-cabinet/EnvDataAll.csv?attredirects=0"))
#dim(CRMSenv)#25580    11
CRMSenv<-read.csv("EnvDataAll.csv")
dim(CRMSenv)#25580    11
head(CRMSenv)

veg <-cbind(CRMSveg,CRMSenv)#Bind by columns not rows.
str(veg)#25580 obs. of  415 variables:

#Compute ratio (%) of plots with and without Phrag:======
veg2<- veg [ ! is.na(veg$year),] #removes all NA-s.
dim(veg2)#25425   416
veg2$Community<- factor(veg2$Community, levels = c("Freshwater", "Intermediate", "Brackish","Saline"))
levels(veg2$PhragPresence)#"PhragAbsent"  "PhragPresent"

#Manipulate data to get % ration of plots with Phrag/total plots:
PhragRatio<- group_by(veg2, year, Community) %>% 
  summarise(ratio = length(PhragPresence[PhragPresence=="PhragPresent"] )/length(PhragPresence) * 100) 
PhragRatio
#Set ggplot theme manually:
theme_pw<-theme(axis.text.y=element_text(size=20),
                axis.title.y=element_text(size=22),
                axis.title.x=element_text(size=22),
                axis.text.x=element_text(size=20),
                strip.text = element_text(size=15),
                panel.grid.minor.x = element_blank(),
                legend.position = "right",
                legend.title = element_text(size=20),
                legend.text = element_text(size=16),
                plot.title = element_text(lineheight=1.1, face="bold", size = 20, hjust = 0.5))

#Draw the plot:
ggplot(PhragRatio, aes(year, ratio, colour = Community)) +geom_line(size=2) +
  xlab("Year") + ylab("Plots (%)")  +
  ggtitle("% plots containing P. australis")  + theme_bw() + theme_pw +
  scale_colour_manual(values = c("blue", "green","purple", "red")) 

#Save the plot to working directory on your comp:
ggsave(filename = "PhragPercLinePlot3.png", width = 8, height = 4, dpi = 300)#saves png in working directory
