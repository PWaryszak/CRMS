#Load Data and libraries:
env <- read.csv("VegAllEnvData_03july2018.csv")
library(ggplot2)

#Having Salinity that is higher than 50 PPT is super unusual. Check:
env[which.max(env$Mean_SoilSalinity),]#31.16667 seems good.

#GGPLOT
env$Community <- factor(env$Community, levels = c("Freshwater","Intermediate", "Brackish","Saline"))
ggplot(env,aes(x=year, y=Mean_SoilSalinity, color=Community))+
  #labs(x = "",y="",colour="Community type")+
  geom_point()+
  geom_line(stat="smooth",method = "lm",size=.8)+
  scale_x_continuous(breaks = c(2007,2009,2011,2013,2015,2017))+
  facet_wrap(~Community,scale="free") +theme_classic()+
  theme(axis.text.x=element_text(vjust=0.5,size=10),
         axis.text.y=element_text(size=10),
         axis.title.y=element_text(size=24),
        axis.title.x=element_text(size=24),
        legend.position = "none",
         strip.text=element_text(size=24))
         
