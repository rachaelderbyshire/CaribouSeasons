#Temporal patterns and comparisons between ecotypes

#code written by Rachael Derbyshire: rderbysh@uoguelph.ca
#2025-2026

#Load workspace from previous step
load("Seasons_workspace.RData")

#Load required packages
library(FeatureImpCluster)
library(flexclust)
library(ggplot2)
library(grid)
library(gridExtra)

#feature importance ----
#start with migratory
#2009
set.seed(77)#randomly selected seed number
seasonsMig2009k <- kmeans(cariwindow2009m, ClusterNum, iter.max = 100)
cl_kcca <- as.kcca(seasonsMig2009k, cariwindow2009m) # cl is a kcca or pam object
FeatureImp_res <-FeatureImpCluster(cl_kcca,as.data.table(cariwindow2009m), biter=100)
summary(FeatureImp_res$misClassRate)
p1<-plot(FeatureImp_res)
p1<-p1+scale_x_discrete(labels = c("HerbsMig2009"="Herbs", 
                                   "WaterMig2009"="Water", "ShrubMig2009"="Shrub",
                                   "TreedConiferMig2009"="Treed Conifer", "SpeedMig2009"="Speed", 
                                   "StraightMig2009"="Straightness",
                                   "HBDistMig2009"="HB Distance"))+
  ggtitle("2009")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.title.x=element_blank())+
  geom_hline(yintercept = 0.1, colour="red", lwd=1)+
  ylim(0.00, 0.3)
p1

#2020
set.seed(77)
seasonsMig2020k <- kmeans(cariwindow2020m, ClusterNum, iter.max = 100)
cl_kcca <- as.kcca(seasonsMig2020k, cariwindow2020m) 
FeatureImp_res <-FeatureImpCluster(cl_kcca,as.data.table(cariwindow2020m), biter=100)
summary(FeatureImp_res$misClassRate)
p2<-plot(FeatureImp_res)
p2<-p2+scale_x_discrete(labels = c("HerbsMig2020"="Herbs", 
                                   "WaterMig2020"="Water", "ShrubMig2020"="Shrub",
                                   "TreedConiferMig2020"="Treed Conifer", "SpeedMig2020"="Speed", 
                                   "StraightMig2020"="Straightness",
                                   "HBDistMig2020"="HB Distance"))+
  ggtitle("2020")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.title.x=element_blank())+
  geom_hline(yintercept = 0.1, colour="red", lwd=1)+
  #scale_y_continuous(labels = scales::label_number(accuracy = 0.01))
  ylim(0.00, 0.3)
p2

#2021
set.seed(77)
seasonsMig2021k <- kmeans(cariwindow2021m, ClusterNum, iter.max = 100)
cl_kcca <- as.kcca(seasonsMig2021k, cariwindow2021m) 
FeatureImp_res <-FeatureImpCluster(cl_kcca,as.data.table(cariwindow2021m), biter=100)
summary(FeatureImp_res$misClassRate)
p3<-plot(FeatureImp_res)
p3<-p3+scale_x_discrete(labels = c("HerbsMig2021"="Herbs", 
                                   "WaterMig2021"="Water", "ShrubMig2021"="Shrub",
                                   "TreedConiferMig2021"="Treed Conifer", "SpeedMig2021"="Speed", 
                                   "StraightMig2021"="Straightness",
                                   "HBDistMig2021"="HB Distance"))+
  ggtitle("2021")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.title.x=element_blank())+
  geom_hline(yintercept = 0.1, colour="red", lwd=1)+
  ylim(0.00, 0.3)
p3

#2022
set.seed(77)
seasonsMig2022k <- kmeans(cariwindow2022m, ClusterNum, iter.max = 100)
cl_kcca <- as.kcca(seasonsMig2022k, cariwindow2022m) 
FeatureImp_res <-FeatureImpCluster(cl_kcca,as.data.table(cariwindow2022m), biter=100)
summary(FeatureImp_res$misClassRate)
p4<-plot(FeatureImp_res, showPoints=F)
p4<-p4+scale_x_discrete(labels = c("HerbsMig2022"="Herbs", 
                                   "WaterMig2022"="Water", "ShrubMig2022"="Shrub",
                                   "TreedConiferMig2022"="Treed Conifer", "SpeedMig2022"="Speed", 
                                   "StraightMig2022"="Straightness",
                                   "HBDistMig2022"="HB Distance"))+
  ggtitle("2022")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.title.x=element_blank())+
  geom_hline(yintercept = 0.1, colour="red", lwd=1)+
  ylim(0.00, 0.3)
p4

#2023
set.seed(77)
seasonsMig2023k <- kmeans(cariwindow2023m, ClusterNum, iter.max = 100)
cl_kcca <- as.kcca(seasonsMig2023k, cariwindow2023m) 
FeatureImp_res <-FeatureImpCluster(cl_kcca,as.data.table(cariwindow2023m), biter=100)
summary(FeatureImp_res$misClassRate)
p5<-plot(FeatureImp_res, showPoints=F)
p5<-p5+scale_x_discrete(labels = c("HerbsMig2023"="Herbs", 
                                   "WaterMig2023"="Water", "ShrubMig2023"="Shrub",
                                   "TreedConiferMig2023"="Treed Conifer", "SpeedMig2023"="Speed", 
                                   "StraightMig2023"="Straightness",
                                   "HBDistMig2023"="HB Distance"))+
  ggtitle("2023")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.title.x=element_blank())+
  geom_hline(yintercept = 0.1, colour="red", lwd=1)+
  #scale_y_continuous(labels = scales::label_number(accuracy = 0.01))+
  ylim(0.00, 0.3)
p5

# Define a layout matrix with an empty cell
# Here, the top-right cell (row 1, column 2) will be empty
layout_matrix <- rbind(c(1, NA),
                       c(2, 3),
                       c(4, 5))

# Arrange the plots with the specified layout
grid.arrange(p1, p2, p3, p4, p5, layout_matrix = layout_matrix,
             bottom = textGrob("Misclassification Rate", gp = gpar(fontsize = 18, fontface = "bold")))



#What about for sedentary/boreal?
#2009
set.seed(77)
seasonsSed2009k <- kmeans(cariwindow2009s, ClusterNum, iter.max = 100)
cl_kcca <- as.kcca(seasonsSed2009k , cariwindow2009s) 
str(cariwindow2009s)
FeatureImp_res <-FeatureImpCluster(cl_kcca,as.data.table(cariwindow2009s), biter = 100)
summary(FeatureImp_res$misClassRate)
p6<-plot(FeatureImp_res)
p6<-p6+scale_x_discrete(labels = c("HerbsSed2009"="Herbs", 
                                   "WaterSed2009"="Water", "ShrubSed2009"="Shrub",
                                   "TreedConiferSed2009"="Treed Conifer", "SpeedSed2009"="Speed", 
                                   "StraightSed2009"="Straightness",
                                   "HBDistSed2009"="HB Distance"))+
  ggtitle("2009")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.title.x=element_blank())+
  geom_hline(yintercept = 0.1, colour="red", lwd=1)+
  ylim(0.00, 0.45)
p6

#2010
set.seed(77)
seasonsSed2010k <- kmeans(cariwindow2010s, ClusterNum, iter.max = 100)
cl_kcca <- as.kcca(seasonsSed2010k, cariwindow2010s) 
FeatureImp_res <-FeatureImpCluster(cl_kcca,as.data.table(cariwindow2010s), biter=100)
summary(FeatureImp_res$misClassRate)
p7<-plot(FeatureImp_res)
p7<-p7+scale_x_discrete(labels = c("HerbsSed2010"="Herbs", 
                                   "WaterSed2010"="Water", "ShrubSed2010"="Shrub",
                                   "TreedConiferSed2010"="Treed Conifer", "SpeedSed2010"="Speed", 
                                   "StraightSed2010"="Straightness",
                                   "HBDistSed2010"="HB Distance"))+
  ggtitle("2010")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.title.x=element_blank())+
  geom_hline(yintercept = 0.1, colour="red", lwd=1)+
  ylim(0.00, 0.45)
p7

#2011
set.seed(77)
seasonsSed2011k <- kmeans(cariwindow2011s, ClusterNum, iter.max = 100)
cl_kcca <- as.kcca(seasonsSed2011k, cariwindow2011s) 
FeatureImp_res <-FeatureImpCluster(cl_kcca,as.data.table(cariwindow2011s), biter=100)
summary(FeatureImp_res$misClassRate)
p8<-plot(FeatureImp_res)
p8<-p8+scale_x_discrete(labels = c("HerbsSed2011"="Herbs", 
                                   "WaterSed2011"="Water", "ShrubSed2011"="Shrub",
                                   "TreedConiferSed2011"="Treed Conifer", "SpeedSed2011"="Speed", 
                                   "StraightSed2011"="Straightness",
                                   "HBDistSed2011"="HB Distance"))+
  ggtitle("2011")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.title.x=element_blank())+
  geom_hline(yintercept = 0.1, colour="red", lwd=1)+
  #scale_y_continuous(labels = scales::label_number(accuracy = 0.01))+
  ylim(0.00, 0.45)
p8

#2012
set.seed(77)
seasonsSed2012k <- kmeans(cariwindow2012s, ClusterNum, iter.max = 100)
cl_kcca <- as.kcca(seasonsSed2012k, cariwindow2012s) 
FeatureImp_res<-FeatureImpCluster(cl_kcca,as.data.table(cariwindow2012s), biter=100)
summary(FeatureImp_res$misClassRate)
p9<-plot(FeatureImp_res)
p9<-p9+scale_x_discrete(labels = c("HerbsSed2012"="Herbs", 
                                   "WaterSed2012"="Water", "ShrubSed2012"="Shrub",
                                   "TreedConiferSed2012"="Treed Conifer", "SpeedSed2012"="Speed", 
                                   "StraightSed2012"="Straightness",
                                   "HBDistSed2012"="HB Distance"))+
  ggtitle("2012")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.title.x=element_blank())+
  geom_hline(yintercept = 0.1, colour="red", lwd=1)+
  ylim(0.00, 0.45)
p9

#2019
set.seed(77)
seasonsSed2019k <- kmeans(cariwindow2019s, ClusterNum, iter.max = 100)
cl_kcca <- as.kcca(seasonsSed2019k, cariwindow2019s) 
FeatureImp_res <-FeatureImpCluster(cl_kcca,as.data.table(cariwindow2019s), biter=100)
summary(FeatureImp_res$misClassRate)
p10<-plot(FeatureImp_res)
p10<-p10+scale_x_discrete(labels = c("HerbsSed2019"="Herbs", 
                                     "WaterSed2019"="Water", "ShrubSed2019"="Shrub",
                                     "TreedConiferSed2019"="Treed Conifer", "SpeedSed2019"="Speed", 
                                     "StraightSed2019"="Straightness",
                                     "HBDistSed2019"="HB Distance"))+
  ggtitle("2019")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.title.x=element_blank())+
  geom_hline(yintercept = 0.1, colour="red", lwd=1)+
  ylim(0.00, 0.45)
p10

#2020
set.seed(77)
seasonsSed2020k <- kmeans(cariwindow2020s, ClusterNum, iter.max = 100)
cl_kcca <- as.kcca(seasonsSed2020k, cariwindow2020s) 
FeatureImp_res <-FeatureImpCluster(cl_kcca,as.data.table(cariwindow2020s), biter = 100)
summary(FeatureImp_res$misClassRate)
p11<-plot(FeatureImp_res)
p11<-p11+scale_x_discrete(labels = c("HerbsSed2020"="Herbs", 
                                     "WaterSed2020"="Water", "ShrubSed2020"="Shrub",
                                     "TreedConiferSed2020"="Treed Conifer", "SpeedSed2020"="Speed", 
                                     "StraightSed2020"="Straightness",
                                     "HBDistSed2020"="HB Distance"))+
  ggtitle("2020")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.title.x=element_blank())+
  geom_hline(yintercept = 0.1, colour="red", lwd=1)+
  ylim(0.00, 0.45)
p11

#2021
set.seed(77)
seasonsSed2021k <- kmeans(cariwindow2021s, ClusterNum, iter.max = 100)
cl_kcca <- as.kcca(seasonsSed2021k, cariwindow2021s) 
FeatureImp_res <-FeatureImpCluster(cl_kcca,as.data.table(cariwindow2021s), biter=100)
summary(FeatureImp_res$misClassRate)
p12<-plot(FeatureImp_res)
p12<-p12+scale_x_discrete(labels = c("HerbsSed2021"="Herbs", 
                                     "WaterSed2021"="Water", "ShrubSed2021"="Shrub",
                                     "TreedConiferSed2021"="Treed Conifer", "SpeedSed2021"="Speed", 
                                     "StraightSed2021"="Straightness",
                                     "HBDistSed2021"="HB Distance"))+
  ggtitle("2021")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.title.x=element_blank())+
  geom_hline(yintercept = 0.1, colour="red", lwd=1)+
  ylim(0.00, 0.45)
p12

#2022
set.seed(77)
seasonsSed2022k <- kmeans(cariwindow2022s, ClusterNum, iter.max = 100)
cl_kcca <- as.kcca(seasonsSed2022k, cariwindow2022s) 
FeatureImp_res <-FeatureImpCluster(cl_kcca,as.data.table(cariwindow2022s), biter=100)
summary(FeatureImp_res$misClassRate)
p13<-plot(FeatureImp_res)
p13<-p13+scale_x_discrete(labels = c("HerbsSed2022"="Herbs", 
                                     "WaterSed2022"="Water", "ShrubSed2022"="Shrub",
                                     "TreedConiferSed2022"="Treed Conifer", "SpeedSed2022"="Speed", 
                                     "StraightSed2022"="Straightness",
                                     "HBDistSed2022"="HB Distance"))+
  ggtitle("2022")+
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 18),
        axis.title.x=element_blank())+
  geom_hline(yintercept = 0.1, colour="red", lwd=1)+
  #scale_y_continuous(labels = scales::label_number(accuracy = 0.01))+
  ylim(0.00, 0.45)
p13

grid.arrange(p6, p7, p8, p9, p10, p11, p12, p13, ncol=2,
             bottom = textGrob("Misclassification Rate", gp = gpar(fontsize = 18, fontface = "bold")))


##################################################################################################
#####End of code for Derbyshire et al. seasons manuscript (submitted September 2026) #############
##################################################################################################
