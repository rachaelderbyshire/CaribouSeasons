#K-means clustering: determine optimal number of clusters##############

#code written by Rachael Derbyshire: rderbysh@uoguelph.ca
#2025-2026

#I would like to define season on an annual basis based on movement parameters and the space occupied by individuals
#To do this, use fuzzy clustering methods, as described by Basille et al.
#Using code described for package "seasonality"

#Here, I will determine the optimal number of clusters for cluster analysis

#Load workspace from previous step
load("Seasons_workspace.RData")

#Load required packages
library(dplyr)
library(seasonality)

#create list for each year and ecotype with all runner data
List2009s<-mget(grep('Sed2009', names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE)) 
List2010s<-mget(grep('Sed2010', names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE)) 
List2011s<-mget(grep('Sed2011', names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE)) 
List2012s<-mget(grep('Sed2012', names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE))
List2013s<-mget(grep('Sed2013', names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE))
List2019s<-mget(grep('Sed2019', names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE))
List2020s<-mget(grep('Sed2020', names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE))
List2021s<-mget(grep('Sed2021', names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE)) 
List2022s<-mget(grep('Sed2022', names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE)) 
List2023s<-mget(grep('Sed2023', names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE)) 

List2009m<-mget(grep('Mig2009',names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE)) 
List2010m<-mget(grep('Mig2010', names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE)) 
List2019m<-mget(grep('Mig2019', names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE))
List2020m<-mget(grep('Mig2020',names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE)) 
List2021m<-mget(grep('Mig2021',names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE))
List2022m<-mget(grep('Mig2022',names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE)) 
List2023m<-mget(grep('Mig2023',names(which(unlist(eapply(.GlobalEnv,is.matrix)))), 
                     value = TRUE)) 


#function to calculate mean values for each ecotype-year

mean_function<-function(EcoYearList, daterange){
  result<-matrix(NA, 365, 7)
  result<-as.data.frame(result)
  for(i in 1:length(EcoYearList)){
    result[,i]<-rowMeans(EcoYearList[[i]][1:365,], na.rm = T)#take only first 365 days of eco-year (remove last day if leap year)
    names(result)[i]<-names(EcoYearList)[i]
  }
  
  result<-as.data.frame(apply(result, 2, min_max_scale))#range-standardize across all measurements
  
  #put data in order from Jan to Dec (needed for functions in seasonality package)
  result$single_date<-seq(as.Date(daterange),by = "1 day", length.out=365)
  result$month_day<-format(result$single_date, "%m-%d")
  
  result<- result[order(result$month_day),]
  row.names(result)<-1:365
  
  return(result)
}

daterange_dfSED

all_mean2009s<-mean_function(List2009s, daterange = daterange_dfSED[1,1])
all_mean2010s<-mean_function(List2010s, daterange = daterange_dfSED[2,1])
all_mean2011s<-mean_function(List2011s, daterange = daterange_dfSED[3,1])
all_mean2012s<-mean_function(List2012s, daterange = daterange_dfSED[4,1])
all_mean2013s<-mean_function(List2013s, daterange = daterange_dfSED[5,1])
all_mean2019s<-mean_function(List2019s, daterange = daterange_dfSED[6,1])
all_mean2020s<-mean_function(List2020s, daterange = daterange_dfSED[7,1])
all_mean2021s<-mean_function(List2021s, daterange = daterange_dfSED[8,1])
all_mean2022s<-mean_function(List2022s, daterange = daterange_dfSED[9,1])
all_mean2023s<-mean_function(List2023s, daterange = daterange_dfSED[10,1])

#Data checks: make sure there is adequate data across entire ecological year
View(all_mean2013s)#check each one to make sure there is data
summary(List2013s$StraightSed2013[300:365,])#these data don't extend the entire ecological year, which causes problems when trying to compute the gap statistic (below)
#plus it probably doesn't make sense to include them if they don't span the entire year
#so I will not include 2013 SED in subsequent analyses
View(all_mean2019s)
summary(List2019s$StraightSed2019[1:50,])#check there are multiple individuals from beginning of year
View(all_mean2023s)
summary(List2023s$StraightSed2023[360:365,])#still some individuals with data at end of this year (but very few);
#this causes issues with clustering later on, and may not be a justifiable sample size
#so, I will exclude

#put together in a list (excluding 2013 and 2023 due to sample size issues)
all_meanListSed<-list(all_mean2009s, all_mean2010s, all_mean2011s, all_mean2012s, 
                      all_mean2019s, all_mean2020s, all_mean2021s, all_mean2022s)
head(all_meanListSed[[2]])

#repeat for migratory
daterange_dfMIG
all_mean2009m<-mean_function(List2009m, daterange = daterange_dfMIG[1,1])
all_mean2010m<-mean_function(List2010m, daterange = daterange_dfMIG[2,1])
all_mean2019m<-mean_function(List2019m, daterange = daterange_dfMIG[3,1])
all_mean2020m<-mean_function(List2020m, daterange = daterange_dfMIG[4,1])
all_mean2021m<-mean_function(List2021m, daterange = daterange_dfMIG[5,1])
all_mean2022m<-mean_function(List2022m, daterange = daterange_dfMIG[6,1])
all_mean2023m<-mean_function(List2023m, daterange = daterange_dfMIG[7,1])

#data checks
summary(all_mean2023m)#check each one to make sure there is data
View(List2009m$StraightMig2009)
summary(all_mean2010m)#these data don't extend the entire ecological year, which causes problems when trying to compute the gap statistic (below)
#plus it probably doesn't make sense to include them if they don't span the entire year
#so I will not include 2010MIG in subsequent analyses
View(List2019m$StraightMig2019)#Very few individuals extend entire eco year:
#this causes issues with clustering later on, and may not be a justifiable sample size
#so, I will exclude
View(List2023m$StraightMig2023)

#put in list (excluding 2010 and 2019 due to sample size issues)
all_meanListMig<-list(all_mean2009m, all_mean2020m, all_mean2021m, 
                      all_mean2022m, all_mean2023m)


#Create dataframe to calculate optimal number of clusters for each year
GG_Sed<-matrix(NA, 8, 2)
for(i in 1:length(all_meanListSed)){
  GG1 <- gap(all_meanListSed[[i]][,c(1:7)])
  GGtemp<-GG1%>%
    slice_max(DDk, n = 1)
  GG_Sed[i,1]<-GGtemp$nCluster
}#warning message for one year, but still produces an estimate. Since I am averaging estimates later, I will proceed for now

GG_Sed[,2]<-c(2009, 2010, 2011, 2012, 2019, 2020, 2021, 2022)
GG_Sed

GG_Mig<-matrix(NA, 5, 2)
for(i in 1:length(all_meanListMig)){
  GG1 <- try(gap(all_meanListMig[[i]][,c(1:7)]))
  GGtemp<-try(GG1%>%
                slice_max(DDk, n = 1))
  GG_Mig[i,1]<-try(GGtemp$nCluster)
}

GG_Mig[,2]<-c(2009, 2020, 2021, 2022, 2023)
GG_Mig

#mean cluster number for each ecotype
mean(GG_Sed[,1])#4.375
mean(GG_Mig[,1])#2.6
mean(c(GG_Sed[,1], GG_Mig[,1]))#3.69
#so I will use four for subsequent analyses

ClusterNum<-4

save.image(file = "Seasons_workspace.RData")
#########