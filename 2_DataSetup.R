#Set up data for seasons analysis

#code written by Rachael Derbyshire: rderbysh@uoguelph.ca
#2025-2026

#I would like to define season on an annual basis based on movement parameters and the space occupied by individuals
#To do this, use fuzzy clustering methods, as described by Basille et al.
#Using code described for package "seasonality"

#First, I need to set up my data for the analysis

#Load packages
library(amt)
library(data.table)
library(dplyr)
library(sf)
library(terra)

#Define project CRS (Latitude/Longitude) used for Parturition analysis
projcrs_part<-crs("+proj=longlat +datum=WGS84 +no_defs")

#Define new CRS that's better for calculating distance
projcrs_ssn<-3978

#Set up data----
dat_all_eco<-fread("Results/dat_all_eco_04Aug2026.csv")
head(dat_all_eco)

#re-name confusing columns
dat_all_eco <- rename(dat_all_eco, Longitude = x_.x, Latitude = y_.x, Date_Time = t_.x)

#remove ambiguous individuals
dat_all_eco_cl<-subset(dat_all_eco, FID.x!="CMS019")
dat_all_eco_cl<-subset(dat_all_eco_cl, FID.x!="CMS027")

#split by ecotype
dat_all_MIG<-subset(dat_all_eco_cl, Ecotype=="MIG")
head(dat_all_MIG)
length(unique(dat_all_MIG$FID_Year))
dat_all_SED<-subset(dat_all_eco_cl, Ecotype=="SED")
length(unique(dat_all_SED$FID_Year))

#split by caribou ID
#NOTE: this is different from calving analysis, where I split by caribou-year
MIG_list<-split(dat_all_MIG, dat_all_MIG$FID.x)
length(MIG_list)
SED_list<-split(dat_all_SED, dat_all_SED$FID.x)
length(SED_list)

#convert data to a track
track_func<-function(x){
  stopifnot(class(x) == "list")
  track_list<-list()
  for(i in 1:length(x)){
    df<-x[[i]]
    df <- df[order(df$Date_Time),]
    track_list[[i]]<-try(track(df$Longitude, df$Latitude, #use long and lat from original data (as opposed to x_y. and y_y., which are left over from merging with the parturition dataset)
                               df$Date_Time, 
                               id=df$FID.x,
                               single_date=df$single_date.x,
                               crs=st_crs(projcrs_part))) 
  }
  return(track_list)
}

MIG_track_list<-track_func(MIG_list)
SED_track_list<-track_func(SED_list)

#convert to different crs for (better for distance analysis)
for(i in 1:length(MIG_track_list)){
  MIG_track_list[[i]]<-transform_coords(MIG_track_list[[i]], crs_to = st_crs(projcrs_ssn))
}

for(i in 1:length(SED_track_list)){
  SED_track_list[[i]]<-transform_coords(SED_track_list[[i]], crs_to = st_crs(projcrs_ssn))
}

#subsample to longest sampling interval
MIG_track_25hr<-list()
for(i in 1:length(MIG_track_list)){
  MIG_track_25hr[[i]]<-track_resample(MIG_track_list[[i]], rate=hours(25), 
                                      tolerance=minutes(150))
}
rm(MIG_track_list)#so I don't accidentally use the wrong dataset

SED_track_25hr<-list()
for(i in 1:length(SED_track_list)){
  SED_track_25hr[[i]]<-track_resample(SED_track_list[[i]], rate=hours(25), 
                                      tolerance=minutes(150))
}
rm(SED_track_list)

#How many GPS points across how many individuals?
data_25hr_all<-rbind(rbindlist(MIG_track_25hr), rbindlist(SED_track_25hr))
nrow(data_25hr_all)
length(table(data_25hr_all$id))


#names, dates, and year vectors needed for analysis-----
namesSED<-vector()
for(i in 1:length(SED_track_25hr)){namesSED[i]<-unique(SED_track_25hr[[i]]$id)}
names(SED_track_25hr) <- sapply(SED_track_25hr, function(x) unique(x$id))

namesMIG<-vector()
for(i in 1:length(MIG_track_25hr)){namesMIG[i]<-unique(MIG_track_25hr[[i]]$id)}
names(MIG_track_25hr) <- sapply(MIG_track_25hr, function(x) unique(x$id))

#How many years of data for each ecotype?
datetable_SED<-matrix(NA, length(SED_track_25hr), 2)
for(i in 1:length(SED_track_25hr)){
  datetable_SED[i, 1]<-as.character(min(SED_track_25hr[[i]]$single_date))
  datetable_SED[i, 2]<-as.character(max(SED_track_25hr[[i]]$single_date))
}
datetable_SED<-as.data.frame(datetable_SED)
sort(datetable_SED$V1)#no start dates 2014-2018
sort(datetable_SED$V2)#no end dates 2014-2018

datetable_MIG<-matrix(NA, length(MIG_track_25hr), 2)
for(i in 1:length(MIG_track_25hr)){
  datetable_MIG[i, 1]<-as.character(min(MIG_track_25hr[[i]]$single_date))
  datetable_MIG[i, 2]<-as.character(max(MIG_track_25hr[[i]]$single_date))
}
datetable_MIG<-as.data.frame(datetable_MIG)
sort(datetable_MIG$V1)#no start dates 2011-2018
sort(datetable_MIG$V2)#no end dates 2011-2018

#Based on this, create year and date vectors for each ecotype
#for SED
yearSED<-c("2009", "2010", "2011", "2012", "2013", "2019", "2020", "2021", 
           "2022", "2023")
daterangeSED<-c(c("2009-07-01", "2010-06-30"), c("2010-07-01", "2011-06-30"),
                c("2011-07-01", "2012-06-30"), c("2012-07-01", "2013-06-30"),
                c("2013-07-01", "2014-06-30"), 
                c("2019-07-01", "2020-06-30"), c("2020-07-01", "2021-06-30"),
                c("2021-07-01", "2022-06-30"), c("2022-07-01", "2023-06-30"),
                c("2023-07-01", "2024-06-30"))
daterange_dfSED<-as.data.frame(matrix(data=daterangeSED, nrow=10, ncol=2, byrow=T))
daterange_dfSED

#for MIG
yearMIG<-c("2009", "2010", "2019", "2020", "2021", 
           "2022", "2023")
daterangeMIG<-c(c("2009-07-01", "2010-06-30"), c("2010-07-01", "2011-06-30"),
                c("2019-07-01", "2020-06-30"), c("2020-07-01", "2021-06-30"),
                c("2021-07-01", "2022-06-30"), c("2022-07-01", "2023-06-30"),
                c("2023-07-01", "2024-06-30"))
daterange_dfMIG<-as.data.frame(matrix(data=daterangeMIG, nrow=7, ncol=2, byrow=T))
daterange_dfMIG

#save R environment for next step
save.image(file = "Seasons_workspace.RData")
