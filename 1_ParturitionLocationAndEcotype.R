#This code is to extract parturition location for Ontario caribou
#And then determine ecotype based on calving location (as per Pond et al. 2016)
#Code written March 2025 by Rachael Derbyshire (rderbysh@uoguelph.ca)

#load libraries----
library(amt)
library(data.table)
library(dplyr)
library(ggplot2)
library(raster)
library(sf)
library(stringr)
library(terra)
library(tidyverse)

#Define project CRS (Latitude/Longitude) used for Parturition analysis
projcrs_part<-crs("+proj=longlat +datum=WGS84 +no_defs")

#Define new CRS for any distance calculations
#USE THIS for analysis, just make sure all data are successfully projected into this CRS
projcrs_ssn<-"EPSG:3978"

#datasets for analysis----

#Parturition results
Par_Results<-read.csv("Datasets/ParResults2_06Mar2026.csv")
head(Par_Results)

#Caribou location data
dat_all_cl<-fread("Datasets/dat_all_cl_04Mar2026.csv")
head(dat_all_cl)

#Province boundary (not actually needed, useful for visualizing data)
ON_path <- "Datasets/Environment/Province/Province.shp"
ON_shp <- st_read(ON_path)
plot(st_geometry(ON_shp))
crs(ON_shp)
ON_shp<-st_transform(ON_shp, crs=projcrs_ssn)

#Ecoregions
Eco_path <- "Datasets/Environment/Ecoregions/ecoregions.shp"
Ecoregions <- vect(Eco_path)#used terra to read this to it is easier to use in amt
Ecoregions<-project(Ecoregions, projcrs_ssn)
plot(Ecoregions)
plot(st_geometry(ON_shp), add=T, col="red")#make sure these line up


#Create dataset of caribou that calved----
head(Par_Results)
Par_Results_calved<-subset(Par_Results, BM>0)#BM=0 means the "no change" movement model fit best
length(unique(Par_Results_calved$unique.x.FID.))

#Merge this dataset with GPS locs based on time/date stamp----
str(dat_all_cl)
dat_all_cl$fullTime<-as.POSIXct(dat_all_cl$t_, 
                                       format = "%Y-%m-%d %H:%M:%S",
                                       tz="UTC")
str(Par_Results_calved)
Par_Results_calved$fullTime<-as.POSIXct(Par_Results_calved$BP1c, #BP1 = Breakpoint 1 (first breakpoint in movement model)
                                       format = "%Y-%m-%d %H:%M",
                                       tz="UTC")
head(Par_Results_calved)

#Create FID_Year column for both datasets
#new time columns
dat_all_cl$single_date <- as.Date(dat_all_cl$t_)
dat_all_cl$Year <- format(dat_all_cl$single_date, "%Y")
dat_all_cl$FID_Year<-paste(dat_all_cl$FID, dat_all_cl$Year, sep = "_")
head(dat_all_cl)

Par_Results_calved$FID_Year<-paste(Par_Results_calved$unique.x.FID., 
                                   Par_Results_calved$unique.x.Year., 
                                   sep = "_")
head(Par_Results_calved)

#Create matching time columns for ParResults to match with larger location df
Par_Results_calved$single_date<-as.Date(Par_Results_calved$BP1c)

#Use inner join to join datasets
Par_Results_loc<-Par_Results_calved %>%
  left_join(dat_all_cl, by = 'FID_Year', suffix = c('.1', '.2')) %>%
  group_by(FID_Year) %>%
  filter(abs(single_date.1 - single_date.2) == min(abs(single_date.1 - single_date.2)))#this just ensures that I don't get every possible combination of dates from both datasets 
nrow(Par_Results_loc)

#Some will be missing from final dataset because times in dates are missing
#Pull out NAs to filter separately
Par_Results_NA<-subset(Par_Results_loc, is.na(fullTime.1)==T)

#For those that had NAs in fullTime, select single point for that day
Par_Results_NA<-Par_Results_NA%>%distinct(FID_Year, .keep_all = T)
nrow(Par_Results_NA)

#Filter the rest by fulltime so I have only one point per day
Par_Results_loc<-Par_Results_loc %>%
  filter(abs(fullTime.1 - fullTime.2) == min(abs(fullTime.1 - fullTime.2)))#take the time from dat_all_cl that is closest to the birth time in ParResult 
nrow(Par_Results_loc)

#Combine the two
Par_Results_loc<-rbind(Par_Results_loc, Par_Results_NA)
length(unique(Par_Results_loc$FID_Year))

Par_Results_loc<-ungroup(Par_Results_loc)
summary(Par_Results_loc)
write.csv(Par_Results_loc, "Results/Par_Results_loc_06Mar2026.csv", row.names = F)


#Plot this on a map!!!----
#Not necessarily needed, just for data visualization and exploration
Par_Results_loc.sf<-st_as_sf(Par_Results_loc, coords=c("x_", "y_"), 
                                     crs=projcrs_part)

#reproject into new CRS (better for subsequent analysis)
Par_Results_loc.sf<-st_transform(Par_Results_loc.sf, projcrs_ssn)
str(Par_Results_loc.sf)
crs(Par_Results_loc.sf)

plot(st_geometry(ON_shp))
plot(st_geometry(Par_Results_loc.sf), add=T)


#Extract ecoregion----

#first, convert dataset to a track
head(Par_Results_loc)
Par_Results_tr<-make_track(Par_Results_loc, x_, y_, fullTime.2, #This is the time from dat_all_cl (associated with GPS point)
                           crs=st_crs(projcrs_part),
                           FID=FID, Year=Year, single_date=single_date.1, region=region,
                           BM=BM)
head(Par_Results_tr)

#re-project into new CRS (better for doing distance calculations)
Par_Results_tr<-transform_coords(Par_Results_tr, crs_to = st_crs(3978))
get_crs(Par_Results_tr)

#crop ecoregions so it is just a little bigger than caribou extent
#then rasterize so I can use "extract_covariates" function in amt
summary(Par_Results_tr)
r<-rast(xmin=51000, xmax=1204000, ymin=131000, ymax=775000)
Ecoregions.r<-terra::rasterize(Ecoregions, r, field="ECOREGION")
Ecoregions.r
plot(Ecoregions.r)
points(Par_Results_tr)

#Extract ecoregions
Par_Results_tr<-Par_Results_tr|>extract_covariates(Ecoregions.r)
summary(Par_Results_tr)

#Merge ecoregion names
Ecoregion_table<-as.data.frame(Ecoregions)
head(Ecoregion_table)
table(Par_Results_tr$ECOREGION)

Ecoregion_table2<-Ecoregion_table[,5:6]#select only ECOREGION and REGION_NAM
Ecoregion_table2<-unique(Ecoregion_table2)
Ecoregion_table2

Par_Results_tr<-Par_Results_tr|>left_join(Ecoregion_table2, by="ECOREGION")
Par_Results_tr$REGION_NAM<-as.factor(Par_Results_tr$REGION_NAM)
levels(Par_Results_tr$REGION_NAM)

#Define ecotype based on parturition location ----
#if parturition occurred in Hudson Bay coast or lowlands: ecotype = migratory
#otherwise, sedentary

Par_Results_tr$Ecotype<-NA
for(i in 1:nrow(Par_Results_tr)){
  if (Par_Results_tr$REGION_NAM[i] == "Coastal Hudson Bay Lowland"){Par_Results_tr$Ecotype[i]<-"MIG"}
  else if (Par_Results_tr$REGION_NAM[i] == "Hudson Bay Lowland"){Par_Results_tr$Ecotype[i]<-"MIG"}
  else {Par_Results_tr$Ecotype[i]<-"SED"}
}

str(Par_Results_tr)

ggplot(data = Par_Results_tr, aes(x = x_, y = y_)) +
  geom_point(aes(color = Ecotype)) +
  theme_bw()

plot(Ecoregions.r)
plot(Ecoregions, add=T)
points(Par_Results_tr,
     col=as.numeric(as.factor(Par_Results_tr$Ecotype)),
     pch=19) 
head(Par_Results_tr)
write.csv(Par_Results_tr, "Results/Par_Results_tr_09Apr2026.csv")#this has parturition locs and ecotypes

#Any switchers? ----
Switchers<-table(Par_Results_tr$FID, Par_Results_tr$Ecotype)
Switchers<-data.frame(rbind(Switchers))
Switchers$Switched<-Switchers$MIG*Switchers$SED #any "switched" column that != 0 indicates a switch
subset(Switchers, Switched>0)
#Two switchers: CMS019 and CMS027

points(subset(Par_Results_tr, FID=="CMS019"),
       col="lightgreen",
       pch=15)

points(subset(Par_Results_tr, FID=="CMS027"),
       col="violet",
       pch=15)


#distance between parturition locations for switchers
CMS019_par<-subset(Par_Results_tr, FID=="CMS019")
tot_dist(CMS019_par[1:2,])/1000#divide by 1000 to get km
tot_dist(CMS019_par[2:3,])/1000
tot_dist(CMS019_par[1:3,])/1000

CMS027_par<-subset(Par_Results_tr, FID=="CMS027")
tot_dist(CMS027_par)/1000

#Add ecotype designation to larger dataset ----
head(dat_all_cl)
head(Par_Results_tr)

Par_Results_tr$FID_Year<-paste(Par_Results_tr$FID, Par_Results_tr$Year,
                                  sep="_")#so I have a column to join with

dat_all_eco<-left_join(dat_all_cl, Par_Results_tr, by="FID_Year")
head(dat_all_eco)
summary(dat_all_eco)
dat_all_eco<-dat_all_eco[!is.na(dat_all_eco$Ecotype),]#remove caribou that did not receive ecotype designation (because no calving)
length(unique(dat_all_eco$FID_Year))#same number as Par_Results_calved (therefore we got all caribou that calved)

#save the file
#save time stamps as.character so they save properly
dat_all_eco$fullTime<-as.character(format(dat_all_eco$fullTime))

write.csv(dat_all_eco, "Results/dat_all_eco_06Mar2026.csv", row.names = F)


#First day of calving for each ecotype ----
#create dataframe with min value for each year for seasonality analysis
SedCalving<-matrix(NA, 8, 2)
SedCalving[,1]<-c(2009, 2010, 2011, 2012, 2019, 2020, 2021, 2022)
SedCalving[1,2]<-as.character(as.POSIXct(min(subset(Par_Results_tr, Year == 2009 & Ecotype == "SED")$single_date)))
SedCalving[2,2]<-as.character(as.POSIXct(min(subset(Par_Results_tr, Year == 2010 & Ecotype == "SED")$single_date)))
SedCalving[3,2]<-as.character(as.POSIXct(min(subset(Par_Results_tr, Year == 2011 & Ecotype == "SED")$single_date)))
SedCalving[4,2]<-as.character(as.POSIXct(min(subset(Par_Results_tr, Year == 2012 & Ecotype == "SED")$single_date)))
SedCalving[5,2]<-as.character(as.POSIXct(min(subset(Par_Results_tr, Year == 2019 & Ecotype == "SED")$single_date)))
SedCalving[6,2]<-as.character(as.POSIXct(min(subset(Par_Results_tr, Year == 2020 & Ecotype == "SED")$single_date)))
SedCalving[7,2]<-as.character(as.POSIXct(min(subset(Par_Results_tr, Year == 2021 & Ecotype == "SED")$single_date)))
SedCalving[8,2]<-as.character(as.POSIXct(min(subset(Par_Results_tr, Year == 2022 & Ecotype == "SED")$single_date)))
SedCalving

SedCalving<-as.data.frame(SedCalving)
names(SedCalving)<-c("Year", "CalvingStart")
SedCalving$CalvingStart<-as.POSIXct(SedCalving$CalvingStart)
str(SedCalving)
write.csv(SedCalving, "Results/SedCalvingStart.csv", row.names = F)

MigCalving<-matrix(NA, 5, 2)
MigCalving[,1]<-c(2009, 2020, 2021, 2022, 2023)
MigCalving[1,2]<-as.character(as.POSIXct(min(subset(Par_Results_tr, Year == 2009 & Ecotype == "MIG")$single_date)))
MigCalving[2,2]<-as.character(as.POSIXct(min(subset(Par_Results_tr, Year == 2020 & Ecotype == "MIG")$single_date)))
MigCalving[3,2]<-as.character(as.POSIXct(min(subset(Par_Results_tr, Year == 2021 & Ecotype == "MIG")$single_date)))
MigCalving[4,2]<-as.character(as.POSIXct(min(subset(Par_Results_tr, Year == 2022 & Ecotype == "MIG")$single_date)))
MigCalving[5,2]<-as.character(as.POSIXct(min(subset(Par_Results_tr, Year == 2023 & Ecotype == "MIG")$single_date)))
MigCalving

MigCalving<-as.data.frame(MigCalving)
names(MigCalving)<-c("Year", "CalvingStart")
MigCalving$CalvingStart<-as.POSIXct(MigCalving$CalvingStart)
str(MigCalving)
write.csv(MigCalving, "Results/MigCalvingStart.csv", row.names = F)

#General parturition statistics ----
#Not necessarily needed, just for data exploration
Par_Results_loc$fullTime.2<-as.POSIXct(Par_Results_loc$fullTime.2, 
                                       format = "%Y-%m-%d %H:%M:%S",
                                       tz="UTC")#This is the column that has no NAs
Par_Results_loc$single_date.1 <- as.Date(Par_Results_loc$single_date.1)
Par_Results_loc$MonthDay <- format(Par_Results_loc$single_date.1, "%m-%d")
Par_Results_loc$MonthDay<-as.POSIXct(Par_Results_loc$MonthDay, 
                                     format = "%m-%d",
                                     tz="UTC")
head(Par_Results_loc)
hist(Par_Results_loc$MonthDay, breaks=60)

#how much earlier than average did CMS027 have her calf in 2020?
#calculate percentile of observation
ecdf_func <- ecdf(Par_Results_loc$MonthDay)
percentile_rank <- ecdf_func(Par_Results_loc$MonthDay[325]) * 100

#mean number of births per individual
ParMean<-Par_Results_calved %>%
  group_by(unique.x.FID.) %>%
  summarise(n_events = n(), .groups = "drop") %>%
  summarise(
    mean_events = mean(n_events),
    se_events = sd(n_events) / sqrt(n())
  )

#####End of Main Script#########
