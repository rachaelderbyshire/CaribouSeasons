#Use moving windows to characterize space use

#code written by Rachael Derbyshire: rderbysh@uoguelph.ca
#UPDATED May 2026

#I would like to define season on an annual basis based on movement parameters and the space occupied by individuals
#To do this, use fuzzy clustering methods, as described by Basille et al.
#Using code described for package "seasonality"

#Here, I will use a moving window to characterize space use for each individual caribou-year

#Load workspace from previous step
load("Seasons_workspace.RData")

#Load required packages
library(amt)
library(memoise)
library(rlist)
library(runner)
library(terra)
library(tictoc)

#create functions to apply moving windows across each dataset----

#function to calculate speed across moving window of 15 days
speed_runner<-function(x, dates){#dates should be a vector of two 
  runner(x,
         k = "15 days",
         lag=1,
         idx = "single_date",
         #na_pad = T,
         at=seq(as.Date(dates[1]), as.Date(dates[2]), by = "1 day"),
         function(x) {mean(na.omit(speed(x)))}
  )
}

#to speed up calculation:
speed_runner_fast <- memoise(speed_runner)

#function to apply runner function across any time frame
apply_runner<-function(fnct_name, cari_list, dates, ...){
  list_temp<-list()
  names_temp<-vector()
  for(i in 1:length(cari_list)){
    list_temp[[i]]<-try(fnct_name(x=cari_list[[i]],dates=dates, ...))
    if (length(list_temp[[i]])==1){#i.e., there was an error
      list_temp[[i]]<-matrix(NA, 365, 1) #so I apply NAs across the dataset
    }else{}
    
    #make sure each element of this list has associated caribou id
    names_temp[i]<-unique(cari_list[[i]]$id)
    names(list_temp) <- names_temp
  }
  return(list_temp)
}

#function to range-standardize each individual-year measurement
min_max_scale <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#apply functions for speed window ----

#for sedentary dataset
tic()#to figure out how long this takes
for (j in 1:length(yearSED)){
  list_name<-paste("SpeedSed", yearSED[j], sep="")
  
  result<-apply_runner(fnct_name=speed_runner_fast,  
                       cari_list = SED_track_25hr,
                       dates=as.character(daterange_dfSED[j,]))
  result<-list.cbind(result)
  result<-min_max_scale(result)
  
  assign(list_name, result, envir = .GlobalEnv )
}
toc()#558.55 sec elapsed

#for migratory dataset
for (j in 1:length(yearMIG)){
  list_name<-paste("SpeedMig", yearMIG[j], sep="")
  
  result<-apply_runner(fnct_name=speed_runner_fast,  
                       cari_list = MIG_track_25hr,
                       dates=as.character(daterange_dfMIG[j,]))
  result<-list.cbind(result)
  result<-min_max_scale(result)
  
  assign(list_name, result, envir = .GlobalEnv )
}

#straightness ----

safe_straightness <- function(w) { #this function needed because of very annoying structure issues associated with straightness function in amt
  if (nrow(w) < 3) return(NA_real_)# Need at least 3 points for 2 movement steps
  
  s <- suppressWarnings(straightness(w))# Compute straightness
  if (is.list(s)) {
    s <- tryCatch(unlist(s), error = function(e) NA_real_)
  }# Defensive handling: if straightness returns a list
  s <- tryCatch(drop_units(s), error = function(e) s)# If still units, drop them
  s_num <- suppressWarnings(as.numeric(s))# Convert to numeric
  if (all(is.na(s_num))) return(NA_real_)# Some windows will produce all NA (e.g., no movement)
  mean(s_num, na.rm = TRUE)# Return mean straightness
}

straight_runner<-function(x, dates){
  runner(
    x,
    k = "15 days",
    lag = 1,
    idx = "single_date",
    at = seq(as.Date(dates[1]), as.Date(dates[2]), by = "1 day"),
    f = safe_straightness
  )
}

straight_test<-straight_runner(x=MIG_track_25hr[[8]], dates=c("2021-02-28", "2021-12-31"))
straight_test

#to speed up calculation:
straight_runner_fast <- memoise(straight_runner)

tic()#to figure out how long this takes
for (j in 1:length(yearSED)){
  list_name<-paste("StraightSed", yearSED[j], sep="")
  
  result<-apply_runner(fnct_name=straight_runner_fast, 
                       cari_list = SED_track_25hr,
                       dates=as.character(daterange_dfSED[j,]))
  result<-list.cbind(result)
  result<-min_max_scale(result)
  
  assign(list_name, result, envir = .GlobalEnv )
}
toc()#2848.06 sec elapsed

tic()#to figure out how long this takes
for (j in 1:length(yearMIG)){
  list_name<-paste("StraightMig", yearMIG[j], sep="")
  
  result<-apply_runner(fnct_name=straight_runner_fast,  
                       cari_list = MIG_track_25hr,
                       dates=as.character(daterange_dfMIG[j,]))
  result<-list.cbind(result)
  result<-min_max_scale(result)
  
  assign(list_name, result, envir = .GlobalEnv )
}
toc()#725.98 sec elapsed (4976.08 sec elapsed without memoise function)


#Incorporate habitat data------
#Basille et al. used proportion of locations in each habitat type: used five different habitat types

#SCANFI dataset
SCANFI_LCC<-rast("Datasets/Environment/SCANFI/SCANFI_att_nfiLandCover_SW_2020_v1.2.tif")
SCANFI_LCC
crs(SCANFI_LCC)

r <- terra::rast()
terra::ext(r) <- c(-5e+05, 1.5e+06, 6.5e+06, 8e+06)#order=xmin, xmax, ymin, ymax
SCANFI_crop<-terra::crop(SCANFI_LCC, r)
plot(SCANFI_crop)

tic()
SCANFI_proj<-project(SCANFI_crop, "EPSG:3978", method="near")#this takes a long time
toc()#3166.17 sec elapsed
plot(SCANFI_proj)#make sure it worked
crs(SCANFI_proj)

#change names of habitat types to remove space (makes things easier later)
cats(SCANFI_proj)

# Create a new data frame with desired changes
new_levels <- data.frame(id = c(1,2,3,4,5,6,7,8), label = c("Bryoid", "Herbs",
                                                            "Rock", "Shrub", 
                                                            "TreedBroadleaf", 
                                                            "TreedConifer", 
                                                            "TreedMixed", "Water"))

# Assign the new categories
levels(SCANFI_proj) <- new_levels

# View the updated categories
cats(SCANFI_proj)

#create function that extracts habitat value at each loc on a given day, then calculates proportion of each habitat
prop_function<-function(x,hab){#hab should be the habitat you are primarily interested in
  a<-extract_covariates(x,SCANFI_proj)
  a<-as.data.frame(a)
  a_sub<-subset(a, label==hab)
  PropA<-nrow(a_sub)/nrow(a)
  return(PropA)
}

#function to extract spatial covariates over moving window ----
spat_runner<-function(x, dates, hab){
  runner(x,
         k = "15 days",
         lag=1,
         idx = "single_date",
         at=seq(as.Date(dates[1]), as.Date(dates[2]), by = "1 day"),
         f=function(x){
           prop_function(x, hab)
         }
  )
}

#to speed up calculation:
spat_runner_fast <- memoise(spat_runner)


#function to apply this function to any years and any habitat
PropSeason<-function(x, dates, hab, names){
  df_temp<-rbindlist(x)
  names_temp<-unique(df_temp$id)
  spat_window<-list()
  for(i in 1:length(x)){
    spat_window[[i]]<-try(spat_runner_fast(x[[i]], 
                                           dates, 
                                           hab))
    if (length(spat_window[[i]])==1){
      spat_window[[i]]<-matrix(NA, 365, 1)
    }else{}
  }
  names(spat_window) <- names
  return(spat_window)
}

#Try using for loop to iteratively run through each month and habitat type

hab_type<-c("Herbs", "Shrub", "TreedConifer", "Water")

tic()
for(i in 1:length(hab_type)){
  for (j in 1:length(yearSED)){
    list_name<-paste(hab_type[i], "Sed", yearSED[j], sep="")
    
    result<-PropSeason(x=SED_track_25hr,
                       dates=as.character(daterange_dfSED[j,]),
                       hab = hab_type[i],
                       names=namesSED)
    result<-list.cbind(result)
    result<-min_max_scale(result)
    
    assign(list_name, result, envir = .GlobalEnv )
  }
}
toc()#19180.45 sec elapsed

for(i in 1:length(hab_type)){
  for (j in 1:length(yearMIG)){
    list_name<-paste(hab_type[i], "Mig", yearMIG[j], sep="")
    
    result<-PropSeason(x=MIG_track_25hr,
                       dates=as.character(daterange_dfMIG[j,]),
                       hab = hab_type[i],
                       names=namesMIG)
    result<-list.cbind(result)
    result<-min_max_scale(result)
    
    assign(list_name, result, envir = .GlobalEnv )
  }
}


#Distance from Hudson Bay Coast -----
DistMap_HB<-rast("Datasets/Environment/DistMap_HB.tif")
DistMap_HB_proj<-project(DistMap_HB, "EPSG:3978", method="near")

#make sure caribou points plot correctly on map
plot(DistMap_HB_proj)
MIG_track_df<-rbindlist(MIG_track_25hr)
points(MIG_track_df[,1:2])

SED_track_df<-rbindlist(SED_track_25hr)
points(SED_track_df[,1:2], col="red")

extract_dist_mean<-function(x,map){
  a<-extract_covariates(x,map)
  a<-as.data.frame(a)
  a_mean<-mean(na.omit(a$DistMap_HB))
  return(a_mean)
}

spat_runner2<-function(x, dates, map){
  runner(x,
         k = "15 days",
         lag=1,
         idx = "single_date",
         at=seq(as.Date(dates[1]), as.Date(dates[2]), by = "1 day"),
         function(x) {extract_dist_mean(x, map)}
  )
}

View(SED_track_25hr[[8]])
HBD_test<-spat_runner2(x=SED_track_25hr[[8]], dates=c("2023-02-26", "2023-12-31"),
                       map=DistMap_HB_proj)
View(HBD_test)
mean(na.omit(HBD_test))/1000

#to speed up calculation:
spat_runner2_fast <- memoise(spat_runner2)

tic()
for (j in 1:length(yearSED)){
  list_name<-paste("HBDistSed", yearSED[j], sep="")
  
  result<-apply_runner(fnct_name=spat_runner2_fast,  
                       cari_list = SED_track_25hr,
                       dates=as.character(daterange_dfSED[j,]), 
                       map=DistMap_HB_proj)
  result<-list.cbind(result)
  result<-min_max_scale(result)
  
  assign(list_name, result, envir = .GlobalEnv )
}
toc()#1287.28 sec elapsed

for (j in 1:length(yearMIG)){
  list_name<-paste("HBDistMig", yearMIG[j], sep="")
  
  result<-apply_runner(fnct_name=spat_runner2_fast, 
                       cari_list = MIG_track_25hr,
                       dates=as.character(daterange_dfMIG[j,]), 
                       map=DistMap_HB_proj)
  result<-list.cbind(result)
  result<-min_max_scale(result)
  
  assign(list_name, result, envir = .GlobalEnv )
}

save.image(file = "Seasons_workspace.RData")
#########
