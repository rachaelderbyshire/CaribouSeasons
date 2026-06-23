#Assign behaviour data to season#######################

#code written by Rachael Derbyshire: rderbysh@uoguelph.ca
#2025-2026

#I would like to define season on an annual basis based on movement parameters and the space occupied by individuals
#Using code described for package "seasonality"

#Here, I will assign season based on cluster analysis

#Load workspace from previous step
load("Seasons_workspace.RData")

#Load required packages
library(dplyr)
library(janitor)
library(seasonality)

#I need a df with every individual that has data
#and put in order from Jan to Dec
#NOTE: as of Sept 2025, I have removed Treed Mixed from the analysis because,
#at the 25 hour sample rate, too many caribou did not use this habitat
#which meant that the cluster analysis did not always work

#create list with movement and spatial parameters
CleanParamList<-function(x, daterange){
  x_cl<- x[1:365,] %>% #remove final day if it is a leap year
    as.data.frame(x)%>%
    mutate_all(~replace(., is.nan(.), NA))%>%
    remove_empty(., which = "cols") %>%#remove individuals with no data
    mutate("single_date"=seq(as.Date(daterange), by = "1 day", length.out=365))%>%
    mutate("month_day"=format(single_date, "%m-%d"))%>%
    arrange(month_day)%>%
    dplyr::select (!c(single_date, month_day))
  
  return(x_cl)
}

CariMoveList<-function(Params, daterange){
  Cleaned_params<-list()
  for(i in 1:length(Params)){
    Cleaned_params[[i]]<-CleanParamList(x=Params[[i]], daterange)
  }
  names(Cleaned_params)<-c("Speed", "Straight", "Herbs", "Shrub", "Conifer",
                           "Water", "DistFromHB")#make sure you input list in this order
  
  #some individuals do not have all seven "behaviours" (i.e., speed, straightness, etc.)
  #so I need to select only the columns/individuals that have all seven
  num_cols <- lapply(Cleaned_params, ncol)#how many columns for each "behaviour"
  min_cols <- min(unlist(num_cols))#find the minimum number of columns
  index_min_cols <- which(unlist(num_cols) == min_cols)#Find the index of the element(s) with the minimum number of columns
  element_with_fewest_cols <- Cleaned_params[[index_min_cols[1]]] # Select the first element if multiple exist
  names_to_select <- names(element_with_fewest_cols)#the names (IDs) of caribou with data for all seven "behaviours"
  
  for(i in 1:length(Cleaned_params)){
    Cleaned_params[[i]]<-Cleaned_params[[i]][names_to_select]#select only the caribou that have data for all seven "behaviours"
  }
  
  return(Cleaned_params)
}

GG_Sed#which years do I have data for?
daterange_dfSED#make sure to select the correct daterange for each year

carimove_cl2009s<-CariMoveList(Params=list(SpeedSed2009, StraightSed2009, HerbsSed2009,
                                           ShrubSed2009, TreedConiferSed2009,
                                           WaterSed2009, HBDistSed2009),
                               daterange = daterange_dfSED[1,1])
carimove_cl2010s<-CariMoveList(Params=list(SpeedSed2010, StraightSed2010, HerbsSed2010,
                                           ShrubSed2010, TreedConiferSed2010,
                                           WaterSed2010, HBDistSed2010),
                               daterange = daterange_dfSED[2,1])
carimove_cl2011s<-CariMoveList(Params=list(SpeedSed2011, StraightSed2011, HerbsSed2011,
                                           ShrubSed2011, TreedConiferSed2011,
                                           WaterSed2011, HBDistSed2011),
                               daterange = daterange_dfSED[3,1])
carimove_cl2012s<-CariMoveList(Params=list(SpeedSed2012, StraightSed2012, HerbsSed2012,
                                           ShrubSed2012, TreedConiferSed2012,
                                           WaterSed2012, HBDistSed2012),
                               daterange = daterange_dfSED[4,1])
carimove_cl2019s<-CariMoveList(Params=list(SpeedSed2019, StraightSed2019, HerbsSed2019,
                                           ShrubSed2019, TreedConiferSed2019,
                                           WaterSed2019, HBDistSed2019),
                               daterange = daterange_dfSED[6,1])
carimove_cl2020s<-CariMoveList(Params=list(SpeedSed2020, StraightSed2020, HerbsSed2020,
                                           ShrubSed2020, TreedConiferSed2020,
                                           WaterSed2020, HBDistSed2020),
                               daterange = daterange_dfSED[7,1])
carimove_cl2021s<-CariMoveList(Params=list(SpeedSed2021, StraightSed2021, HerbsSed2021,
                                           ShrubSed2021, TreedConiferSed2021,
                                           WaterSed2021, HBDistSed2021),
                               daterange = daterange_dfSED[8,1])
carimove_cl2022s<-CariMoveList(Params=list(SpeedSed2022, StraightSed2022, HerbsSed2022,
                                           ShrubSed2022, TreedConiferSed2022,
                                           WaterSed2022, HBDistSed2022),
                               daterange = daterange_dfSED[9,1])

GG_Mig#which years do I have data for?
daterange_dfMIG#make sure to select the correct daterange for each year
carimove_cl2009m<-CariMoveList(Params=list(SpeedMig2009, StraightMig2009, HerbsMig2009,
                                           ShrubMig2009, TreedConiferMig2009,
                                           WaterMig2009, HBDistMig2009),
                               daterange = daterange_dfMIG[1,1])
carimove_cl2020m<-CariMoveList(Params=list(SpeedMig2020, StraightMig2020, HerbsMig2020,
                                           ShrubMig2020, TreedConiferMig2020,
                                           WaterMig2020, HBDistMig2020),
                               daterange = daterange_dfMIG[4,1])
carimove_cl2021m<-CariMoveList(Params=list(SpeedMig2021, StraightMig2021, HerbsMig2021,
                                           ShrubMig2021, TreedConiferMig2021,
                                           WaterMig2021, HBDistMig2021),
                               daterange = daterange_dfMIG[5,1])
carimove_cl2022m<-CariMoveList(Params=list(SpeedMig2022, StraightMig2022, HerbsMig2022,
                                           ShrubMig2022, TreedConiferMig2022,
                                           WaterMig2022, HBDistMig2022),
                               daterange = daterange_dfMIG[6,1])
carimove_cl2023m<-CariMoveList(Params=list(SpeedMig2023, StraightMig2023, HerbsMig2023,
                                           ShrubMig2023, TreedConiferMig2023,
                                           WaterMig2023, HBDistMig2023),
                               daterange = daterange_dfMIG[7,1])

#function to create df with ids for each ecotype-year (needed for bsSeasons function, below)
names_temp<-function(x, Year){
  names_temp<-colnames(x)
  names_temp_df<-as.data.frame(names_temp)
  names_temp_df$year<-Year
  names(names_temp_df)[1]<-"id"
  return(names_temp_df)
}

### Compute the bootstrap seasons

#This step returns The result of a season clustering: 
#A vector of integers indicating the cluster to which each day is allocated
#This should be a list of 100 representing 100 iterations

#I randomly chose a number for set.seed
#but it needs to be re-set every time, otherwise it defaults back to whatever...
#so I just used the same set.seed number throughout

set.seed(39)
caritest2009s <- bsSeasons(data = carimove_cl2009s, 
                           ind = names_temp(carimove_cl2009s[[1]], 2009), 
                           nclust = ClusterNum, 
                           iter = 100)
set.seed(39)
caritest2010s <- bsSeasons(data = carimove_cl2010s, 
                           ind = names_temp(carimove_cl2010s[[1]], 2010), 
                           nclust = ClusterNum, 
                           iter = 100)
set.seed(39)
caritest2011s <- bsSeasons(data = carimove_cl2011s, 
                           ind = names_temp(carimove_cl2011s[[1]], 2011), 
                           nclust = ClusterNum, 
                           iter = 100)
set.seed(39)
caritest2012s <- bsSeasons(data = carimove_cl2012s, 
                           ind = names_temp(carimove_cl2012s[[1]], 2012), 
                           nclust = ClusterNum, 
                           iter = 100)
set.seed(39)
caritest2019s <- bsSeasons(data = carimove_cl2019s, 
                           ind = names_temp(carimove_cl2019s[[1]], 2019), 
                           nclust = ClusterNum, 
                           iter = 100)
set.seed(39)
caritest2020s <- bsSeasons(data = carimove_cl2020s, 
                           ind = names_temp(carimove_cl2020s[[1]], 2020), 
                           nclust = ClusterNum, 
                           iter = 100)
set.seed(39)
caritest2021s <- bsSeasons(data = carimove_cl2021s, 
                           ind = names_temp(carimove_cl2021s[[1]], 2021), 
                           nclust = ClusterNum, 
                           iter = 100)
set.seed(39)
caritest2022s <- bsSeasons(data = carimove_cl2022s, 
                           ind = names_temp(carimove_cl2022s[[1]], 2022), 
                           nclust = ClusterNum, 
                           iter = 100)

#For migratory
set.seed(39)
caritest2009m <- bsSeasons(data = carimove_cl2009m, 
                           ind = names_temp(carimove_cl2009m[[1]], 2009), 
                           nclust = ClusterNum, 
                           iter = 100)

set.seed(39)
caritest2020m <- bsSeasons(data = carimove_cl2020m, 
                           ind = names_temp(carimove_cl2020m[[1]], 2020), 
                           nclust = ClusterNum, 
                           iter = 100)
set.seed(39)
caritest2021m <- bsSeasons(data = carimove_cl2021m, 
                           ind = names_temp(carimove_cl2021m[[1]], 2021), 
                           nclust = ClusterNum, 
                           iter = 100)
set.seed(39)
caritest2022m <- bsSeasons(data = carimove_cl2022m, 
                           ind = names_temp(carimove_cl2022m[[1]], 2022), 
                           nclust = ClusterNum, 
                           iter = 100)
set.seed(39)
caritest2023m <- bsSeasons(data = carimove_cl2023m, 
                           ind = names_temp(carimove_cl2023m[[1]], 2023), 
                           nclust = ClusterNum, 
                           iter = 100)

### Compute the weights, and identify the final seasons:
#window with average values: as.data.frame(all_mean2023s[,1:7])
set.seed(77)
cariwindow2009s<-as.data.frame(all_mean2009s[,1:7])
seasons2009s <- kmeans(cariwindow2009s, ClusterNum, iter.max = 100)$cluster
weights2009s <- bsWeights(caritest2009s)#compute weights from bootstrap sample
seasonsbs2009s <- bsCriterion(seasons=seasons2009s, bsWeights=weights2009s, 
                              threshold = .8)

set.seed(77)
cariwindow2010s<-as.data.frame(all_mean2010s[,1:7])
seasons2010s <- kmeans(cariwindow2010s, ClusterNum, iter.max = 100)$cluster
weights2010s <- bsWeights(caritest2010s)#compute weights from bootstrap
seasonsbs2010s <- bsCriterion(seasons=seasons2010s, bsWeights=weights2010s, 
                              threshold = .8)

set.seed(77)
cariwindow2011s<-as.data.frame(all_mean2011s[,1:7])
seasons2011s <- kmeans(cariwindow2011s, ClusterNum, iter.max = 100)$cluster
weights2011s <- bsWeights(caritest2011s)#compute weights from bootstrap
seasonsbs2011s <- bsCriterion(seasons=seasons2011s, bsWeights=weights2011s, 
                              threshold = .8)

set.seed(77)
cariwindow2012s<-as.data.frame(all_mean2012s[,1:7])
seasons2012s <- kmeans(cariwindow2012s, ClusterNum, iter.max = 100)$cluster
weights2012s <- bsWeights(caritest2012s)#compute weights from bootstrap
seasonsbs2012s <- bsCriterion(seasons=seasons2012s, bsWeights=weights2012s, 
                              threshold = .8)

set.seed(77)
cariwindow2019s<-as.data.frame(all_mean2019s[,1:7])
seasons2019s <- kmeans(cariwindow2019s, ClusterNum, iter.max = 100)$cluster
weights2019s <- bsWeights(caritest2019s)#compute weights from bootstrap
seasonsbs2019s <- bsCriterion(seasons=seasons2019s, bsWeights=weights2019s, 
                              threshold = .8)

set.seed(77)
cariwindow2020s<-as.data.frame(all_mean2020s[,1:7])
seasons2020s <- kmeans(cariwindow2020s, ClusterNum, iter.max = 100)$cluster
weights2020s <- bsWeights(caritest2020s)#compute weights from bootstrap
seasonsbs2020s <- bsCriterion(seasons=seasons2020s, bsWeights=weights2020s, 
                              threshold = .8)

set.seed(77)
cariwindow2021s<-as.data.frame(all_mean2021s[,1:7])
seasons2021s <- kmeans(cariwindow2021s, ClusterNum, iter.max = 100)$cluster
weights2021s <- bsWeights(caritest2021s)#compute weights from bootstrap
seasonsbs2021s <- bsCriterion(seasons=seasons2021s, bsWeights=weights2021s, 
                              threshold = .8)

set.seed(77)
cariwindow2022s<-as.data.frame(all_mean2022s[,1:7])
seasons2022s <- kmeans(cariwindow2022s, ClusterNum, iter.max = 100)$cluster
weights2022s <- bsWeights(caritest2022s)#compute weights from bootstrap
seasonsbs2022s <- bsCriterion(seasons=seasons2022s, bsWeights=weights2022s, 
                              threshold = .8)

#migratory
set.seed(77)
cariwindow2009m<-as.data.frame(all_mean2009m[,1:7])
seasons2009m <- kmeans(cariwindow2009m, ClusterNum, iter.max = 100)$cluster
weights2009m <- bsWeights(caritest2009m)#compute weights from bootstrap
seasonsbs2009m <- bsCriterion(seasons=seasons2009m, bsWeights=weights2009m, 
                              threshold = .8)

set.seed(77)
cariwindow2020m<-as.data.frame(all_mean2020m[,1:7])
seasons2020m <- kmeans(cariwindow2020m, ClusterNum, iter.max = 100)$cluster
weights2020m <- bsWeights(caritest2020m)#compute weights from bootstrap
seasonsbs2020m <- bsCriterion(seasons=seasons2020m, bsWeights=weights2020m, 
                              threshold = .8)

set.seed(77)
cariwindow2021m<-as.data.frame(all_mean2021m[,1:7])
seasons2021m <- kmeans(cariwindow2021m, ClusterNum, iter.max = 100)$cluster
weights2021m <- bsWeights(caritest2021m)#compute weights from bootstrap
seasonsbs2021m <- bsCriterion(seasons=seasons2021m, bsWeights=weights2021m, 
                              threshold = .8)

set.seed(77)
cariwindow2022m<-as.data.frame(all_mean2022m[,1:7])
seasons2022m <- kmeans(cariwindow2022m, ClusterNum, iter.max = 100)$cluster
weights2022m <- bsWeights(caritest2022m)#compute weights from bootstrap
seasonsbs2022m <- bsCriterion(seasons=seasons2022m, bsWeights=weights2022m, 
                              threshold = .8)

set.seed(77)
cariwindow2023m<-as.data.frame(all_mean2023m[,1:7])
seasons2023m <- kmeans(cariwindow2023m, ClusterNum, iter.max = 100)$cluster
weights2023m <- bsWeights(caritest2023m)#compute weights from bootstrap
seasonsbs2023m <- bsCriterion(seasons=seasons2023m, bsWeights=weights2023m, 
                              threshold = .8)

### Simplify and visualize the final seasons ----
seasonsbs_simple2009s<-sSimple(seasonsbs2009s, win=15)
seasonsbs_simple2010s<-sSimple(seasonsbs2010s, win=15)
seasonsbs_simple2011s<-sSimple(seasonsbs2011s, win=15)
seasonsbs_simple2012s<-sSimple(seasonsbs2012s, win=15)
seasonsbs_simple2019s<-sSimple(seasonsbs2019s, win=15)
seasonsbs_simple2020s<-sSimple(seasonsbs2020s, win=15)
seasonsbs_simple2021s<-sSimple(seasonsbs2021s, win=15)
seasonsbs_simple2022s<-sSimple(seasonsbs2022s, win=15)

seasonsbs_simple2009m<-sSimple(seasonsbs2009m, win=15)
seasonsbs_simple2020m<-sSimple(seasonsbs2020m, win=15)
seasonsbs_simple2021m<-sSimple(seasonsbs2021m, win=15)
seasonsbs_simple2022m<-sSimple(seasonsbs2022m, win=15)
seasonsbs_simple2023m<-sSimple(seasonsbs2023m, win=15)

sPrint(seasonsbs2009m)
sPrint(seasonsbs_simple2009m)

sBoxplot(cariwindow2022m, seasonsbs_simple2022m)
bsPlot(seasonsbs_simple2009m, seasons2009m, weights2009m, title = "Mig 2009")
plot(weights2009m, type="l")

#create my own sBoxplot so that I can plot across different dates
#use existing sBoxplot code to do this, but modify slightly
#NOTE: for some reason you have to highlight and run this whole function in order to create it (instead of just "running" the first line)

sBoxplotJJ <- function(data, seasons, temporal = TRUE, months = c("rectangles","lines"), 
                       cluster = TRUE, multi = FALSE, samescale = TRUE) {
  
  old.par <- par(no.readonly = TRUE)
  
  # ---- Larger font settings ----
  par(
    cex = 1.4,        # global text size (default 1.0)
    cex.axis = 1.25,   # axis tick labels
    cex.main = 1.5,   # main titles
    cex.lab = 1.4     # y-axis labels
  )
  
  if (multi)
    par(mfcol = c(ncol(data[[1]]), length(data)), mar = c(2.5, 2, 2, 0) + 0.1)
  else {
    par(mfrow = n2mfrow(ncol(data)), mar = c(2.5, 2, 2, 0) + 0.1)
    data <- list(data)
    seasons <- list(seasons)
  }
  on.exit(par(old.par))
  months <- match.arg(months)
  
  # ---- July–June month definitions ----
  month_gridlines <- c(32, 63, 93, 124, 154, 185, 216, 244, 275, 305, 336)
  
  rect_starts <- c(32, 93, 154, 216, 275, 336)
  rect_ends   <- c(62, 123, 184, 243, 304, 365)
  
  axis_positions <- c(16, 47, 78, 108, 139, 169.5,
                      200, 230.5, 261, 290, 320, 350)
  
  axis_labels <- c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun")
  # --------------------------------------
  
  if (temporal) {
    for (j in 1:length(data)) {
      datarb <- do.call(rbind, data)
      datatmp <- data[[j]]
      seasonstmp <- seasons[[j]]
      
      changes <- as.numeric(c(
        names(seasonstmp)[1],
        names(which(diff(seasonstmp) != 0)),
        names(seasonstmp)[length(seasonstmp)]
      ))
      
      seas <- seasonstmp[as.character(changes[-length(changes)])]
      at <- changes[-length(changes)] + diff(changes)/2
      
      for (i in 1:ncol(datatmp)) {
        
        summ <- do.call(rbind, lapply(
          1:max(seasonstmp),
          function(j) summary(datatmp[seasonstmp == j, i])
        ))
        
        # plot setup
        if (samescale) {
          plot(as.numeric(row.names(datatmp)), datatmp[, i],
               type = "n", xlim = c(1, 365), ylim = range(datarb[, i]),
               axes = FALSE, 
               main = gsub("\\D+", "", names(datatmp)[i])
               )
          par(usr = c(1, 365, min(datarb[, i]), max(datarb[, i])))
        } else {
          plot(as.numeric(row.names(datatmp)), datatmp[, i],
               type = "n", xlim = c(1, 365), ylim = range(datatmp[, i]),
               axes = FALSE, 
               main = gsub("\\D+", "", names(datatmp)[i])
               )
          par(usr = c(1, 365, min(datatmp[, i]), max(datatmp[, i])))
        }
        
        # ---- July–June month lines/rectangles ----
        if (months == "lines")
          abline(v = month_gridlines, lty = 3, col = grey(0.6))
        
        if (months == "rectangles")
          rect(rect_starts, min(datarb[, i]),
               rect_ends, max(datarb[, i]),
               col = grey(0.9), border = NA)
        # ------------------------------------------
        
        # distributions
        segments(at, summ[seas, "Min."], y1 = summ[seas, "Max."], lty = 2)
        
        for (k in 1:length(seas)) {
          polygon(
            x = changes[c(k, k+1, k+1, k)] + c(1, -1, -1, 1),
            y = rep(summ[seas[k], c("1st Qu.", "3rd Qu.")], each = 2),
            col = "white"
          )
        }
        
        segments(changes[-length(changes)] + 1,
                 summ[seas, "Median"],
                 changes[-1] - 1, lwd = 3)
        
        axis(2)
        
        # ---- July–June month labels ----
        axis(1, at = axis_positions, labels = axis_labels,
             tick = FALSE, line = 0, cex.axis = 1.5)
        # --------------------------------
        
        if (cluster)
          axis(3, at = at, labels = seas, tick = FALSE,
               line = -1, cex.axis = 1.5)
        
        box()
      }
    }
    
  } else {
    for (j in 1:length(data)) {
      datatmp <- data[[j]]
      seasonstmp <- seasons[[j]]
      for (i in 1:ncol(datatmp)) {
        boxplot(datatmp[, i] ~ seasonstmp, axes = FALSE,
                main = gsub("\\D+", "", names(datatmp)[i]), 
                ylim = c(0, 1))
        axis(1)
        box()
      }
    }
  }
}#NOTE: I used chatGPT to help make this function

#need to re-order dataframe so it's Jul-June
JulJune<-function(x){
  if (class(x)=="numeric"){
    JJ<-x[c(182:365, 1:181)] 
    names(JJ)<-c(1:365)
  } else if (class(x) == "data.frame"){
    JJ<-x[c(182:365, 1:181),] 
    rownames(JJ)<-c(1:365)
  }
  return(JJ)
}

cariwindow2022sJJ<-JulJune(cariwindow2022s)
head(cariwindow2022sJJ)

seasonsbs_simple2022sJJ<-JulJune(seasonsbs_simple2022s)

sBoxplotJJ(JulJune(cariwindow2022s), JulJune(seasonsbs_simple2022s))#this works!!!

sBoxplotJJ(JulJune(cariwindow2009m), JulJune(seasonsbs_simple2009m))
sBoxplotJJ(JulJune(cariwindow2020m), JulJune(seasonsbs_simple2020m))
sBoxplotJJ(JulJune(cariwindow2021m), JulJune(seasonsbs_simple2021m))
sBoxplotJJ(JulJune(cariwindow2022m), JulJune(seasonsbs_simple2022m))
sBoxplotJJ(JulJune(cariwindow2023m), JulJune(seasonsbs_simple2023m))

sBoxplotJJ(JulJune(cariwindow2009s), JulJune(seasonsbs_simple2009s))
sBoxplotJJ(JulJune(cariwindow2010s), JulJune(seasonsbs_simple2010s))
sBoxplotJJ(JulJune(cariwindow2011s), JulJune(seasonsbs_simple2011s))
sBoxplotJJ(JulJune(cariwindow2012s), JulJune(seasonsbs_simple2012s))
sBoxplotJJ(JulJune(cariwindow2019s), JulJune(seasonsbs_simple2019s))
sBoxplotJJ(JulJune(cariwindow2020s), JulJune(seasonsbs_simple2020s))
sBoxplotJJ(JulJune(cariwindow2021s), JulJune(seasonsbs_simple2021s))
sBoxplotJJ(JulJune(cariwindow2022s), JulJune(seasonsbs_simple2022s))

#To plot only a single variable at a time:
PlotData<-JulJune(cariwindow2009m)
sBoxplotJJ(
    data = PlotData[, "HBDistMig2009", drop = FALSE],
    seasons = JulJune(seasonsbs_simple2009m),
    cluster=F
    )#This does not create a ggplot object
#Which is very annoying...

#######
save.image(file = "Seasons_workspace.RData")
#########