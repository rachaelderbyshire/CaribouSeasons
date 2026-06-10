###90% "uncertainty intervals" around seasons ----

#code written by Rachael Derbyshire: rderbysh@uoguelph.ca
#2025-2026

#I want to calculate measurement of uncertainty around start of season
#Can maybe use weights to do this
#Steps
#1: calculate weight at date of new season
#2: Calculate 10% of that weight
#3 find date before and after at which weight <10% of weight on "season day"

#Load workspace from previous step
load("Seasons_workspace.RData")

#first, create table with seasons, weights, and dates
Table_func<-function(simple_seasons, weights, dates, Year){
  Table<-cbind(simple_seasons, weights, dates)
  Table<-as.data.frame(Table)
  names(Table)<-c("Season", "weights", "Date")
  Table$weights<-as.numeric(Table$weights)
  Table$Date<-as.Date(Table$Date)
  Table$Season<-as.numeric(Table$Season)
  Table$Year<-Year
  return(Table)
}

Table2009s<-Table_func(seasonsbs_simple2009s, weights2009s, as.character(all_mean2009s$single_date), 2009)
View(Table2009s)
Table2010s<-Table_func(seasonsbs_simple2010s, weights2010s, as.character(all_mean2010s$single_date), 2010)
Table2011s<-Table_func(seasonsbs_simple2011s, weights2011s, as.character(all_mean2011s$single_date), 2011)
Table2012s<-Table_func(seasonsbs_simple2012s, weights2012s, as.character(all_mean2012s$single_date), 2012)
Table2019s<-Table_func(seasonsbs_simple2019s, weights2019s, as.character(all_mean2019s$single_date), 2019)
Table2020s<-Table_func(seasonsbs_simple2020s, weights2020s, as.character(all_mean2020s$single_date), 2020)
Table2021s<-Table_func(seasonsbs_simple2021s, weights2021s, as.character(all_mean2021s$single_date), 2021)
Table2022s<-Table_func(seasonsbs_simple2022s, weights2022s, as.character(all_mean2022s$single_date), 2022)

Table2009m<-Table_func(seasonsbs_simple2009m, weights2009m, as.character(all_mean2009m$single_date), 2009)
Table2020m<-Table_func(seasonsbs_simple2020m, weights2020m, as.character(all_mean2020m$single_date), 2020)
Table2021m<-Table_func(seasonsbs_simple2021m, weights2021m, as.character(all_mean2021m$single_date), 2021)
Table2022m<-Table_func(seasonsbs_simple2022m, weights2022m, as.character(all_mean2022m$single_date), 2022)
Table2023m<-Table_func(seasonsbs_simple2023m, weights2023m, as.character(all_mean2023m$single_date), 2023)

###Create function to calculate 90% window of certainty in season dates ----

SeasonUncertainty<-function(x, PerUncert=0.1, Year, Ecotype){#"PerCert" = Percent Uncertainty, e.g. 0.1 = 90% certainty window
  SeasonIntervalAll<-vector()
  
  x$DayNum<-1:365
  
  #first occurence of each season
  s.first <- x[match(unique(x$Season), x$Season),]
  
  #rearrange dataset so winter season isn't cut off
  Late<-subset(x, DayNum < s.first$DayNum[2])
  Rest<-subset(x, DayNum >= s.first$DayNum[2])
  New<-rbind(Rest, Late)
  New$RowIndex<-1:365 #index for selecting dates later
  
  #calculate 10% weights at each season cut-off (or whatever you decide for % uncertainty)
  s.firstNew <- New[match(unique(New$Season), New$Season),]#updated first occurence of each season
  s.firstNew$Percent<-s.firstNew$weights*PerUncert
  s.firstNew$RowIndex2<-1:nrow(s.firstNew)
  
  for(i in 1:nrow(s.firstNew)){
    s.firstNew_sub<-subset(s.firstNew, RowIndex2 == i)
    
    #Find date after which weights <=10% (or whatever you decide for % uncertainty)
    if(s.firstNew_sub$Season != 1){
      s.late<-subset(New, weights <= s.firstNew_sub$Percent & RowIndex > s.firstNew_sub$RowIndex)
      intmax<-s.late[1,]
    }else{
      s.late<-subset(New, weights <= s.firstNew_sub$Percent & Season == 1)
      if(nrow(s.late)>0){intmax<-s.late[1,]}
      else {s.late<-subset(New, weights <= s.firstNew_sub$Percent)
      intmax<-s.late[1,]}
      
    }
    
    #Find date before which weights are <=10%
    s.early<-subset(New, weights <= s.firstNew_sub$Percent & Date < s.firstNew_sub$Date)
    if(nrow(s.early)>0){
      close_index<-which(abs(s.early$Date-s.firstNew_sub$Date) == min(abs(s.early$Date - s.firstNew_sub$Date)))
      intmin <- s.early[close_index, ]
    }else{
      s.early2<-subset(New, weights <= s.firstNew_sub$Percent & DayNum < s.firstNew_sub$DayNum)
      last_index <- which.max(s.early2$DayNum)
      intmin <- s.early2[last_index, ]
    }
    
    #Bind season "CI" together
    SeasonInterval<-rbind(intmin[c("Season", "weights", "Date", "DayNum")], 
                          s.firstNew_sub[c("Season", "weights", "Date", "DayNum")], 
                          intmax[c("Season", "weights", "Date", "DayNum")])
    SeasonInterval$Season<-s.firstNew_sub$Season
    SeasonInterval$Interval<-c("early", "mid", "late")
    SeasonIntervalAll<-try(rbind(SeasonIntervalAll,  SeasonInterval))
    
  }
  SeasonIntervalAll$Year<-Year
  SeasonIntervalAll$Ecotype<-Ecotype
  
  return(SeasonIntervalAll)
}

SeasonInt2009s<-SeasonUncertainty(x=Table2009s, Year=2009, Ecotype="Sed")
SeasonInt2009s
write.csv(SeasonInt2009s, "Results/SeasonIntervals/SeasonIntSed2009_16Mar2026.csv", row.names = F)

SeasonInt2010s<-SeasonUncertainty(x=Table2010s, Year=2010, Ecotype="Sed")
SeasonInt2010s
write.csv(SeasonInt2010s, "Results/SeasonIntervals/SeasonIntSed2010_16Mar2026.csv", row.names = F)

SeasonInt2011s<-SeasonUncertainty(x=Table2011s, Year=2011, Ecotype="Sed")
SeasonInt2011s
write.csv(SeasonInt2011s, "Results/SeasonIntervals/SeasonIntSed2011_16Mar2026.csv", row.names = F)

SeasonInt2012s<-SeasonUncertainty(x=Table2012s, Year=2012, Ecotype="Sed")
SeasonInt2012s
write.csv(SeasonInt2012s, "Results/SeasonIntervals/SeasonIntSed2012_16Mar2026.csv", row.names = F)

SeasonInt2019s<-SeasonUncertainty(x=Table2019s, Year=2019, Ecotype="Sed")
SeasonInt2019s
write.csv(SeasonInt2019s, "Results/SeasonIntervals/SeasonIntSed2019_16Mar2026.csv", row.names = F)

SeasonInt2020s<-SeasonUncertainty(x=Table2020s, Year=2020, Ecotype="Sed")
SeasonInt2020s
write.csv(SeasonInt2020s, "Results/SeasonIntervals/SeasonIntSed2020_16Mar2026.csv", row.names = F)

SeasonInt2021s<-SeasonUncertainty(x=Table2021s, Year=2021, Ecotype="Sed")
SeasonInt2021s
write.csv(SeasonInt2021s, "Results/SeasonIntervals/SeasonIntSed2021_16Mar2026.csv", row.names = F)

SeasonInt2022s<-SeasonUncertainty(x=Table2022s, Year=2022, Ecotype="Sed")
SeasonInt2022s
write.csv(SeasonInt2022s, "Results/SeasonIntervals/SeasonIntSed2022_16Mar2026.csv", row.names = F)

SeasonInt2009m<-SeasonUncertainty(x=Table2009m, Year=2009, Ecotype="Mig")
SeasonInt2009m
write.csv(SeasonInt2009m, "Results/SeasonIntervals/SeasonIntMig2009_16Mar2026.csv", row.names = F)

SeasonInt2020m<-SeasonUncertainty(x=Table2020m, Year=2020, Ecotype="Mig")
SeasonInt2020m
write.csv(SeasonInt2020m, "Results/SeasonIntervals/SeasonIntMig2020_16Mar2026.csv", row.names = F)

SeasonInt2021m<-SeasonUncertainty(x=Table2021m, Year=2021, Ecotype="Mig")
SeasonInt2021m
write.csv(SeasonInt2021m, "Results/SeasonIntervals/SeasonIntMig2021_16Mar2026.csv", row.names = F)

SeasonInt2022m<-SeasonUncertainty(x=Table2022m, Year=2022, Ecotype="Mig")
SeasonInt2022m
write.csv(SeasonInt2022m, "Results/SeasonIntervals/SeasonIntMig2022_16Mar2026.csv", row.names = F)

SeasonInt2023m<-SeasonUncertainty(x=Table2023m, Year=2023, Ecotype="Mig")
SeasonInt2023m
write.csv(SeasonInt2023m, "Results/SeasonIntervals/SeasonIntMig2023_16Mar2026.csv", row.names = F)

#######
save.image(file = "Seasons_workspace.RData")
#########