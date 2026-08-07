#Plotting Seasons

#code written by Rachael Derbyshire: rderbysh@uoguelph.ca
#2025-2026

#Load workspace from previous step
load("Seasons_workspace.RData")

#Load required packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(ungeviz)

#First, set up dataframe 
#I need column for start and end time for each group
SeasonIntAll<-rbind(SeasonInt2009s, SeasonInt2009m, SeasonInt2010s, SeasonInt2011s,
                    SeasonInt2012s, SeasonInt2019s, SeasonInt2020s, SeasonInt2020m,
                    SeasonInt2021s, SeasonInt2021m, SeasonInt2022s, SeasonInt2022m,
                    SeasonInt2023m)
head(SeasonIntAll)
SeasonIntAll$Ecotype_Year<-paste(SeasonIntAll$Ecotype, SeasonIntAll$Year, sep="_")

SeasonIntAll<-SeasonIntAll[,c(-4)]#remove DayNum so it doesn't clutter up subsequent table

SeasonIntWide<-reshape(SeasonIntAll, idvar = c("Ecotype_Year", "Season", "Year", "Ecotype"), 
                       timevar = "Interval", direction = "wide")
head(SeasonIntWide)
SeasonIntWide$Date.early<-as.POSIXct(SeasonIntWide$Date.early)
SeasonIntWide$Date.mid<-as.POSIXct(SeasonIntWide$Date.mid)
SeasonIntWide$Date.late<-as.POSIXct(SeasonIntWide$Date.late)

SeasonIntWide$Date.early2<-SeasonIntWide$Date.early#second Date.early column so I can compare all dates within same year (see below)

#subtract year if Date.late is earlier than Date.early
for(i in 1:nrow(SeasonIntWide)){
  if (SeasonIntWide$Date.late[i] < SeasonIntWide$Date.early[i]){
    SeasonIntWide$Date.early2[i]<-SeasonIntWide$Date.early[i] - years(1)
  }
}
View(SeasonIntWide)
write.csv(SeasonIntWide, "Results/SeasonIntervals/SeasonIntWide_06Aug2026.csv")

####Put all timelines within same year ------
#all dates need to be in same year in order to plot properly
#write function to do this:
dates2009<-function(x, Year){
  x$Date.early2.2009<-x$Date.early2
  
  #leap years keep f***ing up my analysis...
  #specifically the case for Date.early in 2020
  # Identify the Feb 29th date for 2020
  to_change <- x$Date.early2.2009 == as.Date("2020-02-29")
  # Change the day to 28 for those rows
  x$Date.early2.2009[to_change] <- as.Date("2020-02-28")
  
  
  for(i in 1:length(x$Date.early2)){
    if(year(x$Date.early2)[i]==Year){(year(x$Date.early2.2009)[i]<-2009)}
    else{year(x$Date.early2.2009)[i]<-2010}
  }
  
  x$Date.mid.2009<-x$Date.mid
  for(i in 1:length(x$Date.mid)){
    if(year(x$Date.mid)[i]==Year){(year(x$Date.mid.2009)[i]<-2009)}
    else{year(x$Date.mid.2009)[i]<-2010}
  }
  
  x$Date.late.2009<-x$Date.late
  for(i in 1:length(x$Date.late)){
    if(year(x$Date.late)[i]==Year){(year(x$Date.late.2009)[i]<-2009)}
    else{year(x$Date.late.2009)[i]<-2010}
  }
  
  #extra cleaning so "error bars" will plot properly
  for(i in 1:length(x$Date.early2)){
    if(year(x$Date.early2.2009)[i] == 2010 & year(x$Date.late.2009)[i] == 2010){
      year(x$Date.mid.2009)[i]<-2010}
  }
  
  for(i in 1:length(x$Date.early2)){
    if(year(x$Date.early2.2009)[i] == 2009 & year(x$Date.late.2009)[i] == 2009 &
       year(x$Date.mid.2009)[i] == 2010){
      year(x$Date.early2.2009)[i] <- 2010} 
  }
  for(i in 1:length(x$Date.early2)){
    if(year(x$Date.early2.2009)[i] == 2010 & year(x$Date.late.2009)[i] == 2009 &
       year(x$Date.mid.2009)[i] == 2010){
      year(x$Date.late.2009)[i] <- 2010} 
  }
  
  x<-x %>%
    arrange(Date.mid.2009)
  x$Season<-seq(1, nrow(x), by=1)
  
  return(x)
}

#For season intervals:
#first, subset data by ecotype:
SedSeasons<-subset(SeasonIntWide, Ecotype=="Sed")
summary(SedSeasons)

MigSeasons<-subset(SeasonIntWide, Ecotype=="Mig")

#now fix dates
dataSED<-list()
for(i in as.character(unique(SedSeasons$Year))){
  dataSED[[i]]<-try(dates2009(x=subset(SedSeasons, Year==i), Year=i))
}
View(dataSED)

dataMIG<-list()
for(i in as.character(unique(MigSeasons$Year))){
  dataMIG[[i]]<-try(dates2009(x=subset(MigSeasons, Year==i), Year=i))
}
View(dataMIG)

#combine the dataframes
dataSED<-rbindlist(dataSED)
dataMIG<-rbindlist(dataMIG)


#Next, for daily weights data:
#format data so it's all within the same year (necessary for plotting)
dates2009_2<-function(x, Year){
  x$Date<-as.POSIXct(x$Date)
  x$Date.2009<-x$Date
  for(i in 1:length(x$Date)){
    if(year(x$Date)[i]==Year){(year(x$Date.2009)[i]<-2009)}
    else{year(x$Date.2009)[i]<-2010}
  }
  
  return(x)
}

#now fix dates
str(TableAlls)##from 6_UncertaintyIntervals
TableAlls2009<-list()
for(i in as.character(unique(TableAlls$Year))){
  TableAlls2009[[i]]<-try(dates2009_2(x=subset(TableAlls, Year==i), Year=i))
}
View(TableAlls2009)
TableAlls2009<-rbindlist(TableAlls2009)

TableAllm2009<-list()
for(i in as.character(unique(TableAllm$Year))){
  TableAllm2009[[i]]<-try(dates2009_2(x=subset(TableAllm, Year==i), Year=i))
}
TableAllm2009<-rbindlist(TableAllm2009)


#get calving date df set up
#SedCalving dataset from ParturitionLocationAndEcotype script
SedCalving.2009<-read.csv("Results/SedCalvingStart_04Aug2026.csv")
head(SedCalving.2009)
SedCalving.2009$CalvingStart<-as.POSIXct(SedCalving.2009$CalvingStart)
year(SedCalving.2009$CalvingStart)<-2010#so they can be plotted together on same timeline
SedCalving.2009

#Migratory population
#MigCalving dataset from ParturitionLocationAndEcotype script
MigCalving.2009<-read.csv("Results/MigCalvingStart_04Aug2026.csv")
MigCalving.2009$CalvingStart<-as.POSIXct(MigCalving.2009$CalvingStart)
year(MigCalving.2009$CalvingStart)<-2010
MigCalving.2009


#Plot it! -----

#Put together the plot!
SedPlot<-ggplot(TableAlls2009, aes(x=Date.2009, y=1, colour = weights))+
  geom_col(width=1)+
  facet_grid(Year~.)+
  theme_classic()+
  scale_colour_gradient(low = "white", high = "darkgrey")+
  scale_x_datetime(name="Month", date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(name="")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        text=element_text(size=20))+
  geom_vline(data=SedCalving.2009, aes(xintercept = CalvingStart), color = "orange",
             size=2)
SedPlot<-SedPlot+geom_segment(data=dataSED, 
                              aes(x = Date.early2.2009, xend = Date.late.2009, y=0.5),
                              colour=rgb(68,179,15, maxColorValue = 255), size =1.5, alpha=0.7, 
                              position = position_jitter(height = .1, width=0), 
                              arrow=arrow(angle=90, ends="both", length=unit(0.1, "cm")))+
  ungeviz::geom_vpline(data=dataSED, aes(x = Date.mid.2009, y=0.5), colour="black", height=0.2)
SedPlot

MigPlot<-ggplot(TableAllm2009, aes(x=Date.2009, y=1, colour = weights))+
  geom_col(width=1)+
  facet_grid(Year~.)+#, nrow=5, scales="free_y", dir="v")+
  theme_classic()+
  scale_colour_gradient(low = "white", high = "darkgrey")+
  scale_x_datetime(name="Month", date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(name="")+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        text=element_text(size=20))+
  geom_vline(data=MigCalving.2009, aes(xintercept = CalvingStart), color = "orange",
             size=2)
MigPlot<-MigPlot+geom_segment(data=dataMIG, 
                              aes(x = Date.early2.2009, xend = Date.late.2009, y=0.5),
                              colour=rgb(7,173,233, maxColorValue = 255), size=1.5, alpha=0.7, 
                              position = position_jitter(height = .1, width=0), 
                              arrow=arrow(angle=90, ends="both", length=unit(0.1, "cm")))+
  geom_vpline(data=dataMIG, aes(x = Date.mid.2009, y=0.5), colour="black", height=0.2)
MigPlot

#Plot for labelling seasons
MigPlot2<-ggplot(TableAllm, aes(x=Date.2009, y=1, colour = weights))+
  facet_grid(Year~.)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  geom_vpline(data=dataMIG, aes(x = Date.mid.2009, y=0.5), colour="black", height=0.2)+
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
        text=element_text(size=20))+
  scale_x_datetime(name="Month", date_breaks = "1 month", date_labels = "%b")+
  scale_y_continuous(name="")+
  geom_vline(data=MigCalving.2009, aes(xintercept = CalvingStart), color = "orange",
             lwd=1)
MigPlot2

#######
save.image(file = "Seasons_workspace.RData")
#########