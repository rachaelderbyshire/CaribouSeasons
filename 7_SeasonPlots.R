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
write.csv(SeasonIntWide, "Results/SeasonIntervals/SeasonIntWide_16Mar2026.csv")

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

#first, subset data by ecotype and year:
#Start with sedentary
SedSeasons<-subset(SeasonIntWide, Ecotype=="Sed")
SedSeasons09<-subset(SedSeasons, Year=="2009")
SedSeasons10<-subset(SedSeasons, Year=="2010")
SedSeasons11<-subset(SedSeasons, Year=="2011")
SedSeasons12<-subset(SedSeasons, Year=="2012")
SedSeasons19<-subset(SedSeasons, Year=="2019")
SedSeasons20<-subset(SedSeasons, Year=="2020")
SedSeasons21<-subset(SedSeasons, Year=="2021")
SedSeasons22<-subset(SedSeasons, Year=="2022")

#Migratory population
MigSeasons<-subset(SeasonIntWide, Ecotype=="Mig")
MigSeasons09<-subset(MigSeasons, Year=="2009")
MigSeasons20<-subset(MigSeasons, Year=="2020")
MigSeasons21<-subset(MigSeasons, Year=="2021")
MigSeasons22<-subset(MigSeasons, Year=="2022")
MigSeasons23<-subset(MigSeasons, Year=="2023")

#now fix dates
SedSeasons09<-dates2009(x=SedSeasons09, Year=2009)#still do this for 2009 so I can combine datasets more easily
SedSeasons10<-dates2009(SedSeasons10, 2010)
SedSeasons11<-dates2009(SedSeasons11, 2011)
SedSeasons12<-dates2009(SedSeasons12, 2012)
SedSeasons19<-dates2009(x=SedSeasons19, Year=2019)
SedSeasons19
SedSeasons20<-dates2009(SedSeasons20, 2020)
SedSeasons21<-dates2009(SedSeasons21, 2021)
SedSeasons22<-dates2009(SedSeasons22, 2022)

MigSeasons09<-dates2009(MigSeasons09, 2009)
MigSeasons20<-dates2009(MigSeasons20, 2020)
MigSeasons21<-dates2009(MigSeasons21, 2021)
MigSeasons22<-dates2009(MigSeasons22, 2022)
MigSeasons23<-dates2009(MigSeasons23, 2023)

#combine the dataframes
dataSED<-rbind(SedSeasons09, SedSeasons10, SedSeasons11, SedSeasons12, 
               SedSeasons19, SedSeasons20, SedSeasons21, SedSeasons22)

dataMIG<-rbind(MigSeasons09, MigSeasons20, MigSeasons21, MigSeasons22, MigSeasons23)

#Plot it! -----

#first, get calving date df set up
#SedCalving dataset from ParturitionLocationAndEcotype script
SedCalving.2009<-read.csv("Results/SedCalvingStart.csv")
head(SedCalving.2009)
SedCalving.2009$CalvingStart<-as.POSIXct(SedCalving.2009$CalvingStart)
year(SedCalving.2009$CalvingStart)<-2010
SedCalving.2009

#Migratory population
#MigCalving dataset from ParturitionLocationAndEcotype script
MigCalving.2009<-read.csv("Results/MigCalvingStart.csv")
MigCalving.2009$CalvingStart<-as.POSIXct(MigCalving.2009$CalvingStart)
year(MigCalving.2009$CalvingStart)<-2010
MigCalving.2009

#Try plotting the way that bsPlot does it
bsPlot(seasonsbs_simple2009m, seasons2009m, weights2009m, title = "Mig 2009")

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

head(Table2009s)
Table2009s<-dates2009_2(Table2009s, Year=2009)
Table2010s<-dates2009_2(Table2010s, Year=2010)
Table2011s<-dates2009_2(Table2011s, Year=2011)
Table2012s<-dates2009_2(Table2012s, Year=2012)
Table2019s<-dates2009_2(Table2019s, Year=2019)
Table2020s<-dates2009_2(Table2020s, Year=2020)
Table2021s<-dates2009_2(Table2021s, Year=2021)
Table2022s<-dates2009_2(Table2022s, Year=2022)

TableAlls<-rbind(Table2009s, Table2010s, Table2011s, Table2012s, Table2019s, Table2020s, 
                 Table2021s, Table2022s)

Table2009m<-dates2009_2(Table2009m, Year=2009)
head(Table2009m)
Table2020m<-dates2009_2(Table2020m, Year=2020)
Table2021m<-dates2009_2(Table2021m, Year=2021)
Table2022m<-dates2009_2(Table2022m, Year=2022)
Table2023m<-dates2009_2(Table2023m, Year=2023)

TableAllm<-rbind(Table2009m, Table2020m, Table2021m, Table2022m, Table2023m)

#Put together the plot!
SedPlot<-ggplot(TableAlls, aes(x=Date.2009, y=1, colour = weights))+
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

MigPlot<-ggplot(TableAllm, aes(x=Date.2009, y=1, colour = weights))+
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