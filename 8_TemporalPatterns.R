#Temporal patterns and comparisons between ecotypes

#code written by Rachael Derbyshire: rderbysh@uoguelph.ca
#2025-2026

#Load workspace from previous step
load("Seasons_workspace.RData")

#Load required packages
library(ivs)

#overlapping season start intervals -----
#use package ivs

#create interval vectors for each year
make_iv<-function(Seasons){
  starts<-Seasons$Date.early2.2009
  ends<-Seasons$Date.late.2009
  
  ir<-iv(starts, ends)#create interval vector object
  return(ir)
}

#seasons start intervals for boreal
str(dataSED)
irSED09<-make_iv(subset(dataSED, Year=="2009"))
irSED09
irSED10<-make_iv(subset(dataSED, Year=="2010"))
irSED11<-make_iv(subset(dataSED, Year=="2011"))
irSED12<-make_iv(subset(dataSED, Year=="2012"))
irSED19<-make_iv(subset(dataSED, Year=="2019"))
irSED20<-make_iv(subset(dataSED, Year=="2020"))
irSED21<-make_iv(subset(dataSED, Year=="2021"))
irSED22<-make_iv(subset(dataSED, Year=="2022"))

SEDOverlap<--matrix(NA, 7, 2)
iv_locate_overlaps(irSED09, irSED10)
SEDOverlap[1,]<-c(sum(iv_overlaps(irSED10, irSED09)), 2010)#Number of overlaps between 2010 and 2009
SEDOverlap[2,]<-c(sum(iv_overlaps(irSED11, irSED09)), 2011)
SEDOverlap[3,]<-c(sum(iv_overlaps(irSED12, irSED09)), 2012)
SEDOverlap[4,]<-c(sum(iv_overlaps(irSED19, irSED09)), 2019)
SEDOverlap[5,]<-c(sum(iv_overlaps(irSED20, irSED09)), 2020)
SEDOverlap[6,]<-c(sum(iv_overlaps(irSED21, irSED09)), 2021)
SEDOverlap[7,]<-c(sum(iv_overlaps(irSED22, irSED09)), 2022)
SEDOverlap<-as.data.frame(SEDOverlap)
names(SEDOverlap)<-c("Overlaps", "Year")

#migratory ecotype
irMIG09<-make_iv(subset(dataMIG, Year=="2009"))
irMIG09
irMIG20<-make_iv(subset(dataMIG, Year=="2020"))
irMIG21<-make_iv(subset(dataMIG, Year=="2021"))
irMIG22<-make_iv(subset(dataMIG, Year=="2022"))
irMIG23<-make_iv(subset(dataMIG, Year=="2023"))

MIGOverlap<-matrix(NA, 4, 2)
MIGOverlap[1,]<-c(sum(iv_overlaps(irMIG20, irMIG09)), 2020)
MIGOverlap[2,]<-c(sum(iv_overlaps(irMIG21, irMIG09)), 2021)
MIGOverlap[3,]<-c(sum(iv_overlaps(irMIG22, irMIG09)), 2022)
MIGOverlap[4,]<-c(sum(iv_overlaps(irMIG23, irMIG09)), 2023)
MIGOverlap<-as.data.frame(MIGOverlap)
names(MIGOverlap)<-c("Overlaps", "Year")

#General trends in seasonal patterns -----
#Are mean number of seasons different for mig vs sed?

#function to quickly calculate standard error
se <- function(x) { sd(x, na.rm = TRUE) / sqrt(length(na.omit(x))) }

str(dataSED)
SedNum<-tapply(dataSED$Season, dataSED$Year, length)
SedNum
SedNum<-as.data.frame(SedNum)
SedNum$Year<-row.names(SedNum)
str(SedNum)
SedNum$Year<-as.numeric(SedNum$Year)
mean(SedNum$SedNum)
se(SedNum$SedNum)

MigNum<-tapply(dataMIG$Season, dataMIG$Year, length)
MigNum<-as.data.frame(MigNum)
MigNum$Year<-row.names(MigNum)
MigNum$Year<-as.numeric(MigNum$Year)
mean(MigNum$MigNum)
se(MigNum$MigNum)

wilcox.test(MigNum[,1], SedNum[,1], exact=T)

#What about number of overlaps?
mean(MIGOverlap$Overlaps)
se(MIGOverlap$Overlaps)

mean(SEDOverlap$Overlaps)
se(SEDOverlap$Overlaps)

wilcox.test(MIGOverlap[,1], SEDOverlap[,1])