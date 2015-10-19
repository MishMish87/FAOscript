# This code is made by : Michele Rocca (ESS-FAO)
# Created : 13 february
# Last update:
##########
# The aim of this code is to generate a flexible dataframe in order
# to generate the exchange factor of $PPP in constant price.
# We have first download the series of current exchange factor 
# (LCU per $PPP) and then we multiply it for the serie (from 200 to 2013)
# of USA GDP deflator.
##########
library(foreign)
library(plyr)

setwd("T:/Team_working_folder/D/TEAM_D/3.SOCIAL_STATISTICS/Survey Social Protection/R Standard Scripts")
PPPcurrent <- read.dta("PPPcurrent.dta")
USAdeflator <- read.dta("USAdeflator.dta")

for (j in 2:15){
  PPPcurrent[,j] <- as.numeric(PPPcurrent[,j])
 }
Titles <- PPPcurrent[1,]
PPPcurrent <- PPPcurrent[-1,]
colnames(PPPcurrent) <- Titles

for (i in 1:14) {
 PPPcurrent[,i+1] <- PPPcurrent[,i+1]*USAdeflator[i,"deflator"]
}
write.dta(PPPcurrent,"PPPcostant.dta")