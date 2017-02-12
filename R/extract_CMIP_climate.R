# script for extracting data from some of the CMIP5 climate projections
#downscaled climate data downloaded from:

# downloaded the mean and stdev of climate projections from 2050-2099, for multiple models

library(raster)
library(sp)
library(rgeos)
library(ggplot2)
library(reshape2)
reg.cols <- read.csv("data/bcsd5/bcsd5/COLS_PeriodStat.txt", header = F)
reg.rows <- read.csv("data/bcsd5/bcsd5/ROWS_PeriodStat.txt", header = F)

#read in the projections for avg precipitation rate precipitation from ccsm4, rcp scenario 2.6 
ccsm4.26pr<- read.csv("data/bcsd5/bcsd5/pr_PeriodStat_mean.ccsm4.1.rcp26.csv", header = F)
colnames(ccsm4.26pr) <- t(reg.cols) # assign cols
rownames(ccsm4.26pr) <- t(reg.rows) # assign rows
ccsm4.26pr$lat <- rownames(ccsm4.26pr) 

melted <- melt(ccsm4.26pr, id.vars = "lat")
colnames(melted) <- c("lat", "lon", "pr")
melted$pr <- as.numeric(melted$pr)
melted$lon <- as.numeric(as.character(melted$lon))
melted$lat <- as.numeric(melted$lat)
ggplot(melted, aes(lon, lat, fill = pr)) + geom_raster()

melted$pr <- melted$pr*365.25 # crudely converting avg precipitation/day to total precip/year
coordinates(melted) <- ~lon +lat
gridded(melted)<- TRUE

ccsm4.26pr <- raster(melted) # rasterize
