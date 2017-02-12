# script for extracting data from some of the CMIP5 climate projections
#downscaled climate data downloaded from:

#Downscaled CMIP3 and CMIP5 Climate Projections: Release of Downscaled CMIP5
#Climate Projections, Comparison with Preceding Information, and Summary of
#User Needs.  U.S. Department of the Interior, Bureau of Reclamation, 104 p.,
#available at:
#  http://gdo-dcp.ucllnl.org/downscaled_cmip_projections/techmemo/downscaled_ climate.pdf.

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
melted$lon <- melted$lon - 360
ggplot(melted, aes(lon, lat, fill = pr)) + geom_raster()

melted$pr <- melted$pr*365.25 # crudely converting avg precipitation/day to total precip/year
coordinates(melted) <- ~lon +lat
gridded(melted)<- TRUE

ccsm4.26pr <- raster(melted) # rasterize

# assign projeciton to the raster (check this proj4string) 
# should be lambert conformal conic (i think)
proj4string(ccsm4.26pr) <- '+proj=lcc +lat_1=49 +lat_2=77 +lat_0=49 +lon_0=-95 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs' 

#project raster to great lakes albers
ccsm4.26.alb <- projectRaster(ccsm4.26pr, crs = '+init=epsg:3175')


spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb1.6-5.csv')
spec.table <- data.frame(spec.table)

avgs.df <- data.frame(extract(ccsm4.26.alb, spec.table[,c("x","y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y
write.csv(avgs.df, 'C:/Users/JMac/Documents/Kelly/biomodality/data/ccsm4.26.alb_pr_full.csv')
