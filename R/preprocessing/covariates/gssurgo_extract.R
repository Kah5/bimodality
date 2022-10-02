# Extract soil covariates from gssurgo soil data
# Author: Kelly Heilman
# Data: 12/2/16

# Preprocessing details:
# soils data are from gSSURGO soil data 10m rasters downloaded by state
# Soil parameter values % sand were joined to the rasters using the SSURGO on-Demand arcgis toolbox, and the weighted average of soil parameter over 0-30 depth were calculated
# state soil rasters were mosaicked together to create one raster for the upper midwest (mosaic to raster tool)
# rasters were aggreated to 1km and 8km using the aggregate tool in arcgis (this was much faster than aggregating in R)

library(raster)
setwd("/Users/kah/Documents/bimodality/")
# SAND RASTERS
sand8km <- raster("data/8km_UMW_sand1.tif")
plot(sand8km)

#sand1km <- raster("data/1km_UMW_sand1.tif")
#plot(sand1km)

# need to project sand to great lakes albers coordinate system
sand8km.alb <- projectRaster(sand8km, crs ='+init=epsg:3175')
#sand1km.alb <- projectRaster(sand1km, crs = '+init=epsg:3175')


#awc
awc8km <- raster("data/8km_UMW_awc1.tif")
#awc1km <- raster ("C:/Users/JMac/Box Sync/GSSURGOtifs/1km_UMW_awc1.tif")

awc8km.alb <- projectRaster(awc8km, crs ='+init=epsg:3175')
#awc1km.alb <- projectRaster(awc1km, crs = '+init=epsg:3175')

#ksat

ksat8km <- raster("data/8km_UMW_ksatalb.tif")
#ksat1km <- raster ("C:/Users/JMac/Box Sync/GSSURGOtifs/1km_UMW_ksat1.tif")

ksat8km.alb <- projectRaster(ksat8km, crs ='+init=epsg:3175')
#ksat1km.alb <- projectRaster(ksat1km, crs = '+init=epsg:3175')

# load in data to extract by
FIAplots <- read.csv("outputs/FIA_plot_agg_fuzzed_alb.csv")
PLSpoints <- read.csv (paste0("data/midwest_pls_full_density_alb",version,".csv"))


#extract and add to PLS data:
PLSpoints$sandpct <- extract(sand8km.alb, PLSpoints[,c('x', 'y')])
plot(PLSpoints$sandpct, PLSpoints$PLSdensity) # plot against percent cover

PLSpoints$awcpct <- extract(awc8km.alb, PLSpoints[,c('x', 'y')])
plot(PLSpoints$awcpct, PLSpoints$PLSdensity) # plot against percent cover

PLSpoints$ksatpct <- extract(ksat8km.alb, PLSpoints[,c('x', 'y')])
plot(PLSpoints$ksatpct, PLSpoints$PLSdensity) # plot against percent cover

#extract from the 1km grid 
#PLSpoints$sand1km <- extract(sand1km.alb, PLSpoints[,c('x', 'y')])
#plot(PLSpoints$sand1km, PLSpoints$PLSdensity) # plot against percent cover

#PLSpoints$awc1km <- extract(awc1km.alb, PPLSpoints[,c('x', 'y')])
#plot(PLSpoints$awc1km, PLSpoints$pct.cov) # plot against percent cover

#PLSpoints$ksat1km <- extract(ksat1km.alb, PLSpoints[,c('x', 'y')])
#plot(PLSpoints$ksat1km, PLSpoints$pct.cov) # plot against percent cover

#for FIA data
FIAplots$sand1km <- extract(sand1km.alb, FIAplots[,c('x', 'y')])
plot(FIAplots$sand1km, FIAplots$pctcover) #plot against % cover

FIAplots$awc1km <- extract(awc1km.alb, FIAplots[,c('x', 'y')])
plot(FIAplots$awc1km, FIAplots$pctcover) #plot against % cover

FIAplots$ksat1km <- extract(ksat1km.alb, FIAplots[,c('x', 'y')])
plot(FIAplots$ksat1km, FIAplots$pctcover) #plot against % cover


# save the PLS and FIA data sets with soils data:
write.csv(FIAplots, "data/FIAplots_sand_soils.csv")
write.csv(PLSpoints, "data/PLScells_sand_soils.csv")
