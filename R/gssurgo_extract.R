# Extract soil covariates from gssurgo soil data
# Author: Kelly Heilman
# Data: 12/2/16

# Preprocessing details:
# soils data are from gSSURGO soil data 10m rasters downloaded by state
# Soil parameter values % sand were joined to the rasters using the SSURGO on-Demand arcgis toolbox, and the weighted average of soil parameter over 0-30 depth were calculated
# state soil rasters were mosaicked together to create one raster for the upper midwest (mosaic to raster tool)
# rasters were aggreated to 1km and 8km using the aggregate tool in arcgis (this was much faster than aggregating in R)

library(raster)
sand8km <- raster("data/8km_UMW_sand1.tif")
plot(sand8km)

sand1km <- raster("data/1km_UMW_sand1.tif")
plot(sand1km)

# need to project sand to great lakes albers coordinate system
sand8km.alb <- projectRaster(sand8km, crs ='+init=epsg:3175')
sand1km.alb <- projectRaster(sand1km, crs = '+init=epsg:3175')

# load in data to extract by
FIAplots <- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/outputs/FIA_plot_agg_fuzzed_alb.csv")
PLSpoints.agg <- read.csv ("C:/Users/JMac/Documents/Kelly/biomodality/outputs/PLS_pct_cov_by_pt_inil.csv")


#extract and add to FIA data:

FIAplots$sandpct <- extract(sand8km.alb, FIAplots[,c('x', 'y')])
plot(FIAplots$sandpct, FIAplots$pctcover) #plot against % cover

#extract and add to PLS data:
PLSpoints$sandpct <- extract(sand8km.alb, PLSpoints[,c('Pointx', 'Pointy')])
plot(PLSpoints$sandpct, PLSpoints$pct.cov) # plot against percent cover

#extract from the 1km grid 
PLSpoints$sand1km <- extract(sand1km.alb, PLSpoints[,c('Pointx', 'Pointy')])
plot(PLSpoints$sand1km, PLSpoints$pct.cov) # plot against percent cover

FIAplots$sand1km <- extract(sand1km.alb, FIAplots[,c('x', 'y')])
plot(FIAplots$sand1km, FIAplots$pctcover) #plot against % cover


# save the PLS and FIA data sets with soils data:
write.csv(FIAplots, "data/FIAplots_sand_soils.csv")
write.csv(PLSpoints, "data/PLSpoits_sand_soils.csv")
