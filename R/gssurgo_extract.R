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


#load