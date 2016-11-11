# Code for generating % tree cover from Forest Inventory Analysis
# Date: November 11th, 2016
# Author: Kelly Heilman (kheilman@nd.edu)
# Download a state PLOT and TREE datatables. 
# initially, lets try Illinois Data: IL_plot and IL_Tree
library(sp)

IL_PLOT <- read.csv( "data/IL_PLOT.csv" )
IL_TREE <- read.csv("data/IL_TREE.csv")

#IL_PLOT has the fuzzed plot coordinates
#IL_TREE has the azimuth, diameter, and direction from the plot center to create the stem maps
# they can be merged based on the PLT_CN


#before we can map each FIA stand, we need to convert the FIA lat long coordinates to great lakes albers projections (in meters)
coordinates(IL_PLOT) <- ~ LON + LAT # make this a spatial object
proj4string(IL_PLOT)<-CRS( "+proj=longlat +datum=WGS84" ) #define at the WGS84 latl
#now convert to great lakes albers
PLOT.albers <- spTransform(IL_PLOT ,CRS("+init=epsg:3175"))

PLOT.albers <- data.frame(PLOT.albers)

merge(PLOT.albers[,c("PLOT_CN","LAT", "LON")], IL_TREE, by = c("CN", "CN"))

###need to make sure that the distances to trees are in meters

final.data$TreeX1 <- final.data$PointX + cos(as_radians(final.data$az1))*final.data$dist1
final.data$TreeY1 <- final.data$PointY + sin(as_radians(final.data$az1))*final.data$dist1
final.data$TreeX2 <- final.data$PointX + cos(as_radians(final.data$az2))*final.data$dist2
final.data$TreeY2 <- final.data$PointY + sin(as_radians(final.data$az2))*final.data$dist2
