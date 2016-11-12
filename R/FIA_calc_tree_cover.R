# Code for generating % tree cover from Forest Inventory Analysis
# Date: November 11th, 2016
# Author: Kelly Heilman (kheilman@nd.edu)
# Download a state PLOT and TREE datatables. 
# initially, lets try Illinois Data: IL_plot and IL_Tree
library(sp)
library(ggplot2)
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

tree.sp <- merge(x= IL_TREE, y=PLOT.albers, by.x="PLT_CN", by.y = "CN")
                 
feettometers <- 0.3048
# need to make sure that the distances to trees are in meters, and we calculate xy coords of each tree in the plot, based on the xy coordiantes of the plot
# and the azimuth, and distance to each tree. We also add 1/2 of the diameter (also in m) to the distance to account for the distance to center of the tree  
# r trig functions take angles in radians

as_radians<- function(deg) {(deg * pi) / (180)}

tree.sp$TreeX1 <- tree.sp$LON + cos(as_radians(tree.sp$AZIMUTH))*((tree.sp$DIST*feettometers) + (0.5*tree.sp$DIA*feettometers))
tree.sp$TreeY1 <- tree.sp$LAT + sin(as_radians(tree.sp$AZIMUTH))*((tree.sp$DIST*feettometers) + (0.5*tree.sp$DIA*feettometers))

# now need to calculate a crown width for the trees in this plot
# need to match up a species code