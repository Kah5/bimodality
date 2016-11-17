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
spec.codes <- read.csv('data/FIA_conversion-SGD_remove_dups.csv', stringsAsFactor = FALSE)
spec.codes$SPCD <- spec.codes$spcd
spec.codes$Paleon <- spec.codes$PalEON

tree.sp <- merge(tree.sp, spec.codes[, c('SPCD', 'PalEON')],  by = "SPCD")

CW.table <- read.csv('data/FHM_paleon_crown_allometry_coeff.csv', 
                     stringsAsFactors=FALSE)

form <- function(x){
  
  eqn <- match(x$PalEON, CW.table[,2])
  eqn[is.na(eqn)] <- 1  #  Sets it up for non-tree.
  
  b0 <- CW.table[eqn,3]
  b1 <- CW.table[eqn,4]
  
  CW <- (b0 + b1 * (x$DIA*2.54))
  CW
}

CW <- rep(1, nrow(tree.sp))

#this function takes a really long time for all the il points, can we use an apply

for(i in 1:nrow(tree.sp)){
  CW[i] <- form(tree.sp[i,])
  cat(i,'\n')
}

summary(CW)

tree.sp$CROWNWIDTH <- CW # add the crown widths to the dataset
#for now, remove the NA values
tree.sp <- tree.sp[!is.na(tree.sp$CROWNWIDTH),]
#there are alot of NAs in the DIST column too, this is potentially problematic
tree.sp <- tree.sp[!is.na(tree.sp$DIST),]

#now we need to code which trees have crown widths that extend greater than their distance to the center point
tree.sp$coverscenter <- 0 #value if no trees cover center
tree.sp[tree.sp$CROWNWIDTH < tree.sp$DIST, ]$coverscenter <- 1

#next, we need to provide a value per plot that indicates if we have at least 1 tree covereing the FIA plot center


