#Kelly Heilman 
#April 19th, 2016
#this script calculates the stem density & basal area over georeferenced points in Indiana
#We calculate the density & basal area for trees over 8 inches diameter
#this script  uses 'final.data' produced in step_one_georef_clean_IN.r
library(plyr)
library(reshape2)
library(ggplot2)

#
correction.factor <- read.csv("Data//correction_factors.csv", header = TRUE)

## Morisita estimates for indiana densities and basal area with charlies correction factors
# & no diameter veil
source('R/morisita.r')

#make sure density is really being calculated correctly in morisita
estimates <- morisita(final.data, correction.factor, veil = FALSE)

stem.density <- estimates[[1]]
basal.area <- estimates[[2]]
summary(stem.density)
summary(basal.area)
zero.trees <-(final.data$species2 %in% c('No tree', 'Water', 'Wet') | final.data$species1 %in% c('No tree', 'Water', 'Wet'))

#set stem.density where there are zero trees due to No tree or Wet or Water to 0
stem.density[zero.trees] <- 0
basal.area[zero.trees] <- 0

summary(stem.density)
summary(basal.area)

stem.dens <- data.frame(stem.density, basal.area)
#write.csv(stem.density, 'IN_ILdensestimates_v1.5.csv')

#find the 99th percentile of basal area & stem density
nine.nine.pct <- apply(stem.dens, 2, quantile, probs = 0.99, na.rm=TRUE)

#make all values higher than the 99th percentile equal to the 99th percentile--this helps eliminate the blowing up of density values
stem.dens$stem.density[stem.dens$stem.density > nine.nine.pct['stem.density']] <- nine.nine.pct['stem.density']
stem.dens$basal.area[stem.dens$basal.area > nine.nine.pct['basal']] <- nine.nine.pct['basal']





#create a spatial points dataframe that has density & basal area on it
coordinates(final.data)<- ~PointX+PointY 
density <- SpatialPointsDataFrame(coordinates(final.data), 
                                  data=data.frame(density = stem.dens$stem.density,
                                                  basal   = stem.dens$basal.area))

proj4string(density)<-CRS('+init=epsg:3175') # assign the great lakes albers projection to dataset

#writeOGR(density, dsn = "Shapefiles/in_point_density_alb.shp", layer = "Shapefiles/in_point_density_alb", driver = "ESRI Shapefile")

##make sure I can read it
#setwd("C:/Users/Kelly/Documents/biomodality/Shapefiles")
#test <- readOGR(dsn = "il_point_density_alb.shp", layer = "il_point_density_alb", dropNULLGeometries = TRUE)

spplot(density, "density")
density.df <- data.frame(density)

final.data <- data.frame(final.data)
density.df <- merge(final.data, density.df, by = c("PointX", "PointY"))

density.df$spec1summ <- "Other Taxa"
density.df$spec1summ[density.df$species1 == "Oak"] <- "Oak"
density.df$spec1summ[density.df$species1 == "No tree"] <- "No tree"
density.df$spec1summ[density.df$species1 == "Water" | density.df$species1 == 'Wet'] <- "water/wet"


density.df$spec2summ <- "Other Taxa"
density.df$spec2summ[density.df$species2 == "Oak"] <- "Oak"
density.df$spec2summ[density.df$species2 == "No tree"] <- "No tree"
density.df$spec2summ[density.df$species2 == "Water" | density.df$species2 == 'Wet'] <- "water/wet"

#plot
X11(width =12)
ggplot(data = density.df, aes(x = PointX, y = PointY, color = density)) + geom_point()

X11(width =12)
ggplot(data = density.df, aes(x = PointX, y = PointY, color = spec1summ)) + geom_point()

X11(width =12)
ggplot(data = density.df, aes(x = PointX, y = PointY, color = spec2summ)) + geom_point()
