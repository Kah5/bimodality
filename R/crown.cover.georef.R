#Kelly Heilman 
#April 19th, 2016
#this script is intended as a first pass at estimating crown or % covere in the pls data
#this script  uses 'final.data' produced in step_one_clean_IN.r
library(plyr)
library(reshape2)
library(ggplot2)
library(aspace)

#read.csv("data/pointwise")

###need to make sure that the distances to trees are in meters

final.data$TreeX1 <- final.data$PointX + cos(as_radians(final.data$az1))*final.data$dist1
final.data$TreeY1 <- final.data$PointY + sin(as_radians(final.data$az1))*final.data$dist1
final.data$TreeX2 <- final.data$PointX + cos(as_radians(final.data$az2))*final.data$dist2
final.data$TreeY2 <- final.data$PointY + sin(as_radians(final.data$az2))*final.data$dist2


correction.factor <- read.csv("Data//correction_factors.csv", header = TRUE)

## Morisita estimates for indiana densities and basal area with charlies correction factors
# & no diameter veil
source('R/morisita.r')

#make sure density is really being calculated correctly in morisita
estimates <- morisita(final.data, correction.factor, veil = FALSE)

stem.density <- estimates[[1]]
basal.area <- estimates[[2]]
plot.area <- estimates[[3]]
summary(stem.density)
summary(basal.area)
zero.trees <- is.na(stem.density) & (species[,2] %in% c('No tree', 'Water', 'Wet') | species[,1] %in% c('No tree', 'Water', 'Wet'))

#set stem.density where there are zero trees due to No tree or Wet or Water to 0
stem.density[zero.trees] <- 0
basal.area[zero.trees] <- 0

summary(stem.density)
summary(basal.area)

stem.dens <- data.frame(stem.density, basal.area)
#write.csv(stem.density, 'IN_ILdensestimates_v1.5.csv')

nine.nine.pct <- apply(stem.dens, 2, quantile, probs = 0.95, na.rm=TRUE)
stem.dens$stem.density[stem.dens$stem.density > nine.nine.pct['stem.density']] <- nine.nine.pct['stem.density']
stem.dens$basal.area[stem.dens$basal.area > nine.nine.pct['basal']] <- nine.nine.pct['basal']



#b0 <- 0.529302
#b1 <- 0.137036

#crown.area.1 <- pi*((b0 + b1 * stem.density$diam1)^2) # assuming diams in cm
#crown.area.2 <- pi*((b0 + b1 * stem.density$diam2)^2)

##create base raster that is extent of midwest domain

base.rast <- raster(xmn = -71000, xmx = 2297000, ncols=296,
                    ymn = 58000,  ymx = 1498000, nrows = 180,
                    crs = '+init=epsg:3175')
coordinates(final.data)<- ~PointX+PointY

#create spatial object with density, basal area & diameters data
density <- SpatialPointsDataFrame(coordinates(final.data), 
                                       data=data.frame(density = estimates[[1]],
                                                       basal   = estimates[[2]],
                                                       diams = rowMeans(diams[,1:2], na.rm=TRUE) * 2.54))

proj4string(density)<-CRS('+init=epsg:3175')
numbered.rast <- setValues(base.rast, 1:ncell(base.rast))
numbered.cell <- extract(numbered.rast, spTransform(density,CRSobj=CRS('+init=epsg:3175')))

species[species==""]<- "No tree" #gets rid of blank listing for no trees

final.data <- data.frame(final.data)
#create dataframe with stem density, speceies
spec.table <- data.frame(xyFromCell(base.rast, numbered.cell),
                         cell = numbered.cell,
                         PointX = final.data$PointX, 
                         PointY = final.data$PointY,
                         TreeX = c(final.data$TreeX1, final.data$Treex2),
                         TreeY = c(final.data$TreeY1, final.data$TreeY2),
                         spec = c(as.character(final.data$species1), as.character(final.data$species2)),
                         density = rep(stem.dens$stem.density/2, 2),
                         basal =  rep(stem.dens$basal.area/2, 2),
                         plot.area = plot.area,
                         diams = c(final.data$diam1, final.data$diam2),
                         stringsAsFactors = FALSE)
#library(sampSurf)
#sp.dbh = spCircle(spec.table$density, centerPoint=c(x=spec.table$PointX, y=spec.table$PointY), spID='tree.1') 

#classify trees as zero or as wet trees
zero.trees <- is.na(stem.dens$stem.density) & (final.data$species1 %in% c('No tree') | final.data$species2 %in% c('No tree'))
wet.trees <- (final.data$species1 %in% c('Wet', "Water") | final.data$species2 %in% c('Wet','Water'))

#designate all zero trees as density of 0
stem.dens$stem.density[zero.trees] <- 0
stem.dens$basal.area[zero.trees] <- 0

#desgnate all wet trees as 'NA'
stem.dens$stem.density[wet.trees] <- NA
stem.dens$basal.area[wet.trees] <- NA

#fix the captalized "No tree" problem
spec.table$spec[spec.table$spec == 'No Tree'] <- 'No tree'

#change all No tree densities to 0
spec.table$density[spec.table$spec == 'No tree'] <- 0



##this section estimates point level crown widths using density and allometic equation s 
#allometric equations from simon
CW.table <- read.csv('data/FHM_paleon_crown_allometry_coeff.csv', 
                     stringsAsFactors=FALSE)

form <- function(x){
  
  eqn <- match(x$spec, CW.table[,2])
  eqn[is.na(eqn)] <- 1  #  Sets it up for non-tree.
  
  b0 <- CW.table[eqn,3]
  b1 <- CW.table[eqn,4]
  
  CW <- (b0 + b1 * (x$diams))
  CW
}

CW <- rep(NA, nrow(spec.table))

for(i in 1:nrow(spec.table)){
  CW[i] <- form(spec.table[i,])
  cat(i,'\n')
}

summary(CW)

spec.table$CW <- CW #add crown width to the spec.table
spec.table<- spec.table[complete.cases(spec.table),] #get rid of NA values
spec.table[spec.table$density < 1,] <- NA # make densities less than one NA

spec.table<- spec.table[complete.cases(spec.table),] #get rid of na values

nine.nine.pct <- apply(spec.table[,7:ncol(spec.table)], 2, quantile, probs = 0.99, na.rm=TRUE)
#point    density      basal      diams       biom 
#49506.0000   504.5722   520.1407    55.0000  3520.7191   
nine.five.pct <- apply(spec.table[,7:ncol(spec.table)], 2, quantile, probs = 0.95, na.rm=TRUE)
#point    density      basal      diams       biom 
#47511.0000   207.6151   217.5150    55.0000   930.0320 

#assign all species greater than the 99th percentile to 99th percentile values
spec.table$density[spec.table$density > nine.nine.pct['density']] <- nine.nine.pct['density']
spec.table$basal[spec.table$basal > nine.nine.pct['basal']] <- nine.nine.pct['basal']
spec.table$crown.area[spec.table$CW > nine.nine.pct['CW']] <- nine.nine.pct['CW']
spec.table$crown.scaled[spec.table$crown.scaled > nine.nine.pct['crown.scaled']] <- nine.nine.pct['crown.scaled']

spec.table$crown.area <- 0.25*pi*(spec.table$CW^2) #crown area of each tree
spec.table$ratio <- spec.table$crown.area/spec.table$plot.area
spec.table$cover <- spec.table$crown.area*2 * spec.table$density
spec.table$crown.scaled <-100*((spec.table$crown.area*2*spec.table$density)/10000) #(crown area (m2))/tree)*(trees/hectare)

CC.adj <-  100*(1-exp(-0.01*spec.table$crown.scaled))
spec.table$CC.adj <- CC.adj

hist(spec.table$crown.scaled, breaks = 200, xlim = c(0,200))





#now write species table as a shapefile with the TreeX and TreeY as coordinates


coordinates(spec.table)<- ~TreeX+TreeY 
geo.tree <- SpatialPointsDataFrame(coordinates(spec.table), 
                                  data=data.frame(spec.table))

proj4string(geo.tree)<-CRS('+init=epsg:3175') # assign the great lakes albers projection to dataset

library(rgdal)
writeOGR(geo.tree, dsn = "Shapefiles/in_tree_alb.shp", layer = "Shapefiles/in_tree_alb", driver = "ESRI Shapefile")


## now make a shapefile with thesame dataset, but using pointX & pointY as the coordinates
spec.table <- data.frame(spec.table)
coordinates(spec.table)<- ~PointX+PointY 
geo.point <- SpatialPointsDataFrame(coordinates(spec.table), 
                                   data=data.frame(spec.table))

proj4string(geo.point)<-CRS('+init=epsg:3175') # assign the great lakes albers projection to dataset
writeOGR(geo.point, dsn = "Shapefiles/in_point_alb.shp", layer = "Shapefiles/in_point_alb", driver = "ESRI Shapefile")




#make plots
library(plyr)
library(reshape2)


#need to aggregate the spec.table cover values by each point
Crown.width <- dcast(spec.table, x + y ~. , sum, na.rm=TRUE, value.var = 'CW')
Crown.area <- dcast(spec.table, x + y ~. , sum, na.rm=TRUE, value.var = 'crown.area')
Crown.scales <- dcast(spec.table, x + y ~. , sum, na.rm=TRUE, value.var = 'crown.scaled')


hist(Crown.width[,3])
hist(Crown.area[,3], breaks = 25)
hist(Crown.scales[,3], breaks = 25)



##plot these across the region 
ggplot(data = spec.table, aes(x = PointX, y = PointY, color = density)) + geom_point()
ggplot(data = spec.table, aes(x = PointX, y = PointY, color = CW)) + geom_point()
ggplot(data = spec.table, aes(x = PointX, y = PointY, color = basal)) + geom_point()
ggplot(data = spec.table, aes(x = PointX, y = PointY, color = crown.scaled)) + geom_point()
ggplot(data = spec.table, aes(x = PointX, y = PointY, color = CC.adj)) + geom_point()
ggplot(data = spec.table, aes(x = PointX, y = PointY, color = cover)) + geom_point()

ggplot(data = Crown.area, aes(x = x, y = y, color = .)) + geom_point()

ggplot(data = Crown.width, aes(x = x, y = y, color = .)) + geom_point()



stem.density$ca1 <- crown.area.1
stem.density$ca2 <- crown.area.2
stem.density$ca.mean <- (crown.area.1 +crown.area.2)/2
stem.density$ca.total <- crown.area.1 +crown.area.2
stem.density$ca.total.8km <- (stem.density$ca1+stem.density$ca2)/(10000)

ggplot(data = stem.density, aes(x = PointX, y = PointY, color = ca.mean)) + geom_point()

coordinates(stem.density)<- ~PointX+PointY

proj4string(stem.density)<-CRS('+init=epsg:3175')

numbered.rast <- setValues(base.rast, 1:ncell(base.rast))
numbered.cell <- extract(numbered.rast, spTransform(stem.density,CRSobj=CRS('+init=epsg:3175')))

#species[species==""]<- "No tree" #gets rid of blank listing for no trees

#stem.dens.agg <- 
  stem.density <- data.frame(stem.density)
table <- data.frame(xyFromCell(base.rast, numbered.cell),
                        count = 1,
                         cell = numbered.cell,
                         stem.density,
                         stringsAsFactors = FALSE)
table$stem.density.m <- table$stem.density*6400 #this converts stems  per hectare to stems per grid cell
table$stem.density.8km <- (table$stem.density*6400)

table$ca.pt.i <- table$stem.density.8km*table$ca.total.8km # what each corner point takes up within grid cell

ca.totals<- ddply(table,~ cell,  summarise,  x = mean(x), y= mean(y), mean.ca = mean(ca.total, na.rm=TRUE), mean.dens=mean(stem.density, na.rm=TRUE), 
                  mean.dens.m = mean(stem.density.m, na.rm=TRUE), ca.dens.pt = mean((ca.total*stem.density.m), na.rm=TRUE), 
                  sum.ca = sum(ca.pt.i, na.rm=TRUE),
                  mean.ca = mean(ca.pt.i, na.rm=TRUE),
                  mean.ca2 = mean(ca.total, na.rm=TRUE),
                  total.pts = sum(count, na.rm= TRUE))


#ca.totals$uncorrected.cover<- (((ca.totals$mean.ca*ca.totals$mean.dens)*6400)/(8000*8000))*100

ca.totals$uncorrected.cover<- ((ca.totals$mean.ca2*ca.totals$mean.dens.m)/(8000*8000))*100

#uncorrected.cover<- 100*(ca.totals$sum.ca)/(8000*8000)

#ca.totals$pct.cov <- ca.totals$mean.ca*ca.totals$mean.dens/10000
#ca.totals$dens.8km <- ca.totals$mean.dens.m

#FIA correctiong for overlapping trees
ca.totals$C <- 100*(1-exp(-0.01*ca.totals$uncorrected.cover))
pdf("percent.cover.pdf")
par(mfrow=c(2,1))

hist(ca.totals$uncorrected.cover, breaks = 25, main = "Uncorrected Cover", xlab = "%cover")
hist (ca.totals$C, breaks = 25, main = "Corrected Cover", xlab = "Corrected % Cover")

dev.off()
coordinates(ca.totals) <- ~x+y
spplot(ca.totals, "C")

# make a better plot than this
ca.totals<- data.frame(ca.totals)

library(maps)
#load us map data
all_states <- map_data("state")
states <- subset(all_states, region %in% c( "indiana" , "illinois" ) )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS(great_lakes))
mapdata<-data.frame(mapdata)

pdf("crown_maps.pdf")
ca.map <- ggplot() +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group))+ scale_x_continuous(limits = c(250000, 1000000)) +
  scale_y_continuous(limits=c(0, 800000))
ca.map <- ca.map + geom_point(data=ca.totals, aes(x=x, y=y, color = C)) + labs(x="easting", y="northing", title="PLS average C in Indiana and Illinois") + scale_color_gradientn(colours = rainbow(4), name ="Total density")
ca.map

ca.un.map <- ggplot() +geom_polygon(data=data.frame(mapdata), aes(x=long, y=lat, group=group))+ scale_x_continuous(limits = c(250000, 1000000)) +
  scale_y_continuous(limits=c(0, 800000))
ca.un.map <- ca.un.map + geom_point(data=ca.totals, aes(x=x, y=y, color = uncorrected.cover)) + labs(x="easting", y="northing", title="PLS uncorrected C in Indiana and Illinois") + scale_color_gradientn(colours = rainbow(4), name ="Total density")
ca.un.map
dev.off()

#ca.totals$pct.cover.m2 <- ca.totals$mean.dens.m * ca.totals$mean.ca
#ca.totals$pct.cover.ha <- ca.totals$mean.ca * ca.totals$mean.dens 

#pct.cover[pct.cover>1] <- 1
#assume that if pct.cover is over 1 that it is = 100%


##aggregated or not?
# need P --point to tree distances, and I--tree to tree distances
