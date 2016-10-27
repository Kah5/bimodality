##############################
#PLS Crown Width calculations#
#Kelly Heilman               #
#January 20, 2016   
#Updated October 26, 2016
##############################
library(plyr)
library(reshape2)
version <- "1.5-2"

final.data <- read.csv("outputs/ndilinpls_for_density_v1.5-2.csv", stringsAsFactors = FALSE)
# calculate stem density:
correction.factor <- read.csv("data//correction_factors.csv", header = TRUE)

## Morisita estimates for indiana densities and basal area with charlies correction factors
# & no diameter veil
source('R/morisita.r')

#make sure density is really being calculated correctly in morisita
estimates <- morisita(final.data, correction.factor, veil = FALSE)

stem.density <- estimates[[1]]
basal.area <- estimates[[2]]
summary(stem.density)
summary(basal.area)
zero.trees <- is.na(stem.density) & (species[,2] %in% c('No tree', 'Water', 'Wet') | species[,1] %in% c('No tree', 'Water', 'Wet'))
#plot Histogram of stem density
hist(stem.density, breaks = 1000, xlim = c(0,1000))


#set stem.density where there are zero trees due to No tree or Wet or Water to 0
stem.density[zero.trees] <- 0
basal.area[zero.trees] <- 0

summary(stem.density)
summary(basal.area)

stem.density <- data.frame(stem.density, basal.area, final.data)
write.csv(stem.density, 'outputs/IN_ILdensestimates_v1.5-2.csv')


## maximum Stem density estimates decreases when you remove trees below 8 cm veil line
##
source('R/morisita.R')
estimates.v <- morisita(final.data, correction.factor, veil = TRUE)

stem.density.v <- estimates.v[[1]]
basal.area.v <- estimates.v[[2]]
summary(stem.density.v)
summary(basal.area.v)
zero.trees <- is.na(stem.density.v) & (species[,2] %in% c('No tree', 'Water', 'Wet') | species[,1] %in% c('No tree', 'Water', 'Wet'))

stem.density.v[zero.trees] <- 0
stem.density.v[zero.trees] <- 0

summary(stem.density.v)
summary(basal.area.v)

#for now we will use these without the veil line

##need to regrid the density estimates onto the paleon centroids

##create base raster that is extent of midwest domain

base.rast <- raster(xmn = -71000, xmx = 2297000, ncols=296,
                  ymn = 58000,  ymx = 1498000, nrows = 180,
                    crs = '+init=epsg:3175')



###################################
#point level Crown Width  calculations #
###################################


coordinates(final.data)<- ~PointX+PointY

#create spatial object with density, basal area & diameters data
stem.density <- SpatialPointsDataFrame(coordinates(final.data), 
                                       data=data.frame(density = estimates[[1]],
                                                       basal   = estimates[[2]],
                                                       diams = rowMeans(diams[,1:2], na.rm=TRUE) * 2.54))

proj4string(stem.density)<-CRS('+init=epsg:3175')
numbered.rast <- setValues(base.rast, 1:ncell(base.rast))
numbered.cell <- extract(numbered.rast, spTransform(stem.density,CRSobj=CRS('+init=epsg:3175')))

species[species==""]<- "No tree" #gets rid of blank listing for no trees
final.data <- read.csv("outputs/ndilinpls_for_density_v1.5-2.csv", stringsAsFactors = FALSE)

#create dataframe with stem density, speceies
spec.table <- data.frame(PointX = final.data$PointX, 
                         PointY = final.data$PointY,
                         cell = numbered.cell,
                         spec = c(as.character(final.data$species1), as.character(final.data$species2)),
                         count = 1,
                         point = 1:nrow(final.data),
                         density = rep(stem.density$density/2, 2),
                         #shhould density be /2 or not??
                         basal =  rep(stem.density$basal/2, 2),
                         diams = c(final.data$diam1, final.data$diam2),
                         stringsAsFactors = FALSE)

#classify trees as zero or as wet trees
zero.trees <- is.na(stem.density$density) & (species[,2] %in% c('No tree') | species[,1] %in% c('No tree'))
wet.trees <- (species[,2] %in% c('Wet', "Water") | species[,1] %in% c('Wet','Water'))

#designate all zero trees as density of 0
stem.density$density[zero.trees] <- 0
stem.density$basal[zero.trees] <- 0

#desgnate all wet trees as 'NA'
stem.density$density[wet.trees] <- NA
stem.density$basal[wet.trees] <- NA

#fix the captalized "No tree" problem
spec.table$spec[spec.table$spec == 'No Tree'] <- 'No tree'

#change all No tree densities to 0
spec.table$density[spec.table$spec == 'No tree'] <- 0

####################################################3
# Estimate Biomass from density
###################################

spec.table[,1:2] <- xyFromCell(base.rast, spec.table$cell)

biom.table <- read.csv('data/plss.pft.conversion_v0.1-1.csv', 
                       stringsAsFactors = FALSE)

form <- function(x) {
  
  eqn <- match(x$spec, biom.table[,1])
  eqn[is.na(eqn)] <- 1  #  Sets it up for non-tree.
  
  b0 <- biom.table[eqn,2]
  b1 <- biom.table[eqn,3]
  
  biomass <- exp(b0 + b1 * log(x$diams * 2.54))
  biomass
  
}

#  This is the biomass of individual trees.  It needs to be converted into
#  a stand level value, through the stem density estimate?  The values are
#  in kg.

biomass <- rep(NA, nrow(spec.table))

for (i in 1:nrow(spec.table)) {
  # It's just really slow, so I do it this way to see what's happening.
  biomass[i] <- form(spec.table[i,])
  cat(i,'\n')
}

# convert to Mg.
spec.table$biom <- biomass * spec.table$density / 1000
spec.table      <- spec.table[!is.na(spec.table$density), ]

spec.table$spec[spec.table$spec == 'No Tree'] <- 'No tree'

colnames(spec.table)[1:2] <- c('x', 'y')

write.csv(spec.table, 
        file = paste0('outputs/density_biomass_pointwise.ests','_v', 
                      version, 
                      '.csv'))



spec.table <- spec.table[!is.na(spec.table$density), ]



saveRDS(spec.table, file=paste0('outputs/biomass_no_na_pointwise.ests','_v',version, '.RDS'))

#take the 99 percentile of these, since density blows up in some places
nine.nine.pct <- apply(spec.table[,6:ncol(spec.table)], 2, quantile, probs = 0.99, na.rm=TRUE)
#     point    density      basal      diams       biom 
#57436.3500   530.6562   188.2554    36.0000  1002.7144   
nine.five.pct <- apply(spec.table[,6:ncol(spec.table)], 2, quantile, probs = 0.95, na.rm=TRUE)
#      point     density       basal       diams        biom 
#55212.75000   204.90706    71.25188    30.00000   332.58766 

# assign all species greater than the 99th percentile to 99th percentile values
spec.table$density[spec.table$density > nine.nine.pct['density']] <- nine.nine.pct['density']
spec.table$basal[spec.table$basal > nine.nine.pct['basal']] <- nine.nine.pct['basal']






# These are not the full tables since the include only the cells with points in the database.
#dcast rearranges the spec.table data by x, y and cell
count.table <- dcast(spec.table, x + y + cell ~ spec, sum, na.rm=TRUE, value.var = 'count')

unique.len <- function(x){length(unique(x))}

#melt data by by x, y, cell as rows and spec as columns and provide the sum or the number of unique points
biomass.trees  <- dcast(spec.table, x + y + cell ~ spec, sum, na.rm=TRUE, value.var = 'count')

#biomass.trees represnts the number of trees in each category
biomass.points <- dcast(spec.table, x + y + cell ~ spec, unique.len, value.var = 'point')
#biomass.points represents the number of unique points

#sum the number of points per cell (includes no tree) and the number of trees per cell (not including no tree or Water)
#points.by.cell <- rowSums(count.table[,4:ncol(count.table)], na.rm=TRUE)

#patch fix for the non-georefereenced data of in & il
#twp.by.cell <- points.by.cell
#twp.by.cell[twp.by.cell< 111 ] <- 1
#twp.by.cell[twp.by.cell>=111 ]<- 2
#twp.by.cell[twp.by.cell>=211 ]<- 3

points.by.cell <- rowSums(biomass.points[, 4:ncol(biomass.points)], na.rm=TRUE)
trees.by.cell  <- rowSums(count.table[,!colnames(count.table) %in% c('x', 'y', 'cell', 'No tree','Wet', 'Water')], na.rm=TRUE)

#calculate the sum of total density, basal area, biomass & diameter by cell and species
density.table <- dcast(spec.table, x + y  + cell ~ spec, sum, na.rm=TRUE, value.var = 'density')
basal.table <- dcast(spec.table, x + y  + cell ~ spec, sum, na.rm=TRUE, value.var = 'basal')
biomass.table <- dcast(spec.table, x + y  + cell ~ spec, sum, na.rm=TRUE, value.var = 'biom')
diam.table <-  dcast(spec.table, x + y  + cell ~ spec, sum, na.rm=TRUE, value.var = 'diams')



#calculate standard deviations of density, basal area, biomass, and diameters
density.sd.table <- dcast(spec.table, x + y  + cell ~ spec, sd, na.rm=TRUE, value.var = 'density')
basal.sd.table <- dcast(spec.table, x + y  + cell ~ spec, sd, na.rm=TRUE, value.var = 'basal')
diam.sd.table <-  dcast(spec.table, x + y  + cell ~ spec, sd, na.rm=TRUE, value.var = 'diams')
biom.sd.table <- dcast(spec.table, x + y + cell ~ spec, sum, na.rm=TRUE, value.var = 'biom')

#  The function averages the estimates a single value for density, basal area, and biomass based on the points.by.cell
normalize <- function(x, mult = 2, value = points.by.cell) {
  x[,4:ncol(x)] <-  x[,4:ncol(x)] / value *mult; x}

density.table <- normalize(density.table)
basal.table <- normalize(basal.table)
diam.table <- normalize(diam.table, mult = 2.54, trees.by.cell)
biomass.table <- normalize(biomass.table)


X11(width =12)
ggplot(data = biomass.table, aes(x = x, y = y, color = Beech)) + geom_point()



biomass.table$plss_pts <- points.by.cell


#  We want rasterized versions of these tables with sums:
rast.fun <- function(x){
  
 to_grid <- data.frame(cell = x$cell, 
                  total = rowSums(x[,4:ncol(x)], na.rm=TRUE))
  
  empty <- rep(NA, ncell(base.rast))
  empty[to_grid$cell] <- to_grid$total
  setValues(base.rast, empty)
  }

base.rast <- raster(xmn = -71000, xmx = 2297000, ncols=296,
                    ymn = 58000,  ymx = 1498000, nrows = 180,
                    crs = '+init=epsg:3175')

#create total rasters
dens     <- rast.fun(density.table)
count.up <- rast.fun(count.table)
basal    <- rast.fun(basal.table)
mdiam    <- rast.fun(diam.table); mdiam[mdiam==0] <- NA
biomass <- rast.fun(biomass.table)

writeRaster(biomass, "data/biomass.grd", overwrite=TRUE)




#pdf("biomass.density.99percentile.rasters.pdf")
plot(biomass,xlim= c(320000 ,861200.5), ylim = c(104720.2,708673.5), main = "Mean total biomass (Mg/ha)", xlab ="Easting", ylab = "Northing") 
plot(dens, xlim= c(320000 ,861200.5), ylim = c(104720.2,708673.5), main = "Mean stem density (stems/ha)", xlab ="Easting", ylab = "Northing")
#plot(biomass, xlim= c(320000 ,861200.5), ylim = c(104720.2,708673.5), main = "Mean biomass (Mg/ha)", xlab ="Easting", ylab = "Northing")
plot(basal, xlim= c(592741.1 ,861200.5), ylim = c(104720.2,708673.5), main = "Mean basal area")
plot(mdiam, xlim= c(320000 ,861200.5), ylim = c(104720.2,708673.5), main = "Mean tree diameter (cm)", xlab ="Easting", ylab = "Northing")
#dev.off()


#to get sd for these
rowset <- cbind(1:(nrow(spec.table)/2), (nrow(spec.table)/2+1):nrow(spec.table))

sd.table <- data.frame(cell = spec.table$cell[rowset[,1]],
                       density = spec.table$density[rowset[,1]] * 2,
                       basal   = spec.table$basal[rowset[,1]] + spec.table$basal[rowset[,2]],
                       biomass = spec.table$biom[rowset[,1]] + spec.table$biom[rowset[,2]],
                       diam = spec.table$diam[rowset[,1]] + spec.table$diam[rowset[,2]])

dens.sd <- dcast(sd.table, cell ~ ., fun.aggregate=sd, na.rm=TRUE, value.var = 'density')
basal.sd <- dcast(sd.table, cell ~ ., sd, na.rm=TRUE, value.var = 'basal')
biomass.sd <- dcast(sd.table, cell ~ ., sd, na.rm=TRUE, value.var = 'biomass')
diam.sd <- dcast(sd.table, cell ~., fun.aggregate = sd, na.rm = TRUE, value.var = 'diam')
colnames(dens.sd) <-c("cell", "dens.sd")
colnames(basal.sd) <-c("cell", "basal.sd")
colnames(biomass.sd) <-c("cell", "biomass.sd")
colnames(diam.sd) <-c("cell", "diam.sd")

#combine sd's with the tables
density.table <- merge(density.table, dens.sd, by = "cell")
diam.table <- merge(diam.table, diam.sd, by = "cell")



data.table <- data.frame(xyFromCell(dens, 1:ncell(dens)), 
                         stem.dens = getValues(dens),
                         basal.area = getValues(basal),
                         biomass = getValues(biomass),
                         diam = getValues(mdiam))

data.table <- data.table[!is.na(data.table[,3]), ]
write.csv(data.table, paste0('data/density.basal.biomass_alb','_v',1, '.csv'))


#  Now we need to add zero cells to the dataframe:
comp.table <- data.frame(xyFromCell(base.rast, 1:ncell(base.rast)), 
                         cell = 1:ncell(base.rast),
                         matrix(ncol = ncol(biomass.table)-3, nrow = ncell(base.rast)))

colnames(comp.table)[4:ncol(biomass.table)] <- colnames(biomass.table)[4:ncol(comp.table)]

reform <- function(x){
  comp.table[x$cell,] <- x
  comp.table
}


biomass.full <- reform(biomass.table)
density.full <- reform(density.table)
diameter.full <- reform(diam.table)
composition.table <- reform(basal.table)


#not sure why you need this next line of code
composition.table[,4:ncol(composition.table)] <- composition.table[,4:ncol(composition.table)]/rowSums(composition.table[,4:ncol(composition.table)], na.rm=TRUE)
#ind_il <- readOGR(dsn = "C:/Users/Kelly/Documents/Indiana_Density_Biomass/Data//IL_IN_merge_project.shp", layer = 'IL_IN_merge_project')
#ind_il <- spTransform(ind_il, CRS('+proj=longlat +ellps=WGS84'))
#ind_il<- spTransform(ind_il, CRS('+proj=aea +lat_1=42.122774 +lat_2=49.01518 +lat_0=45.568977 +lon_0=-83.248627 +x_0=1000000 +y_0=1000000 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

#extract biomass.full table by the extent of the state of indiana to get a datatable
#with just indiana data

biomass.df.test <- extract(biomass, extent(ind_il), cellnumbers=TRUE, xy=TRUE)
#biomass.df.test has the cell numbers of the extent of indiana, so I will use these

#get the indiana biomass.full cells out of biomass.full
biomass.df.test.cells <- data.frame(biomass.df.test[,1])
colnames(biomass.df.test.cells) <- 'cell'
biomass.indiana <- merge(biomass.full, biomass.df.test.cells, by ="cell")

#to get biomass.pts and tree.pts, use merge(x, y, by, all=TRUE)
cells.xy.ind <- biomass.indiana[,1:3] #xy and cells in indiana
biomass.points.ind <- merge(cells.xy.ind, biomass.points, all=TRUE)
count.trees.ind <- merge(cells.xy.ind, count.table, all=TRUE)


add.v <- function(x, name){
  
  #  Quick file name formatter:
  
    p.ext <- '_alb'
  
  write.csv(x, paste0('data/outputs/', name,  '_v',1, '.csv'))
}

add.v(count.table, 'plss_trees')
add.v(biomass.points, 'plss_points')
#add.v(biomass.points.pft, 'plss_points_pft')
#add.v(biomass.trees.pft, 'plss_trees_pft')
#add the total number of points per cell to end of biomass.table
biomass.table$plsspts_cell <- points.by.cell
#biomass.indiana$plsspts_cell <- points.by.cell
add.v(density.table, 'plss_density')
add.v(basal.table, 'plss_basal')
add.v(biomass.table, 'plss_biomass')
#add.v(crown.table, 'plss_crown_area')
#add.v(crown.dens.table, 'plss_crown_dens')
add.v(biomass.full,  'plss_spec_biomass') #full species biomass for indiana
add.v(density.full, 'plss_spec_density')
add.v(diameter.full, 'plss_spec_diam')
add.v(diam.table, 'plss_diam')
#add.v(crown.full, 'plss_spec_crown_area')
#add.v(crown.dens.full, 'plss_spec_crown_area_dens')
#these three outputs created for biomass model runs may 21, 2015
#add.v(biomass.indiana, 'plss_biomass_indiana_illinois')
#add.v(biomass.points.ind, 'plss_points_indiana_illinois')
#add.v(count.trees.ind, 'plss_trees_indiana_illinois')

