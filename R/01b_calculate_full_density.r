
#Tree Density  calculations  
#Kelly Heilman               
#January 20, 2016   
#Updated October 26, 2016
#Code based on Simon Goring's code at: https://github.com/SimonGoring/WitnessTrees

library(plyr)
library(reshape2)
library(raster)
version <- "1.7-5"

#-----------------------load data------------------------------------------------

#read in final.data from the step_one_clean_IN.r script:
final.data <- read.csv(paste0("outputs/ndilin_pls_for_density_v",version,".csv"), stringsAsFactors = FALSE)
# corrections for stem density:
correction.factor <- read.csv("data//correction_factors.csv", header = TRUE)
final.data <- final.data[,1:23]

# read in final data from michigan
final.data.mi <- read.csv(paste0("data/lower_mi_final_data.csv"), stringsAsFactors = FALSE)
# corrections for stem density:
correction.factor.mi <- read.csv("data//MI_correction_factors.csv", header = TRUE)

# add the lower MI data below the INIL data: 

final.data <- rbind(final.data, final.data.mi)
correction.factor <- rbind(correction.factor, correction.factor.mi)

# also join together the lower MI species and upper mi species
species <- final.data[,14:17]
#------------------------Estimate Tree Density-----------------------------------
## Morisita estimates for indiana densities and basal area with charlies correction factors
# & no diameter veil
source('R/morisita.r') # morisita density estimator from Simon Goring's Witness Trees code

# morisita function calculates basal area and stem density
estimates <- morisita(final.data, correction.factor, veil = FALSE)

stem.density <- estimates[[1]]
basal.area <- estimates[[2]]

# there are some very high estimates of stem density 
summary(stem.density)
summary(basal.area)
zero.trees <- is.na(stem.density) 

#plot Histogram of point estimates of stem density
hist(stem.density, breaks = 100, xlim = c(0,1000))

#set stem.density where there are zero trees due to No tree or Wet or Water to 0
stem.density[zero.trees] <- 0
basal.area[zero.trees] <- 0

summary(stem.density)
summary(basal.area)

# make into a data frame and export as csv
stem.density <- data.frame(stem.density, basal.area, final.data)
write.csv(stem.density, paste0('outputs/IN_IL_densestimates_v',version,'.csv'))

#----------------------------Density Regridding------------------------------

##need to regrid the density estimates onto the paleon centroids
##create base raster that is extent of midwest domain

base.rast <- raster(xmn = -71000, xmx = 2297000, ncols=296,
                  ymn = 58000,  ymx = 1498000, nrows = 180,
                    crs = '+init=epsg:3175')



#coordinates(final.data)<- ~PointX+PointY

#create spatial object with density, basal area & diameters data
stem.density <- data.frame(x = final.data$PointX, 
                           y = final.data$PointY,
                           density = stem.density$stem.density,
                           basal   = stem.density$basal.area)#,
                          #diams = rowMeans(diams[,1:2], na.rm=TRUE) * 2.54)

# find the 99% percentile here for stem density and basal area:
#nine.nine.pct <- apply(stem.density[,3:4], 2, quantile, probs = 0.99, na.rm=TRUE)
#99th percentiles still seem high
#density     basal 
#1076.0074  251.9382 

# convert anything over 99th percentile to the 99th percentile value
stem.density$density[stem.density$density > nine.nine.pct['density']] <- nine.nine.pct['density']
stem.density$basal[stem.density$basal > nine.nine.pct['basal']] <- nine.nine.pct['basal']

ggplot(stem.density, aes(x, y, color=density))+geom_point(size =0.5)
# ---------------------fixing some lingering data naming issues:-------------------




#fix the captalized "No tree" problem
species[species == 'No Tree'] <- 'No tree'
species[species==""]<- "No tree"

#change all No tree densities to 0
stem.density$density[species[,1] == 'No tree'| species[,2]=='No tree'] <- 0
#classify trees as zero or as wet trees
zero.trees <- is.na(stem.density$density) & (species[,2] %in% c('No tree') | species[,1] %in% c('No tree'))
wet.trees <- (species[,2] %in% c('Wet', "Water") | species[,1] %in% c('Wet','Water'))

#designate all zero trees as density of 0
stem.density$density[zero.trees] <- 0
stem.density$basal[zero.trees] <- 0

#desgnate all wet trees as 0
stem.density$density[wet.trees] <- 0
stem.density$basal[wet.trees] <- 0

# kill cells with na for x or y:
stem.density <- stem.density[!is.na(stem.density$x),]

# make stem.density spatial
coordinates(stem.density)<- ~x+y
proj4string(stem.density)<-CRS('+init=epsg:3175')

# write to an arcGIS compatible shapefile
writeOGR(obj = stem.density, dsn = "outputs/stem_density_alb_v1.7-5.shp", layer = "stem_density_alb_v1.7-5", driver = "ESRI Shapefile", overwrite=TRUE)


#------------------------Formatting for biomass estimation-------------------------


numbered.rast <- setValues(base.rast, 1:ncell(base.rast))
numbered.cell <- extract(numbered.rast, spTransform(stem.density,CRSobj=CRS('+init=epsg:3175')))

species[species==""]<- "No tree" #gets rid of blank listing for no trees
final.data <- data.frame(final.data)
#final.data <- read.csv(paste0("outputs/ndilinpls_for_density_v",version,".csv"), stringsAsFactors = FALSE)

#create dataframe with stem density, speceies
spec.table <- data.frame(PointX = final.data$PointX, 
                         PointY = final.data$PointY,
                         cell = numbered.cell,
                         spec = c(as.character(final.data$species1),as.character(final.data$species2)),
                         count = 1,
                         point = 1:nrow(final.data),
                         density = rep(stem.density$density/2, 2),
                         #shhould density be /2 or not??
                         basal =  rep(stem.density$basal/2, 2),
                         diams = c(final.data$diam1, final.data$diam2),
                         dists = c(final.data$dist1, final.data$dist2))#,
                         #scc = stem.density$SCC,stringsAsFactors = FALSE)




#fix the captalized "No tree" problem
spec.table$spec[spec.table$spec == 'No Tree'] <- 'No tree'

#change all No tree densities to 0
spec.table$density[spec.table$spec == 'No tree'] <- 0
spec.table$density[spec.table$spec == 'Water'] <- 0
spec.table$density[spec.table$spec == 'Wet'] <- 0

#-----------------Estimating Biomass from density and diameter-------------------
# changing column names
spec.table$Pointx <- spec.table$PointX
spec.table$Pointy <- spec.table$PointY
spec.table[,1:2] <- xyFromCell(base.rast, spec.table$cell)

# read in table with allometric equations for each taxa
biom.table <- read.csv('data/plss.pft.conversion_v0.1-1.csv', 
                       stringsAsFactors = FALSE)

# this function calculates biomass of an individual tree using taxa-specific allometric equations
form <- function(x) {
  
  eqn <- match(x$spec, biom.table[,1])
  eqn[is.na(eqn)] <- 1  #  Sets it up for non-tree.
  
  b0 <- biom.table[eqn,2]
  b1 <- biom.table[eqn,3]
  
  biomass <- exp(b0 + b1 * log(x$diams))
  biomass
  
}

#  This is the biomass of individual trees.  It needs to be converted into
#  a stand level value, through the stem density estimate  The values are
#  in kg.

biomass <- rep(NA, nrow(spec.table))

for (i in 1:nrow(spec.table)) {
  # It's just really slow, so I do it this way to see what's happening.
  biomass[i] <- form(spec.table[i,])
  cat(i,'\n')
  flush.console()
}

# convert to Mg./hectare
spec.table$biom <- biomass * spec.table$density / 1000
#spec.table      <- spec.table[!is.na(spec.table$density), ]

spec.table$spec[spec.table$spec == 'No Tree'] <- 'No tree' # this should already be corrected

#spec.table <- spec.table[,2:14]
colnames(spec.table)[1:2] <- c("x", "y")# rename grid cell x and y colnames
write.csv(spec.table, 
        file = paste0('outputs/density_biomass_pointwise.ests_inilmi','_v', 
                      version, 
                      '.csv'), row.names = FALSE)

# in case you don't want to redo the biomass calcuations
spec.table<- read.csv(file = paste0('outputs/density_biomass_pointwise.ests_inilmi','_v', 
                       version, 
                       '.csv'))

pre.quantile <- spec.table

#take the 99 percentile of these, since density blows up in some places
nine.nine.pct <- apply(spec.table[,6:ncol(spec.table)], 2, quantile, probs = 0.99, na.rm=TRUE)
#count       point     density       basal       diams       dists      Pointx 
#1.0000  95230.1700    517.4408    200.6453     38.0000    567.0000 857335.2342 
#Pointy        biom 
#652074.7000   1321.4863
 
nine.five.pct <- apply(spec.table[,6:ncol(spec.table)], 2, quantile, probs = 0.95, na.rm=TRUE)
#count       point     density       basal       diams       dists      Pointx 
#1.0000  95230.1700    517.4408    200.6453     38.0000    567.0000 857335.2342 
#Pointy        biom 
#652074.7000   1321.4863 


# assign all points greater than the 99th percentile to 99th percentile values
spec.table$density[spec.table$density > nine.nine.pct['density']] <- nine.nine.pct['density']
spec.table$basal[spec.table$basal > nine.nine.pct['basal']] <- nine.nine.pct['basal']
spec.table  <- spec.table[!is.na(spec.table$density), ]

write.csv(spec.table, file=paste0('outputs/biomass_no_na_pointwise.ests_inilmi','_v',version, '.csv'), row.names = FALSE)

#-------------------------Paleon gridding----------------------------------------------

# These are not the full tables since the include only the Paleon grid cells with points in the database.
#dcast rearranges the spec.table data by x, y and cell
count.table <- dcast(spec.table, x + y + cell ~ spec, sum, na.rm=TRUE, value.var = 'count')

count.table.pt <- dcast(spec.table, Pointx + Pointy + cell ~ spec, sum, na.rm=TRUE, value.var = 'count')

unique.len <- function(x){length(unique(x))}

#melt data by by x, y, cell as rows and spec as columns and provide the sum or the number of unique points
biomass.trees  <- dcast(spec.table, x + y + cell ~ spec, sum, na.rm=TRUE, value.var = 'count')

#biomass.trees represnts the number of trees in each category

biomass.points <- dcast(spec.table, x + y + cell ~ spec, unique.len, value.var = 'point')
#biomass.points represents the number of unique points
# This is to get the points with trees:
spec.adj <- spec.table
spec.adj$tree <- spec.table$spec %in% "No tree"

treed.points <- dcast(spec.adj, x + y + cell ~ tree, unique.len, value.var = 'point')
treed.points <- treed.points[order(treed.points$cell),]
colnames(treed.points) <- c('x', 'y', 'cell', 'tree', 'no tree')

# Now get the total number of plots per cell:
plots_per_cell <- dcast(spec.table, x + y + cell ~ ., unique.len, value.var = 'point')
plots_per_cell <- plots_per_cell[order(plots_per_cell$cell),]

points_per_cell_df <- data.frame(plots_per_cell,
                                 treed = treed.points$tree,
                                 untreed = treed.points$`no tree`)

colnames(points_per_cell_df)[4:5] <- c("total_pls_points", "total_treed_points")


# points by cell is the # of points 
points.by.cell <- rowSums(biomass.points[, 4:ncol(biomass.points)], na.rm=TRUE)
trees.by.cell  <- rowSums(biomass.trees[,!colnames(biomass.trees) %in% c('x', 'y', 'cell', 'No tree','Wet', 'Water')], na.rm=TRUE)

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

# output preliminary density table to look at:
write.csv(density.table, "outputs/density.table_test.csv")


density.table$total = rowSums(density.table[,4:ncol(density.table)], na.rm=TRUE)

# plotting example taxa
X11(width =12)
ggplot(data = biomass.table, aes(x = x, y = y, fill = Beech)) + geom_raster()+coord_equal()+
  scale_fill_gradient(low = "yellow", high= "red")



biomass.table <- merge(biomass.table, points_per_cell_df[,c("x", "y", "cell", "total_pls_points", "total_treed_points")], by = c("x", "y", "cell"))
#biomass.table$plss_pts <- points_per_cell_df$total_pls_points
#biomass.table$plss_trees <- points_per_cell_df$total_treed_points

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
writeRaster(dens, "data/dens.grd", overwrite=TRUE)




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
composition.table <- composition.table[,!names(composition.table) %in% c('Water', "Wet", "No tree", "plss_pts")]
composition.table[,4:ncol(composition.table)] <- composition.table[,4:ncol(composition.table)]/rowSums(composition.table[,4:ncol(composition.table)], na.rm=TRUE)

# composition table without paleon grid NA values:
comp.inil <- composition.table[complete.cases(composition.table),]

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


add.v <- function(x, name, row.names){
  
  #  Quick file name formatter:
  
    p.ext <- '_alb'
  
  write.csv(x, paste0('data/outputs/', name,  '_v',version, '.csv'), row.names = row.names)
}

add.v(count.table, 'plss_counts', row.names=FALSE)
add.v(biomass.points, 'plss_points', row.names=FALSE)
add.v(count.table, "plss_composition", row.names=FALSE)
add.v(count.table, "plss_inil_composition", row.names=FALSE)
biomass.table$plsspts_cell <- points.by.cell

add.v(density.table, 'plss_density', row.names=FALSE)
add.v(basal.table, 'plss_basal', row.names=FALSE)
add.v(biomass.table, 'plss_biomass', row.names=FALSE)

add.v(biomass.full,  'plss_spec_biomass', row.names=FALSE) #full species biomass for indiana
add.v(density.full, 'plss_spec_density', row.names=FALSE)
add.v(diameter.full, 'plss_spec_diam', row.names=FALSE)
add.v(diam.table, 'plss_diam', row.names=FALSE)

# extra stuff to look at density estimates for now
#ggplot(density.table, aes(x = x, y=y, fill = total))+geom_raster()

#density.table$ecotype<- 'test'
#ecotype <- ifelse(density.table$total == 0,  "prairie", 
 #                 ifelse(density.table$total <= 47, "Savanna",
  #                       ifelse(density.table$total > 47, "Forest", "Check")))
#density.table$ecotype <- ecotype
#ggplot(density.table, aes(x = x, y=y, fill = ecotype)) + geom_raster()

