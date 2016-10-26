#   This code obtained from Simon Goring and adapted to be applied to Indiana Data
#
#  This file opens the Wisconsin and Minnesota datasets, renames the columns of
#  the Minnesota shapefile to match those of the Wisconsin dataset, and then
#  binds a number of columns from both datasets together (but not the complete
#  set of columns, since there are some columns unique to each dataset.  The
#  The ultimate dataset has the following columns:
#  Point:  PLS point number
#  Township:  Township line
#  Range:  Range line
#  diam (1 through 4):  Bearing tree diameter
#  dist  (1 through 4):  Distance to bearing tree
#  species  (1 through 4):  Species of the bearing tree based on the lumping 
#                           described in 'witnesstreecodesv4.csv'
#  az (1 through 4): Azimuth to the bearing trees.

#  Data:  The following files are distributed with this code:
#  Maps/glo_corn_ex.shp : This dataset represents a subset of the full Wisconsin data,
#  and uses only 1% of the original dataset.
#  Maps/Minnesota_ex.shp : 5% of the original Minnesota dataset.
#  Maps/glpotveg_5min.asc : The potential vegetation map of Ramankutty and Foley.
#  witnesstreecodesv4.csv : The lumping codes used with the witness tree dataset.

#  Simon Goring, January 28, 2012.
#  Updated {1.3): Nov 18, 2014
#  Updated (1.4): Dec  8, 2014
#  Kelly Heilman 1.5: April 14, 2015
#  Updated (1.5-2: illinois georeferenced): October 24th, 2016
library(sp)
library(spdep)
library(rgdal)
library(raster)
library(ggplot2)
library(Rcpp)
# Read in the data
ind <- read.csv("data/IN v1.5-1 Georeferenced/ndinpls_v1.5-1.csv", stringsAsFactors = FALSE)

# Read in the il data
il <- read.csv("data/ndilpls_v1.5-2/ndilpls_v1.5-2.csv", stringsAsFactors = FALSE) # version 1.5-2
#il <- read.csv("data/ndilpls_v1.5-1.csv", stringsAsFactors = FALSE) # version 1.5-1
ggplot(data = il, aes(x = x, y = y, color = chainstree)) + geom_point()
#il[is.na(il)] <- '' #fixes problems with 'NA' in dataset


#coordinates(il) <- ~x+y
#X11(width = 12)
#spplot(il, "chainstree")

#take out all original data with L3_tree1 = No data. Simon does this at the end


ind[ind$L3_tree1 %in% 'No data',] <- NA
ind[ind$L3_tree2 %in% 'No data',] <- NA
#ind <- ind[!ind$L3_tree3 == 'NA',]
#ind <- ind[!ind$L3_tree4 == 'No data',]


il[il$L3_tree1 %in% 'No data',] <- NA
il[il$L3_tree2 %in% 'No data',] <- NA
#il <- il[!il$L3_tree3 == 'No data',]
#il <- il[!il$L3_tree4 == 'No data',]


# remove all points with 'No data' points for chainstree in Indiana
#ind <- ind[!ind$chainstree == 'NA',]
#ind <- ind[ind$chainstree == 88888,]<- NA
#ind <- ind[!ind$chainstree == 99999,]
#ind <- ind[!ind$chainstree2 == 88888,]
#ind <- ind[!ind$chainstree2 == 99999,]

# remove all instances of no data for the l3tree 1
ind <- ind[!is.na(ind$L3_tree1),]
il <- il[!is.na(il$L3_tree1),]

#it just has NA if it is no tree due to water or no data, 
#so we have to removed points where there is "No data" or "Water listed as a tree
#il <- il[!il$L3_tree1 == 'Water'| !il$L3_tree1=='No data',]
#il <- il[!il$L3_tree2 == 'Water'| !il$L3_tree2 == 'No data',]
#il <- il[!il$L1_tree3 == 'Water'| !il$L1_tree3 == 'No data',]
#il <- il[!il$L1_tree4 == 'Water'| !il$L1_tree4 == 'No data',]

#  IN distances are in chains
ind$DIST1 <- as.numeric(ind$chainstree)
ind$DIST2 <- as.numeric(ind$chainstree2)
ind$DIST3 <- as.numeric(ind$chainstree3)
ind$DIST4 <- as.numeric(ind$chainstree4)

il$DIST1 <- as.numeric(il$chainstree)
il$DIST2 <- as.numeric(il$chainstr_2)
il$DIST3 <- as.numeric(il$chainstr_3)
#il$DIST4 <- as.numeric(il$chainstr_4)


#this removes all points (including no trees) that dont have distances provided. 
#il <- il[!il$DIST1 == 88888,]
#il <- il[!il$DIST1 == 99999,]
#il <- il[!il$DIST2 == 88888,]
#il <- il[!il$DIST2 == 99999,]


#  Character vectors are read into R as factors, to merge them they need to
#  be converted to character strings first and then bound together.  To ensure
#  township and ranges are unique we add a state abbreviation to the front of
#  the twonship name.
twp <- c(paste('IN', as.character(ind$TRP)))
#rng <- c(as.character(ind$TownshipRange))
ind$twp <- twp
twp_il <- c(paste('IL', as.character(il$TRP)))
il$twp <- twp_il

## for the getAngle function, need a 4character Azimuth
ind$bearings1 <- c(paste0(as.character(ind$bearing),  as.character(ind$bearingdir)))
ind$bearings2 <- c(paste0(as.character(ind$bearing2),  as.character(ind$bearingdir2)))
ind$bearings3 <- c(paste0(as.character(ind$bearing3),  as.character(ind$bearingdir3)))
ind$bearings4 <- c(paste0(as.character(ind$bearing4),  as.character(ind$bearingdir4)))

il$bearings1 <- c(paste0(as.character(il$bearing),  as.character(il$bearingdir)))
il$bearings2 <- c(paste0(as.character(il$bearing2),  as.character(il$bearingdir2)))
il$bearings3 <- c(paste0(as.character(il$bearing3),  as.character(il$bearingdir3)))
il$bearings4 <- c(paste0(as.character(il$bearing4),  as.character(il$bearingdir4)))

il$state <-'IL'
ind$state <-'IN'
#create and rename columns to match that of indiana
il$twp <- il$TRP
il$bearings1 <- il$bearing
il$bearings2 <- il$bearing2
il$bearings3 <- il$bearing3
il$bearings4 <- il$bearing4
il$DIST4 <- NA

keeps <- c("x","y","twp", "L3_tree1", "L3_tree2", "L3_tree3", "L3_tree4", "bearings1", 
  "bearings2", "bearings3", "bearings4","degrees", "degrees2", "degrees3","degrees4", "DIST1", "DIST2", "DIST3", "DIST4",
  "diameter", "diameter2", "diameter3", "diameter4", "cornerid", "state")

ind.data <- ind[keeps] 
il.data <- il[keeps]

#  The merged dataset is called inil
inil <- rbind(data.frame(ind.data), data.frame(il.data))



#inil$rng <- rng
inil<-data.frame(inil, stringsAsFactors = FALSE)
#inil [inil$DIST1 == 99999] <- 'NA'
#inil [inil == '99999'] <- 'NA'
#inil [inil == 88888] <- 'NA'
#inil [inil =='88888'] <- 'NA'

#  There are a set of 9999 values for distances which I assume are meant to be NAs. 
inil$DIST1[inil$DIST1 == 88888] <- NA
inil$DIST1[inil$DIST1 == 99999] <- NA
inil$DIST2[inil$DIST2 == 88888] <- NA
inil$DIST2[inil$DIST2 == 99999] <- NA
inil$DIST3[inil$DIST3 == 88888] <- NA
inil$DIST3[inil$DIST3 == 99999] <- NA
inil$DIST4[inil$DIST4 == 88888] <- NA
inil$DIST4[inil$DIST4 == 99999] <- NA
inil$diameter[inil$diameter == 99999] <- NA 
inil$diameter[inil$diameter == 88888] <- NA
inil$diameter2[inil$diameter2 == 99999] <- NA     
inil$diameter2[inil$diameter2 == 88888] <- NA
inil$diameter3[inil$diameter3 == 99999] <- NA     
inil$diameter3[inil$diameter3 == 88888] <- NA
inil$diameter4[inil$diameter4 == 99999] <- NA     
inil$diameter4[inil$diameter4 == 88888] <- NA
inil$degrees[inil$degrees == 99999] <- NA     
inil$degrees[inil$degrees == 88888] <- NA
inil$degrees2[inil$degrees2 == 99999] <- NA     
inil$degrees2[inil$degrees2 == 88888] <- NA
inil$degrees2[inil$degrees2 == 8888] <- NA
inil$degrees3[inil$degrees3 == 99999] <- NA     
inil$degrees3[inil$degrees3 == 88888] <- NA
inil$degrees4[inil$degrees4 == 99999] <- NA     
inil$degrees4[inil$degrees4 == 88888] <- NA

summary(inil)
# There are some points in Illinois where distances are listed as 0, but they are "Water" or "wet" or "No tree"
# Here we change these distnces to 'NA'
summary(inil[inil$L3_tree1 %in% c('No tree', 'Water', 'Wet') | inil$L3_tree2 %in% c('No tree', 'Water', 'Wet'),])
zero.trees <-(inil$L3_tree1 %in% c('No tree', 'Water', 'Wet') | inil$L3_tree2 %in% c('No tree', 'Water', 'Wet'))

inil[zero.trees, c("DIST1", "DIST2", "DIST3")] <- NA
inil[zero.trees, c('diameter', 'diameter2', "diameter3")] <- NA
# now kill missing cells:
inil <- inil[!is.na(inil$y),]
inil <- inil[!is.na(inil$x),]
X11(width = 12)
ggplot(data = inil, aes(x = x, y = y, color = DIST1)) + geom_point()


inil <- data.frame(inil)

#diameters in centimeters
diams <-  cbind(as.numeric(inil$diameter), 
                as.numeric(inil$diameter2), 
                as.numeric(inil$diameter3), 
                as.numeric(inil$diameter4))

#distances in meters
dists <-  cbind(as.numeric(inil$DIST1), 
                as.numeric(inil$DIST2), 
                as.numeric(inil$DIST3), 
                as.numeric(inil$DIST4))

bearings <- cbind(as.character(inil$bearings1), 
                  as.character(inil$bearings2),
                  as.character(inil$bearings3),
                  as.character(inil$bearings4))

degrees <- cbind(as.numeric(inil$degrees), 
                 as.numeric(inil$degrees2),
                 as.numeric(inil$degrees3),
                 as.numeric(inil$degrees4))

#  getAngle converts the four character azimuth (e.g. N43E) to a numeric, 360
#  degree angle.  It also has to deal with a number of special cases.
#  The code for getAngles is a bit scuzzy, but it leaves only 231 azimuths 
#  untranslated, this is a manageable number.
source('R/get_angle_IN.R')
azimuths <- get_angle_IN(bearings,degrees, dists)

#####  Cleaning Trees:  Kelly thinks this is already done in the CSV file, but this is from Simon's Code
#      Changing tree codes to lumped names:
#spec.codes <- read.csv('data/input/relation_tables/fullpaleon_conversion_v0.3-3.csv', stringsAsFactor = FALSE)
#spec.codes <- subset(spec.codes, Domain %in% 'Upper Midwest')

#lumped <- data.frame(abbr = as.character(spec.codes$Level.1),
 #                    lump = as.character(spec.codes$Level.3a))

species.old <- data.frame(as.character(inil$L3_tree1), 
                          as.character(inil$L3_tree2), 
                          as.character(inil$L3_tree3), 
                          as.character(inil$L3_tree4), stringsAsFactors = FALSE)
species <- species.old #since species is already converted
#species <- t(apply(species.old, 1, 
 #                  function(x) lumped[match(tolower(x), tolower(lumped[,1])), 2]))

#  We need to indicate water and remove it.  

#species[species %in% 'NA'] <- 'No tree'

#  Now we assign species that don't fit to the 'No tree' category.
species[is.na(species)] <- 'No tree'

#  Here there needs to be a check, comparing species.old against species.
test.table <- table(unlist(species.old), unlist(species), useNA='always')
write.csv(test.table, 'data/outputs/clean.bind.test.csv')

######
#  Some annoying things that need to be done:
#  First, there are some points where the first tree has a distance of zero
#  since it is the plot center.  
#  In these cases, the first azimuth is sometimes listed in a strange way, either
#  it's an NA (obviously) or it's a strange value.  In any case, we need to
#  ensure the azimuth is something recognized, I set it to 0.  It doesn't really
#  matter though.

treed.center <- (dists[,1] == 0 & !is.na(azimuths[,1]) & diams[,1] > 0)
treed.center[is.na(treed.center)] <- FALSE

azimuths[treed.center,1] <- 0 #assign azimuth to 0

#  Another special case, two trees of distance 1.  What's up with that?!
dists[rowSums(dists == 1, na.rm=T) > 1, ] <- rep(NA, 4)

#  When the object is NA, or the species is not a tree (NonTree or Water), set
#  the distance to NA.
# this way when we estimate density, we get density of places that have trees

#dists[is.na(species) | species %in% c('No tree', 'Wet', 'Water')] <- NA

#this code removes all distances that have 1 or 2 as the distance (test)
#ones <- (dists[,1] == 1 | dists[,2] ==1  ) # 57 points where distance = 1
#dists[ones] <- NA

#twos <- (dists[,1] == 2 | dists[,2] ==2  ) # 226 where the distance = 2
#dists[twos] <- NA

#test to see if we get different densities if we include No trees
#dists[is.na(species) | species %in% c( 'Wet', 'Water')] <- NA



#  At this point we need to make sure that the species are ordered by distance
#  so that trees one and two are actually the closest two trees.

#  There's an annoying problem that has to do with having a character string in
#  the subsequent sort/order function, in that it converts everything to a
#  character string.  To fix it I change the string 'species' into a numeric
#  where each number is associated with a factor level.

sp.levels <- levels(factor(unlist(species)))

species.num <- t(apply(species, 1, function(x) match(x, sp.levels)))

usable.data <- data.frame(diams,
                          dists,
                          species.num,
                          azimuths,
                          stringsAsFactors = FALSE)

rank.fun <- function(x){
  #  This is the function we use to re-order the points so that the closest is first, based on distances.
  
  test.dists <- as.vector(x[c(5:8)])
  
  ranker <- order(test.dists, na.last=TRUE) #order function sorts the distances
  
  return(x[c(ranker, ranker+4, ranker + 8, ranker + 12)])
}

colnames(usable.data) <- c(paste('diam', 1:4, sep =''),
                           paste('dist', 1:4, sep = ''), 
                           paste('species', 1:4, sep = ''),
                           paste('az', 1:4, sep = ''))

ranked.data <- matrix(nrow = nrow(usable.data),
                      ncol = ncol(usable.data))

for(i in 1:nrow(ranked.data)){
  if( sum(is.na(ranked.data[i,5:8]))<2 ){
    ranked.data[i,] <- unlist(usable.data[i,]) # if there is only 1 tree, just use the usable data as is
  } else{
    ranked.data[i,] <- unlist(rank.fun(usable.data[i,])) #if there is more than 1 tree, rank.fun sorts by distance
  }
  if(i%%6500 == 0)cat(':)') #prints a ':)' for each 6500 rows
}

ranked.data <- t(apply(usable.data, 1, rank.fun)) # need to drop 'id'
ranked.data <-data.frame(ranked.data)
# Convert species from numeric codes back into text
species <- data.frame(species1 = sp.levels[ranked.data[, 9]],
                      species2 = sp.levels[ranked.data[,10]],
                      species3 = sp.levels[ranked.data[,11]],
                      species4 = sp.levels[ranked.data[,12]],
                      stringsAsFactors=FALSE)







#  Indiana data has same correction factors for the whole state
#  There are 8 corners that have 4 trees, and some corners with 3 trees. 
#  For now, we are treating all corners as '2 trees' for the correction factors
# correction factors vary depending on which type of corner you are at
extsec <- c(100100,200100, 300100, 400100, 500100, 600100, 700100,
            100200, 100300,100400,100500, 100600, 100700, 
            200700, 300700, 400700, 500700, 600700, 700700, 
            700100, 700200, 700300, 700400, 700500, 700600)
extqtr <- c(100140, 100240, 100340, 100440, 100540, 100640,
            140100, 240100, 340100, 440100, 540100, 640100, 
            140700, 240700, 340700, 440700, 540700, 640700, 
            700140, 700240, 700340, 700440, 700540, 700640)
intsec <- c(200200, 300200, 400200, 500200, 600200,
            200300, 300300, 400300, 500300, 600300,
            200400, 300400, 400400, 500400, 600400,
            200500, 300500, 400500, 500500, 600500,
            200600, 300600, 400600, 500600, 600600)
intqtr <- c(140200, 240200, 340200, 440200, 540200, 640200,
            140300, 240300, 340300, 440300, 540300, 640300,
            140400, 240400, 340400, 440400, 540400, 640400,
            140500, 240500, 340500, 440500, 540500, 640500,
            140600, 240600, 340600, 440600, 540600, 640600,
            200140, 300140, 400140, 500140, 600140,
            200240, 300240, 400240, 500240, 600240,
            200340, 300340, 400340, 500340, 600340,
            200440, 300440, 400440, 500440, 600440,
            200540, 300540, 400540, 500540, 600540,
            200640, 300640, 400640, 500640, 600640)
corner <- rep('NA', length(inil$cornerid))
corner[ inil$cornerid %in% intsec ] <- 'intsec'
corner[ inil$cornerid %in% intqtr ] <- 'intqtr'
corner[ inil$cornerid %in% extsec ] <- 'extsec'
corner[ inil$cornerid %in% extqtr ] <- 'extqtr'

inil$cornertype <- paste0(corner, inil$state)

Pair <- inil$cornertype
corr.factor <- read.csv('data//charlie_corrections.csv')
test.correct <- data.frame(corr.factor$Pair,corr.factor$kappa, corr.factor$zeta,corr.factor$theta, corr.factor$phi, corr.factor$tau)
colnames(test.correct) <- c('Pair', 'kappa', 'zeta', 'theta', 'phi', 'tau')
require(plyr)
corrections <- join(data.frame(Pair), data.frame(test.correct), type="left")

write.csv(corrections, 'data/correction_factors.csv')
# create correction factors based on corner types

#These are the columns for the final dataset.

final.data <- data.frame(inil$x,
                         inil$y,
                         inil$twp,
                         ranked.data[,1:8],
                         species[,1:4],
                         ranked.data[,13:16], inil$cornertype,
                         stringsAsFactors = FALSE)

colnames(final.data) <- c('PointX','PointY', 'Township',
                          paste('diam',    1:4, sep =''),
                          paste('dist',    1:4, sep = ''), 
                          paste('species', 1:4, sep = ''),
                          paste('az',      1:4, sep = ''), 'corner')

                          
# part of the high density problem in indiana might be due to a large number of points with really low distances                  

summary(final.data[final.data$dist1 ==1.0,])
# 78 points with dist1= 1 
count(final.data[final.data$dist1 ==2,]) # 202 points with dist1 = 2 (most in IN)

#not sure I need this for Indiana data..dont have points for spatial points df
#  Turn it into a SpatialPointsDataFrame:
coordinates(final.data) <- ~PointX + PointY
spplot(final.data, "dist1")
final.data <- data.frame(final.data)
summary(final.data[final.data$species1 == c("No tree", "Water", "Wet") & final.data$species2 == c("No tree", "Water", "Wet"),])
summary(final.data)

# now kill missing cells:
#final.data <- final.data[!is.na(final.data$PointY),]
#final.data <- final.data[!is.na(final.data$PointX),]
ggplot(data = final.data, aes(x = PointX, y = PointY, color = dist2)) + geom_point()

#write the data as a csv
write.csv(final.data, "ndilinpls_for_density_v1.5-2.csv")
