#  Binding and cleaning the Wisconsin and Minnesota Public Lands Surveys, data is
#  sourced from the Mladenoff Lab at the University of Wisconsin.  The lab has
#  granted us permission to use the data, but not permission to distribute the
#  original datasets.  A version of the Minnesota data can be obtained from 
#  http://deli.dnr.state.mn.us/metadata/pveg_btreept3.html
#  The wisconsin data may be obtained by contacting David Mladenoff at:
#  mladenoff@wisc.edu
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

library(sp)
library(spdep)
library(rgdal)
library(raster)
library(ggplot2)
library(data.table)

mich <- read.csv("data/southernmi_projected_v1/southernMI_projected_v1.0.csv", stringsAsFactors = FALSE)

head(mich) 


ggplot(mich[mich$L3_tree1 %in% "No tree",], aes(point_x, point_y))+geom_point(size = 0.1)+theme(legend.position = "none")



not.no.tree <- !(!is.na(mich$L3_tree1) & is.na(mich$species1))
no.tree     <- is.na(mich$species1)
#mich <- mich[not.no.tree & !no.tree,]

#  Character vectors are read into R as factors, to merge them they need to
#  be converted to character strings first and then bound together.  To ensure
#  township and ranges are unique we add a state abbreviation to the front of
#  the twonship name.
twp <- c(#paste('mn', as.character(minn$TWP)), 
         #paste('wi', as.character(wisc$TOWNSHIP)), 
         paste('mi', as.character(mich$town)))
rng <- c(#as.character(minn$RNG), 
         #paste(as.character(wisc$RANGE),as.character(wisc$RANGDIR), sep=''), 
         as.character(mich$range))

#  The merged dataset is called nwmw, Minnesota comes first, then Wisconsin.
#nwmw <- rbind(minn[,c(8, 10:25)], wisc[,c(5, 13:28)], mich[,c(36, 13:28)])
nwmw <- mich
nwmw$twp <- twp
nwmw$rng <- rng

 #  There are a set of 9999 values for distances which I assume are meant to be NAs.  Also, there are a set of points where
#  the distance to the tree is 1 or 2 feet.  They cause really big density estimates!
nwmw [ nwmw == '9999'] <- NA
nwmw [ nwmw == '8888'] <- NA
nwmw [ nwmw == '_'] <- NA       # Except those that have already been assigned to 'QQ'
nwmw [ nwmw == '99999'] <- NA
nwmw [ nwmw == '999999'] <- NA
nwmw [ nwmw == '6666'] <- NA
nwmw [ nwmw == '999'] <- NA

# There is some cleaning to do.  A bit frustrating.  We can't confirm the diameters of
#  a number of points, although we hope to at some point in the future:
#  No stem density removals, none of the plots look like they have 'weird' points.
#  Basal area removals:
#nwmw[which(as.numeric(nwmw$diam1) >100),] <- rep(NA, ncol(nwmw))  #  removes 19 trees with reported diameters over 250cm.
#nwmw[which(as.numeric(nwmw$diam2) >100),] <- rep(NA, ncol(nwmw))  #  removes an additional 14 trees.
nwmw[(is.na(nwmw$species1) & nwmw$diam1 > 0) | (is.na(nwmw$species2) & nwmw$diam2>0),] <- rep(NA, ncol(nwmw))  #  removes four records with no identified trees, but identified diameters

diams <-  cbind(as.numeric(nwmw$diam1), 
                as.numeric(nwmw$diam2), 
                as.numeric(nwmw$diam3), 
                as.numeric(nwmw$diam4))

dists <-  cbind(as.numeric(nwmw$dist1), 
                as.numeric(nwmw$dist2), 
                as.numeric(nwmw$dist3), 
                as.numeric(nwmw$dist4))

# mi azimuths are from 0 to 60
azimuths <- cbind(as.numeric(nwmw$az1_360), 
                  as.numeric(nwmw$az2_360),
                  as.numeric(nwmw$az3_360),
                  as.numeric(nwmw$az4_360))

colnames(azimuths) <- c("az1", "az2","az3", "az3")

# make a dataframe with the values from Q1, Q2, Q3, Q4:
qvals <- cbind(as.numeric(nwmw$Q1), 
               as.numeric(nwmw$Q2),
               as.numeric(nwmw$Q3),
               as.numeric(nwmw$Q4))

#  michigan data already has raw azimuths, but they range from 0-100, so we need to convert these:
#source('R/get_angle_MI.R')
#azimuths <- get_angle_MI(azimuths, qvals)
#azimuths <- angl
#####  Cleaning Trees:  
#      Changing tree codes to lumped names:
#spec.codes <- read.csv('WitnessTrees-1.0/WitnessTrees-1.0/data/input/relation_tables/fullpaleon_conversion_v0.3-3.csv', stringsAsFactor = FALSE)
#spec.codes <- subset(spec.codes, Domain %in% 'Upper Midwest')

#lumped <- data.frame(abbr = as.character(spec.codes$Level.1),
 #                    lump = as.character(spec.codes$Level.3a))

#species.old <- data.frame(as.character(nwmw$species1), 
 #                         as.character(nwmw$species2), 
  #                        as.character(nwmw$species3), 
   #                       as.character(nwmw$species4), stringsAsFactors = FALSE)

#species <- t(apply(species.old, 1, 
 #                  function(x) lumped[match(tolower(x), tolower(lumped[,1])), 2]))

species <- cbind(as.character(nwmw$L3_tree1), 
                 as.character(nwmw$L3_tree2), 
                 as.character(nwmw$L3_tree3), 
                 as.character(nwmw$L3_tree4))

#  We need to indicate water and remove it.  There are 43495 cells with 'water'
#  indicated, and another 784 cells with 'missing' data.
#  when we limit these to the first two columns of the species table we get
#  a total of 25416 samples removed.

#  There are a set of dead taxa (DA, DB & cetera) that we exclude.  Only AM is
#  unknown at this point.  This excludes 213 trees.
species[species %in% ''] <- 'No tree'

#  Now we assign species that don't fit to the 'No tree' category.
species[is.na(species)] <- 'No tree'

#  Here there needs to be a check, comparing species.old against species.
#test.table <- table(unlist(species.old), unlist(species), useNA='always')
#write.csv(test.table, 'data/outputs/mi_clean.bind.test.csv')

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

azimuths[treed.center,1] <- 0
# check this:
#  Another special case, two trees of distance 1.  What's up with that?!
dists[rowSums(dists == 1, na.rm=T) > 1, ] <- NA#,rep(NA, 4)

#  When the object is NA, or the species is not a tree (NonTree or Water), set
#  the distance to NA.
dists[is.na(species) | species %in% c('No tree', 'Water', 'Missing')] <- NA


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
  
  ranker <- order(test.dists, na.last=TRUE)
  
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
    ranked.data[i,] <- unlist(usable.data[i,])
  } else{
    ranked.data[i,] <- unlist(rank.fun(usable.data[i,]))
  }
  if(i%%6500 == 0)cat('.')
}

ranked.data <- t(apply(usable.data, 1, rank.fun)) # need to drop 'id'
colnames(ranked.data) <- c(paste('diam', 1:4, sep =''),
                           paste('dist', 1:4, sep = ''), 
                           paste('species', 1:4, sep = ''),
                           paste('az', 1:4, sep = ''))

species <- data.frame(species1 = sp.levels[as.numeric(ranked.data[, 9])],
                      species2 = sp.levels[as.numeric(ranked.data[,10])],
                      species3 = sp.levels[as.numeric(ranked.data[,11])],
                      species4 = sp.levels[as.numeric(ranked.data[,12])])

year <- rep("NA", length(species$species1))
state <- data.frame(state = rep("MI", length(species$species1)))

corner <- mich$sec_corner



#  These are the columns for the final dataset.

final.data <- data.frame(nwmw$point_x,
                         nwmw$point_y,
                        nwmw$twnrng,
                        state,
                        ranked.data[,1:8],
                        species,
                        ranked.data[,13:16],
                        corner,
                        nwmw$cornertype,
                        year ,
                        nwmw$twnrng,
                        stringsAsFactors = FALSE)

colnames(final.data) <- c('PointX','PointY', 'Township',"state",
                          paste('diam',    1:4, sep =''),
                          paste('dist',    1:4, sep = ''), 
                          paste('species', 1:4, sep = ''),
                          paste('az',      1:4, sep = ''), 'corner',"cornertype",'surveyyear')


final.data$az1[final.data$az1 <= 0 ] <- NA
final.data$az2[final.data$az2 <= 0] <- NA
final.data$az3[final.data$az3 <= 0] <- NA
final.data$az4[final.data$az4 <= 0] <- NA

summary(final.data)
final.data <- final.data[!is.na(final.data$PointX),]
# kill ths cells that are not == Extsec or ==Intsec

final.data <- final.data[final.data$corner %in% c("Extsec", "Intsec"),]

# now kill missing cells:

# there are a few strange points with erroneous X or Y values. Get rid of them here:
final.data <- final.data[ !final.data$PointX < 1000, ]
final.data <- final.data[ !final.data$PointY < 1000, ]


#write data to a csv:
write.csv(final.data, "data/lower_mi_final_data.csv")
#note there are still many NA values in the dataset--need to remove these!

X11(width=12)
ggplot(final.data[final.data$species2 %in% c("Oak", "Maple", "Beech","Pine", "Hemlock", "No tree", "Ash"),], aes(PointX, PointY, color = species2))+geom_point()
ggplot(final.data, aes(PointX, PointY, color = diam1))+geom_point()+scale_color_continuous(low = 'blue', high = 'red', limits = c(0,400))
hist(final.data$diam1)
hist(final.data$diam2)
hist(final.data$dist1)
hist(final.data$dist2)

ggplot(final.data, aes(PointX, PointY, color = dist2))+geom_point()+scale_color_continuous(low = 'blue', high = 'red', limits = c(0,400))

#Pair <- ifelse(final.data$Township %like% "N",paste0(as.character(final.data$corner), "MI-E"), paste0(as.character(final.data$corner), "MI-W"))
used.data <- final.data
# --------------------generate correction factors------------------------------:
# read in the correction factors provided by Charlie Cogbill:
corr.vals <- read.csv('data/cogbill_corrections.csv')


correction <- data.frame(kappa = rep(NA, length(used.data)),
                         theta = rep(NA, length(used.data)),
                         zeta  = rep(NA, length(used.data)),
                         phi   = rep(NA, length(used.data)))

species2table <- data.frame(species1 = final.data$species1,
                            species2 = final.data$species2,
                            species3 = final.data$species3,
                            species4 = final.data$species4)
plot.trees <- rowSums(!(species2table == 'Water' | species2table == 'NonTree'), na.rm = TRUE)

point.no <- as.character(final.data$corner)

#  So there are a set of classes here, we can match them all up:

internal <- ifelse(!point.no %in% "Extsec", 'internal', 'external')
trees    <- ifelse(plot.trees == 2, 'P', '2NQ')
section  <- ifelse(final.data$cornertype %in% "section", 'section', 'quartersection')
state <- final.data$state

corr.year     <- as.character(final.data$year)
corr.year[state == 'MI' & final.data$Township %like% "W"] <- 'W'
corr.year[state == 'MI' & final.data$Township %like% "E"] <- 'E'

match.vec <- apply(corr.vals[,1:4], 1, paste, collapse = '')
to.match <- apply(data.frame(state, corr.year, internal, section, stringsAsFactors = FALSE), 1, paste, collapse = '')

correction <- corr.vals[match(to.match, match.vec),]


write.csv(correction, 'data/MI_correction_factors.csv')

