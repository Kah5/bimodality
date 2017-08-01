#  Functions used in the Settlement Vegetation analysis from Simon Goring:

#  The Morisita density estimator:
morisita <- function(processed.data, correction.factor = NULL, veil=FALSE) {
  #  Function to calculate stem density using the morista function.  The input 
  #  is 'processed.data', which should be the file 'used.data'.  'correction 
  #  factor' is the modified Cottam Correction factor determined in 
  #  'load.estimate.correction.R' using a generalized linear model (with a Gamma
  #  distribution).
  
  
  azim <- processed.data[,c('az1', 'az2', 'az3', 'az4')]
  diam <- processed.data[,c('diam1', 'diam2', 'diam3', 'diam4')]
  dist <- processed.data[,c('dist1', 'dist2', 'dist3', 'dist4')]
  spec <- processed.data[,c('species1', 'species2', 'species3', 'species4')]
  corn <- processed.data[,'corner']
 
  
  if(veil){
    diam[diam < 8] <- NA #double check that this is corrected veil line, and do we want to use it?
  }
  
  diam[diam == 0 & !spec == 'No tree'] <- NA
  
  #m.diam <- diam/100 #diameters are in cm already
  m.diam <- (diam * 2.54) / 100 # convert diameter from inches to meters
  
  dist <- floor(apply(dist, 2, function(x)as.numeric(as.character(x))))
  azim <- floor(apply(azim, 2, function(x)as.numeric(as.character(x))))
  
  #  This tells us how many quadrats are used.  I'd prefer to use all points
  #  where samples are drawn from two quadrats, but in some cases it seems that
  #  there are NAs in the data.
  #  If a point has recorded azimuths we state that they must be in two different
  #  quadrats:
  
  two.quads <- apply(azim[,1:2], 1, function(x) sum(!is.na(unique(floor(x/90)))))
  
  #  There are 10,155 points for which the first two trees were sampled in the
  #  same quadrat.  In general these are randomly distributed, but interestingly
  #  there's a big clump of them in Wisconsin.  Even so, there are lots of other
  #  points around.  We can accept that these points are categorically wrong.
  #  
  #  
  #  sum((two.quads == 1 & !(is.na(azim[,1]) | is.na(azim[,2]))))
  

  #  we need to change:
  
  two.quads[((two.quads < 2 & (is.na(azim[,1]) | is.na(azim[,2]))) &
               !(is.na(dist[,1]) | is.na(dist[,2])))] <- 2
  
  #  Exclusions include:
  #  Plots with a tree as plot center:
  two.quads[dist[,1] == 0] <- 0
  
  #  Plots where one of the trees has no measured diameter:
  two.quads[is.na(diam[,1]) | is.na(diam[,2])] <- 0
  
  #  Plots where a distance to tree is missing:
  two.quads[is.na(dist[,1]) | is.na(dist[,2])] <- 0
  #  This is the same as k in Charlie's spreadsheet:
  
  q <- two.quads
  
  #  Tree dist is measured in links in the dataset, I am converting to
  #  meters and adding one half a dimater (in cm), on Charlie's advice.
  
  
  
     m.dist <- dist * 0.201168 + 0.5 * m.diam # convert distances from chains (links) to meters
     #m.dist <- dist + 0.5 * m.diam 
  #  rsum is the sum of the squared radii, in cases where there are two trees in
  #  the same quadrant I'm going to drop the site, as I will with any corner 
  #  with only one tree since the morista density estimator can't calculate
  #  density with less than two trees, and requires two quadrats.
  
  #  I'm going to let the NAs stand in this instance.
  rsum <- rowSums((m.dist[,1:2])^2, na.rm=T)
  #rmax <- max(m.dist[,1:2], na.rm=T)
  #rmax<- apply(m.dist[,1:2], 1, max, na.rm= T)
  
  #  A set of conditions to be met for the rsum to be valid:
  rsum[rowSums(is.na(m.dist[,1:2])) == 2 |  q < 2 | rsum == 0 | rowSums(m.dist[,1:2], na.rm=T) < 0.6035] <- NA
  #rmax[rowSums(is.na(m.dist[,1:2])) == 2 |  q < 2 | rmax == 0 | rowSums(m.dist[,1:2], na.rm=T) < 0.6035] <- NA
  
  #  From the formula,
  #  lambda = kappa * theta * (q - 1)/(pi * n) * (q / sum_(1:q)(r^2))
  #  here, n is equal to 1.
  #  units are in stems / m^2
  
  #charilies morisita.est have separate corrections for each type of corner
  morisita.est <- ((q - 1) / (pi * 1)) * (2 / rsum) *
    correction.factor$kappa  * correction.factor$theta* correction.factor$zeta * correction.factor$phi
  
  morisita.est[q < 2] <- NA
  
  #  Now they're in stems / hectare
  morisita.est <- morisita.est * 10000
  
  #  Basal area is the average diameter times the stem density.
  #  The stem density is measured in trees / ha.
  met.rad <- (diam / 2) * 2.54 / 100
  #met.rad <- (diam / 2)/100
  basal.area <- morisita.est * rowSums(pi * met.rad^2, na.rm=TRUE)
  
  basal.area[ q < 2 ] <- NA
  
  
  
 # radius <- max(m.dist, na.rm = TRUE)
  #plot.area <- pi*rmax/2 # calculate average plot area
  return(list(morisita.est, basal.area))
  
}