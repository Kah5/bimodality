#  This function is adapted from S. Goring Witness Tree paper

get_angle_MI <- function(azimuth, qvals) {
  #  This function is used in 'step.one.clean.bind_lowerMI.R', it converts the
  #  azimuth values from 0 - 90 degrees in each quadrat to a 0-360 scale
  angl <- azimuth
  angl 
  #  This is a special case, generally where the tree is plot center.
  angl[azimuth == '0'] <- 0
  angl[azimuth > 360] <- NA
  # create indices for trees in N,E,S,W quadrats
  north <- qvals == 1
  east <- qvals == 2
  south <- qvals == 3
  west <- qvals == 4
  
  
  
  
  
  #  This is a short function that takes cares of NAs in boolean functions, it's
  #  just a simple wrapper for the boolean function that sets the NA values in
  #  the vector to FALSE.
  fx.na <- function(x) { x[ is.na( x ) ] <- FALSE; x }
  
  #  Given the text azimuths in the dataset, return the quadrant values.
  #  This gives a boolean index of the quadrant
  
  north <- fx.na(  qvals == 1 )
  east  <- fx.na( qvals == 2)
  south <- fx.na(  qvals == 3)
  west <-  fx.na(  qvals == 4)
  
  #ne <- fx.na( (north & east) | azimuth == 'N  E')
  #se <- fx.na( (south & east) | azimuth == 'S  E')
  #sw <- fx.na( (south & west) | azimuth == 'S  W')
  #nw <- fx.na( (north & west) | azimuth == 'N  W') 
  
  #  The cell is in a quadrant, regardless of which.
  quad <- north | east | south | west
  
  #  Special case of the trees with a unidirectional direction.
  #uni  <- (!quad) & (north | south | east | west) & (nchar(azimuth) == 1)
  
  angl[ uni & north ] <- 0
  angl[ uni & south ] <- 180
  angl[ uni & east  ] <- 90 
  angl[ uni & west  ] <- 270
  
  #  The problem is that some notes have either N04E, N 4E or N4E, or NE!
  strlen <- nchar(azimuth)
  strlen[is.na(azimuth)] <- NA
  
  #angl[quad & strlen == 2] <- 45
  #angl[quad & strlen == 3] <- as.numeric(substr(azimuth[ quad & strlen == 3 ], 2, 2))
  #angl[quad & strlen == 4 & !substr(azimuth, 2, 3) == '  '] <- 
   # as.numeric(substr(azimuth[quad & strlen == 4 & !substr(azimuth, 2, 3) == '  '], 2, 3))
  
  # Special case of double spaces in the azimuth.
  #angl[quad & strlen == 4 & substr(azimuth, 2, 3) == '  '] <- 45
  
  #  Another set of special cases:
  #angl[ fx.na(azimuth == 'NORT') ] <- 0
  #angl[ fx.na(azimuth == 'EAST') ] <- 90
  #angl[ fx.na(azimuth == 'WEST') ] <- 270
  #angl[ fx.na(azimuth == 'SOUT') ] <- 180
  angl[ north ] <- azimuth [ north ]
  angl[ south ] <- 180 - azimuth[ south ]
  angl[ east ] <- 180 + azimuth[ east ]
  angl[ west ] <- 360 - azimuth [ west ]
  angl <- apply(angl, 2, as.numeric)
  angl[angl> 360] <- NA
  angl[angl< 0] <- NA
  return(angl)
  
}