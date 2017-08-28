#  This function deconvolves the azimuth as stored in the 

get_angle_IN <- function(bearings, degrees, dists) {
  #  This function is used in 'step.one.clean.bind_v1.1.R', it converts the
  #  text azimuth strings to numeric, 360 degree values.
  #  This is the vector that will store the values.
  angl <- degrees
  
  #  This is a special case, generally where the tree is plot center.
  angl[degrees == '0' & dists =='0'] <- 0
  
  #  This is a short function that takes cares of NAs in boolean functions, it's
  #  just a simple wrapper for the boolean function that sets the NA values in
  #  the vector to FALSE.
  fx.na <- function(x) { x[ is.na( x ) ] <- FALSE; x }
  
  #  Given the text azimuths in the dataset, return the quadrant values.
  #  This gives a boolean index of the quadrant
  
  north <- fx.na(bearings == 'NNA'| bearings == 'NAN' | bearings =='N'|bearings =='N99999'|bearings =='N88888')
  east <- fx.na(bearings == 'NAE' | bearings =="ENA" | bearings =='E'|bearings =='99999E'|bearings =='88888E')
  south <- fx.na(bearings == 'SNA' | bearings =="NAS"| bearings == 'S'|bearings =='S99999'|bearings =='S88888')
  west <- fx.na(bearings == 'NAW' | bearings =="WNA" | bearings == 'W'|bearings =='99999W'|bearings =='88888W')
  #north <- fx.na( regexpr('N', bearings) > 0 )
  #east  <- fx.na( regexpr('E', bearings) > 0 | bearings == 'EAST')
  #south <- fx.na( regexpr('S', bearings) > 0 | bearings == 'SOUTH')
  #west <-  fx.na( regexpr('W', bearings) > 0 | bearings == 'WEST')
  
  ne <- fx.na( (north & east) | bearings == 'NE')
  se <- fx.na( (south & east) | bearings == 'SE')
  sw <- fx.na( (south & west) | bearings == 'SW')
  nw <- fx.na( (north & west) | bearings == 'NW') 
  
  #  The cell is in a quadrant, regardless of which.
  quad <- ne | se | sw | nw
  
  #  Special case of the trees with a unidirectional direction.
  uni  <- (!quad) & (north | south | east | west) 
  
  angl[ uni & north ] <- 0
  angl[ uni & south ] <- 180
  angl[ uni & east  ] <- 90 
  angl[ uni & west  ] <- 270
  
  #  The problem is that some notes have either N04E, N 4E or N4E, or NE!
  #strlen <- nchar(bearings)
  #strlen[is.na(bearings)] <- NA
  
  #######not sure if I need this section
  
  #angl[quad & strlen == 2] <- 45
  #angl[quad & strlen == 3] <- as.numeric(substr(azimuth[ quad & strlen == 3 ], 2, 2))
  #angl[quad & strlen == 4 & !substr(azimuth, 2, 3) == '  '] <- 
   # as.numeric(substr(azimuth[quad & strlen == 4 & !substr(azimuth, 2, 3) == '  '], 2, 3))
  
  # Special case of double spaces in the azimuth.
  #angl[quad & strlen == 4 & substr(azimuth, 2, 3) == '  '] <- 45
  
  ##########
  
  #  Another set of special cases:
  angl[ fx.na(bearings == 'NORT') ] <- 0
  angl[ fx.na(bearings == 'EAST') ] <- 90
  angl[ fx.na(bearings == 'WEST') ] <- 270
  angl[ fx.na(bearings == 'SOUT') ] <- 180
  
  angl[ ne ] <- angl[ ne ]
  angl[ se ] <- 180 - angl[ se ]
  angl[ sw ] <- 180 + angl[ sw ]
  angl[ nw ] <- 360 - angl [ nw ]
  
  return(angl)
  
}