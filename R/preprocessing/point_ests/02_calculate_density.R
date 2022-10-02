#-----------------------------------------------------------
# 02_calculate_density.R
#This script does the PLS Density calculations at each PLS survey point
#Code adapted from Simon Goring by Kelly Heilman               
# 
#Updated April 20, 2017
#Run 01_clean_pls_data.R before running this script
#This script requires the morista.R script
#-----------------------------------------------------------

library(plyr)
library(reshape2)
library(raster)
version <- "1.6-5"

#read in final.data from the step_one_clean_IN.r script:
final.data <- read.csv("outputs/indiana_final_data_for_dens_est.csv", stringsAsFactors = FALSE)
# calculate stem density:
correction.factor <- read.csv("data//correction_factors.csv", header = TRUE)

## Morisita estimates for indiana densities and basal area with charlies correction factors
# & no diameter veil
source('R/morisita.r')

# Calculate the density using the correction factors
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
write.csv(stem.density, paste0('outputs/INdensestimates_v',version,'.csv'))

nowater <- ifelse(stem.density$species1 %in% c("Water", "Wet"),
  NA,
  stem.density$stem.density
)

stem.density$dens.nowater <- nowater

nowaters <- stem.density[!is.na(stem.density$dens.nowater),]


coordinates(final.data)<- ~PointX+PointY

#create spatial object with density, basal area & diameters data
stem.density <- data.frame(x = final.data$PointX, 
                           y = final.data$PointY,
                           density = estimates[[1]],
                           basal   = estimates[[2]])#,
                          #diams = rowMeans(diams[,1:2], na.rm=TRUE) * 2.54)

# -----------------------------------------------------------
# correct some of the outlier densities
#
# find the 99% percentile here for yr
nine.nine.pct <- apply(stem.density[,3:4], 2, quantile, probs = 0.99, na.rm=TRUE)

stem.density$density[stem.density$density > nine.nine.pct['density']] <- nine.nine.pct['density']
stem.density$basal[stem.density$basal > nine.nine.pct['basal']] <- nine.nine.pct['basal']

#density     basal 
#1782.5492  415.0344 


#---------------------------------------------------------------
# designate no trees as 0 tree density and "wet" or water as NA
#---------------------------------------------------------------
species[species==""]<- "No tree"
#fix the captalized "No tree" problem
species[species == 'No Tree'] <- 'No tree'

#change all No tree densities to 0
stem.density$density[species[,1] == 'No tree'| species[,2]=='No tree'] <- 0
#classify trees as zero or as wet trees
zero.trees <- is.na(stem.density$density) & (species[,2] %in% c('No tree') | species[,1] %in% c('No tree'))
wet.trees <- (species[,2] %in% c('Wet', "Water") | species[,1] %in% c('Wet','Water'))

#designate all zero trees as density of 0
stem.density$density[zero.trees] <- 0
stem.density$basal[zero.trees] <- 0

#desgnate all wet trees as 'NA'
stem.density$density[wet.trees] <- NA
stem.density$basal[wet.trees] <- NA

# write out as csv:
write.csv(stem.density,"outputs/in_stem_density_alb_v1.6-1.csv")

# make stem.density spatial
coordinates(stem.density)<- ~x+y
proj4string(stem.density)<-CRS('+init=epsg:3175')

# write out the stem density as a shapefile:
writeOGR(obj = stem.density, dsn = "outputs/in_stem_density_alb_v1.6-1.shp", layer = "stem_density_alb_v1.6-5", driver = "ESRI Shapefile", overwrite=TRUE)

# extra code for visualizing the data mapped out
stem.density <- data.frame(stem.density)
X11(width = 12)
ggplot(stem.density, aes(x = x, y = y, color = density))+geom_point()+ ylim(450000, 565000)+coord_equal()
