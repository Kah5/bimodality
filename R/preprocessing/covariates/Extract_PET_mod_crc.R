library(plyr)
library(raster)
library(data.table)
#ibrary(rgdal)
library(reshape2)

version <- "1.7-5" # pls version

# set the working dir (where the prism data folder is)
#workingdir <- "/Users/kah/Documents/bimodality/data/"
# for crc:
workingdir <- "/afs/crc.nd.edu/user/k/kheilman/bimodality/data/"


# again read in the 8km grid for extracting
#spec.table <- read.csv(paste0(workingdir,'midwest_pls_full_density_alb',version,'.csv'))
#coordinates(spec.table) <- ~x + y
#proj4string(spec.table) <- '+init=epsg:3175' 
#spec.table.ll<- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

#spec.table.ll<- as.data.frame(spec.table.ll)
#spec.table.ll <- read.csv(paste0(workingdir, "spec.lat.long.csv"))
######################################################################################
# calculate PET from the temperature data:
######################################################################################
# may need to do this in CRC:

#library(SPEI)
#setwd to data directory
setwd(paste0(workingdir,'PRISM_tmean_stable_4kmM2_198101_201709_bil/'))

#read in the grid again
#spec.table <- read.csv(paste0(workingdir,'midwest_pls_full_density_alb',version,'.csv'))
#coordinates(spec.table) <- ~x + y

# project the grid to lat long
#proj4string(spec.table) <- '+init=epsg:3175'
#spec.lat <- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0' ))

years <- 1981:2015
yrs <- "1981-2015"

# read in the filenames, stack as rasters, extract raster to points
filenames <- list.files(pattern=paste(".*_",".*\\.bil$", sep = ""))

# use substring to index filenames that match the years designated:

filenames <- filenames [substring(filenames, first = 26, last = 29) %in% years]

s <- stack(filenames) #make all into a raster
t <- crop(s, extent(c(-97.24357, -82.40131 , 37.1442 , 49.38583))) #crop to the extent of the region 
#s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
y <- data.frame(rasterToPoints(t)) #covert to dataframe


test <- y
# this does not work in pulling out the data from one grid cell
y$CellID <- seq(1:nrow(y))

my.list <- list()
# this for loop calculates thornthwaite PET for each month in each grid cell
#for(i in 1:length(y$y)){
source("/afs/crc.nd.edu/user/k/kheilman/bimodality/R/Thornthwaite_PET.R")
system.time(for(i in 1:length(y$y)){
#  system.time(for(i in 1:20){
  ynew <- t(y[i,3:ncol(y)]) # this makes it so that the number of years can vary
  lat <- y[i,]$y
  long <- y[i,]$x
  cellID <- y[i,]$CellID
  # get month an year from row.names
  year <-  data.frame(year = substring(row.names(ynew), first = 26, last = 29))
  month <- data.frame(month = substring(row.names(ynew), first = 30, last = 31))
  ynew2 <- data.frame(Tave = ynew[,1], 
                      year <- year, 
                      month <- month)
  ynew2 <- ynew2[ynew2$month %in% c("01","02","03","04","05","06", "07", "08", "09", "10", "11", "12"),]
  # use the thorthwaite equation to attach the PET data to the 
  
  ynew2$PET_tho <- as.numeric(thornthwaite_PET(ynew2$Tave, lat))
  ynew2$lat <- lat
  ynew2$long <- long
  ynew2$CellID <- cellID
  
  my.list[[i]] <- ynew2
  
})
# if PET.df already exists, then add to the df, if not, then create "PET.df"
#   if(exists  ("PET.df")){
PET.df <- do.call(rbind, my.list)


# rename the object:
full.PET <- PET.df
#remove PET.df
#rm(PET.df)


PET.means <- dcast(full.PET, lat + long  ~ month , mean , value.var='PET_tho', na.rm = TRUE)
colnames(PET.means) <- c("lat", "long",
                         "apr","may","jun", "jul", "aug", "sep", "oct")

# get the precipitation data in the same format:
saveRDS(full.PET, paste0(workingdir, "full.PET_full_reg_mod.rds"))
saveRDS(PET.means, paste0(workingdir, "PET.means_full_reg_mod.rds"))

# --------------------------code below for processing data in correct format: done outside of crc



full.PET <- readRDS("outputs/full.PET_full_reg_mod.rds")
full.PET <- full.PET[,c( "month","PET_tho", "lat","long")]


full <- dcast(full.PET, lat + long ~ month, mean, value.var = 'PET_tho', na.rm = TRUE)

full$Mean <- rowMeans(full[,4:length(full)], na.rm=TRUE)
full2 <- full

coordinates(full) <- ~long + lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages
proj4string(avgs) <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0' 
avgs.alb <- projectRaster(avgs, crs='+init=epsg:3175')

# spec.table is a spatial points df, convert to regular df:
spec.table <- as.data.frame(spec.table)
avgs.df <- data.frame(raster::extract(avgs.alb, spec.table[,c("x" ,"y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y

ggplot(avgs.df, aes(x,y, fill = Mean))+geom_raster()
write.csv(avgs.df, paste0(workingdir, "PET_fia_extracted", yrs,".csv"))



# now lets get the yearly values

full.PET$year <- as.numeric(substring(row.names(full.PET), first = 26, last = 29))
PET.byll <- dcast(full.PET, lat + long ~  year + month, mean, value.var = 'PET_tho', na.rm = TRUE)

coordinates(PET.byll) <- ~long + lat
gridded(PET.byll) <- TRUE
avgs <- stack(PET.byll) 

plot(avgs) #plots averages
proj4string(avgs) <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0' 
avgs.alb <- projectRaster(avgs, crs='+init=epsg:3175')

# spec.table is a spatial points df, convert to regular df:
spec.table <- as.data.frame(spec.table)
avgs.df <- data.frame(raster::extract(avgs.alb, spec.table[,c("x" ,"y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y

ggplot(avgs.df, aes(x,y, fill = X2005_11))+geom_raster()
write.csv(avgs.df, paste0(workingdir, "PET_fia_extracted_full", yrs,".csv"))


