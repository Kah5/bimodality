library(plyr)
library(raster)
library(data.table)
#ibrary(rgdal)
library(reshape2)

version <- "1.7-5" # pls version

# set the working dir (where the prism data folder is)
workingdir <- "/Users/kah/Documents/bimodality/data/"
# for crc:
#workingdir <- "/afs/crc.nd.edu/user/k/kheilman/bimodality/data/"


# again read in the 8km grid for extracting
spec.table <- read.csv(paste0(workingdir,'midwest_pls_full_density_alb',version,'.csv'))
coordinates(spec.table) <- ~x + y
proj4string(spec.table) <- '+init=epsg:3175' 
spec.table.ll <- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

spec.table.11 <- as.data.frame(spec.table.ll)
#spec.table.ll <- read.csv(paste0(workingdir, "spec.lat.long.csv"))
######################################################################################
# calculate PET from the temperature data:
######################################################################################
# may need to do this in CRC:

#library(SPEI)
#setwd to data directory
setwd(paste0(workingdir,'PRISM_tmean_stable_4kmM2_189501_198012_bil/'))

#read in the grid again
spec.table <- read.csv(paste0(workingdir,'midwest_pls_full_density_alb',version,'.csv'))
coordinates(spec.table) <- ~x + y

# project the grid to lat long
proj4string(spec.table) <- '+init=epsg:3175'
#spec.lat <- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0' ))

years <- 1895:1935
yrs <- "1895-1935"

# read in the filenames, stack as rasters, extract raster to points
filenames <- list.files(pattern=paste(".*_",".*\\.bil$", sep = ""))

# use substring to index filenames that match the years designated:
#s <- stack(filenames[1:12])

filenames <- filenames [substring(filenames, first = 26, last = 29) %in% years]

#s <- stack(filenames) #make all into a raster
s <- apply(data.frame(filenames), MARGIN = 1, FUN = raster)
t.rast <- lapply(s, FUN= function(x){crop(x, extent(c(-97.24357, -82.40131 , 37.1442 , 49.38583)))})
#t.rast <- crop(s, extent(c(-97.24357, -82.40131 , 37.1442 , 49.38583))) #crop to the extent of indiana & illinois 
#s <- projectRaster(t.rast, crs='+init=epsg:3175') # project in great lakes albers
y <- lapply(t.rast, FUN= function(x){data.frame(rasterToPoints(x))}) #covert to list of dataframe

y.list <- lapply(y, function(x){data.frame(year = substring(colnames(x)[3], first = 26, last = 29),
                      month = substring(colnames(x)[3], first = 30, last = 31), 
                      x = x$x, 
                      y = x$y, 
                      tmean = x[,3], 
                      cellID = 1:length(x$y))})


y.df <- do.call(rbind, y.list)
head(y.df)
test <- y.df



# need to get a list of time series by grid cell/by unique x + y values:
library(tidyr)
library(dplyr)
#test.tib <- y.df %>% group_by(cellID)

#head(test.tib)
y.df$cellID <- as.factor(y.df$cellID)



y.df.list <- y.df %>% group_by(cellID) %>% do(vals=data.frame(.)) %>% select(vals) %>% lapply(function(x) {(x)})

# then apply the thronthwaite process to each dataframe within the list:
ynew2 <- y.df.list[[1]]

#saveRDS(ynew2, "outputs/ynew_1895_1935_pet.long.rds")
setwd("/Users/kah/Documents/bimodality")
source("Thornthwaite_PET.R")
#ynew2[[1]]$
library(SPEI)

y.pet <- lapply(ynew2, function(x){data.frame(x = x$x,
                                     y = x$y,
                                     month = x$month,
                                     year = x$year,
                                     Tave = x$tmean,
                                     cellID = x$cellID,
                                     PET_tho = as.numeric(thornthwaite_PET(Tave = x$tmean, lat =  unique(x$x))) 
                                     )})





# rename the object:
full.PET <- PET.df <- y.pet


y.pet.long <- lapply(y.pet, function(df){df %>% select(x, y, year, month, PET_tho) %>% spread(key = month,   value = PET_tho)})
class(y.pet[[1]]$year)
dcast(y.pet[[1]] , x + y  ~ year + month , mean , value.var='PET_tho', na.rm = TRUE)

fwrite(y.pet, "outputy.pet.csv")
fwrite(y.pet.long, "output.pet.long.csv")

y.pet.long.df <- do.call(rbind, y.pet.long)
fwrite(y.pet.long.df, "output.pet.long.df.csv")

#<- lapply(y.pet, spread( df.list ,   key = Month  ,   value = PET_tho  ) 
y.pet2 <- lapply(y.pet, function(df.list){dcast(df.list, x + y  ~ year + month , mean , value.var='PET_tho', na.rm = TRUE)})

y.pet2.df <- do.call(rbind, y.pet2)
fwrite(y.pet2.df, "data/PET_pls_extracted1895-1935.csv")


#>>>>>>>>>>>>>>>>>>>>>> now lets extract the precipitation from this same time period for soil mositure model <<<<<<<<<<<<<<<<<<<<<<<<<

setwd(paste0(workingdir,'PRISM_ppt_stable_4kmM2_189501_198012_bil/'))

#read in the grid again
spec.table <- read.csv(paste0(workingdir,'midwest_pls_full_density_alb',version,'.csv'))
coordinates(spec.table) <- ~x + y

# project the grid to lat long
proj4string(spec.table) <- '+init=epsg:3175'
#spec.lat <- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0' ))

years <- 1895:1935
yrs <- "1895-1935"

# read in the filenames, stack as rasters, extract raster to points
filenames <- list.files(pattern=paste(".*_",".*\\.bil$", sep = ""))

# use substring to index filenames that match the years designated:
#s <- stack(filenames[1:12])

filenames <- filenames [substring(filenames, first = 24, last = 27) %in% years]

s <- stack(filenames) #make all into a raster
#s <- apply(data.frame(filenames), MARGIN = 1, FUN = raster)
#t.rast <- lapply(s, FUN= function(x){crop(x, extent(c(-97.24357, -82.40131 , 37.1442 , 49.38583)))})
t.rast <- crop(s, extent(c(-97.24357, -82.40131 , 37.1442 , 49.38583))) #crop to the extent of indiana & illinois 
#s <- projectRaster(t.rast, crs='+init=epsg:3175') # project in great lakes albers
#y <- lapply(t.rast, FUN= function(x){data.frame(rasterToPoints(x))}) #covert to list of dataframe
y <- data.frame(rasterToPoints(t.rast))

colnames(y)[3:length(colnames(y))] <- paste0(substring(colnames(y)[3:length(colnames(y))], first = 24, last = 27), "_",
       substring(colnames(y)[3:length(colnames(y))], first = 28, last = 29))
setwd("/Users/kah/Documents/bimodality")
fwrite(y, "data/Precip_pls_extracted1895-1935.csv")

#y.df <- data.frame(year = substring(colnames(y), first = 24, last = 27),
 #          month = substring(colnames(y), first = 28, last = 29), 
  #         x = y$x, 
   #        y = y$y, 
    #       pcp = y[,3], 
     #      cellID = 1:length(y$y))



#y.df$Year <- y.df$year
#y.df$Month <- y.df$month

#y.df$year <- 
#y.df$month<- substring(y.df$Year, 3)
#y.df$year <- ifelse(as.numeric(substring(y.df$Year, 1, last = 2))>= 95, paste0(18,substring(y.df$Year, 1, last = 2)), paste0(19,substring(y.df$Year, 1, last = 2)))
#y.df<- y.df[,1:6]

precip.df <- dcast(y.df[,c("x", 'y',"year", "month", "pcp")], x + y ~ year + month , mean , value.var='pcp', na.rm = TRUE)
fwrite(precip.df, "data/Precip_pls_extracted1895-1935.csv")

#y.df <- do.call(rbind, y.list)
head(y.df)
test <- y.df



# need to get a list of time series by grid cell/by unique x + y values:
library(tidyr)
library(dplyr)
#test.tib <- y.df %>% group_by(cellID)

#head(test.tib)
y.df$cellID <- as.factor(y.df$cellID)






# old code, not run:
PET.means <- dcast(y.df, x + y  ~ month , mean , value.var='PET_tho', na.rm = TRUE)
colnames(PET.means) <- c("lat", "long", "jan", "feb", "mar", 
                         "apr","may","jun", "jul", "aug", "sep", "oct", "nov", "dec")
ggplot(PET.means, aes(lat, long, fill = jul))+geom_raster()
# get the precipitation data in the same format:
saveRDS(full.PET, paste0(workingdir, "full.PET.rds"))
saveRDS(PET.means, paste0(workingdir, "PET.means.rds"))


#ynew2[[1]]$PET_tho <- as.numeric(thornthwaite_PET(Tave = ynew2[[1]]$tmean, lat =  unique(ynew2[[1]]$x)))

#PET_tho <- thornthwaite(Tave = ynew2[[1]]$tmean, lat = unique(ynew2[[1]]$x), na.rm = FALSE)
# this does not work in pulling out the data from one grid cell
#y.df$CellID <- seq(1:nrow(y.df))

my.list <- list()
setwd("/Users/kah/Documents/bimodality")
saveRDS(y.df.list, paste0("/outputs/PRISM_temp", yrs, "_LL_temp.RDS"))

# this for loop calculates thornthwaite PET for each month in each grid cell
#for(i in 1:length(y$y)){
source("/afs/crc.nd.edu/user/k/kheilman/bimodality/R/Thornthwaite_PET.R")
system.time(
  for(i in 1:length(y$y)){
  
  ynew <- t(y[i,3:363])
  lat <- y[i,]$y
  long <- y[i,]$x
  cellID <- y[i,]$CellID
  # get month an year from row.names
  year <-  data.frame(year = substring(row.names(ynew), first = 26, last = 29))
  month <- data.frame(month = substring(row.names(ynew), first = 30, last = 31))
  ynew2 <- data.frame(Tave = ynew[,1], 
                      year <- year, 
                      month <- month)
  ynew2 <- ynew2[ynew2$month %in% c("01","03","04","05","06", "07", "08", "09", "10", "11", "12"),]
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
#ggplot(PET.means, aes(lat, long, fill = jul))+geom_raster()
# get the precipitation data in the same format:
saveRDS(full.PET, paste0(workingdir, "full.PET.rds"))
saveRDS(PET.means, paste0(workingdir, "PET.means.rds"))
full.PET <- readRDS("data/PET_full/full.PET.rds")
#full.PET <- readRDS("data/full.PET.rds")
full.PET <- full.PET[,c( "month","PET_tho", "lat","long")]
full <- dcast(full.PET, lat + long ~ month, mean, value.var = 'PET_tho', na.rm = TRUE)

full$Mean <- rowMeans(full[,4:length(full)], na.rm=TRUE)



full2 <- full

#ggplot(full2, aes(long, lat, fill = Aug))+geom_raster()
# for monthly dataset
# convert to rasterstack
coordinates(full) <- ~long + lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages
#proj4string(avgs) <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0' 
#avgs.alb <- projectRaster(avgs, crs='+init=epsg:3175')

# spec.table is a spatial points df, convert to regular df:
#spec.table <- as.data.frame(spec.table)
avgs.df <- data.frame(extract(avgs, spec.table.ll[,c("x","y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y

write.csv(avgs.df, paste0(workingdir, "PET_pls_extracted", yrs,".csv"))

