# kelly heilman
# Script for averaging and extracting PRISM data to 8km paleon grid

library(plyr)
library(raster)
library(data.table)
#ibrary(rgdal)
library(reshape2)

version <- "1.7-5" # pls version

# set the working dir (where the prism data folder is)
workingdir <- "/Users/kah/Documents/bimodality/data/"
# for crc:
workingdir <- "/afs/crc.nd.edu/user/k/kheilman/bimodality/data/"

##########################################################################
# extracting PRISM  precip data from the 1895-1980 historic prism dataset#
##########################################################################

#setwd to data directory for specific prism parameter (temp or precip)
setwd(paste0(workingdir,'PRISM_ppt_stable_4kmM2_189501_198012_bil/'))

# again read in the 8km grid for extracting
spec.table <- read.csv(paste0(workingdir,'midwest_pls_full_density_alb',version,'.csv'))
#coordinates(spec.table) <- ~x + y
#proj4string(spec.table) <- '+init=epsg:3175' 
#spec.table.ll<- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

#spec.table.11<- data.frame(spec.table.ll)
spec.table.ll <- read.csv(paste0(workingdir, "spec.lat.long.csv"))

#designate the years we want to extract/ average over
years <- 1895:1925
yrs <- "1895-1925"

# this chunk of code reads in the filenames within the PRISM data folder
filenames <- list.files(pattern=paste(".*_",".*\\.bil$", sep = ""))

# use substring to index filenames that match the years designated:

filenames <- filenames [substring(filenames, first = 26, last = 29) %in% years]

s <- stack(filenames) #make all into a raster
t <- crop(s, extent(spec.table.ll)) 
#s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
y <- data.frame(rasterToPoints(t)) #covert to dataframe
years <- rep(1895:1924, each = 12)
mo <- rep(c('Jan', 'Feb', 'Mar', "Apr", "May", 
            'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec"), 10)  

monthly <- y
yearly <- y

# melt so that we can calculate the mean total precipitation by month  
melted.mo <- melt(monthly, id.var = c('x', 'y'))
melted.mo$yrs <- substring(melted.mo$variable, first = 24, last = 27)
melted.mo$mos <- substring(melted.mo$variable, first = 28, last = 29)

#calculate means for months
full<- dcast(melted.mo, x + y ~ mos, mean , value.var='value', na.rm = TRUE)
full$total <- rowSums(full[,3:14])
colnames(full) <- c('x','y','Jan', 'Feb', 'Mar', "Apr", "May", 
                    'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec",'total')


# calculate a seasonality index from the monthly precipitatoin
full$SI <- rowSums(abs(full[,3:14]-(full[,15]/12)))/full[,15]
#melted.yr <- melt(yearly, id.var = c('x', 'y'))
write.csv(full, paste0(workingdir,"outputs/temporary_melted_1895_1925.csv"))


coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full) 

spec.table <- data.frame(spec.table)

plot(avgs) #plots the raster averages

avgs.df <- data.frame(extract(avgs, spec.table[,c("x","y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y

write.csv(avgs.df, paste0(workingdir,"outputs/pr_monthly_Prism_",yrs,"_full.csv"))



######################################################################################
#extract mean temperature data from the prism data#
######################################################################################


#setwd to data directory
setwd(paste0(workingdir,'PRISM_tmean_stable_4kmM2_189501_198012_bil/'))

#read in the grid again
spec.table <- read.csv(paste0(workingdir,'midwest_pls_full_density_alb',version,'.csv'))
coordinates(spec.table) <- ~x + y

# project the grid to lat long
proj4string(spec.table) <- '+init=epsg:3175'
#spec.lat <- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0' ))

years <- 1895:1925
yrs <- "1895-1925"

# read in the filenames, stack as rasters, extract raster to points
filenames <- list.files(pattern=paste(".*_",".*\\.bil$", sep = ""))

# use substring to index filenames that match the years designated:

filenames <- filenames [substring(filenames, first = 26, last = 29) %in% years]

s <- stack(filenames) #make all into a raster
t <- crop(s, extent(spec.table.ll)) #crop to the extent of indiana & illinois 
#s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
y <- data.frame(rasterToPoints(t)) #covert to dataframe
years <- rep(years, each = 12)
mo <- rep(c('Jan', 'Feb', 'Mar', "Apr", "May", 
            'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec"), 10)  


test <- y
monthly <- test
yearly <- test

melted.mo <- melt(monthly, id.var = c('x', 'y'))
melted.mo$yrs <- substring(melted.mo$variable, first = 26, last = 29)
melted.mo$mos <- substring(melted.mo$variable, first = 30, last = 31)
full<- dcast(melted.mo, x + y ~ mos, mean , value.var='value', na.rm = TRUE)
full$Mean <- rowMeans(full[,3:14])
colnames(full) <- c('x','y','Jan', 'Feb', 'Mar', "Apr", "May", 
                    'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec",'Mean')
test <- full

#calculate the CV temperature seasonality:
#TSI = sd(m1....m12)/Tavgannual *100
test$cv <- (apply(test[,3:14],1, sd, na.rm = TRUE)/test[,15])*100
full <- test
spec.table <- data.frame(spec.table)

# for monthly dataset
# convert to rasterstack
coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages

avgs.df <- data.frame(extract(avgs, spec.table[,c("x","y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y

write.csv(avgs.df, paste0(workingdir,"outputs/tmean_yr_Prism_",yrs,"_full.csv"))

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

years <- 1895:1905
yrs <- "1895-1905"

# read in the filenames, stack as rasters, extract raster to points
filenames <- list.files(pattern=paste(".*_",".*\\.bil$", sep = ""))

# use substring to index filenames that match the years designated:

filenames <- filenames [substring(filenames, first = 26, last = 29) %in% years]

s <- stack(filenames) #make all into a raster
t <- crop(s, extent(spec.table.ll)) #crop to the extent of indiana & illinois 
#s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
y <- data.frame(rasterToPoints(t)) #covert to dataframe


test <- y
# this does not work in pulling out the data from one grid cell
y$CellID <- seq(1:nrow(y))

my.list <- list()
# this for loop calculates thornthwaite PET for each month in each grid cell
#for(i in 1:length(y$y)){
source("/afs/crc.nd.edu/user/k/kheilman/bimodality/R/Thornthwaite_PET.R")
for(i in 1:length(y$y)){
 
       ynew <- t(y[i,3:135])
       lat <- y[i,]$y
       long <- y[i,]$x
       cellID <- y[i,]$CellID
       # get month an year from row.names
       year <-  data.frame(year =substring(row.names(ynew), first = 26, last = 29))
       month <- data.frame(month = substring(row.names(ynew), first = 30, last = 31))
       ynew2 <- data.frame(Tave = ynew[,1], 
                           year <- year, 
                           month <- month)
       
       # use the thorthwaite equation to attach the PET data to the 
       
      ynew2$PET_tho <- as.numeric(thornthwaite_PET(ynew2$Tave, lat))
       ynew2$lat <- lat
       ynew2$long <- long
       ynew2$CellID <- cellID
  
       my.list[[i]] <- ynew2
}
   # if PET.df already exists, then add to the df, if not, then create "PET.df"
#   if(exists  ("PET.df")){
   PET.df <- do.call(rbind, my.list)
     
 #  PET.df <- rbind(PET.df, ynew2)
  # }else{
   #  PET.df <- ynew2
   #}
#}

# rename the object:
full.PET <- PET.df
#remove PET.df
#rm(PET.df)


#PET.means <- dcast(full.PET, lat + long  ~ month , mean , value.var='PET_tho', na.rm = TRUE)
#colnames(PET.means) <- c("lat", "long", "CellID", "jan", "feb", "mar", "apr", "may",
                     #    "jun", "jul", "aug", "sep", "oct", "nov", "dec")
#ggplot(PET.means, aes(lat, long, fill = jul))+geom_raster()
# get the precipitation data in the same format:
saveRDS(full.PET, paste0(workingdir, "full.PET.rds"))

# read in rds from crc to do thornthwaite (cant get SPEI package on crc):
#library(SPEI)
#full.PET <- readRDS(paste0(workingdir, "full.PET.rds"))
#full.PET$Tave <- full.PET$Tave/100 # need to convert to celcius
#test.PET <- full.PET[1:48,]
#full.PET$PET_tho <- as.numeric(thornthwaite(Tave= full.PET$Tave, lat = full.PET$lat))


#test.PET <- thornthwaite(Tave = test$Tave, lat = test$lat)
#full <- dcast(full.PET, lat + long ~ month, mean, value.var = 'PET_tho', na.rm = TRUE)
#full <- dcast(full.PET, lat + long ~ month, mean, value.var = 'Tave', na.rm = TRUE)

#full$Mean <- rowMeans(full[,4:14], na.rm=TRUE)

#colnames(full) <- c('lat','long',"Var",'Jan', 'Feb', 'Mar', "Apr", "May", 
 #                   'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec",'Mean')

#full2 <- full
