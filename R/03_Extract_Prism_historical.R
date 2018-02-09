# kelly heilman
# Script for averaging and extracting PRISM data to 8km paleon grid

library(plyr)
library(raster)
library(data.table)
library(rgdal)
library(reshape2)
library(ggplot2)

version <- "1.7-5" # pls version

# set the working dir (where the prism data folder is)
workingdir <- "/Users/kah/Documents/bimodality/data/"
# for crc:
workingdir <- "/afs/crc.nd.edu/user/k/kheilman/bimodality/data/"
# read in the 8km grid for extracting
spec.table <- read.csv(paste0(workingdir,"data/midwest_pls_full_density_alb",version,".csv"))


##########################################################################
# extracting PRISM  precip data from the 1895-1980 historic prism dataset#
##########################################################################

#setwd to data directory for specific prism parameter (temp or precip)
setwd(paste0(workingdir,'PRISM_ppt_stable_4kmM2_189501_198012_bil/'))


#coordinates(spec.table) <- ~x + y
#proj4string(spec.table) <- '+init=epsg:3175' 
#spec.table.ll<- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

#spec.table.11<- as.data.frame(spec.table.ll)
spec.table.ll <- read.csv(paste0(workingdir, "spec.lat.long.csv"))

#designate the years we want to extract/ average over
years <- 1895:1925
yrs <- "1895-1925"

# this chunk of code reads in the filenames within the PRISM data folder
filenames <- list.files(pattern=paste(".*_",".*\\.bil$", sep = ""))

# use substring to index filenames that match the years designated:

filenames <- filenames [substring(filenames, first = 24, last = 27) %in% years]

s <- stack(filenames) #make all into a raster
t <- crop(s, extent(spec.table.ll)) 
s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
y <- data.frame(rasterToPoints(s)) #covert to dataframe
years <- rep(1895:1904, each = 12)
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
full<- data.frame(full)
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


######################################################################
# Extracting P- E from the Extract_PET_crc.R output
#
setwd ("/Users/kah/Documents/bimodality")

#full.PET <- readRDS('data/full.PET.rds')
full.PET <- readRDS("data/PET_full/full.PET.rds")
full.PET <- full.PET[,c( "month","PET_tho", "lat","long")]
full <- dcast(full.PET, lat + long ~ month, mean, value.var = 'PET_tho', na.rm = TRUE)
colnames(full) <- c("lat", "long", "Jan", 'Mar',"Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


coordinates(full) <- ~long + lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages

proj4string(avgs) <- '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0' 
avgs.alb <- projectRaster(avgs, crs='+init=epsg:3175')

# spec.table is a spatial points df, convert to regular df:
spec.table <- read.csv(paste0(workingdir,'midwest_pls_full_density_alb',version,'.csv'))
coordinates(spec.table) <- ~x + y

# project the grid to lat long
proj4string(spec.table) <- '+init=epsg:3175'
spec.table <- as.data.frame(spec.table)

plot(avgs.alb)

avgs.df <- data.frame(extract(avgs.alb, spec.table[,c("x","y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y

write.csv(avgs.df, paste0(workingdir, "PETJJA_1895_1925_pls_extracted_mar_nov.csv"))
#ggplot(avgs.df, aes(x,y, fill = Aug))+geom_raster()



#PETjja <- read.csv("data/PETJJA_1895_1925_pls_extracted.csv")
Precip <- read.csv(paste0("data/outputs/pr_monthly_Prism_1895-1925_full.csv"))
PETjja <- avgs.df
PrJJA <- Precip[,c("x", "y","Mar","Apr", "May","Jun", "Jul", "Aug", "Sep", "Oct", "Nov")]
colnames(PrJJA) <- c("x", "y", "Mar_pr","Apr_pr", "May_pr","Jun_pr", "Jul_pr", "Aug_pr", "Sep_pr", "Oct_pr", "Nov_pr")

colnames(PETjja) <- c("Jan_pet", "Mar_pet","Apr_pet", "May_pet","Jun_pet", "Jul_pet", "Aug_pet", "Sep_pet", "Oct_pet", "Nov_pet","Dec_pet","x","y")
PETjja <- PETjja[,!names(PETjja) %in% c("Jan_pet", "Dec_pet")] # remove january

#ggplot(PrJJA, aes(x,y, fill = Jun_pr))+geom_raster()
#ggplot(PETjja, aes(x,y, fill = Jun_pet))+geom_raster()

P.PET <- merge(PrJJA, PETjja, by = c("x", "y"))



# now lets calculaate P - PET for Jun - Aug
P.PET$Mar_ppet <- P.PET$Mar_pr - P.PET$Mar_pet
P.PET$Apr_ppet <- P.PET$Apr_pr - P.PET$Apr_pet
P.PET$May_ppet <- P.PET$May_pr - P.PET$May_pet
P.PET$Jun_ppet <- P.PET$Jun_pr - P.PET$Jun_pet
P.PET$Jul_ppet <- P.PET$Jul_pr - P.PET$Jul_pet
P.PET$Aug_ppet <- P.PET$Aug_pr - P.PET$Aug_pet
P.PET$Sep_ppet <- P.PET$Sep_pr - P.PET$Sep_pet
P.PET$Oct_ppet <- P.PET$Oct_pr - P.PET$Oct_pet
P.PET$Nov_ppet <- P.PET$Nov_pr - P.PET$Nov_pet

P.PET$GS_ppet <- rowSums(P.PET[,3:11], na.rm=TRUE) - rowSums(P.PET[,12:20], na.rm=TRUE)

ggplot(P.PET, aes(x,y, fill = Jun_ppet))+geom_raster()
ggplot(P.PET, aes(x,y, fill = Jul_ppet))+geom_raster()
ggplot(P.PET, aes(x,y, fill = Aug_ppet))+geom_raster()
ggplot(P.PET, aes(x,y, fill = GS_ppet))+geom_raster()


write.csv(P.PET, "outputs/P.PET_prism_1895_1925_Mar_Nov.csv")


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
system.time(for(i in 1:1000){
  
  ynew <- t(y[i,3:135])
  lat <- y[i,]$y
  long <- y[i,]$x
  cellID <- y[i,]$CellID
  # get month an year from row.names
  year <-  data.frame(year = substring(row.names(ynew), first = 26, last = 29))
  month <- data.frame(month = substring(row.names(ynew), first = 30, last = 31))
  ynew2 <- data.frame(Tave = ynew[,1], 
                      year <- year, 
                      month <- month)
  ynew2 <- ynew2[ynew2$month %in% c("06", "07", "08"),]
  # use the thorthwaite equation to attach the PET data to the 
  
  ynew2$PET_tho <- as.numeric(thornthwaite_PET(ynew2$Tave, lat))
  ynew2$lat <- lat
  ynew2$long <- long
  ynew2$CellID <- cellID
  
  my.list[[i]] <- ynew2
  cat("/")
})
# if PET.df already exists, then add to the df, if not, then create "PET.df"
#   if(exists  ("PET.df")){
PET.df <- do.call(rbind, my.list)


# rename the object:
full.PET <- PET.df
#remove PET.df
#rm(PET.df)


PET.means <- dcast(full.PET, lat + long  ~ month , mean , value.var='PET_tho', na.rm = TRUE)
colnames(PET.means) <- c("lat", "long", "CellID", "jan", "feb", "mar", "apr", "may",
                         "jun", "jul", "aug", "sep", "oct", "nov", "dec")
#ggplot(PET.means, aes(lat, long, fill = jul))+geom_raster()
# get the precipitation data in the same format:
saveRDS(full.PET, paste0(workingdir, "full.PET.rds"))
saveRDS(PET.means, paste0(workingdir, "PET.means.rds"))

#full.PET <- readRDS("data/full.PET.rds")
full.PET <- full.PET[,c( "month","PET_tho", "lat","long")]
full <- dcast(full.PET, lat + long ~ month, mean, value.var = 'PET_tho', na.rm = TRUE)

full$Mean <- rowMeans(full[,4:15], na.rm=TRUE)

colnames(full) <- c('lat','long',"Var",'Jan', 'Feb', 'Mar', "Apr", "May", 
                    'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec",'Mean')

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

write.csv(avgs.df, paste0(workingdir, "PET_pls_extracted.csv"))





