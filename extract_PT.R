##########################################
#extracting precip & plotting Crown cover#
#run calculate_dens_v2.r first
#Kelly Heilman               
#January 20, 2016            
##########################################

#getting gridded climate data
#need to clean this up

library(reshape2)
library(data.table)
library(sp)
library(raster)

setwd('C:/Users/JMac/Documents/Kelly/biomodality/data/precip_2014/')
years <- 1900:1910
month.abb <- c('Jan', 'Feb', 'Mar', "Apr", "May", 
  'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")

#this loop extracts the data and adds the year to a dataframe
for (i in years) {
  filenames <- list.files(pattern=paste("precip.",i, sep = ""))
  s <- read.table(filenames)
  y <- data.frame(s) #covert to dataframe
  colnames(y) <- c("Lon", "Lat", month.abb)
  y$year <- i
}

y$total <- rowSums(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                        'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])

#this averages for each month within each gridcell
full <- dcast(setDT(y), Lon + Lat ~ ., value.var=c('Jan', 'Feb', 'Mar', "Apr", "May", 
                                               'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec", 'total'))

#convert to rasterstack
coordinates(full) <- ~Lon + Lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages
projection(avgs) <- CRS("+init=epsg:4326") # assign the projection from GHCN
avg.alb <- projectRaster(avgs, crs='+init=epsg:3175') # project in great lakes albers


spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/spec.table.csv')
coordinates(spec.table) <- ~x + y

precip.alb <- crop(avg.alb, extent(spec.table)) 
spec.table <- data.frame(spec.table)

precip <- data.frame(extract(avg.alb, spec.table[,c('x', 'y')]))
precip$x <- spec.table$x
precip$y <- spec.table$y

write.csv(precip, 'C:/Users/JMac/Documents/Kelly/biomodality/data/pr_alb_1900_1910_GHCN.csv')

######################
##For Temperature now
#####################
setwd("C:/Users/JMac/Documents/Kelly/biomodality/data/air_temp_2014/")
years <- 1900:1910
month.abb <- c('Jan', 'Feb', 'Mar', "Apr", "May", 
               'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")

#this loop extracts the data and adds the year to a dataframe
for (i in years) {
  filenames <- list.files(pattern=paste("air_temp.",i, sep = ""))
  s <- read.table(filenames)
  y <- data.frame(s) #covert to dataframe
  colnames(y) <- c("Lon", "Lat", month.abb)
  y$year <- i
}

y$annual <- rowMeans(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                        'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])

#this averages for each month within each gridcell
full <- dcast(setDT(y), Lon + Lat ~ ., value.var=c('Jan', 'Feb', 'Mar', "Apr", "May", 
                                                   'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec", 'total'))

#convert to rasterstack
coordinates(full) <- ~Lon + Lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages
projection(avgs) <- CRS("+init=epsg:4326") # assign the projection from GHCN
avg.alb <- projectRaster(avgs, crs='+init=epsg:3175') # project in great lakes albers


spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/spec.table.csv')
coordinates(spec.table) <- ~x + y

air_temp.alb <- crop(avg.alb, extent(spec.table)) 
spec.table <- data.frame(spec.table)

air_temp <- data.frame(extract(air_temp.alb, spec.table[,c('x', 'y')]))
air_temp$x <- spec.table$x
air_temp$y <- spec.table$y

write.csv(air_temp, 'C:/Users/JMac/Documents/Kelly/biomodality/data/air_temp_alb_1900_1910_GHCN.csv')


######################
##For PET (Potential Evapotranspiation) now
#####################
setwd("C:/Users/JMac/Documents/Kelly/biomodality/data/Eo150_2014/")
years <- 1900:1910
month.abb <- c('Jan', 'Feb', 'Mar', "Apr", "May", 
               'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")

#this loop extracts the data and adds the year to a dataframe
for (i in years) {
  filenames <- list.files(pattern=paste("Eo150.",i, sep = ""))
  s <- read.table(filenames)
  y <- data.frame(s) #covert to dataframe
  colnames(y) <- c("Lon", "Lat", month.abb)
  y$year <- i
}

y$annual <- rowMeans(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                        'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])

#this averages for each month within each gridcell
full <- dcast(setDT(y), Lon + Lat ~ ., value.var=c('Jan', 'Feb', 'Mar', "Apr", "May", 
                                                   'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec", 'total'))

#convert to rasterstack
coordinates(full) <- ~Lon + Lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages
projection(avgs) <- CRS("+init=epsg:4326") # assign the projection from GHCN
avg.alb <- projectRaster(avgs, crs='+init=epsg:3175') # project in great lakes albers


spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/spec.table.csv')
coordinates(spec.table) <- ~x + y

Eo150.alb <- crop(avg.alb, extent(spec.table)) 
spec.table <- data.frame(spec.table)

Eo150 <- data.frame(extract(Eo150.alb, spec.table[,c('x', 'y')]))
Eo150$x <- spec.table$x
Eo150$y <- spec.table$y

write.csv(Eo150, 'C:/Users/JMac/Documents/Kelly/biomodality/data/Eo150_alb_1900_1910_GHCN.csv')


######################
##For Actual Evapotranspiration now
#####################
setwd("C:/Users/JMac/Documents/Kelly/biomodality/data/E150_2014/")
years <- 1900:1910
month.abb <- c('Jan', 'Feb', 'Mar', "Apr", "May", 
               'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")

#this loop extracts the data and adds the year to a dataframe
for (i in years) {
  filenames <- list.files(pattern=paste("E150.",i, sep = ""))
  s <- read.table(filenames)
  y <- data.frame(s) #covert to dataframe
  colnames(y) <- c("Lon", "Lat", month.abb)
  y$year <- i
}

y$annual <- rowMeans(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                        'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])

#this averages for each month within each gridcell
full <- dcast(setDT(y), Lon + Lat ~ ., value.var=c('Jan', 'Feb', 'Mar', "Apr", "May", 
                                                   'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec", 'total'))

#convert to rasterstack
coordinates(full) <- ~Lon + Lat
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages
projection(avgs) <- CRS("+init=epsg:4326") # assign the projection from GHCN
avg.alb <- projectRaster(avgs, crs='+init=epsg:3175') # project in great lakes albers


spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/spec.table.csv')
coordinates(spec.table) <- ~x + y

E150.alb <- crop(avg.alb, extent(spec.table)) 
spec.table <- data.frame(spec.table)

E150 <- data.frame(extract(E150.alb, spec.table[,c('x', 'y')]))
E150$x <- spec.table$x
E150$y <- spec.table$y

write.csv(E150, 'C:/Users/JMac/Documents/Kelly/biomodality/data/E150_alb_1900_1910_GHCN.csv')


