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
