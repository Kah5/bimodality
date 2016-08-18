library(plyr)
library(raster)
library(data.table)
# read in and average prism data
prism<- raster("C:/Users/JMac/Documents/Kelly/biomodality/data/PRISM_ppt_30yr_normal_4kmM2_all_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
prism.alb<- projectRaster(prism, crs='+init=epsg:3175')
spec.table<- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb.csv")
spec.table <- data.frame(spec.table)
spec.table$pr30yr <- extract(prism.alb, spec.table[,c("x","y")])

write.csv(spec.table[,c('x', 'y', 'cell', 'pr30yr')], 'C:/Users/JMac/Documents/Kelly/biomodality/data/spec_table_30yr_prism.csv')
#try prims
#install.packages('prism')
#library(prism)
#process_zip("data/PRISM_ppt_stable_4kmM2_189501_198012_bil")

#get_prism_annual(type = 'ppt', years = 1901:1905, keepZip = FALSE)



#try this loop, takes awhile, but works

library(raster)
#setwd to data directory
setwd('C:/Users/JMac/Documents/Kelly/biomodality/data/PRISM_ppt_stable_4kmM2_189501_198012_bil/')

#spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/spec.table.csv')
coordinates(spec.table) <- ~x + y

years <- 1970:1980
for (i in years) {
  filenames <- list.files(pattern=paste(".*_",i,".*\\.bil$", sep = ""))
  s <- stack(filenames) #make all into a raster
  s <- projectRaster(s, crs='+init=epsg:3175') # project in great lakes albers
  t <- crop(s, extent(spec.table)) #crop to the extent of indiana & illinois 
  y <- data.frame(rasterToPoints(t)) #covert to dataframe
  colnames(y) <- c("x", "y", month.abb)
  y$year <- i
  y$gridNumber <- cellFromXY(t, y[, 1:2])
  # write.csv( ) ?
}


spec.table <- data.frame(spec.table)

y$total <- rowSums(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                        'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])

#this averages for each month within each gridcell
full <- dcast(setDT(y), x + y ~ ., value.var=c('Jan', 'Feb', 'Mar', "Apr", "May", 
                                             'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec", 'total'))

#convert to rasterstack
coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages

avgs.df <- data.frame(extract(avgs, spec.table[,c("x","y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y

write.csv(avgs.df, "C:/Users/JMac/Documents/Kelly/biomodality/outputs/pr_monthly_Prism_1895_1905.csv")




#############################################
#extract mean temperature data
############################################


#setwd to data directory
  setwd('C:/Users/JMac/Documents/Kelly/biomodality/data/PRISM_ppt_stable_4kmM2_189501_198012_bil/')

spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/spec.table.csv')
coordinates(spec.table) <- ~x + y

years <- 1895:1905
for (i in years) {
  filenames <- list.files(pattern=paste(".*_",i,".*\\.bil$", sep = ""))
  s <- stack(filenames) #make all into a raster
  s <- projectRaster(s, crs='+init=epsg:3175') # project in great lakes albers
  t <- crop(s, extent(spec.table)) #crop to the extent of indiana & illinois 
  y <- data.frame(rasterToPoints(t)) #covert to dataframe
  colnames(y) <- c("x", "y", month.abb)
  y$year <- i
  y$gridNumber <- cellFromXY(t, y[, 1:2])
  # write.csv( ) ?
}


spec.table <- data.frame(spec.table)

y$total <- rowSums(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
                        'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])

#this averages for each month within each gridcell
full.t <- dcast(setDT(y), x + y ~ ., value.var=c('Jan', 'Feb', 'Mar', "Apr", "May", 
                                               'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec", 'total'))

#convert to rasterstack
coordinates(full.t) <- ~x + y
gridded(full.t) <- TRUE
avgs <- stack(full.t) 

plot(avgs) #plots averages

avgs.df <- data.frame(extract(avgs, tree.dens[,c("x","y")]))
avgs.df$x <- tree.dens$x
avgs.df$y <- tree.dens$y

write.csv(avgs.df, "C:/Users/JMac/Documents/Kelly/biomodality/outputs/tmean_monthly_Prism_1895_1905.csv")

