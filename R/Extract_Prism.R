library(plyr)
library(raster)
library(data.table)
library(rgdal)
version <- "1.6-5"

# set the working dir (where the prism data folder is)
workingdir <- "C:/Users/JMac/Documents/Kelly/biomodality/data/"

# read in and average prism data (this is modern 30year normals)
prism<- raster(paste0(workingdir,"PRISM_ppt_30yr_normal_4kmM2_all_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil"))
prism.alb<- projectRaster(prism, crs='+init=epsg:3175')


# spec. table is a data frame that has the xy coordinates of the grid that you want the prism data on
spec.table <- read.csv(paste0(workingdir,"/data/midwest_pls_full_density_pr_alb1.6-5.csv"))

# extract 30 year normals at the xy points of teh grid
spec.table$pr30yr <- extract(prism.alb, spec.table[,c("x","y")])

write.csv(spec.table[,c('x', 'y', 'cell', 'pr30yr')], paste0(workingdir,'spec_table_30yr_prism_full.csv'))


#
#get the monthly averages for the modern 30 year normals
setwd(paste0(workingdir,'PRISM_ppt_30yr_normal_4kmM2_all_bil/'))


coordinates(spec.table) <- ~x +y
proj4string(spec.table) <- '+init=epsg:3175'
spec.table.ll<- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

month <- sprintf("%02d", 1:12)

# read in and stack the prism rasters, then extract rasters to points
for (i in 1:length(month)) {
  filenames <- list.files(pattern=paste0(".*_", month[i],".*\\.bil$", sep = ""))
  s <- stack(filenames)
  t <- crop(s, extent(spec.table.ll))#make all into a raster
  s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
   #crop to the extent of indiana & illinois 
  y <- data.frame(rasterToPoints(t)) #covert to dataframe
  #colnames(y) <- c("x", "y", month.abb)
  y$month <- month[i]
  y$gridNumber <- cellFromXY(t, y[, 1:2])
  write.csv(y ,paste0('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_',month[i],'_precip.csv' )) 
}

jan<- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_01_precip.csv')
feb<- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_02_precip.csv')
mar<- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_03_precip.csv')
apr<- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_04_precip.csv')
may<- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_05_precip.csv')
jun<- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_06_precip.csv')
jul<- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_07_precip.csv')
aug<- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_08_precip.csv')
sep<- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_09_precip.csv')
oct<- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_10_precip.csv')
nov<- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_11_precip.csv')
dec<- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_12_precip.csv')

# create a full dataset with all months
full <- cbind(jan[,1:4], feb[,4],mar[,4], apr[,4], may[,4], jun[,4], jul[,4], aug[,4], sep[,4], oct[, 4], nov[,4], dec[,4])
colnames(full) <- c('X','x', 'y' ,'Jan', 'Feb', 'Mar', "Apr", "May", 
                       'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")
full$total<- rowSums(full[,4:15], na.rm=TRUE)
full$SI <- rowSums(abs(full[,4:15]-(full[,16]/12)))/full[,16]
#full$cv <- ((apply(full[,4:15],1, sd, na.rm = TRUE)/full[,16]))*100


coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full) 
proj4string(avgs) <- crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 ')
avgs<- projectRaster(avgs, crs='+init=epsg:3175')
plot(avgs) #plots averages


spec.table <- data.frame(spec.table)
avgs.df <- data.frame(extract(avgs, spec.table[,c("x","y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y

write.csv(avgs.df, "C:/Users/JMac/Documents/Kelly/biomodality/outputs/pr_monthly_Prism_30yrnorms_full.csv")



##########################################################################
# extracting PRISM  precip data from the 1895-1980 historic prism dataset#
##########################################################################

#setwd to data directory
setwd(paste0(workingdir,'PRISM_ppt_stable_4kmM2_189501_198012_bil/'))

# again read in the 8km grid for extracting
spec.table <- read.csv(paste0(workingdir,'midwest_pls_full_density_pr_alb1.6-5.csv'))
coordinates(spec.table) <- ~x + y
proj4string(spec.table) <- '+init=epsg:3175'
spec.table.ll<- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

#designate the years we want to extract/ average over
years <- 1900
yrs <- "1900"

# this chunk of code reads in the filenames within the PRISM data folder
filenames <- list.files(pattern=paste(".*_","190",".*\\.bil$", sep = ""))
  s <- stack(filenames) #make all into a raster
  t <- crop(s, extent(spec.table.ll)) 
  s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
  y <- data.frame(rasterToPoints(s)) #covert to dataframe
  years <- rep(1895:1924, each = 12)
  mo <- rep(c('Jan', 'Feb', 'Mar', "Apr", "May", 
              'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec"), 10)  
  
  monthly <- y
  yearly <- y
  
# melt so that we can calculate the mean total precipitation by month  
 melted.mo <- melt(monthly, id.var = c('x', 'y'))
 melted.mo$yrs <- substring(melted.mo$variable, first = 24, last = 27)
 melted.mo$mos <- substring(melted.mo$variable, first = 28, last = 29)
 
 #calculate means
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


#ggplot(avgs.df, aes(x = x, y=y, color = total))+geom_point()
###################################################
#extract mean temperature data from the prism data#
###################################################


#setwd to data directory
setwd(paste0(workingdir,'PRISM_tmean_stable_4kmM2_189501_198012_bil/'))

#read in the grid again
spec.table <- read.csv(paste0(workingdir,'midwest_pls_full_density_pr_alb1.6-5.csv'))
coordinates(spec.table) <- ~x + y
  
# project the grid to lat long
proj4string(spec.table) <- '+init=epsg:3175'
spec.lat <- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0' ))

years <- 1900
yrs <- "1900"

# read in the filenames, stack as rasters, extract raster to points
filenames <- list.files(pattern=paste(".*_","190",".*\\.bil$", sep = ""))
s <- stack(filenames) #make all into a raster
t <- crop(s, extent(spec.lat)) #crop to the extent of indiana & illinois 
s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
y <- data.frame(rasterToPoints(s)) #covert to dataframe
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


#monthly seasonal temperature
#get the monthly averages
setwd(paste0(workingdir,'PRISM_tmean_30yr_normal_4kmM2_all_bil/'))
coordinates(spec.table) <- ~x +y
proj4string(spec.table) <- '+init=epsg:3175'
spec.table.ll<- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

month <- sprintf("%02d", 1:12)
for (i in 1:length(month)) {
  filenames <- list.files(pattern=paste0(".*_", month[i],".*\\.bil$", sep = ""))
  s <- stack(filenames)
  t <- crop(s, extent(spec.table.ll))#make all into a raster
  s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
  #crop to the extent of indiana & illinois 
  y <- data.frame(rasterToPoints(t)) #covert to dataframe
  #colnames(y) <- c("x", "y", month.abb)
  y$month <- month[i]
  y$gridNumber <- cellFromXY(t, y[, 1:2])
  write.csv(y ,paste0('C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_',month[i],'_tmean.csv' )) 
}

# above loop doesn't add all teh 
jan<- read.csv(paste0(workingdir,'/30yrnorm_01_tmean.csv'))
feb<- read.csv(paste0(workingdir,'/30yrnorm_02_tmean.csv'))
mar<- read.csv(paste0(workingdir,'/30yrnorm_03_tmean.csv'))
apr<- read.csv(paste0(workingdir,'/30yrnorm_04_tmean.csv'))
may<- read.csv(paste0(workingdir,'/30yrnorm_05_tmean.csv'))
jun<- read.csv(paste0(workingdir,'/30yrnorm_06_tmean.csv'))
jul<- read.csv(paste0(workingdir,'/30yrnorm_07_tmean.csv'))
aug<- read.csv(paste0(workingdir,'/30yrnorm_08_tmean.csv'))
sep<- read.csv(paste0(workingdir,'/30yrnorm_09_tmean.csv'))
oct<- read.csv(paste0(workingdir,'/30yrnorm_10_tmean.csv'))
nov<- read.csv(paste0(workingdir,'/30yrnorm_11_tmean.csv'))
dec<- read.csv(paste0(workingdir,'/30yrnorm_12_tmean.csv'))

# rearragen into a dataframe
full <- cbind(jan[,1:4], feb[,4],mar[,4], apr[,4], may[,4], jun[,4], jul[,4], aug[,4], sep[,4], oct[, 4], nov[,4], dec[,4])
colnames(full) <- c('X','x', 'y' ,'Jan', 'Feb', 'Mar', "Apr", "May", 
                    'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")

#calculate annual means
full$Mean <- rowMeans(full[,4:15])
colnames(full) <- c('x','y','Jan', 'Feb', 'Mar', "Apr", "May", 
                    'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec",'Mean')

test <- full
#calculate the CV temperature seasonality:
#TSI = sd(m1....m12)/Tavgannual *100
test$cv <- (apply(test[,4:15],1, sd, na.rm = TRUE)/test[,16])*100
full <- test

coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages

# extract mean temp to the grid 
spec.table <- data.frame(spec.table)
avgs.df <- data.frame(extract(avgs, spec.table[,c("x","y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y
avgs.df$cv <- (apply(avgs.df[,1:12],1, sd, na.rm = TRUE)/avgs.df[,13])*100


write.csv(avgs.df, "C:/Users/JMac/Documents/Kelly/biomodality/outputs/tmean_monthly_Prism_30yrnorms_full.csv")


ggplot(avgs.df, aes(x=x, y=y, color = Mean)) + geom_point()





#now for the 30yr mean temperature data
# read in and average prism data
prism<- raster(paste0(workingdir,"PRISM_tmean_30yr_normal_4kmM2_annual_bil/PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil"))
prism.alb<- projectRaster(prism, crs='+init=epsg:3175')
#spec.table<- read.csv(paste0(workingdir,"midwest_pls_fia_density_alb.csv")
#spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb1.6.csv')
spec.table <- data.frame(spec.table)
temp30yr <- data.frame(extract(prism.alb, spec.table[,c("x","y")]))
temp30yr$x <- spec.table$x
temp30yr$y <- spec.table$y
colnames(temp30yr) <- c('modtmean', 'x', 'y')
write.csv(temp30yr, paste0(workingdir,'outputs/tmean_30yr_prism.csv'))





