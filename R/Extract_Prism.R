library(plyr)
library(raster)
library(data.table)
library(rgdal)
version <- "1.6-5"

# read in and average prism data
prism<- raster("C:/Users/JMac/Documents/Kelly/biomodality/data/PRISM_ppt_30yr_normal_4kmM2_all_bil/PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil")
prism.alb<- projectRaster(prism, crs='+init=epsg:3175')
#spec.table<- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb.csv")
spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb1.6-5.csv')
spec.table <- data.frame(spec.table)
spec.table$pr30yr <- extract(prism.alb, spec.table[,c("x","y")])

write.csv(spec.table[,c('x', 'y', 'cell', 'pr30yr')], 'C:/Users/JMac/Documents/Kelly/biomodality/data/spec_table_30yr_prism_full.csv')
#try prims
#install.packages('prism')
#library(prism)
#process_zip("data/PRISM_ppt_stable_4kmM2_189501_198012_bil")

#get the monthly averages
setwd('C:/Users/JMac/Documents/Kelly/biomodality/data/PRISM_ppt_30yr_normal_4kmM2_all_bil/')
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

full <- cbind(jan[,1:4], feb[,4],mar[,4], apr[,4], may[,4], jun[,4], jul[,4], aug[,4], sep[,4], oct[, 4], nov[,4], dec[,4])
colnames(full) <- c('X','x', 'y' ,'Jan', 'Feb', 'Mar', "Apr", "May", 
                       'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")
coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full) 

#plot(avgs) #plots averages

#extract at the tree level for tree cover modeling
PLSpoints.agg <- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/outputs/PLS_pct_cov_by_pt_inil.csv")
avgs.pts <- data.frame(extract(avgs, PLSpoints.agg[,c('Pointx', 'Pointy')]))
avgs.pts$Pointx <- PLSpoints.agg$Pointx
avgs.pts$Pointy <- PLSpoints.agg$Pointy
write.csv(avgs.pts, "C:/Users/JMac/Documents/Kelly/biomodality/data/PLSpoints.agg.full_mo_modernPRISMP.csv")

spec.table <- data.frame(spec.table)
avgs.df <- data.frame(extract(avgs, spec.table[,c("x","y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y

write.csv(avgs.df, "C:/Users/JMac/Documents/Kelly/biomodality/outputs/pr_monthly_Prism_30yrnorms_full.csv")


#write.csv(full.mo, "C:/Users/JMac/Documents/Kelly/biomodality/outputs/30yrnorm_allmonths_precip.csv")
#try this loop, takes awhile, but works

library(raster)
#setwd to data directory
setwd('C:/Users/JMac/Documents/Kelly/biomodality/data/PRISM_ppt_stable_4kmM2_189501_198012_bil/')

#spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/spec.table.csv')
spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb1.6-5.csv')
coordinates(spec.table) <- ~x + y

years <- 1895:1925
yrs <- "1895-1925"

filenames <- list.files(pattern=paste(".*_","190",".*\\.bil$", sep = ""))
  s <- stack(filenames) #make all into a raster
  t <- crop(s, extent(spec.table.ll)) 
  s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
  y <- data.frame(rasterToPoints(s)) #covert to dataframe
  years <- rep(1895:1924, each = 12)
  mo <- rep(c('Jan', 'Feb', 'Mar', "Apr", "May", 
              'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec"), 10)  
  
  
  #add 1910 to this
  #yr1910 <- stack(list.files(pattern=paste(".*_","1910",".*\\.bil$", sep = "")))
  #t2 <- crop(yr1910, extent(spec.table.ll)) #crop to the extent of indiana & illinois 
  #t2 <- projectRaster(t2, crs='+init=epsg:3175')
  #y2 <- data.frame(rasterToPoints(t2)) 
  
  #test <- cbind(y, y2[,3:14])
  monthly <- y
  yearly <- y
  
  
  
 melted.mo <- melt(monthly, id.var = c('x', 'y'))
 melted.mo$yrs <- substring(melted.mo$variable, first = 24, last = 27)
 melted.mo$mos <- substring(melted.mo$variable, first = 28, last = 29)
 full<- dcast(melted.mo, x + y ~ mos, mean , value.var='value', na.rm = TRUE)
 full$total <- rowSums(full[,3:14])
 colnames(full) <- c('x','y','Jan', 'Feb', 'Mar', "Apr", "May", 
                     'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec",'total')
 
 

full$SI <- rowSums(abs(full[,3:14]-(full[,15]/12)))/full[,15]
 #melted.yr <- melt(yearly, id.var = c('x', 'y'))
 write.csv(full, "C:/Users/JMac/Documents/Kelly/biomodality/outputs/temporary_melted_1895_1925.csv")
 #total <- dcast(melted.yr, x + y ~ variable, sum, value.var = 'value', na.rm = TRUE)
 #coordinates(total) <- ~x + y
 #gridded(total) <- TRUE
 #avgs <- stack(total) 
 
 
 
 
 #do the same for the monthly variables
 #full<- dcast(melted.mo, x + y ~ variable, mean , value.var='value', na.rm = TRUE)
 
 coordinates(full) <- ~x + y
 gridded(full) <- TRUE
 avgs <- stack(full) 
  #colnames(y) <- c("x", "y", month.abb)
  #y$year <- i
  #y$gridNumber <- cellFromXY(t, y[, 1:2])
  # write.csv( ) ?


spec.table <- data.frame(spec.table)



plot(avgs) #plots averages

#extract at the tree level for tree cover modeling
PLSpoints.agg <- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/outputs/PLS_pct_cov_by_pt_inil.csv")
avgs.pts <- data.frame(extract(avgs, PLSpoints.agg[,c('Pointx', 'Pointy')]))
avgs.pts$Pointx <- PLSpoints.agg$Pointx
avgs.pts$Pointy <- PLSpoints.agg$Pointy
write.csv(avgs.pts, "C:/Users/JMac/Documents/Kelly/biomodality/data/PLSpoints.agg.1895_1905prismppt.csv")

avgs.df <- data.frame(extract(avgs, spec.table[,c("x","y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y

write.csv(avgs.df, paste0("C:/Users/JMac/Documents/Kelly/biomodality/outputs/pr_monthly_Prism_",yrs,"_full.csv"))




#############################################
#extract mean temperature data
############################################


#setwd to data directory
setwd('C:/Users/JMac/Documents/Kelly/biomodality/data/PRISM_tmean_stable_4kmM2_189501_198012_bil/')

spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb1.6-5.csv')
coordinates(spec.table) <- ~x + y
  

proj4string(spec.table) <- '+init=epsg:3175'
spec.lat <- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0' ))

years <- 1895:1925
yrs <- "1895-1925"

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

#y$mean <- rowSums(y[,c('Jan', 'Feb', 'Mar', "Apr", "May", 
 #                       'Jun', "Jul", "Aug", "Sep", "Oct", "Nov","Dec")])/12

#this averages for each month within each gridcell
#full.t <- dcast(setDT(y), x + y ~ year, mean)
#full.t <- dcast(data.frame(y), x + y ~ ., mean, value.var = 'mean')
 # for monthly dataset
#convert to rasterstack
coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full) 

plot(avgs) #plots averages

avgs.df <- data.frame(extract(avgs, spec.table[,c("x","y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y

write.csv(avgs.df, paste0("C:/Users/JMac/Documents/Kelly/biomodality/outputs/tmean_yr_Prism_",yrs,"_full.csv"))




#now for the 30yr means
# read in and average prism data
prism<- raster("C:/Users/JMac/Documents/Kelly/biomodality/data/PRISM_tmean_30yr_normal_4kmM2_annual_bil/PRISM_tmean_30yr_normal_4kmM2_annual_bil.bil")
prism.alb<- projectRaster(prism, crs='+init=epsg:3175')
#spec.table<- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb.csv")
spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb1.6.csv')
spec.table <- data.frame(spec.table)
temp30yr <- data.frame(extract(prism.alb, spec.table[,c("x","y")]))
temp30yr$x <- spec.table$x
temp30yr$y <- spec.table$y
colnames(temp30yr) <- c('modtmean', 'x', 'y')
write.csv(temp30yr, 'C:/Users/JMac/Documents/Kelly/biomodality/outputs/tmean_30yr_prism.csv')

#setwd to data directory
setwd('C:/Users/JMac/Documents/Kelly/biomodality/data/PRI')

spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/outputs/spec.table.csv')
coordinates(spec.table) <- ~x + y
yrs<- "1895-1905"

  s <- stack("PRISM_ppt_30yr_normal_4kmM2_annual_bil.bil") #make all into a raster
  s <- projectRaster(s, crs='+init=epsg:3175') # project in great lakes albers
  t <- crop(s, extent(spec.table)) #crop to the extent of indiana & illinois 
  y <- data.frame(rasterToPoints(t)) #covert to dataframe
  colnames(y) <- c("x", "y", "prism30yr")
  #y$year <- i
  y$gridNumber <- cellFromXY(t, y[, 1:2])
  # write.csv( ) ?

spec.table<- read.csv("C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_fia_density_alb.csv")
  
write.csv(y, paste0("C:/Users/JMac/Documents/Kelly/biomodality/outputs/tmean_Prism_30yr.csv"))





