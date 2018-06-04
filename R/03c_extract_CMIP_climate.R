# script for extracting data from some of the CMIP5 climate projections
#downscaled climate data downloaded from:

#Downscaled CMIP3 and CMIP5 Climate Projections: Release of Downscaled CMIP5
#Climate Projections, Comparison with Preceding Information, and Summary of
#User Needs.  U.S. Department of the Interior, Bureau of Reclamation, 104 p.,
#available at:
#  http://gdo-dcp.ucllnl.org/downscaled_cmip_projections/techmemo/downscaled_ climate.pdf.

# downloaded the mean and stdev of climate projections from 2050-2099, for multiple models

library(raster)
library(sp)
library(rgeos)
library(ggplot2)
library(reshape2)
library(ncdf4)
library(lubridate)

# This section uses the world clim CMIP projections:
#http://www.worldclim.org/CMIP5_2.5m

# for the initial tests of this, we use only the model projections from CCSM4
# we  will likely want to use those from a variety of different models

# This function extracts the rcps data from the Worldclim downscaled CMIP5 dataset
# This convertes to paleon grid scale, calculates the SI an d the total 

# get grid cells from lower and upper midwest so whe have the full extent:
lowmdw <- read.csv("outputs/density.table_test.csv")
ggplot(lowmdw, aes(x=x, y=y, fill = Oak))+geom_raster()

umdw <- read.csv('data/plss_density_alb_v0.9-10.csv')
ggplot(umdw, aes(x=x, y=y, fill = Oak))+geom_raster()

mdw <- rbind(lowmdw[,c("x", "y","cell", "Oak")], umdw[,c("x", "y","cell", "Oak")])
ggplot(mdw, aes(x=x, y=y, fill = Oak))+geom_raster()

extract.rcps<- function(climate, rcp){
  
      setwd(paste0('/Users/kah/Documents/bimodality/data/cc',rcp,climate,'70/'))
      spec.table <- read.csv('/Users/kah/Documents/bimodality/data/midwest_pls_full_density_pr_alb1.7-5.csv')
      coordinates(spec.table) <- ~x +y
      proj4string(spec.table) <- '+init=epsg:3175'
      spec.table.ll <- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))
      
      month <- sprintf("%02d", 1:12)
      month.abb <- c('Jan', 'Oct', 'Nov', "Dec","Feb","Mar","Apr", "May", 
                             'Jun', "Jul", "Aug", "Sep")
        filenames <- list.files(pattern=paste0("cc",rcp,climate,"70",".*\\.tif$", sep = ""))
        s <- stack(filenames)
        t <- crop(s, extent(spec.table.ll))#make all into a raster
        s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
        #crop to the extent of indiana & illinois 
        y <- data.frame(rasterToPoints(s)) #covert to dataframe
        
        colnames(y) <- c("x", "y", month.abb)
        y$gridNumber <- cellFromXY(s, y[, 1:2])
        #write.csv(y ,paste0('C:/Users/JMac/Documents/Kelly/biomodality/outputs/ccsm4_2.6_precip.csv' ))
        
      full <- y
      
      if(climate == 'pr'){
      full$total<- rowSums(full[,3:14], na.rm=TRUE)
      full$SI <- rowSums(abs(full[,3:14]-(full[,16]/12)))/full[,16]
      
       }else{
         full[,3:14] <- full[,3:14]/10
       full$mean <- rowMeans((full[,3:14]), na.rm = TRUE)
       mean.corr <- full$mean
        mean.corr[abs(mean.corr) < 0.8 ] <- 0.8 # assign all mean values near 0 to 0.8 to avoid the cv blowing up
       full$SI <- (abs(apply((full[,3:14]),1, sd, na.rm = TRUE))/abs((mean.corr)))
       full$cv <- (apply(full[,3:14],1, sd, na.rm = TRUE)/full[,15])*100
        }
      
      coordinates(full) <- ~x + y
      gridded(full) <- TRUE
      avgs <- stack(full) 
      
      
      #plot(avgs) #plots averages
      
      spec.table <- data.frame(spec.table)
      avgs.df <- data.frame(x = spec.table$x, y =spec.table$y)
      if(climate == "pr"){
      avgs.df$total <- extract(avgs$total, spec.table[,c("x","y")])
      avgs.df$SI <- extract(avgs$SI, spec.table[,c("x","y")])
      colnames(avgs.df) <- c('x', "y", paste0(climate,"-", rcp), paste0(climate,'-',rcp,'SI')) 
      }else{
        avgs.df$mean <- extract(avgs$mean, spec.table[,c("x","y")])
        avgs.df$SI <- extract(avgs$SI, spec.table[,c("x","y")])
        colnames(avgs.df) <- c('x', "y", paste0(climate,"-", rcp), paste0(climate,'-',rcp,'cv')) 
        
      }
avgs.df
}

# run this function for the different rcp scenarios (some of these may take along time):
pr.rcp26 <- extract.rcps("pr", "26")
pr.rcp45 <- extract.rcps("pr", "45")
pr.rcp60 <- extract.rcps("pr", "60")
pr.rcp85 <- extract.rcps("pr", "85")
t.rcp26 <- extract.rcps("tn", "26")
t.rcp45 <- extract.rcps("tn", "45")
t.rcp60 <- extract.rcps("tn", "60")
t.rcp85 <- extract.rcps("tn", "85")


avgs.df <- data.frame(x = pr.rcp26$x,
                      y = pr.rcp26$y, 
                      pr.rcp26[,3:4],
                      pr.rcp45[,3:4],
                      pr.rcp60[,3:4],
                      pr.rcp85[,3:4],
                      t.rcp26[,3:4],
                      t.rcp45[,3:4],
                      t.rcp60[,3:4],
                      t.rcp85[,3:4])

write.csv(avgs.df, "/Users/kah/Documents/bimodality/outputs/CCSM4pr_t_2070_full.csv")


# function to get RCP's over the whole domain:
full.rcps<- function(climate, rcp){
  
  setwd(paste0('/Users/kah/Documents/bimodality/data/cc',rcp,climate,'70/'))
  #spec.table<- read.csv('/Users/kah/Documents/bimodality/data/midwest_pls_full_density_pr_alb1.7-5.csv')
  lowmdw <- read.csv("/Users/kah/Documents/bimodality/outputs/density.table_test.csv")
  
  umdw <- read.csv('/Users/kah/Documents/bimodality/data/plss_density_alb_v0.9-10.csv')
  
  spec.table <- rbind(lowmdw[,c("x", "y","cell", "Oak")], umdw[,c("x", "y","cell", "Oak")])
  
  coordinates(spec.table) <- ~x +y
  proj4string(spec.table) <- '+init=epsg:3175'
  spec.table.ll<- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))
  
  month <- sprintf("%02d", 1:12)
  month.abb <- c('Jan', 'Oct', 'Nov', "Dec","Feb","Mar","Apr", "May", 
                 'Jun', "Jul", "Aug", "Sep")
  filenames <- list.files(pattern=paste0("cc",rcp,climate,"70",".*\\.tif$", sep = ""))
  s <- stack(filenames)
  t <- crop(s, extent(spec.table.ll))#make all into a raster
  s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
  #crop to the extent of indiana & illinois 
  y <- data.frame(rasterToPoints(s)) #covert to dataframe
  
  colnames(y) <- c("x", "y", month.abb)
  y$gridNumber <- cellFromXY(s, y[, 1:2])
  #write.csv(y ,paste0('C:/Users/JMac/Documents/Kelly/biomodality/outputs/ccsm4_2.6_precip.csv' ))
  
  full <- y
  
  if(climate == 'pr'){
    full$total<- rowSums(full[,3:14], na.rm=TRUE)
    full$SI <- rowSums(abs(full[,3:14]-(full[,16]/12)))/full[,16]
    
  }else{
    full[,3:14] <- full[,3:14]/10
    full$mean <- rowMeans((full[,3:14]), na.rm = TRUE)
    mean.corr <- full$mean
    mean.corr[abs(mean.corr) < 0.8 ] <- 0.8 # assign all mean values near 0 to 0.8 to avoid the cv blowing up
    full$SI <- (abs(apply((full[,3:14]),1, sd, na.rm = TRUE))/abs((mean.corr)))
    full$cv <- (apply(full[,3:14],1, sd, na.rm = TRUE)/full[,15])*100
  }
  
  coordinates(full) <- ~x + y
  gridded(full) <- TRUE
  avgs <- stack(full) 
  
  
  #plot(avgs) #plots averages
  
  spec.table <- data.frame(spec.table)
  
 
  rast.fun <- function(x) {
    
    to_grid <- data.frame(cell = x$cell, 
                          total = rowSums(x[,3:4], na.rm = TRUE))
    
    empty <- rep(NA, ncell(base.rast))
    empty[to_grid$cell] <- to_grid$total
    setValues(base.rast, empty)
    
  }
  
  full.spec.table     <- as.data.frame(rast.fun(spec.table), xy = TRUE)
  avgs.df<- data.frame(x = full.spec.table$x, y =full.spec.table$y)
   if(climate == "pr"){
    avgs.df$total <- extract(avgs$total, full.spec.table[,c("x","y")])
    avgs.df$SI <- extract(avgs$SI, full.spec.table[,c("x","y")])
    colnames(avgs.df) <- c('x', "y", paste0(climate,".", rcp), paste0(climate,'.',rcp,'SI')) 
  }else{
    avgs.df$mean <- extract(avgs$mean, full.spec.table[,c("x","y")])
    avgs.df$SI <- extract(avgs$SI, full.spec.table[,c("x","y")])
    colnames(avgs.df) <- c('x', "y", paste0(climate,".", rcp), paste0(climate,'.',rcp,'cv')) 
    
  }
  avgs.df
}

# run this function for the different rcp scenarios (some of these may take along time):
pr.rcp26 <- full.rcps("pr", "26")
pr.rcp45 <- full.rcps("pr", "45")
pr.rcp60 <- full.rcps("pr", "60")
pr.rcp85 <- full.rcps("pr", "85")
t.rcp26 <- full.rcps("tn", "26")
t.rcp45 <- full.rcps("tn", "45")
t.rcp60 <- full.rcps("tn", "60")
t.rcp85 <- full.rcps("tn", "85")



avgs.df <- data.frame(x = pr.rcp26$x,
                      y = pr.rcp26$y, 
                      pr.rcp26[,3:4],
                      pr.rcp45[,3:4],
                      pr.rcp60[,3:4],
                      pr.rcp85[,3:4],
                      t.rcp26[,3:4],
                      t.rcp45[,3:4],
                      t.rcp60[,3:4],
                      t.rcp85[,3:4])

write.csv(avgs.df, "/Users/kah/Documents/bimodality/outputs/CCSM4pr_t_2070_full.csv")

# test map to make sure it covers the domain of interest:
all_states <- map_data("state")
states <- subset(all_states, region %in% c("minnesota",'michigan',"wisconsin",   "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)

ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=pr.rcp26, aes(x=x, y=y, fill = pr.26))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")



# >>>>>>>>>>>>>>>>>>> extract full RCPs for tmean and get PET for future >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

full.rcps.long <- function(climate, rcp){
  
  setwd(paste0('/Users/kah/Documents/bimodality/data/cc',rcp,climate,'70/'))
  #spec.table<- read.csv('/Users/kah/Documents/bimodality/data/midwest_pls_full_density_pr_alb1.7-5.csv')
  lowmdw <- read.csv("/Users/kah/Documents/bimodality/outputs/density.table_test.csv")
  
  umdw <- read.csv('/Users/kah/Documents/bimodality/data/plss_density_alb_v0.9-10.csv')
  
  spec.table <- rbind(lowmdw[,c("x", "y","cell", "Oak")], umdw[,c("x", "y","cell", "Oak")])
  
  coordinates(spec.table) <- ~x +y
  proj4string(spec.table) <- '+init=epsg:3175'
  spec.table.ll<- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))
  
  month <- sprintf("%02d", 1:12)
  month.abb <- c('Jan', 'Oct', 'Nov', "Dec","Feb","Mar","Apr", "May", 
                 'Jun', "Jul", "Aug", "Sep")
  filenames <- list.files(pattern=paste0("cc",rcp,climate,"70",".*\\.tif$", sep = ""))
  s <- stack(filenames)
  t <- crop(s, extent(spec.table.ll))#make all into a raster
  s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
  #crop to the extent of indiana & illinois 
  y <- data.frame(rasterToPoints(s)) #covert to dataframe
  
  colnames(y) <- c("x", "y", month.abb)
  y$gridNumber <- cellFromXY(s, y[, 1:2])
  #write.csv(y ,paste0('C:/Users/JMac/Documents/Kelly/biomodality/outputs/ccsm4_2.6_precip.csv' ))
  
  full <- y
  
  if(climate == 'pr'){
    full$total<- rowSums(full[,3:14], na.rm=TRUE)
    full$SI <- rowSums(abs(full[,3:14]-(full[,16]/12)))/full[,16]
    
  }else{
    full[,3:14] <- full[,3:14]/10
    full$mean <- rowMeans((full[,3:14]), na.rm = TRUE)
    mean.corr <- full$mean
    mean.corr[abs(mean.corr) < 0.8 ] <- 0.8 # assign all mean values near 0 to 0.8 to avoid the cv blowing up
    full$SI <- (abs(apply((full[,3:14]),1, sd, na.rm = TRUE))/abs((mean.corr)))
    full$cv <- (apply(full[,3:14],1, sd, na.rm = TRUE)/full[,15])*100
  }
  
  coordinates(full) <- ~x + y
  gridded(full) <- TRUE
  avgs <- stack(full) 
  
  
  #plot(avgs) #plots averages
  
  spec.table <- data.frame(spec.table)
  
  
  rast.fun <- function(x) {
    
    to_grid <- data.frame(cell = x$cell, 
                          total = rowSums(x[,3:4], na.rm = TRUE))
    
    empty <- rep(NA, ncell(base.rast))
    empty[to_grid$cell] <- to_grid$total
    setValues(base.rast, empty)
    
  }
  
  full.spec.table     <- as.data.frame(rast.fun(spec.table), xy = TRUE)
  avgs.df<- data.frame(x = full.spec.table$x, y =full.spec.table$y)
  if(climate == "pr"){
    avgs.df$total <- extract(avgs$total, full.spec.table[,c("x","y")])
    avgs.df$SI <- extract(avgs$SI, full.spec.table[,c("x","y")])
    colnames(avgs.df) <- c('x', "y", paste0(climate,".", rcp), paste0(climate,'.',rcp,'SI')) 
  }else{
    avgs.df$mean <- extract(avgs$mean, full.spec.table[,c("x","y")])
    avgs.df$SI <- extract(avgs$SI, full.spec.table[,c("x","y")])
    colnames(avgs.df) <- c('x', "y", paste0(climate,".", rcp), paste0(climate,'.',rcp,'cv')) 
    
  }
  avgs.df
}

#####################################################################
# code below for alternative verisons of downscaled climate--
# deprecated
####################################################################

coordinates(full) <- ~x + y
gridded(full) <- TRUE
avgs <- stack(full) 
proj4string(avgs) <- crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 ')
avgs<- projectRaster(avgs, crs='+init=epsg:3175')
plot(avgs) #plots averages

# open the netcdf
nc <- nc_open("data/hydro5.tar/hydro5/hydro5/Extraction_pr.nc")
nc <- nc_open("data/Extraction_pr_hydro.nc")
lon <- ncvar_get(nc,"longitude")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(nc,"latitude",verbose=F)
nlat <- dim(lat)
head(lat)


t <- ncvar_get(nc,"time")
t
tunits <- ncatt_get(nc,"time","units")
nt <- dim(t)
nt

dname <- "pr"

tmp_array <- ncvar_get(nc,dname)
dlname <- ncatt_get(nc,dname,"long_name")
dunits <- ncatt_get(nc,dname,"units")
fillvalue <- ncatt_get(nc,dname,"_FillValue")
dim(tmp_array)

title <- ncatt_get(nc,0,"title")
institution <- ncatt_get(nc,0,"institution")
datasource <- ncatt_get(nc,0,"source")
references <- ncatt_get(nc,0,"references")
history <- ncatt_get(nc,0,"history")
Conventions <- ncatt_get(nc,0,"Conventions")





#nc <- nc_open(file.path(path.guess, "LPJ-GUESS_annual.nc"))
#summary(nc$var)

nc.out <- list()
nc.out$lat <- ncvar_get(nc, "latitude")
nc.out$lon <- ncvar_get(nc, "longitude")
nc.out$Time <- ncvar_get(nc, "time") 
nc.out$pr <- ncvar_get(nc, "pr")
#for(v in names(nc$var)){
#  nc.out[[v]] <- ncvar_get(nc, v)  
#}


as_date(nc.out$Time[2])
dim(nc.out$pr)


for(y in 1:dim(nc.out$pr)[2]){
  print(paste0(" ---- Lat: ", y, " ---- "))
  dat.temp <- stack(data.frame(nc.out$pr[,y,,1]))
  names(dat.temp) <- c("pr", "Year")
  dat.temp$Year <- as_date(nc.out$Time[y])
  dat.temp$lat  <- nc.out$lat[y]
  dat.temp$lon  <- nc.out$lon
  
  if(y==1) dat.pr <- dat.temp else dat.pr <- rbind(dat.pr, dat.temp)
}



#dat.pr <- dat.pr[complete.cases(dat.pr),]
summary(dat.pr)
#mean(dat.pr$NEE.yr, na.rm=T)
#max(dat.pr$NEE.yr, na.rm=T)

#mean(dat.pr$NEE, na.rm=T)
dat.pr$yr <- year(dat.pr$Year)
dat.pr$mo <- month(dat.pr$Year)
summary(dat.pr[dat.pr$yr == 2092,])
# Graphing
ggplot(data=dat.pr[dat.pr$yr == 2092,]) +
  #facet_grid(mo~.) +
  geom_raster(aes(x=lon, y=lat, fill=pr)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  ggtitle("test_pr") +
  coord_equal(ratio=1)
# -----------

# for some reason this extraction method is only resulting in years 2090-2100, not the full 2070-2099. 
# additionally, it is not extracting the full spatial domain





reg.cols <- read.csv("data/bcsd5/bcsd5/COLS_PeriodStat.txt", header = F)
reg.rows <- read.csv("data/bcsd5/bcsd5/ROWS_PeriodStat.txt", header = F)

#read in the projections for avg precipitation rate precipitation from ccsm4, rcp scenario 2.6 
ccsm4<- read.csv("data/bcsd5/bcsd5/pr_PeriodStat_mean.ccsm4.1.rcp26.csv", header = F)

extract.rcps<- function(climate,stat,rcp, cols, rows){
ccsm4<- read.csv(paste0("data/bcsd5/bcsd5/",climate,"_PeriodStat_",stat,".ccsm4.1.",rcp,".csv"), header = F)
  
colnames(ccsm4) <- t(reg.cols) # assign cols
rownames(ccsm4) <- t(reg.rows) # assign rows
ccsm4$lat <- rownames(ccsm4) 

melted <- melt(ccsm4, id.vars = "lat")
colnames(melted) <- c("lat", "lon", "pr")
melted$pr <- as.numeric(melted$pr)
melted$lon <- as.numeric(as.character(melted$lon))
melted$lat <- as.numeric(melted$lat)
melted$lon <- melted$lon - 360
#ggplot(melted, aes(lon, lat, fill = pr)) + geom_raster()
if(climate == 'pr'){
melted$pr <- melted$pr*365.25 # crudely converting avg precipitation/day to total precip/year
}else{
  melted$pr <- melted$pr
}
coordinates(melted) <- ~lon +lat
gridded(melted)<- TRUE

ccsm4 <- raster(melted) # rasterize

# assign projeciton to the raster (check this proj4string) 
# should be lambert conformal conic (i think)
#proj4string(ccsm4) <- '+init=epsg:3857' 
proj4string(ccsm4) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

#project raster to great lakes albers
#ccsm4 <- projectRaster(ccsm4, crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
ccsm4.26.alb <- projectRaster(ccsm4, crs = '+init=epsg:3175')


spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb1.6-5.csv')
spec.table <- data.frame(spec.table)
avgs.df<- data.frame(x = spec.table$x, y =spec.table$y)

avgs.df$pr <- extract(ccsm4.26.alb, spec.table[,c("x","y")])

colnames(avgs.df) <- c( "x", "y", paste0(climate,'-',rcp,'-',stat))
avgs.df

}

pr.rcp26.sd <- extract.rcps("pr","stdev", "rcp26", reg.cols, reg.rows)
pr.rcp26 <- extract.rcps("pr","mean", "rcp26", reg.cols, reg.rows)
pr.rcp45 <- extract.rcps("pr","mean", "rcp45", reg.cols, reg.rows)
pr.rcp85 <- extract.rcps("pr","mean", "rcp85", reg.cols, reg.rows)
tas.rcp26 <- extract.rcps("tas","mean", "rcp26", reg.cols, reg.rows)
tas.rcp45 <- extract.rcps("tas", "mean","rcp45", reg.cols, reg.rows)
tas.rcp85 <- extract.rcps("tas","mean", "rcp85", reg.cols, reg.rows)

tas.rcp26 <- extract.rcps("tas", "mean","rcp26", reg.cols, reg.rows)
tas.rcp45 <- extract.rcps("tas", "mean","rcp45", reg.cols, reg.rows)
tas.rcp85 <- extract.rcps("tas", "mean","rcp85", reg.cols, reg.rows)

avgs.df <- data.frame(x = pr.rcp26$x,
                      y = pr.rcp26$y, 
                      pr.rcp26 = pr.rcp26[,3],
                      pr.rcp45 = pr.rcp45[,3],
                      pr.rcp85 = pr.rcp85[,3],
                      tas.rcp26 = tas.rcp26[,3],
                      tas.rcp45 = tas.rcp45[,3],
                      tas.rcp85 = tas.rcp85[,3])

write.csv(avgs.df, 'C:/Users/JMac/Documents/Kelly/biomodality/data/ccsm4_pr_tas_preds_full.csv')

#read in temperature
##########################
#below code is deprecated
######################
#read in the projections for avg precipitation rate precipitation from ccsm4, rcp scenario 2.6 
ccsm4.26t<- read.csv("data/bcsd5/bcsd5/tas_PeriodStat_mean.ccsm4.1.rcp26.csv", header = F)
colnames(ccsm4.26t) <- t(reg.cols) # assign cols
rownames(ccsm4.26t) <- t(reg.rows) # assign rows
ccsm4.26t$lat <- rownames(ccsm4.26t) 

melted <- melt(ccsm4.26t, id.vars = "lat")
colnames(melted) <- c("lat", "lon", "tas")
melted$tas <- as.numeric(melted$tas)
melted$lon <- as.numeric(as.character(melted$lon))
melted$lat <- as.numeric(melted$lat)
melted$lon <- melted$lon - 360
ggplot(data = melted, aes(lon, lat, color = tas)) + geom_point()#+


#melted$tas <- melted$tas # crudely converting avg precipitation/day to total precip/year
coordinates(melted) <- ~lon +lat
gridded(melted)<- TRUE
ccsm4.26tas <- raster(melted) # rasterize

# assign projeciton to the raster (check this proj4string) 
# should be lambert conformal conic (i think)
proj4string(ccsm4.26tas) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

#project raster to great lakes albers
ccsm4.26t.alb <- projectRaster(ccsm4.26tas, crs = '+init=epsg:3175')

spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb1.6-5.csv')
spec.table <- data.frame(spec.table)

avgs.df$tas <- extract(ccsm4.26t.alb, spec.table[,c("x","y")])
#avgs.df$x <- spec.table$x
#avgs.df$y <- spec.table$y
colnames(avgs.df)[4] <-"tas_2070_rcp2.6" 
write.csv(avgs.df, 'C:/Users/JMac/Documents/Kelly/biomodality/data/ccsm4.26.alb_pr_tas_full.csv')


# read in sd for the seasonality of temperature

#read in the projections for avg precipitation rate precipitation from ccsm4, rcp scenario 2.6 
ccsm4.26tsd<- read.csv("data/bcsd5/bcsd5/tas_PeriodStat_stdev.ccsm4.1.rcp26.csv", header = F)
colnames(ccsm4.26tsd) <- t(reg.cols) # assign cols
rownames(ccsm4.26tsd) <- t(reg.rows) # assign rows
ccsm4.26tsd$lat <- rownames(ccsm4.26tsd) 

melted <- melt(ccsm4.26tsd, id.vars = "lat")
colnames(melted) <- c("lat", "lon", "tsd")
melted$tsd <- as.numeric(melted$tsd)
melted$lon <- as.numeric(as.character(melted$lon))
melted$lat <- as.numeric(melted$lat)
melted$lon <- melted$lon - 360
ggplot(data = melted, aes(lon, lat, color = tsd)) + geom_point()#+


#melted$tas <- melted$tas # crudely converting avg precipitation/day to total precip/year
coordinates(melted) <- ~lon +lat
gridded(melted)<- TRUE
ccsm4.26tsd <- raster(melted) # rasterize

# assign projeciton to the raster (check this proj4string) 
# should be lambert conformal conic (i think)
proj4string(ccsm4.26tsd) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

#project raster to great lakes albers
ccsm4.26tsd.alb <- projectRaster(ccsm4.26tsd, crs = '+init=epsg:3175')

spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb1.6-5.csv')
spec.table <- data.frame(spec.table)

avgs.df$tsd <- extract(ccsm4.26tsd.alb, spec.table[,c("x","y")])
#avgs.df$x <- spec.table$x
#avgs.df$y <- spec.table$y
colnames(avgs.df)[5] <-"tsd_2070_rcp2.6" 

avgs.df$tas_cv <- avgs.df$tsd_2070_rcp2.6/avgs.df$tas_2070_rcp2.6
write.csv(avgs.df, 'C:/Users/JMac/Documents/Kelly/biomodality/data/ccsm4.26.alb_pr_tas_full.csv')
