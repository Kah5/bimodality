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

# open the netcdf
nc <- nc_open("data/hydro5.tar/hydro5/hydro5/Extraction_pr.nc")
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
  dat.temp <- stack(data.frame(nc.out$pr[,y,,]))
  names(dat.temp) <- c("NEE", "Year")
  dat.temp$Year <- as_date(nc.out$Time[y])
  dat.temp$lat  <- nc.out$lat[y]
  dat.temp$lon  <- nc.out$lon
  
  if(y==1) dat.pr <- dat.temp else dat.pr <- rbind(dat.pr, dat.temp)
}

#dat.pr$ <- dat.pr$NEE*sec2yr
summary(dat.pr)
mean(dat.pr$NEE.yr, na.rm=T)
max(dat.pr$NEE.yr, na.rm=T)

mean(dat.pr$NEE, na.rm=T)

summary(dat.pr[dat.pr$Year==2009,])
# Graphing
ggplot(data=dat.pr[dat.pr$Year %in% c(850, 1850, 2000),]) +
  facet_grid(Year~.) +
  geom_raster(aes(x=lon, y=lat, fill=NEE.yr)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  ggtitle("LPJ-GUESS") +
  coord_equal(ratio=1)
# -----------





reg.cols <- read.csv("data/bcsd5/bcsd5/COLS_PeriodStat.txt", header = F)
reg.rows <- read.csv("data/bcsd5/bcsd5/ROWS_PeriodStat.txt", header = F)

#read in the projections for avg precipitation rate precipitation from ccsm4, rcp scenario 2.6 
ccsm4.26pr<- read.csv("data/bcsd5/bcsd5/pr_PeriodStat_mean.ccsm4.1.rcp26.csv", header = F)
colnames(ccsm4.26pr) <- t(reg.cols) # assign cols
rownames(ccsm4.26pr) <- t(reg.rows) # assign rows
ccsm4.26pr$lat <- rownames(ccsm4.26pr) 

melted <- melt(ccsm4.26pr, id.vars = "lat")
colnames(melted) <- c("lat", "lon", "pr")
melted$pr <- as.numeric(melted$pr)
melted$lon <- as.numeric(as.character(melted$lon))
melted$lat <- as.numeric(melted$lat)
melted$lon <- melted$lon - 360
ggplot(melted, aes(lon, lat, fill = pr)) + geom_raster()

melted$pr <- melted$pr*365.25 # crudely converting avg precipitation/day to total precip/year
coordinates(melted) <- ~lon +lat
gridded(melted)<- TRUE

ccsm4.26pr <- raster(melted) # rasterize

# assign projeciton to the raster (check this proj4string) 
# should be lambert conformal conic (i think)
#proj4string(ccsm4.26pr) <- '+init=epsg:3857' 
proj4string(ccsm4.26pr) <- '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' 

#project raster to great lakes albers
#ccsm4.26pr <- projectRaster(ccsm4.26pr, crs = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
ccsm4.26.alb <- projectRaster(ccsm4.26pr, crs = '+init=epsg:3175')


spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb1.6-5.csv')
spec.table <- data.frame(spec.table)

avgs.df <- data.frame(extract(ccsm4.26.alb, spec.table[,c("x","y")]))
avgs.df$x <- spec.table$x
avgs.df$y <- spec.table$y
colnames(avgs.df) <- c('pr_2070_rcp2.6', "x", "y")
write.csv(avgs.df, 'C:/Users/JMac/Documents/Kelly/biomodality/data/ccsm4.26.alb_pr_full.csv')

#read in temperature

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

avgs.df$tas <- data.frame(extract(ccsm4.26t.alb, spec.table[,c("x","y")]))
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
