# investigating the LBDA
library(ncdf4)
library(raster)
library(sp)
library(ggplot2)

LBDA <- brick('C:/Users/Jmac/Box Sync/LBDA/nada_hd2_cl.nc')
LBDA <- nc_open('C:/Users/Jmac/Box Sync/LBDA/nada_hd2_cl.nc')
summary(LBDA$var)
LBDA.out <- list()
LBDA.out$lat <- ncvar_get(LBDA, "lat")
LBDA.out$lon <- ncvar_get(LBDA, "lon")
LBDA.out$Year <- ncvar_get(LBDA, "time")
# LBDA.out$pdsi <- ncvar_get(LBDA, "pdsi")
for(v in names(LBDA$var)){
  LBDA.out[[v]] <- ncvar_get(LBDA, v)  
}

PDSI <- LBDA.out$pdsi[1700:2006,138:1,1:237]

new.data <- aperm(PDSI, c(2,3,1)) # need to reorder array so it has x,y,pdsi

# lets make a raster brick of this
PDSI.brk<- brick(new.data)
names(PDSI.brk) <- 1700:2006
plot(PDSI.brk[[1]])
plot(PDSI.brk$X1936)
              
#now lets average drought over 1700:1800 and 1900:2000

spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb1.6-5.csv')
coordinates(spec.table) <- ~x +y
proj4string(spec.table) <- '+init=epsg:3175'
spec.table.ll<- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

t <- crop(s, extent(spec.table.ll)) 
s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
y <- data.frame(rasterToPoints(s)) #covert to dataframe
