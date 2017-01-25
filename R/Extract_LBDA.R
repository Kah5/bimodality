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
LBDA.out$pdsi <- ncvar_get(LBDA, "pdsi")
#for(v in names(LBDA$var)){
 # LBDA.out[[v]] <- ncvar_get(LBDA, v)  
#}

PDSI <- LBDA.out$pdsi[1700:2006,138:1,1:237]

new.data <- aperm(PDSI, c(2,3,1)) # need to reorder array so it has x,y,pdsi

# lets make a raster brick of this
PDSI.brk<- brick(new.data)
names(PDSI.brk) <- 1700:2006
plot(PDSI.brk[[1]])
plot(PDSI.brk$X1936)
              
for(y in 1:dim(LBDA.out$pdsi)[3]){
  print(paste0(" ---- Lat: ", y, " ---- "))
  dat.temp <- stack(data.frame(LBDA.out$pdsi[,y,]))
  names(dat.temp) <- c("pdsi", "Year")
  dat.temp$Year <- as.numeric(substr(dat.temp$Year,2,nchar(paste(dat.temp$Year))))
  dat.temp$lat  <- LBDA.out$lat[y]
  dat.temp$lon  <- LBDA.out$lon
  
  if(y==1) lbda.pdsi <- dat.temp else lbda.pdsi <- rbind(lbda.pdsi, dat.temp)
}

ggplot(data=lbda.pdsi[lbda.pdsi$Year %in% c(1890,2006),]) +
  facet_grid(Year~.) +
  geom_raster(aes(x=lon, y=lat, fill=pdsi)) +
  scale_y_continuous(name="Latitude", expand=c(0,0)) +
  scale_x_continuous(name="Longitude", expand=c(0,0)) +
  ggtitle("LPJ-wsl") +
  coord_equal(ratio=1)


spec.table <- read.csv('C:/Users/JMac/Documents/Kelly/biomodality/data/midwest_pls_full_density_pr_alb1.6-5.csv')
coordinates(spec.table) <- ~x +y
proj4string(spec.table) <- '+init=epsg:3175'
spec.table.ll<- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

t <- crop(s, extent(spec.table.ll)) 
s <- projectRaster(t, crs='+init=epsg:3175') # project in great lakes albers
y <- data.frame(rasterToPoints(s)) #covert to dataframe
