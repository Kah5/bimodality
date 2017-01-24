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
#LBDA.out$pdsi <- ncvar_get(LBDA, "pdsi")
for(v in names(LBDA$var)){
  LBDA.out[[v]] <- ncvar_get(LBDA, v)  
}

PDSI <- LBDA.out$pdsi

new.data <- aperm(PDSI, c(2,3,1))

#test plot the LBDA
test<- raster(LBDA.out$pdsi[1850,138:1,1:237])
plot(test)
              
# lets make a raster brick of this
LBDAbrick<- brick(LBDA.out$pdsi[, 138:1, 1:237])
