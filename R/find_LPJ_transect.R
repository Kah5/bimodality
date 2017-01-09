#finding places where GUESS and PLS data transition (and are bimodal)
library(ggplot2)

LPJ.dens <- read.csv('C:/Users/Jmac/Box Sync/biomodality/data/LPJ-GUESS/LPJ-GUESS_annual_dens.csv')
dens.1850 <- LPJ.dens[LPJ.dens$Year %in% 1850,]
ggplot(dens.1850, aes(lon, lat, fill = Dens))+geom_raster() +coord_equal()+ facet_grid(PFT~.)

dens.1850 <- dens.1850[dens.1850$PFT %in% "Deciduous", ] # lets look at decidious only

#need to convert lat long to albers
coordinates(dens.1850) <- ~lon + lat
gridded(dens.1850) <- TRUE
#dens.1850 <- raster(dens.1850)
proj4string(dens.1850) <- CRS('+init=epsg:4326')
dens.1850.df <- data.frame(dens.1850)

PLS <- read.csv("outputs/PLS_full_bimodal_plsprbins75.csv")
coordinates(PLS) <- ~x + y
gridded(PLS) <- TRUE
proj4string(PLS) <- CRS('+init=epsg:3175')
PLS.ll <- spTransform(PLS, crs('+init=epsg:4326'))

PLS.ll.df <- data.frame(PLS.ll)

ggplot() + geom_raster(data = dens.1850.df, aes(x = lon, y = lat, fill = Dens)) + 
  geom_raster(data = PLS.ll.df, aes(x = x, y = y, fill = PLSdensity))
