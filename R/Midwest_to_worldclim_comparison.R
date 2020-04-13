# downloading world clim data and comparing the midwest to the terrestrial biosphere
# 

library(raster)
library(tidyverse)
# download all the bioclimateib variabes at 2.5 minute resolution (takes awhile)
climate <- getData('worldclim', var='bio', res=10)

# example plot for the temperature
#plot(climate$bio1, main="Annual Mean Temperature")

# get the shapefile covering the uppermidwest
us <- getData('GADM', country='USA', level=1) 
#plot(us)

us.mw <- us[us$NAME_1 %in% c("Minnesota", "Michigan", "Indiana", "Wisconsin", "Illinois"),]
# get only the uppermidwest states:


#plot(us.mw)


# crop toe the size of the midwest 

mw.climate <- crop(climate, us.mw)
#plot(mw.climate$bio1)
mw.clim.mat <- rasterToPoints(mw.climate)
#summary(mw.climate)

#plot(mw.climate$bio1, mw.climate$bio12)

range(mw.clim.mat[,"bio1"])
range(mw.clim.mat[,"bio12"])



# find all other grid cells that fall within the same temperature and precipitation ranges:

climate.df <- rasterToPoints(climate)
#climate.uniq.df <- unique(climate.df)
climate.df2 <- data.frame(climate.df)
#climate.df.nona <- climate.df[!is.na(climate.df$bio1),]


# calculate P threshold
climate.df2$p.thresh <- 2*climate.df2$bio1/10

# probably an inefficent way of selecting, but trying it for now
library(tidyverse)


# make a table of climate classification scheme of the Koppen classifciation as modified by Trewartha (Finch et al. 1957, Trewartha 1968, Ahrens 1998)

Koppen.general<- data.frame(classification = c("Tropical", 
                   "Polar", 
                   "Moist continental",
                   "Moist subtropical",
                   "Dry"),
climate = c("Coldest Month >= 18degC", 
                "Warmest month <10degC", 
                "coldest month < - 3degC; warmest month >10 degC",
                "coldest month >-3degC, but <18degC; warmest month >10 degC", 
                "PET exceeds Precipitation, Mean annual precipitation < 10×Pthreshold"))

climate.df2$koppen <- ifelse(climate.df2$bio12 < 10*climate.df2$p.thresh,"Dry",
                             ifelse(climate.df2$bio6 >= 180, "Tropical",
                             ifelse(climate.df2$bio5 <=100, "Polar",
                                    ifelse(climate.df2$bio6 <= -30 & climate.df2$bio5 >100, "Continental",
                                           ifelse(climate.df2$bio6 > -30 & climate.df2$bio5 <=180, "Subtropical","Subtropical" )))))


ggplot(climate.df2, aes(x, y, fill = koppen))+geom_tile()

# find the temperate climate in general
# temperature in the coldest month is between -3deg C and 18 deg C
bio11.temperate <- climate.df2 %>% filter(koppen %in% c("Continental", "Subtropical"))

bio1.similar <- climate.df2 %>% filter(bio1 >= range(mw.clim.mat[,"bio1"])[1] & bio1 <= range(mw.clim.mat[,"bio1"])[2])
bio1.bio2.similar <- bio1.similar %>% filter(bio12 >= range(mw.clim.mat[,"bio12"])[1] & bio12 <= range(mw.clim.mat[,"bio12"])[2] & !koppen %in% "Polar")


# try ggplot to plot out where these are:
ggplot(bio1.bio2.similar, aes(x, y, fill = bio1))+geom_raster()

ggplot(bio1.bio2.similar, aes(bio1, bio12))+geom_point()


world_map <- map_data("world")
bio1.map <-ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill="lightgray", colour = "white")+
  geom_raster(data = bio1.bio2.similar, aes(x = x, y=y, fill = bio1/10))+theme_bw()+scale_fill_distiller(palette = "Spectral")

bio11.map <-ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill="lightgray", colour = "white")+
  geom_raster(data = bio6.temperate, aes(x = x, y=y, fill = bio11/10))+theme_bw()+scale_fill_distiller(palette = "Spectral")


bio12.map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill="lightgray", colour = "white")+
  geom_tile(data = bio1.bio2.similar, aes(x = x, y=y, fill = bio12))+theme_bw()+scale_fill_distiller(palette = "Spectral",direction = 1)


bio12.map.nopolar <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill="lightgray", colour = "white")+
  geom_tile(data = bio1.bio2.similar[bio1.bio2.similar$koppen %in% c("Continental", "Subtropical"),], aes(x = x, y=y, fill = bio12))+theme_bw()+scale_fill_distiller(palette = "Spectral",direction = 1)


koppen.map <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill="lightgray", colour = "white")+
  geom_tile(data = climate.df2, aes(x = x, y=y, fill = koppen))+theme_bw()+ scale_fill_manual(values =  c("Polar" = 'lightgray',
                                                                                                         "Tropical" = '#008837',
                                                                                                         "Subtropical" = '#a6dba0',
                                                                                                         "Continental" = '#7b3294', 
                                                                                                         "Dry"= '#c2a5cf'), na.translate = F)+ ylab("Latitude")+xlab("Longitude")#+scale_fill_distiller(palette = "Spectral",direction = 1)

koppen.map.mw.climate <- koppen.map + geom_point(data = bio1.bio2.similar, aes(x =x, y = y), size = 0.5)
  
  
png(height = 8, width = 4, units = "in", res = 300, "outputs/paper_figs_unc/climate.maps.png")
cowplot::plot_grid(bio1.map, bio12.map, ncol = 1, labels = "AUTO")
dev.off()

png(height = 8, width = 8, units = "in", res = 300, "outputs/paper_figs_unc/koppen.climate.maps.png")
cowplot::plot_grid(koppen.map, koppen.map.mw.climate, ncol = 1, labels = "AUTO")
dev.off()


nrow(climate.df)
nrow(bio1.bio2.similar)

(nrow(bio1.bio2.similar)/nrow(climate.df))*100
(nrow(bio1.bio2.similar)/nrow(bio11.temperate))*100

# BIO1 = Annual Mean Temperature
# BIO2 = Mean Diurnal Range (Mean of monthly (max temp – min temp))
# BIO3 = Isothermality (BIO2/BIO7) (* 100)
# BIO4 = Temperature Seasonality (standard deviation *100)
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO9 = Mean Temperature of Driest Quarter
# BIO10 = Mean Temperature of Warmest Quarter
# BIO11 = Mean Temperature of Coldest Quarter
# BIO12 = Annual Precipitation
# BIO13 = Precipitation of Wettest Month
# BIO14 = Precipitation of Driest Month
# BIO15 = Precipitation Seasonality (Coefficient of Variation)
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIO18 = Precipitation of Warmest Quarter
# BIO19 = Precipitation of Coldest Quarter
