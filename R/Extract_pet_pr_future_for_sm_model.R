library(plyr)
library(raster)
library(data.table)
#ibrary(rgdal)
library(reshape2)
library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
library(tidyr)
library(dplyr)

#https://gdo-dcp.ucllnl.org/downscaled_cmip_projections/#Projections:%20Subset%20Request

version <- "1.7-5" # pls version

# set the working dir (where the prism data folder is)
workingdir <- "/Users/kah/Documents/bimodality/data/"
# for crc:
#workingdir <- "/afs/crc.nd.edu/user/k/kheilman/bimodality/data/"


# again read in the 8km grid for extracting
#spec.table <- read.csv(paste0(workingdir,'midwest_pls_full_density_alb',version,'.csv'))
#coordinates(spec.table) <- ~x + y
#proj4string(spec.table) <- '+init=epsg:3175' 
#spec.table.ll <- spTransform(spec.table, crs('+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0 '))

#spec.table.11 <- as.data.frame(spec.table.ll)
#spec.table.ll <- read.csv(paste0(workingdir, "spec.lat.long.csv"))

ccesm.pr <- stack("data/ccesm_2059_2099/Extraction_petnatveg.nc")
climate_output <- nc_open("data/ccesm_2059_2099/Extraction_petnatveg.nc")

lon <- ncvar_get(climate_output, varid = "longitude")
lat <- ncvar_get(climate_output, varid = "latitude")
lat <- ncvar_get(climate_output, varid = "latitude")
time <- ncvar_get(climate_output, varid = "time")
summary(lon)
summary(lat)
climate_output$dim$time$units
climate_output$dim$time$calendar

tas <- ncvar_get(climate_output, "petnatveg")
head(tas)
dim(tas)
#colnames(tas) <- lon

#tas.m <- as.data.frame(tas, )

tas.df<- as.data.frame(as.table(tas))
tas.df$lon <- factor(tas.df$Var1, labels=lon)
tas.df$lat <- factor(tas.df$Var2, labels=lat)
month <- rep(1:12,  40)
year <- rep(2059:2099, each = 12)
year_mo <- paste0(year, "_", month)
tas.df$year_mo <- factor(tas.df$Var3, labels = year_mo)

pet <- tas.df[,c("lon", "lat", "year_mo", "Freq")]
colnames(pet) <- c("lon", "lat", "year_mo", "PET")

pet.long <- dcast(pet, lon + lat ~ year_mo, fun.aggregate = mean, value.var='PET', na.rm = TRUE)
write.csv(pet.long, "outputs/cmip5_rcp8.5_pet_long.csv", row.names = FALSE)
nc_close("data/ccesm_2059_2099/Extraction_petnatveg.nc")



# get the precipitation data from nc files:
climate_output <- nc_open("data/ccesm_2059_2099/Extraction_pr.nc")

lon <- ncvar_get(climate_output, varid = "longitude")
lat <- ncvar_get(climate_output, varid = "latitude")
lat <- ncvar_get(climate_output, varid = "latitude")
time <- ncvar_get(climate_output, varid = "time")
#proj <- ncvar_get(climate_output, varid = "projection")
summary(lon)

climate_output$dim$time$units
climate_output$dim$time$calendar

prs <- ncvar_get(climate_output, "pr")
head(prs)
dim(prs)
colnames(prs) <- lon

#tas.m <- as.data.frame(tas, )

prs.df <- as.data.frame(as.table(prs))
prs.A.df <- prs.df[prs.df$Var4 %in% "A",]
prs.df$lon <- factor(prs.df$Var1, labels=lon)
prs.df$lat <- factor(prs.df$Var2, labels=lat)

month <- rep(1:12,  40)
year <- rep(2059:2099, each = 12)
year_mo <- paste0(year, "_", month)
prs.df$year_mo <- factor(prs.df$Var3, labels = year_mo)

pr <- prs.df[,c("lon", "lat", "year_mo", "Freq")]
colnames(pr) <- c("lon", "lat", "year_mo", "PR")

pr.long <- dcast(pr, lon + lat ~ year_mo, fun.aggregate = mean, value.var='PR', na.rm = TRUE)
write.csv(pr.long, "outputs/cmip5_rcp8.5_pr_long.csv", row.names = FALSE)
nc_close("data/ccesm_2059_2099/Extraction_pr.nc")


# calculate p-pet for the future climate data:
head(pr.long)
head(pet.long)
head(pr)
head(pet)
#pr.pet <- merge(pr, pet, by = c("lon", "lat", "year_mo"))
test <- pr
pr$PET <- pet$PET
pr$PPET <- pr$PR - pr$PET

ppet.long <- dcast(pr, lon + lat ~ year_mo, fun.aggregate = mean, value.var='PPET', na.rm = TRUE)
ggplot2::ggplot(ppet.long, aes(lon,lat, "2099_4"))+geom_raster()
write.csv(ppet.long, "outputs/cmip5_rcp8.5_ppet_long.csv", row.names = FALSE)
ppet.long <- read.csv("outputs/cmip5_rcp8.5_ppet_long.csv")
ppet.long$lon <- as.numeric(as.character(ppet.long$lon))
ppet.long$lat <- as.numeric(as.character(ppet.long$lat))

# convert to great lakes albers
coordinates(ppet.long) <- ~ lon + lat
gridded(ppet.long) <- TRUE
ppet.rast <- stack(ppet.long)
proj4string(ppet.rast) <-  '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'

ppet.rast.alb <- projectRaster(ppet.rast, crs='+init=epsg:3175') # project in great lakes albers

dens.pr <- read.csv("outputs/Future_PCA.csv")
dens.pr <- dens.pr[!is.na(dens.pr$awc),] 

ppet.rast.alb.df <- raster::extract(ppet.rast.alb, dens.pr[,c("x", "y")], df = TRUE)

ppet.rast.alb.df$x <- dens.pr$x
ppet.rast.alb.df$y <- dens.pr$y
ggplot(ppet.rast.alb.df, aes(x,y, fill = X2059_2))+geom_raster()


write.csv(ppet.rast.alb.df, "outputs/cmip5_rcp8.5_ppet_long.csv", row.names = FALSE)


# get ppet averaged for growing season over 2055-2099
GS_index <- colnames(ppet.rast.alb.df) %like% "_6" | colnames(ppet.rast.alb.df) %like% "_7"| colnames(ppet.rast.alb.df) %like% "_8" | colnames(ppet.rast.alb.df) %like% "_9"

ppet.rast.alb.df$mean_ppet_GS <- rowMeans(ppet.rast.alb.df[,GS_index], na.rm=TRUE)
write.csv(ppet.rast.alb.df, "outputs/cmip5_rcp8.5_ppet_long.csv", row.names = FALSE)






# --------------------------get lower rcp emission scenarios------------------------
ccesm.pr <- stack("data/ccesm_2059_2099_rcp2.6_4_6/Extraction_pr.nc")
climate_output <- nc_open("data/ccesm_2059_2099_rcp2.6_4_6/Extraction_petnatveg.nc")

lon <- ncvar_get(climate_output, varid = "longitude")
lat <- ncvar_get(climate_output, varid = "latitude")
lat <- ncvar_get(climate_output, varid = "latitude")
time <- ncvar_get(climate_output, varid = "time")
summary(lon)
summary(lat)
climate_output$dim$time$units
climate_output$dim$time$calendar

tas <- ncvar_get(climate_output, "petnatveg")
head(tas)
dim(tas)
#colnames(tas) <- lon

#tas.m <- as.data.frame(tas, )

tas.df <- as.data.frame(as.table(tas))
tas.df$lon <- factor(tas.df$Var1, labels=lon)
tas.df$lat <- factor(tas.df$Var2, labels=lat)
month <- rep(1:12,  40)
year <- rep(2059:2099, each = 12)
year_mo <- paste0(year, "_", month)
tas.df$year_mo <- factor(tas.df$Var3, labels = year_mo)
tas.df$rcp <- factor(tas.df$Var4, labels = c("rcp2.6", "rcp4.5", "rcp6.0", "rcp8.5"))

pet <- tas.df[,c("lon", "lat", "year_mo", "rcp", "Freq")]
colnames(pet) <- c("lon", "lat", "year_mo","rcp", "PET")

pet.long <- pet %>% group_by(lon, lat, rcp) %>% spread(key = year_mo, value = PET)
head(pet.long)

write.csv(pet.long, "outputs/cmip5_all_rcp_pet_long.csv", row.names = FALSE)
nc_close("data/ccesm_2059_2099_rcp2.6_4_6/Extraction_petnatveg.nc")

#pet.long <- read.csv("outputs/cmip5_rcp8.5_pet_long.csv")

# get the precipitation data from nc files:
climate_output <- nc_open("data/ccesm_2059_2099_rcp2.6_4_6/Extraction_pr.nc")

lon <- ncvar_get(climate_output, varid = "longitude")
lat <- ncvar_get(climate_output, varid = "latitude")
lat <- ncvar_get(climate_output, varid = "latitude")
time <- ncvar_get(climate_output, varid = "time")
#proj <- ncvar_get(climate_output, varid = "projection")
summary(lon)

climate_output$dim$time$units
climate_output$dim$time$calendar

prs <- ncvar_get(climate_output, "pr")
head(prs)
dim(prs)
colnames(prs) <- lon

#tas.m <- as.data.frame(tas, )

prs.df<- as.data.frame(as.table(prs))
prs.A.df<- prs.df[prs.df$Var4 %in% "A",]
prs.df$lon <- factor(prs.df$Var1, labels=lon)
prs.df$lat <- factor(prs.df$Var2, labels=lat)

month <- rep(1:12,  40)
year <- rep(2059:2099, each = 12)
year_mo <- paste0(year, "_", month)
prs.df$year_mo <- factor(prs.df$Var3, labels = year_mo)
prs.df$rcp <- factor(prs.df$Var4, labels = c("rcp2.6", "rcp4.5", "rcp6.0", "rcp8.5"))


pr <- prs.df[,c("lon", "lat", "year_mo", "rcp","Freq")]
colnames(pr) <- c("lon", "lat", "year_mo","rcp", "PR")


pr.long <- pr %>% group_by(lon, lat, rcp) %>% spread(key = year_mo, value = PR)
head(pr.long)


write.csv(pr.long, "outputs/cmip5_all_rcp_pr_long.csv", row.names = FALSE)
nc_close("data/ccesm_2059_2099_rcp2.6_4_6/Extraction_pr.nc")


# calculate p-pet for the future climate data:
head(pr.long)
head(pet.long)
head(pr)
head(pet)

#pr.arrange <- pr.long %>% dplyr::arrange(lon, lat, rcp, year_mo)

test <- pr
pr$PET <- pet$PET
pr$PPET <- pr$PR - pr$PET

library(tidyr)
library(dplyr)
pet <- pr[,c("lon", "lat", "year_mo", "rcp", "PPET")]

ppet.long <- pet %>% group_by(lon, lat, rcp) %>% spread(key = year_mo, value = PPET)

#ppet.long <- dcast(pr, lon + lat ~ year_mo, fun.aggregate = mean, value.var='PPET', na.rm = TRUE)
write.csv(ppet.long, "outputs/cmip5_all_rcp_ppet_long.csv", row.names = FALSE)


ppet.long <- read.csv("outputs/cmip5_all_rcp_ppet_long.csv")
ppet.long$lon <- as.numeric(as.character(ppet.long$lon))
ppet.long$lat <- as.numeric(as.character(ppet.long$lat))

# convert to great lakes albers

ggplot2::ggplot(data = ppet.long, aes(lon,lat, fill = `2064_9`))+geom_raster()+facet_wrap(~rcp)

# get ppet 2.6
ppet.long.26 <- data.frame(ppet.long[ppet.long$rcp %in% "rcp2.6",])
coordinates(ppet.long.26) <- ~ lon + lat
gridded(ppet.long.26) <- TRUE
ppet.rast <- stack(ppet.long.26)
proj4string(ppet.rast) <-  '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'

ppet.rast.alb <- projectRaster(ppet.rast, crs='+init=epsg:3175') # project in great lakes albers

dens.pr <- read.csv("outputs/Future_PCA.csv")
dens.pr <- dens.pr[!is.na(dens.pr$awc),] 

ppet.rast.alb.df <- raster::extract(ppet.rast.alb, dens.pr[,c("x", "y")], df = TRUE)

ppet.rast.alb.df$x <- dens.pr$x
ppet.rast.alb.df$y <- dens.pr$y
ggplot(ppet.rast.alb.df, aes(x,y, fill = X2059_2))+geom_raster()


write.csv(ppet.rast.alb.df, "outputs/cmip5_rcp2.6_ppet_long.csv", row.names = FALSE)


# get ppet averaged for growing season over 2055-2099
GS_index <- colnames(ppet.rast.alb.df) %like% "_6" | colnames(ppet.rast.alb.df) %like% "_7"| colnames(ppet.rast.alb.df) %like% "_8" | colnames(ppet.rast.alb.df) %like% "_9"

ppet.rast.alb.df$mean_ppet_GS <- rowMeans(ppet.rast.alb.df[,GS_index], na.rm=TRUE)
write.csv(ppet.rast.alb.df, "outputs/cmip5_rcp2.6_ppet_long.csv", row.names = FALSE)


# get ppet 4.5
ppet.long.45 <- data.frame(ppet.long[ppet.long$rcp %in% "rcp4.5",])
coordinates(ppet.long.45) <- ~ lon + lat
gridded(ppet.long.45) <- TRUE
ppet.rast <- stack(ppet.long.45)
proj4string(ppet.rast) <-  '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'

ppet.rast.alb <- projectRaster(ppet.rast, crs='+init=epsg:3175') # project in great lakes albers

dens.pr <- read.csv("outputs/Future_PCA.csv")
dens.pr <- dens.pr[!is.na(dens.pr$awc),] 

ppet.rast.alb.df <- raster::extract(ppet.rast.alb, dens.pr[,c("x", "y")], df = TRUE)

ppet.rast.alb.df$x <- dens.pr$x
ppet.rast.alb.df$y <- dens.pr$y
ggplot(ppet.rast.alb.df, aes(x,y, fill = X2059_2))+geom_raster()


write.csv(ppet.rast.alb.df, "outputs/cmip5_rcp4.5_ppet_long.csv", row.names = FALSE)


# get ppet averaged for growing season over 2055-2099
GS_index <- colnames(ppet.rast.alb.df) %like% "_6" | colnames(ppet.rast.alb.df) %like% "_7"| colnames(ppet.rast.alb.df) %like% "_8" | colnames(ppet.rast.alb.df) %like% "_9"

ppet.rast.alb.df$mean_ppet_GS <- rowMeans(ppet.rast.alb.df[,GS_index], na.rm=TRUE)
write.csv(ppet.rast.alb.df, "outputs/cmip5_rcp4.5_ppet_long.csv", row.names = FALSE)


# get ppet 6.0
ppet.long.60 <- data.frame(ppet.long[ppet.long$rcp %in% "rcp6.0",])
coordinates(ppet.long.60) <- ~ lon + lat
gridded(ppet.long.60) <- TRUE
ppet.rast <- stack(ppet.long.60)
proj4string(ppet.rast) <-  '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'

ppet.rast.alb <- projectRaster(ppet.rast, crs='+init=epsg:3175') # project in great lakes albers

dens.pr <- read.csv("outputs/Future_PCA.csv")
dens.pr <- dens.pr[!is.na(dens.pr$awc),] 

ppet.rast.alb.df <- raster::extract(ppet.rast.alb, dens.pr[,c("x", "y")], df = TRUE)

ppet.rast.alb.df$x <- dens.pr$x
ppet.rast.alb.df$y <- dens.pr$y
ggplot(ppet.rast.alb.df, aes(x,y, fill = X2059_2))+geom_raster()


write.csv(ppet.rast.alb.df, "outputs/cmip5_rcp6.0_ppet_long.csv", row.names = FALSE)


# get ppet averaged for growing season over 2055-2099
GS_index <- colnames(ppet.rast.alb.df) %like% "_6" | colnames(ppet.rast.alb.df) %like% "_7"| colnames(ppet.rast.alb.df) %like% "_8" | colnames(ppet.rast.alb.df) %like% "_9"

ppet.rast.alb.df$mean_ppet_GS <- rowMeans(ppet.rast.alb.df[,GS_index], na.rm=TRUE)
write.csv(ppet.rast.alb.df, "outputs/cmip5_rcp6.0_ppet_long.csv", row.names = FALSE)


# get 8.5:
# get ppet 6.0
ppet.long.85 <- data.frame(ppet.long[ppet.long$rcp %in% "rcp8.5",])
coordinates(ppet.long.85) <- ~ lon + lat
gridded(ppet.long.85) <- TRUE
ppet.rast <- stack(ppet.long.85)
proj4string(ppet.rast) <-  '+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0'

ppet.rast.alb <- projectRaster(ppet.rast, crs='+init=epsg:3175') # project in great lakes albers

dens.pr <- read.csv("outputs/Future_PCA.csv")
dens.pr <- dens.pr[!is.na(dens.pr$awc),] 

ppet.rast.alb.df <- raster::extract(ppet.rast.alb, dens.pr[,c("x", "y")], df = TRUE)

ppet.rast.alb.df$x <- dens.pr$x
ppet.rast.alb.df$y <- dens.pr$y
ggplot(ppet.rast.alb.df, aes(x,y, fill = X2059_2))+geom_raster()


write.csv(ppet.rast.alb.df, "outputs/cmip5_rcp8.5_ppet_long.csv", row.names = FALSE)


# get ppet averaged for growing season over 2055-2099
GS_index <- colnames(ppet.rast.alb.df) %like% "_6" | colnames(ppet.rast.alb.df) %like% "_7"| colnames(ppet.rast.alb.df) %like% "_8" | colnames(ppet.rast.alb.df) %like% "_9"

ppet.rast.alb.df$mean_ppet_GS <- rowMeans(ppet.rast.alb.df[,GS_index], na.rm=TRUE)
write.csv(ppet.rast.alb.df, "outputs/cmip5_rcp8.5_ppet_long.csv", row.names = FALSE)

