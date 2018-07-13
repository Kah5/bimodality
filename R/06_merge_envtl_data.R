# This script combines all the environmental data and joins with the the vegetation data
# read in temperature and precipitation data

library(tidyr)
library(dplyr)
setwd("/Users/kah/Documents/bimodality")
#precipitation, temperature, and temperuater are all extracted/calculated in R/crc03_Extract_Prism_historical.R.R
#outputs/pr_monthly_Prism_1985-1925_full.csv
past.precip.mo <- read.csv('data/pr_monthly_Prism_1895-1925_full.csv')

past.precip.mo$deltaP <- past.precip.mo$SI

mod.precip.mo <- read.csv('outputs/pr_monthly_Prism_30yrnorms_full.csv')

#read in modern precipitation seasonality:
#mod.precip.mo$moddeltaP <- rowSums(abs(mod.precip.mo[,2:13]-(mod.precip.mo[,14]/12)))/mod.precip.mo[,14]
mod.precip.mo <- mod.precip.mo[complete.cases(mod.precip.mo),]

#read in mean annual precipitaiton for modern and past
mod.precip <- read.csv('data/spec_table_30yr_prism_full.csv')
past.precip <- read.csv('data/pr_monthly_Prism_1895-1925_full.csv')

#read in mean annual temperature for modern and the past:
mod.tmean <- read.csv('outputs/tmean_monthly_Prism_30yrnorms_full.csv')
past.tmean <- read.csv('data/tmean_yr_Prism_1895-1925_full.csv')


mod.tmean.mo <- read.csv('outputs/tmean_monthly_Prism_30yrnorms_full.csv')
#rename temperature seasonality:
past.tmean$deltaT <- past.tmean$cv/100

mod.tmean.mo$moddeltaT <- mod.tmean.mo$cv/100 



# merge all the climate data together:
climate.data <- list(mod.precip[,c('x', 'y', 'pr30yr')], past.precip[,c('x', 'y', 'total')], 
     mod.precip.mo[,c('x', 'y', 'cv')], past.precip.mo[,c('x', 'y', 'deltaP')], mod.tmean[,c('x', 'y', 'Mean')],
     past.tmean[,c('x', 'y', 'Mean', 'deltaT')], mod.tmean.mo[,c('x', 'y', 'moddeltaT')]) %>% Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by=c("x", "y")), .)
colnames(climate.data) <- c("x", "y",  "MAP2011", "MAP1910","moderndeltaP", "pastdeltaP", "modtmean", "pasttmean", "deltaT", "moddeltaT")

ggplot(climate.data, aes(x, y, color=MAP1910))+geom_point()





# read in the P-PET data (generated from crc03_Extract_Prism_Historical.R)
#newf<- file.choose()

P.PET <- read.csv("outputs/P.PET_prism_1895_1925_Mar_Nov.csv")
climate.data <- merge(climate.data, P.PET[,c("x", "y", "GS_ppet")], by = c("x", "y"))


ggplot(climate.data, aes(x, y, fill=GS_ppet))+geom_raster()

write.csv(climate.data, paste0("data/midwest_climate_past_present_alb",version,".csv"))

# read in Modern PPET data:

P.PET.mod <- read.csv('/Users/kah/Documents/bimodality/outputs/P.PET_prism_modern_Mar_Nov.csv')
P.PET.mod<- P.PET.mod[,c("X", "x", "y", "Mar_ppet", "Apr_ppet", "May_ppet",
             "Jun_ppet", "Jul_ppet", "Aug_ppet", "Sep_ppet", "Oct_ppet","Nov_ppet", "GS_ppet")]
colnames(P.PET.mod) <- c("X", "x", "y", "Mar_ppet", "Apr_ppet", "May_ppet",
                         "Jun_ppet", "Jul_ppet", "Aug_ppet", "Sep_ppet", "Oct_ppet","Nov_ppet", "GS_ppet_mod")


climate.data <- merge(climate.data, P.PET.mod[,c("x", "y", "GS_ppet_mod")], by = c("x", "y"))
ggplot(climate.data, aes(x, y, fill=GS_ppet_mod))+geom_raster()

ggplot(climate.data, aes(GS_ppet, GS_ppet_mod))+geom_point()

# merge with soil moisture balance (calucated from P, PET and AWC):
moist_bal <- read.csv('outputs/soil.moisture_1895_1905_with_mean.csv')
climate.data <- merge(moist_bal[,c("x", "y", "Mean_GS")], climate.data, by = c("x", "y"))
colnames(climate.data)[3] <- "mean_GS_soil"
ggplot(moist_bal, aes(x,y, fill = Mean_GS)) + geom_raster()

# merge with modern soil moisture balance (calucated from P, PET and AWC)
moist_bal.m <- read.csv('outputs/soil.moisture_1999_2015_with_mean.csv')
#moist_bal.m <- read.csv("outputs/soil.moisture_end_of_mo_1985_2015.csv")
#ggplot(moist_bal, aes(x,y, fill = X2015_06)) + geom_raster()
climate.data <- merge(moist_bal.m[,c("x", "y", "Mean_GS")], climate.data, by = c("x", "y"))
colnames(climate.data)[3] <- "mean_GS_soil_m"
ggplot(climate.data, aes(x,y, fill = mean_GS_soil_m)) + geom_raster()
ggplot(climate.data, aes(x,y, fill = mean_GS_soil)) + geom_raster()

#----------------------------- Read in Soils Data -------------------------------

#read in soils data--soils data from gssurgo database, aggregated in ArcGIS
# percent sand 0-100cm soil
sand8km <- raster("data/8km_UMW_sand1.tif")

plot(sand8km)
# need to project sand to great lakes albers coordinate system
sand8km.alb <- projectRaster(sand8km, crs ='+init=epsg:3175')



plot(sand8km)
# need to project sand to great lakes albers coordinate system
sand8km.alb <- projectRaster(sand8km, crs ='+init=epsg:3175')

#awc-availible water content
awc8km <- raster("data/8km_UMW_awc1.tif")
awc8km.alb <- projectRaster(awc8km, crs ='+init=epsg:3175')

plot(awc8km)

# issues with ksat raster--missing all of IL
# read in the CEC raster
CEC <- raster("data/8km_UMW_CEC.tif")
CEC8km.alb <- projectRaster(CEC, crs ='+init=epsg:3175')

# read in CaCO3
caco3 <- raster("data/8_km_caco3.tif")
CaCO38km.alb <- projectRaster(caco3, crs ='+init=epsg:3175')

plot(CaCO38km.alb)

#write albers rasters to files:
writeRaster(awc8km.alb, "data/8km_UMW_awcalb.tif", overwrite = TRUE)
writeRaster(sand8km.alb, "data/8km_UMW_sandalb.tif", overwrite = TRUE)
writeRaster(CEC8km.alb, "data/8km_UMW_cecalb.tif", overwrite = TRUE)
writeRaster(CaCO38km.alb, "data/8km_UMW_CaCO3alb.tif", overwrite = TRUE)

#extract soils data using PLS points
climate.data$sandpct <- raster::extract(sand8km.alb, climate.data[,c('x', 'y')], method = 'bilinear')
climate.data$awc <- raster::extract(awc8km.alb, climate.data[,c('x', 'y')])
climate.data$ksat <- raster::extract(ksat8km.alb, climate.data[,c('x', 'y')])
climate.data$CEC <- raster::extract(CEC8km.alb, climate.data[,c('x','y')])
climate.data$CaCO3 <- raster::extract(CaCO38km.alb, climate.data[,c('x','y')])


# -----------------------Merge climate and vegetation data----------------------------------------------
#pls <- read.csv(paste0("data/midwest_pls_full_density_alb",version,".csv")) # pls density data
pls <- readRDS("data/cell_dens.RDS") # pls density data from PLS_products repo
head(pls)
#pls <- data.frame(pls)
colnames(pls) <- c("cell", "PLSdensity", "x", "y", "PLSdensity_adj")
pls <- pls[!is.na(pls$PLSdensity),]
fia <- read.csv(paste0("data/midwest_pls_fia_density_alb",version,".csv")) # fia density data

pls.clim <- merge(pls, climate.data, by = c("x", "y"), all.x = TRUE)
fia.clim <- merge(fia, climate.data, by = c("x", "y"), all.x = TRUE)

ggplot(pls.clim, aes(x,y, fill = sandpct))+geom_raster()

ggplot(pls.clim, aes(mean_GS_soil, PLSdensity, color = sandpct))+geom_point(size = 0.5)+ylim(0,650)

# ---------------------- Principal Component Analysis of Environmental Data ----------------------------

#------------------------PCA of environmental variables-------------------------------

dens.rm <- na.exclude(pls.clim[,c("x","y", "cell",'MAP1910', "MAP2011", "moderndeltaP", 
                                  "pastdeltaP", "modtmean", "pasttmean",
                                  "moddeltaT", "deltaT", "sandpct", "awc", "CEC", "CaCO3", "mean_GS_soil")])
dens.rm <- data.frame(dens.rm)
scale.dens <- scale(dens.rm[, c('MAP1910', "MAP2011", "moderndeltaP", 
                                "pastdeltaP", "modtmean", "pasttmean",
                                "moddeltaT", "deltaT", "sandpct", "awc", "CEC", "CaCO3")]) #PC all but ksat and diff

dens.dens <- pls.clim[pls.clim$cell %in% dens.rm$cell,]$PLSdensity # pls density

# apply PCA - scale. = TRUE 
dens.pca <- princomp(scale.dens[,c('MAP1910',   
                                   "pastdeltaP", "pasttmean",
                                   "deltaT", "sandpct", "awc", "CEC", "CaCO3")],
                     na.rm=TRUE) 

plot(dens.pca)
dens.pca$loadings
scores <- data.frame(dens.pca$scores[,1:2])
scores$PLS <- dens.dens
#scores$ecotype <- dens.rm$ecotype

dens.rm$PC1 <- scores[,1]
dens.rm$PC2 <- scores[,2]
dens.rm <- data.frame(dens.rm)
PC <- dens.pca

#plot scores by tree density in trees per hectare
png(paste0("outputs/pca_no_loadings.png"))
ggplot(na.omit(scores), aes(x = Comp.1, y = Comp.2, color = PLS)) +geom_point()+
  scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw()
dev.off()

data <- data.frame(obsnames=row.names(PC$scores[1]), PC$Comp.1)


plot(dens.pca, type = "l")
print(dens.pca)


# using ggbiplot
#library(ggbiplot)
source("R/newggbiplot.R")

pretty.scale <- scale.dens[,c('MAP1910',   
                              "pastdeltaP", "pasttmean",
                              "deltaT", "sandpct", "awc", "CEC", "CaCO3")]
colnames(pretty.scale) <- c("MAP", "PSI", "MAT", "TSI", "sand", "awc", "cec", "CaCO3")

pretty.pca <- princomp(pretty.scale,
                       na.rm=TRUE) 

g <- newggbiplot(na.omit(pretty.pca), obs.scale = 1, var.scale = 1, labels.size
                 = 25,alpha = 0,color = "blue",  alpha_arrow = 1, line.size = 1.5)

# layer the points from pls underneath the pca biplot
# using a clever trick to manipulate the layers
g$layers <- c(geom_point(data = na.omit(scores), aes(x = Comp.1, y = Comp.2, color = PLS)), g$layers)
g <- g + scale_color_gradientn(colours = rev(terrain.colors(8)), limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +theme_bw(base_size = 15) 

#write to png
png(width = 8, height = 4,unit="in", res=300, paste0("outputs/paper_figs/pca_biplot.png"))
g 
dev.off()





test1 <- merge(pls.clim, unique(dens.rm[,c('x','y', 'PC1', 'PC2')]),  by = c('x','y'), all.x= T)
#convert dens.rm to the new dens.pr---we only lose ~150 grid cells
dens.rm <- test1

png(width = 6, height = 6, units = "in", res = 300, "outputs/paper_figs/PPET_PC1_linear_relationship.png")
ggplot(dens.rm, aes(PC1, GS_ppet))+geom_point(size = 0.1) + stat_smooth(method = 'lm') + theme_bw(base_size = 20)+ylab("Growing Season P-PET (mm)")+xlab("Principal Component 1")
dev.off()

png(width = 6, height = 6, units = "in", res = 300, "outputs/paper_figs/PPET_PC1_linear_relationship.png")
ggplot(dens.rm, aes( GS_ppet, PLSdensity))+geom_point(size = 0.1) + stat_smooth(method = 'lm') + theme_bw(base_size = 20)+ylab("PLSdensity")+xlab("Growing Season P-PET (mm)")+ylim(0,1000)
dev.off()


# --------------------------------------PCA on FIA dataset------------------------------


# predict PCA scores for modern landscape
res <-princomp(scale.dens[,c('MAP1910',   
                             "pastdeltaP", "pasttmean",
                             "deltaT", "sandpct", "awc", "CEC", "CaCO3")])

#created a function to predict the PC scores for the different RCP's using the PCA from PLS
# scale modern environmental data
cc <- scale(dens.rm[,c("MAP2011", "moderndeltaP", 
                       "modtmean", "moddeltaT", 
                       "sandpct", "awc", "CEC", "CaCO3")])
# rename so that it is the same column names as PLS--for prediction purposes
colnames(cc) <- c('MAP1910',   
                  "pastdeltaP", "pasttmean",
                  "deltaT", "sandpct", "awc", "CEC", "CaCO3")

newscores <- predict(res, newdata=cc) # predict new scores based on the PLS pca 

#add to dens.pr dataframe
dens.rm[,paste0('PC1fia')] <- newscores[,1]
dens.rm[,paste0('PC2fia')]  <- newscores[,2]

dens.rm <- merge(dens.rm, fia.clim[,c("x", "y", "cell", "FIAdensity")], by = c("x", "y", "cell"), all.x = TRUE)
full.dens.pls <- merge(pls.clim[,c('x',"y","cell","PLSdensity")], fia.clim[,c("x","y","cell", "FIAdensity")], by = c("x","y","cell"),all.x = TRUE)
full.clim.dens <- merge(full.dens.pls, dens.rm[,c("x", "y", "cell","MAP1910", "MAP2011", "moderndeltaP", "modtmean", "moddeltaT", 
                                "sandpct", "awc", "CEC", "CaCO3","GS_ppet","GS_ppet_mod","mean_GS_soil","mean_GS_soil_m", "PC1", "PC2", "PC1fia", "PC2fia")], by = c("x", "y", "cell"), all.x = TRUE)

write.csv(full.clim.dens, "data/PLS_FIA_density_climate_full.csv")

dens.pr <- full.clim.dens

#--------------------------- Read in CMIP 4 projections----------------------
# CCESM climate projections extracted using the R/Extract_CMIP_climate.R
ccesm <- read.csv("/Users/kah/Documents/bimodality/outputs/CCSM4pr_t_2070_full.csv")

# create dataframe with density and all of the future climate valuesfor the whole region (not just those with PLS data)
future.pr <- merge(dens.pr, ccesm, by = c("x", "y"), all.y = TRUE)


# for each rcp, we need to determine the places outside of the range of PLS climate:
# function to find climate space outside of PLS range:
source("R/find_noanalog_cmip.R")


# find noanalog climates, label high or low
NArcp26 <- find.noanalog(clim=future.pr, rcp = "26")
NArcp45 <- find.noanalog(clim=future.pr, rcp = "45")
NArcp85 <- find.noanalog(clim=future.pr, rcp = "85")
NArcp60 <- find.noanalog(clim = future.pr, rcp = "60")

future.pr <- merge(future.pr, NArcp85, by = c("x", "y", "cell"))
future.pr <- merge(future.pr, NArcp60, by = c("x", "y", "cell"))
future.pr <- merge(future.pr, NArcp45, by = c("x", "y", "cell"))
future.pr <- merge(future.pr, NArcp26, by = c("x", "y", "cell"))


dens.pr <- merge(dens.pr, NArcp85, by = c("x", "y", "cell"))
dens.pr <- merge(dens.pr, NArcp60, by = c("x", "y", "cell"))
dens.pr <- merge(dens.pr, NArcp45, by = c("x", "y", "cell"))
dens.pr <- merge(dens.pr, NArcp26, by = c("x", "y", "cell"))

# maps for places where rcps predict no-analog climates for 2070:
a <- ggplot(dens.pr, aes(x,y, color = rcp26NA))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw()+coord_equal()+theme_bw() + ggtitle("RCP 2.6")
b <- ggplot(dens.pr, aes(x,y, color = rcp45NA))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw() + ggtitle("RCP 4.5")
c <- ggplot(dens.pr, aes(x,y, color = rcp60NA))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw() + ggtitle("RCP 6.0")
d <- ggplot(dens.pr, aes(x,y, fill = rcp85NA))+geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw() + ggtitle("RCP 8.5")

source("R/grid_arrange_shared_legend.R")

png(height = 4, width = 12, units = "in", res = 300, "outputs/v1.6-5/full/no-analog-ccsm4-climates.png")
grid_arrange_shared_legend(a,b,c,d, nrow = 1, ncol = 4)
dev.off()

# plot reasons for no-analog climate (exploratory):

a <- ggplot(dens.pr, aes(x,y, color = rcp26NAclim))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw()+coord_equal()+theme_bw() + ggtitle("RCP 2.6")
b <- ggplot(dens.pr, aes(x,y, color = rcp45NAclim))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw() + ggtitle("RCP 4.5")
c <- ggplot(dens.pr, aes(x,y, color = rcp60NAclim))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw() + ggtitle("RCP 6.0")
d <- ggplot(dens.pr, aes(x,y, color = rcp85NAclim))+geom_point()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  coord_equal()+theme_bw() + ggtitle("RCP 8.5")


png(height = 4, width = 12, units = "in", res = 300, "outputs/v1.6-5/full/no-analog-ccsm4-climates-reason.png")
grid_arrange_shared_legend(a,b,c,d, nrow = 1, ncol = 4)
dev.off()

# predict PCA with the diffrent CMIP projections:

res<-princomp(scale.dens[,c('MAP1910',   
                            "pastdeltaP", "pasttmean",
                            "deltaT", "sandpct", "awc", "CEC", "CaCO3")])

# need to merge sand pct, awc, CEC, and CaCO3 with the future climate data product:
sand8km <- raster("data/8km_UMW_sand1.tif")

plot(sand8km)
# need to project sand to great lakes albers coordinate system
sand8km.alb <- projectRaster(sand8km, crs ='+init=epsg:3175')

#awc-availible water content
awc8km <- raster("data/8km_UMW_awc1.tif")
awc8km.alb <- projectRaster(awc8km, crs ='+init=epsg:3175')

# issues with ksat raster--missing all of IL
# read in the CEC raster
CEC <- raster("data/8km_UMW_CEC.tif")
CEC8km.alb <- projectRaster(CEC, crs ='+init=epsg:3175')

# read in CaCO3
caco3 <- raster("data/8_km_caco3.tif")
CaCO38km.alb <- projectRaster(caco3, crs ='+init=epsg:3175')

plot(CaCO38km.alb)

#extract soils data using PLS points
future.pr$sandpct <- raster::extract(sand8km.alb, future.pr[,c('x', 'y')], method = 'bilinear')
future.pr$awc <- raster::extract(awc8km.alb, future.pr[,c('x', 'y')])
future.pr$CEC <- raster::extract(CEC8km.alb, future.pr[,c('x','y')])
future.pr$CaCO3 <- raster::extract(CaCO38km.alb, future.pr[,c('x','y')])

#created a function to predict the PC scores for the different RCP's using the PCA from PLS
predict.PCA <- function(rcp){
  cc <- scale(future.pr[,c(paste0("pr.",rcp), 
                           paste0("pr.",rcp,"SI"), 
                           paste0("tn.",rcp),
                           paste0("tn.",rcp, "cv"),
                           "sandpct","awc", "CEC", "CaCO3")])
  colnames(cc) <- c('MAP1910',   
                    "pastdeltaP", "pasttmean",
                    "deltaT", "sandpct", "awc", "CEC", "CaCO3")
  
  newscores <- predict(res,newdata=cc) # predict new scores based on the prevous 
  
  future.pr[,paste0('PC1_cc',rcp)] <- newscores[,1]
  future.pr[,paste0('PC2_cc',rcp)]  <- newscores[,2]
  future.pr
}

future.pr <- predict.PCA("26")
future.pr <- predict.PCA("45")
future.pr <- predict.PCA("60")
future.pr <- predict.PCA("85")


moist_bal.future <- read.csv('outputs/soil.moisture_2059_2099_rcp8.5_with_mean.csv')
ggplot(moist_bal.future, aes(x,y, fill = Mean_GS_post_spin))+geom_raster()

future.pr2<- merge(future.pr, moist_bal.future[,c("x", "y", "Mean_GS", "Mean_GS_post_spin")])
colnames(future.pr2)[57:58] <- c("mean_GS_soil_8.5", "mean_GS_soil_8.5_post_spin")

ppet.future <- read.csv("outputs/cmip5_rcp8.5_ppet_long.csv")
ggplot(ppet.future, aes(x,y, fill = mean_ppet_GS))+geom_raster()
future.pr2 <- merge(future.pr2, ppet.future[,c("x", "y", "mean_ppet_GS")])

write.csv(future.pr2, "outputs/Future_PCA.csv",row.names = FALSE)



# edit: merging environmental data and the count data for composition modeling:
counts <- read.csv("/Users/kah/Documents/PLS_products/PLS_species_counts.csv")
head(full.clim.dens)
counts.df <- merge(counts, full.clim.dens[,c("x", "y", "cell", "PC1")], by = c("x", "y", "cell"))
write.csv(counts.df, "/Users/kah/Documents/PLS_products/PLS_species_counts_pc1.csv", row.names = FALSE)
