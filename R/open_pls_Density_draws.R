# read in the density estimates + uncertainty:

library(sp)
library(raster)
library(ncdf4) 
library(reshape2)
library(tidyr)
library(dplyr)
library(ggplot2)

# new version! 6.3.19
pls.nc <- nc_open(filename = "data/PLS_density_western_v1.0rc1.nc") # data found here https://paleon.geography.wisc.edu/doku.php/data_and_products;settlement_vegetation_density

# data structure: x = 146, y = 180, sample = 250 MCMC samples
# has x, y, sample for each taxa and for Total density
# same structure for the pls.point.nc
# not sure exactly how these differ:

#pls.point.nc <- nc_open(filename = "data/PLS_density_western_point_v0.999.nc")


x <- ncvar_get(pls.nc, "x")
y <- ncvar_get(pls.nc, "y")
n <- ncvar_get(pls.nc, "sample")

total.array <- ncvar_get(pls.nc, "Total") # store the data in a 3-dimensional array
nc_close(pls.nc) # close the file

dim(total.array)# an x by y by sample array
rownames(total.array) <- x # assign column and row names to the array
colnames(total.array) <- y

# melt arry into a dataframe
total.m <- melt(total.array, varnames=c("x","y","sample"), as.is = TRUE)
# convert x + y to numerics
total.m$x <- as.numeric(total.m$x)
total.m$y <- as.numeric(total.m$y)

# write the extracted total density values to a csv:
write.csv(total.m, "data/extracted_total_PLS_density_draws.csv", row.names = FALSE)

# get a summary of density draws by grid cell: mean, & 95% CI
dens.summary <- total.m %>% group_by(x, y) %>% dplyr::summarize(mean_dens = mean(value, na.rm=TRUE),
                                                         median_dens = median(value, na.rm=TRUE),
                                        ci.low_dens = quantile(value, 0.025, na.rm=TRUE), 
                                        ci.high_dens = quantile(value, 0.975, na.rm=TRUE))
# get rid of the NA cells:
dens.summary <- dens.summary[!is.na(dens.summary$mean_dens),]

# just plot a basic map tree density:
ggplot(dens.summary, aes(x,y, fill =  mean_dens))+geom_raster()+ scale_fill_distiller(palette = "Spectral")

# make a prettier map of density with a discrete scale:

dens.summary$density_discrete <- ifelse(dens.summary$mean_dens <= 0.5, "Prairie", 
                                ifelse(dens.summary$mean_dens <= 47, "Savanna",
                                       ifelse(dens.summary$mean_dens > 47 & dens.summary$mean_dens <= 100, "47-100",
                                              ifelse(dens.summary$mean_dens > 100 & dens.summary$mean_dens <= 200, "100-200", 
                                                     ifelse(dens.summary$mean_dens > 200 & dens.summary$mean_dens <= 300, "200-300", 
                                                            ifelse(dens.summary$mean_dens > 300 & dens.summary$mean_dens <= 400, "300-400",
                                                                   ifelse(dens.summary$mean_dens > 400 & dens.summary$mean_dens <= 500, "400-500",
                                                                          ifelse(dens.summary$mean_dens > 500 , "500+", "No data"))))))))

dens.summary$density_discrete<- factor(dens.summary$density_discrete, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))


# need to set up state outlines:
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota', 'wisconsin', 'michigan', "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)



pls.map.alt.color <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.summary, aes(x=x, y=y, fill = density_discrete))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_manual(values = c('#dfc27d',
                               '#8c510a',
                               '#d9f0a3',
                               '#addd8e',
                               '#78c679',
                               '#41ab5d',
                               '#238443',
                               '#005a32',"darkgrey"), name ="Tree density", na.value = 'darkgrey', drop = F) +
  
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.3, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank())+ggtitle("")+coord_equal()


# save map to png
png(height = 4, width = 3, units = "in", res = 300,"/outputs/paper_figs_unc/PLS_smooth_dens_summary_map_full_alt_colors_v1.0.png")
pls.map.alt.color
dev.off()



# read in old estimates of density + climate data to see how well it matches the previous grid cell estimates:
dens.pr <- read.csv("data/PLS_FIA_density_climate_full.csv")

head(dens.summary)
head(dens.pr)

dens <- merge(dens.pr, dens.summary, by = c("x", "y"))


ggplot()+geom_errorbar(data= dens, aes(ymin = ci.low_dens, ymax = ci.high_dens), color = "grey")+
  geom_point(data = dens, aes(PLSdensity, mean_dens), size = 0.2)#+geom_abline(intercept = 0, slope = 1, color = "red")+theme_bw()


pred.old.plot <- ggplot(dens, aes(PLSdensity, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(PLSdensity, mean_dens), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+theme_bw()+ylab("Smoothed Density")+xlab("Previous grid cell density")

png(height = 5, width = 5, units = "in", res = 200, "outputs/chris_estimates_v1.0_vs_previous_estimates.png")
pred.old.plot
dev.off()



pc1_unc <- ggplot(dens, aes(PC1, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(PC1, mean_dens), color = "black", size = 0.5)+theme_bw()

ppet_unc <- ggplot(dens, aes(GS_ppet, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(GS_ppet, mean_dens), color = "black", size = 0.5)+theme_bw()


soil_unc <- ggplot(dens, aes(mean_GS_soil, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(mean_GS_soil, mean_dens), color = "black", size = 0.5)+theme_bw()


ggplot()+geom_histogram(data = dens, aes(mean_dens), fill = "red")+geom_histogram(data= dens, aes(ci.high_dens), aes = 2, color = "grey")+geom_histogram(data= dens, aes(ci.low_dens), alpha = 0.2, fill = "blue")


# plot difference maps of the mean or median and the pls density estimates:
dens$pls.diff <- dens$PLSdensity - dens$mean_dens
ggplot(dens, aes(x,y, fill = pls.diff))+geom_raster()+scale_fill_gradient2(midpoint = 0, limits = c(-300, 300))


dens$diff_discrete <- ifelse(dens$pls.diff >= -300 & dens$pls.diff < -200 , "-300--200",
                                  ifelse(dens$pls.diff >= -200 & dens$pls.diff < -100, "-200--100", 
                                        ifelse(dens$pls.diff >= -100 & dens$pls.diff < -50, "-100--50",
                                               ifelse(dens$pls.diff >= -50 & dens$pls.diff < 0, "-50-0",
                                                      ifelse(dens$pls.diff > 0 & dens$pls.diff <= 50, "0-50", 
                                                             ifelse(dens$pls.diff > 50 & dens$pls.diff <= 100, "50-100", 
                                                             ifelse(dens$pls.diff > 100 & dens$pls.diff <= 200, "200-300", 
                                                                    ifelse(dens$pls.diff > 200 & dens$pls.diff <= 300, "300-400",
                                                                           ifelse(dens$pls.diff > 300 & dens$pls.diff <= 400, "400-500",
                                                                                  ifelse(dens$pls.diff > 400 , "400+", "No data"))))))))))

dens$diff_discrete <- as.factor(dens$diff_discrete)
dens$diff_discrete <- factor(dens$diff_discrete, levels = c("-300--200","-200--100","-100--50","-50-0","0-50", "50-100", "100-200","200-300", "300-400", "400+","No data"))
pls.data.stat.diff <- ggplot(dens, aes(x, y, fill = diff_discrete)) + geom_raster() + scale_fill_manual(values = c("#a50026",
  "#d73027",
  "#f46d43",
  "#fdae61",
  "#fee090",
  "#e0f3f8",
  "#abd9e9",
  "#74add1",
  "#4575b4",
  "#313695", "darkgrey"), name ="data - mean est", na.value = 'darkgrey', drop = F)+theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                                                                                                  legend.key.size = unit(0.3, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                  
                                                                                                                  axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()
  

png(height= 6, width = 5, units = "in", res = 200, "outputs/paper_figs_unc/pls_data_minus_mean_stat_v1.0.png")
pls.data.stat.diff
dev.off()
# ------------------Read in FIA density draws-------------------

# read in the density estimates + uncertainty:

FIA.nc <- nc_open(filename = "data/FIA_density_v0.999.nc")

# data structure: x = 146, y = 180, sample = 250 MCMC samples
# has x, y, sample for each taxa and for Total density
# same structure for the FIA.point.nc
# not sure exactly how these differ:

FIA.point.nc <- nc_open(filename = "data/FIA_density_point_v0.999.nc")


x <- ncvar_get(FIA.nc, "x")
y <- ncvar_get(FIA.nc, "y")
n <- ncvar_get(FIA.nc, "sample")

total.array <- ncvar_get(FIA.nc, "Total") # store the data in a 3-dimensional array
nc_close(FIA.nc) # close the file

dim(total.array)# an x by y by sample array
rownames(total.array) <- x # assign column and row names to the array
colnames(total.array) <- y

# melt arry into a dataframe
total.m <- melt(total.array, varnames=c("x","y","sample"), as.is = TRUE)
# convert x + y to numerics
total.m$x <- as.numeric(total.m$x)
total.m$y <- as.numeric(total.m$y)

# write the extracted total density values to a csv:
write.csv(total.m, "data/extracted_total_FIA_density_draws.csv", row.names = FALSE)

# get a summary of density draws by grid cell: mean, & 95% CI
dens.summary <- total.m %>% group_by(x, y) %>% summarize(mean_dens_fia = mean(value, na.rm=TRUE),
                                                         median_dens_fia = median(value, na.rm=TRUE),
                                                         ci.low_dens_fia = quantile(value, 0.025, na.rm=TRUE), 
                                                         ci.high_dens_fia = quantile(value, 0.975, na.rm=TRUE))
# get rid of the NA cells:
dens.summary <- dens.summary[!is.na(dens.summary$mean_dens_fia),]

# just plot the raw data:
ggplot(dens.summary, aes(x,y, fill =  mean_dens_fia))+geom_raster()+ scale_fill_distiller(palette = "Spectral")

# make a prettier map of density:

dens.summary$density_discrete_fia <- ifelse(dens.summary$mean_dens_fia <= 0.5, "Prairie", 
                                        ifelse(dens.summary$mean_dens_fia <= 47, "Savanna",
                                               ifelse(dens.summary$mean_dens_fia > 47 & dens.summary$mean_dens_fia <= 100, "47-100",
                                                      ifelse(dens.summary$mean_dens_fia > 100 & dens.summary$mean_dens_fia <= 200, "100-200", 
                                                             ifelse(dens.summary$mean_dens_fia > 200 & dens.summary$mean_dens_fia <= 300, "200-300", 
                                                                    ifelse(dens.summary$mean_dens_fia > 300 & dens.summary$mean_dens_fia <= 400, "300-400",
                                                                           ifelse(dens.summary$mean_dens_fia > 400 & dens.summary$mean_dens_fia <= 500, "400-500",
                                                                                 ifelse(dens.summary$mean_dens_fia > 500 , "500+", "No data"))))))))

dens.summary$density_discrete_fia<- factor(dens.summary$density_discrete_fia, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))




FIA.map.alt.color <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.summary, aes(x=x, y=y, fill = density_discrete_fia))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_manual(values = c('#dfc27d',
                               '#8c510a',
                               '#d9f0a3',
                               '#addd8e',
                               '#78c679',
                               '#41ab5d',
                               '#238443',
                               '#005a32',"darkgrey"), name ="Tree density", na.value = 'darkgrey', drop = F) +
  
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.3, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


# save map to png
png(height = 4, width = 3, units = "in", res = 300,"outputs/paper_figs_unc/FIA_smooth_dens_summary_map_full_alt_colors.png")
FIA.map.alt.color
dev.off()



# read in old estimates of density + climate data to see how well it matches the previous grid cell estimates:
#dens.pr <- read.csv("data/PLS_FIA_density_climate_full.csv")

head(dens.summary)
head(dens)

dens <- merge(dens, dens.summary, by = c("x", "y"))


ggplot()+geom_errorbar(data= dens, aes(ymin = ci.low_dens_fia, ymax = ci.high_dens_fia), color = "grey")+
  geom_point(data = dens, aes(FIAdensity, mean_dens_fia), size = 0.2)#+geom_abline(intercept = 0, slope = 1, color = "red")+theme_bw()


pred.old.plot <- ggplot(dens, aes(FIAdensity, mean_dens_fia))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens_fia, ymax=ci.high_dens_fia), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(FIAdensity, mean_dens_fia), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+theme_bw()+ylab("Smoothed Density")+xlab("Previous grid cell density")

png(height = 5, width = 5, units = "in", res = 200, "outputs/chris_estimates_vs_previous_estimates.png")
pred.old.plot
dev.off()



pc1_unc_fia <- ggplot(dens, aes(PC1fia, mean_dens_fia))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens_fia, ymax=ci.high_dens_fia), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(PC1fia, mean_dens_fia), color = "black", size = 0.5)+theme_bw()

ppet_unc_fia <- ggplot(dens, aes(GS_ppet_mod, mean_dens_fia))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens_fia, ymax=ci.high_dens_fia), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(GS_ppet_mod, mean_dens_fia), color = "black", size = 0.5)+theme_bw()


soil_unc_fia <- ggplot(dens, aes(mean_GS_soil_m, mean_dens_fia))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens_fia, ymax=ci.high_dens_fia), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(mean_GS_soil_m, mean_dens_fia), color = "black", size = 0.5)+theme_bw()


ggplot()+geom_histogram(data = dens, aes(mean_dens_fia), fill = "red")+geom_histogram(data= dens, aes(ci.high_dens_fia), aes = 2, color = "grey")+geom_histogram(data= dens, aes(ci.low_dens_fia), alpha = 0.2, fill = "blue")

png("outputs/paper_figs_unc/smoothed_mean_fia_pls_hist.png")
ggplot()+geom_density(data = dens, aes(mean_dens), color = "blue", bw = 10)+geom_density(data = dens, aes(mean_dens_fia), color = "red",bw = 10)+xlab("Mean Density esimtates")
dev.off()


# plot difference maps of the mean or median and the fia density estimates:
dens$fia.diff <- dens$FIAdensity - dens$mean_dens_fia
ggplot(dens, aes(x,y, fill = fia.diff))+geom_raster()+scale_fill_gradient2(midpoint = 0, limits = c(-300, 300))

summary(dens$fia.diff)
dens$diff_discrete_fia <- ifelse(dens$fia.diff >= -300 & dens$fia.diff < -200 , "-300--200",
                             ifelse(dens$fia.diff >= -200 & dens$fia.diff < -100, "-200--100", 
                                    ifelse(dens$fia.diff >= -100 & dens$fia.diff < -50, "-100--50",
                                           ifelse(dens$fia.diff >= -50 & dens$fia.diff < 0, "-50-0",
                                                  ifelse(dens$fia.diff > 0 & dens$fia.diff <= 50, "0-50", 
                                                         ifelse(dens$fia.diff > 50 & dens$fia.diff <= 100, "50-100", 
                                                                ifelse(dens$fia.diff > 100 & dens$fia.diff <= 200, "200-300", 
                                                                       ifelse(dens$fia.diff > 200 & dens$fia.diff <= 300, "300-400",
                                                                              ifelse(dens$fia.diff > 300 & dens$fia.diff <= 400, "400-500",
                                                                                     ifelse(dens$fia.diff > 400 , "400+", "No data"))))))))))

dens$diff_discrete_fia <- as.factor(dens$diff_discrete_fia)
dens$diff_discrete_fia <- factor(dens$diff_discrete_fia, levels = c("-300--200","-200--100","-100--50","-50-0","0-50", "50-100", "100-200","200-300", "300-400", "400+","No data"))
fia.data.stat.diff <- ggplot(dens, aes(x, y, fill = diff_discrete_fia)) + geom_raster() + scale_fill_manual(values = c("#a50026",
                                                                                                                   "#d73027",
                                                                                                                   "#f46d43",
                                                                                                                   "#fdae61",
                                                                                                                   "#fee090",
                                                                                                                   "#e0f3f8",
                                                                                                                   "#abd9e9",
                                                                                                                   "#74add1",
                                                                                                                   "#4575b4",
                                                                                                                   "#313695", "darkgrey"), name ="data - mean est", na.value = 'darkgrey', drop = F)+theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                                                                                                                                                                                                                    legend.key.size = unit(0.3, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                                                                                                                                    
                                                                                                                                                                                                                                    axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png(height= 6, width = 5, units = "in", res = 200, "outputs/paper_figs_unc/fia_data_minus_mean_stat_fia.png")
fia.data.stat.diff
dev.off()


# read in total draws for PLS and FIA and make a histogram:

total.fia <- read.csv("data/extracted_total_FIA_density_draws.csv")
total.pls <- read.csv("data/extracted_total_PLS_density_draws.csv")

png(height= 6, width = 5, units = "in", res = 200, "outputs/paper_figs_unc/FIA_PLS_hist_all_draws.png")
ggplot()+geom_histogram(data = total.fia, aes(value),binwidth = 15, fill= "red", alpha = 0.75)+geom_histogram(data = total.pls, aes(value), binwidth = 15,fill= "blue", alpha = 0.75)
dev.off()


# same plot but with mean densities:

png(height= 6, width = 5, units = "in", res = 200, "outputs/paper_figs_unc/FIA_PLS_hist_mean_draws.png")
ggplot()+geom_histogram(data = dens, aes(mean_dens),binwidth = 15, fill= "red", alpha = 0.75)+geom_histogram(data = dens, aes(mean_dens_fia), binwidth = 15,fill= "blue", alpha = 0.75)
dev.off()


png(height= 6, width = 5, units = "in", res = 200, "outputs/paper_figs_unc/FIA_PLS_hist_median_draws.png")
ggplot()+geom_histogram(data = dens, aes(median_dens),binwidth = 15, fill= "red", alpha = 0.75)+geom_histogram(data = dens, aes(median_dens_fia), binwidth = 15,fill= "blue", alpha = 0.75)
dev.off()
