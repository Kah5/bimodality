# finding the overall environmental space where the bimodality occures
# the script uses output from density_species_analysis.R 
library(modes)
library(ggplot2)
library(diptest)
library(grid)
library(gridExtra)
library(cowplot)

# Objective: calculate bimodality on a rolling bin basis and narrow in on the environmental space where data is signifcantly bimodal

full <- read.csv("outputs/cluster/full_comp_dens_df.csv")

dens.pr <- read.csv("outputs/v1.6-5/full/dens_pr_dataframe_full.csv")
new.pcs <- dens.pr[,c("x","y", "cell", "PC1", "PC2", "MAP1910", "MAP2011", "moderndeltaP", "pastdeltaP", "modtmean", "pasttmean", "deltaT", "moddeltaT", "sandpct", "awc", "ksat", "CEC","GS_ppet", "PLSdensity")]

pls.full <- full[full$period %in% "PLS",]

pls.full <- merge(pls.full[,1:47], new.pcs )

#rolling BC Function (this only works if the data are ordered by the environment)


rollBC_r <-  function(x, y, xout, width, df) { # x and y are the environment val and the density/comp that we want to determin bimodality with
  
  out <- data.frame(xout = xout,
                    bc = NA,
                    pval = NA,
                    n = NA)
  
  for( i in seq_along(xout) ){
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i,]$bc <-  bimodality_coefficient( na.omit(y[ window ] ) )# what is the BC for places with less than 300 trees per hectare
    out[i,]$pval <- ifelse(length(na.omit(y[window])) > 2,diptest::dip.test(na.omit(density(na.omit(y[window]))$y))$p, NA)
    out[i,]$n <- length(na.omit( y[ window ]))
  }
  
  df2 <- merge(df, out, by.x = "PC1", by.y = "xout")
  df2$bimodal <- ifelse(df2$bc >= 0.55 & df2$pval < 0.05, "Bimodal", "Stable")
  bim2 <- ifelse(y == ordered$Density, "Density", "species pc2")
  
  
  ggplot(df2, aes(x= PC1, y = n, color = bimodal))+geom_point()+ggtitle(paste0(bim2, " samples binwidth = ", width))
}



ordered <- pls.full[order(pls.full$PC1),]
ordered$rownum <- 1:length(ordered$PC1)

a <- rollBC_r(x = ordered$PC1, y = ordered$PLSdensity, xout = ordered$PC1, width = 1, df = ordered)
b <- rollBC_r(x = ordered$PC1, y = ordered$PLSdensity, xout = ordered$PC1, width = 0.5, df = ordered)
c <- rollBC_r(x = ordered$PC1, y = ordered$PLSdensity, xout = ordered$PC1, width = 0.25, df= ordered)
d <- rollBC_r(x = ordered$PC1, y = ordered$PLSdensity, xout = ordered$PC1, width = 0.15, df= ordered)

#ordered <- pls.full[order(pls.full$PC1),]
#ordered$rownum <- 1:length(ordered$PC1)

e <- rollBC_r(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 1, df = ordered)
f <- rollBC_r(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.5, df = ordered)
g <- rollBC_r(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.25, df = ordered)
h <- rollBC_r(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.15, df = ordered)

png(height = 12, width = 7, units = "in", res = 200, "outputs/cluster/nsamples_rolling_bimodality_pca_new.png")
grid.arrange(a,e,b,f,c,g,d,h, ncol = 2, nrow=4)
dev.off()


# make a function that maps out the bimodal area and saves the bimodal files as a csv:

rollBC_map = function(x, y, xout, width, df, bim.df) { # x and y are the environment val and the density/comp that we want to determin bimodality with
  
    out <- data.frame(xout = xout,
                      bc = NA,
                      pval = NA,
                      n = NA)
    
    for( i in seq_along(xout) ){
      window = x >= (xout[i]-width) & x <= (xout[i]+width)
      out[i,]$bc <-  bimodality_coefficient( na.omit(y[ window ] ) )# what is the BC for places with less than 300 trees per hectare
      out[i,]$pval <- ifelse(length(na.omit(y[window])) > 2, diptest::dip.test(na.omit(density(na.omit(y[window]))$y))$p, NA)
      out[i,]$n <- length(na.omit( y[ window ]))
    }
    
     df2 <- merge(df, out, by.x = "PC1", by.y = "xout")
     #df2[df2$bc == "NaN", ]$bc <- 0
     df2$bimodal <- ifelse(df2$bc >= 0.55 & df2$pval < 0.05, "Bimodal", "Unimodal")
     bim2 <- ifelse(y == ordered$Density, "Density", "species pc2")
     # write df2 to a csv file to work with later
     write.csv(df2, paste0("outputs/cluster/bimodal_widths/", bim.df, "_width_", width, ".csv"), row.names = FALSE)
     
    
     
     # read in state boundaries for plotting:
     all_states <- map_data("state")
     states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
     coordinates(states)<-~long+lat
     class(states)
     proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
     mapdata<-spTransform(states, CRS('+init=epsg:3175'))
     mapdata <- data.frame(mapdata)
     
    
     bim <-ggplot(df2, aes(x = x, y = y, fill = bimodal ))+geom_raster()+coord_equal()+
       geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                      axis.title.x=element_blank(),
                                                                                                      axis.title.y=element_blank())+ggtitle(paste0("bimodality ", bim2, " width =", width))
     
     samp <- ggplot(df2, aes(x = x, y = y, fill = n ))+geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+coord_equal()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                          axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                          axis.title.x=element_blank(),
                                                                                          axis.title.y=element_blank())+ggtitle(paste0("N samples ", bim2, " width =", width))
     #samp2 <- ggplot(df2, aes(x= PC1, y = n, color = bimodal))+geom_point()
      plot_grid(bim, samp, ncol = 2, nrow = 1, align = 'hv')
      
      }

a <- rollBC_map(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 1, df = ordered, bim.df = "Comp_Bimodal")
b <- rollBC_map(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.5, df = ordered, bim.df = "Comp_Bimodal")
c <- rollBC_map(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.25, df = ordered, bim.df = "Comp_Bimodal")
d <- rollBC_map(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.15, df = ordered, bim.df = "Comp_Bimodal")

# density: 
e <- rollBC_map(x = ordered$PC1, y = ordered$PLSdensity, xout = ordered$PC1, width = 1, df = ordered, bim.df = "Dens_Bimodal")
f <- rollBC_map(x = ordered$PC1, y = ordered$PLSdensity, xout = ordered$PC1, width = 0.5, df = ordered, bim.df = "Dens_Bimodal")
g <- rollBC_map(x = ordered$PC1, y = ordered$PLSdensity, xout = ordered$PC1, width = 0.25, df = ordered, bim.df = "Dens_Bimodal")
h <- rollBC_map(x = ordered$PC1, y = ordered$PLSdensity, xout = ordered$PC1, width = 0.15, df = ordered, bim.df = "Dens_Bimodal")


i <- rollBC_map(x = ordered$PC1, y = ordered$Oak, xout = ordered$PC1, width = 1, df = ordered, bim.df = "Oak_Bimodal")
j <- rollBC_map(x = ordered$PC1, y = ordered$Oak, xout = ordered$PC1, width = 0.5, df = ordered, bim.df = "Oak_Bimodal")
k <- rollBC_map(x = ordered$PC1, y = ordered$Oak, xout = ordered$PC1, width = 0.25, df = ordered, bim.df = "Oak_Bimodal")
l <- rollBC_map(x = ordered$PC1, y = ordered$Oak, xout = ordered$PC1, width = 0.15, df = ordered, bim.df = "Oak_Bimodal")

m <- rollBC_map(x = ordered$PC1, y = ordered$Hemlock, xout = ordered$PC1, width = 1, df = ordered, bim.df = "Hemlock_Bimodal")
n <- rollBC_map(x = ordered$PC1, y = ordered$Hemlock, xout = ordered$PC1, width = 0.5, df = ordered, bim.df = "Hemlock_Bimodal")
o <- rollBC_map(x = ordered$PC1, y = ordered$Hemlock, xout = ordered$PC1, width = 0.25, df = ordered, bim.df = "Hemlock_Bimodal")
p <- rollBC_map(x = ordered$PC1, y = ordered$Hemlock, xout = ordered$PC1, width = 0.15, df = ordered, bim.df = "Hemlock_Bimodal")



png(width = 8, height = 11, units = "in", res = 300, "outputs/cluster/rolling_bimodal_maps_species_pc2_pls_pcanew.png")
grid.arrange(a,b,c,d, nrow=4, ncol = 1)
dev.off()

png(width = 8, height = 11, units = "in", res = 300, "outputs/cluster/rolling_bimodal_maps_density_pls_pcanew.png")
grid.arrange(e,f,g,h, nrow=4, ncol = 1)
dev.off()

png(width = 8, height = 11, units = "in", res = 300, "outputs/cluster/rolling_bimodal_maps_Oak_pls_pcanew.png")
grid.arrange(i,j,k,l, nrow=4, ncol = 1)
dev.off()

png(width = 8, height = 11, units = "in", res = 300, "outputs/cluster/rolling_bimodal_maps_Hemlock_pls_pcanew.png")
grid.arrange(m,n,o,p, nrow=4, ncol = 1)
dev.off()


# now lets determine the PC bins for bimodality:

df.b <- read.csv("outputs/cluster/bimodal_widths/Dens_Bimodal_width_0.25.csv")
ggplot(df.b[df.b$n >= 50,], aes(x, y, fill = bimodal))+geom_raster()
ggplot(df.b[ df.b$PC1 > 0.5 & df.b$PC1 < 1,], aes(PLSdensity, fill = bimodal))+geom_histogram(position = "identity", alpha = 0.5)

ggplot(df.b, aes(PC1, PLSdensity, color = bimodal))+geom_point()

ggplot(df.b[ df.b$PC1 > -0.13 & df.b$PC1 < 0.34,], aes(PC1, PLSdensity, color = bimodal))+geom_point()



range(df.b[df.b$bimodal %in% "Bimodal" & df.b$PC1 > -5 & df.b$PC1 < - 2.5,]$PC1)
# identify the groups of widths that are significantly bimodal:
ggplot(df.b[ df.b$PC1 > -0.13 & df.b$PC1 < .,], aes( PLSdensity, fill = bimodal))+geom_histogram(position = "identity", alpha = 0.5)+facet_wrap(~bimodal)

# the ranges of density with widthcs 0.25
range(df.b[df.b$bimodal %in% "Bimodal" & df.b$PC1 > -5 & df.b$PC1 < - 2.5,]$PC1)
range(df.b[df.b$bimodal %in% "Bimodal" & df.b$PC1 > -0.5 & df.b$PC1 < 0.5,]$PC1)
range(df.b[df.b$bimodal %in% "Bimodal" & df.b$PC1 > 0.5 & df.b$PC1 < 1.6,]$PC1)
range(df.b[df.b$bimodal %in% "Bimodal" & df.b$PC1 > 1.6 & df.b$PC1 < 3.5,]$PC1)

# lets look at several case studies:
ggplot(df.b, aes(x,y, fill = bimodal))+geom_raster()+geom_polygon()

df.rast <- df.b[,c("x", "y", "cell")]
coordinates(df.rast) <- ~x +y
gridded(df.rast) <- TRUE
df.rast <- raster(df.rast)

df.rast2 <- df.b[,c("x", "y", "PLSdensity")]
coordinates(df.rast2) <- ~x +y
gridded(df.rast2) <- TRUE
df.rast2 <- raster(df.rast2)

plot(df.rast2, xlim = c(-60000, 1000000))
# extent for aspen parklands:
plot(extent(x = c(-55000, 190000, y = c(1480000, 1280000))),col = "black", add = TRUE)

# extent for Wisconsin Big Woods:
plot(extent(x = c(350000, 640000), y = c( 710000, 930000)),col = "black", add = TRUE)

# extent for Wisconsin/Minnesota border:
plot(extent(x = c(170000,450000), y = c(950000, 1150000)),col = "black", add = TRUE)

# extent for Hemlock region:
plot(extent(x = c(400000, 750000), y = c(950000, 1150000)),col = "black", add = TRUE)


# extent for Minnesota big woods/prairie:
plot(extent(x = c(95000,300000), y = c(850000, 1100000)),col = "black", add = TRUE)

# extent for Minnesota P-Forest boudary:
plot(extent(x = c(-10000,180000), y = c( 1050000, 1280000)),col = "black", add = TRUE)

# crop to make a raster of ech of  these extents out:
aspen.park <- as.data.frame(crop(df.rast, extent(x = c(-55000, 190000), y = c( 1280000, 1480000))), xy= TRUE)
big.woods <- as.data.frame(crop(df.rast, extent(x = c(350000, 640000), y = c( 710000, 930000))), xy = TRUE)
mn.wi.border <- as.data.frame(crop(df.rast, extent(x = c(170000,450000), y = c(950000, 1150000))), xy = TRUE)
big.woods.mn <- as.data.frame(crop(df.rast, extent(x = c(95000,300000), y = c(850000, 1100000))), xy = TRUE)
p.f.border <- as.data.frame(crop(df.rast, extent(x = c(-10000,180000), y = c( 1050000, 1280000))), xy = TRUE)
hemlock.mtn <- as.data.frame(crop(df.rast, extent(x = c(400000, 750000), y = c(950000, 1150000))), xy = TRUE)

# save these grid cell indices:
write.csv(aspen.park, "outputs/aspen_park_boundary.csv", row.names = FALSE)
write.csv(big.woods, "outputs/big_woods_boundary.csv", row.names = FALSE)
write.csv(mn.wi.border, "outputs/mn_wi_box_boundary.csv", row.names = FALSE)
write.csv(big.woods.mn, "outputs/mn_big_woods.csv", row.names = FALSE)
write.csv(p.f.border, "outputs/pf_border.csv", row.names = FALSE)
write.csv(hemlock.mtn, "outputs/hemlock_region.csv", row.names = FALSE)

# plot out histograms and maps of the case studies:

# aspen parklands
ap1 <- ggplot(df.b[df.b$cell %in% aspen.park$cell,], aes(x,y, fill = bimodal))+geom_raster()+ggtitle("Aspen Parklands Bimodal Regions")
ap2 <- ggplot(df.b[df.b$cell %in% aspen.park$cell,], aes(x,y, fill = PLSdensity))+geom_raster()+ggtitle("Aspen Parklands PLS density")
ap3 <- ggplot(df.b[df.b$cell %in% aspen.park$cell,], aes(x,y, fill = Oak))+geom_raster()+ggtitle("Aspen Parklands Oak Composition")
ap4 <- ggplot(df.b[df.b$cell %in% aspen.park$cell,], aes(x,y, fill = Poplar))+geom_raster()+ggtitle("Aspen Parklands Poplar Composition")
ap5 <- ggplot(df.b[df.b$cell %in% aspen.park$cell,], aes(x,y, fill = PC1))+geom_raster()+ggtitle("Aspen Parklands PC1")




ap6 <-ggplot(df.b[df.b$cell %in% aspen.park$cell,], aes(Poplar))+geom_histogram()+ggtitle("Aspen Parklands % Poplar Histogram")
ap8 <- ggplot(df.b[df.b$cell %in% aspen.park$cell,], aes(PLSdensity))+geom_histogram()+ggtitle("Aspen Parklands PLS density Histogram")
ap7 <- ggplot(df.b[df.b$cell %in% aspen.park$cell,], aes(Oak))+geom_histogram()+ggtitle("Aspen Parklands % Oak Histogram")

png(width = 8, height = 11, units = "in", res = 300, "outputs/cluster/aspen_parkland_rolling_bimodal_maps.png")
grid.arrange(ap1,ap2,ap3,ap4,ap5, ap6, ap7, ap8, nrow=4, ncol = 2)
dev.off()

# big woods--Wisconsin
b1<- ggplot(df.b[df.b$cell %in% big.woods$cell,], aes(x,y, fill = bimodal))+geom_raster()+ggtitle("Big Woods Bimodal Regions")
b2<- ggplot(df.b[df.b$cell %in% big.woods$cell,], aes(x,y, fill = PLSdensity))+geom_raster()+ggtitle("Big Woods PLS Density")
b3<- ggplot(df.b[df.b$cell %in% big.woods$cell,], aes(x,y, fill = Oak))+geom_raster()+ggtitle("Big Woods Oak Composition")
b4<- ggplot(df.b[df.b$cell %in% big.woods$cell,], aes(x,y, fill = PC1))+geom_raster()+ggtitle("Big Woods Environmental PC1")

b7<- ggplot(df.b[df.b$cell %in% big.woods$cell,], aes(PLSdensity))+geom_histogram()+ggtitle("PLS PLS tree density histogram")
b5<- ggplot(df.b[df.b$cell %in% big.woods$cell,], aes(Oak))+geom_histogram()+ggtitle("PLS %Oak histogram")

b6<- ggplot(df.b[df.b$cell %in% big.woods$cell,], aes(Elm))+geom_histogram()+ggtitle("PLS %Elm histogram")
png(width = 8, height = 11, units = "in", res = 300, "outputs/cluster/big_woods_rolling_bimodal_maps.png")
grid.arrange(b1,b2,b3,b4,b5, b6, b7, nrow=4, ncol = 2)
dev.off()

# big woods--Minnesota
bw1<- ggplot(df.b[df.b$cell %in% big.woods.mn$cell,], aes(x,y, fill = bimodal))+geom_raster()+ggtitle("Big Woods, MN Bimodal Regions")
bw2<- ggplot(df.b[df.b$cell %in% big.woods.mn$cell,], aes(x,y, fill = PLSdensity))+geom_raster()+ggtitle("Big Woods, MN PLS Density")
bw3<- ggplot(df.b[df.b$cell %in% big.woods.mn$cell,], aes(x,y, fill = Oak))+geom_raster()+ggtitle("Big Woods, MN Oak Composition")
bw4<- ggplot(df.b[df.b$cell %in% big.woods.mn$cell,], aes(x,y, fill = PC1))+geom_raster()+ggtitle("Big Woods, MN Environmental PC1")

bw7<- ggplot(df.b[df.b$cell %in% big.woods.mn$cell,], aes(PLSdensity))+geom_histogram()+ggtitle("PLS PLS tree density histogram")
bw5<- ggplot(df.b[df.b$cell %in% big.woods.mn$cell,], aes(Oak))+geom_histogram()+ggtitle("PLS %Oak histogram")

bw6<- ggplot(df.b[df.b$cell %in% big.woods.mn$cell,], aes(Elm))+geom_histogram()+ggtitle("PLS %Elm histogram")
png(width = 8, height = 11, units = "in", res = 300, "outputs/cluster/big_woods_minnesota_rolling_bimodal_maps.png")
grid.arrange(bw1,bw2,bw3,bw4,bw5, bw6, bw7, nrow=4, ncol = 2)
dev.off()

# mn.wi.border
m1<- ggplot(df.b[df.b$cell %in% mn.wi.border$cell,], aes(x,y, fill = bimodal))+geom_raster()+ggtitle("MN/WI border Bimodal Regions")
m2<- ggplot(df.b[df.b$cell %in% mn.wi.border$cell,], aes(x,y, fill = PLSdensity))+geom_raster()+ggtitle("MN/WI border PLS density")
m3<- ggplot(df.b[df.b$cell %in% mn.wi.border$cell,], aes(x,y, fill = Oak))+geom_raster()+ggtitle("MN/WI border Oak Composition")
m4<- ggplot(df.b[df.b$cell %in% mn.wi.border$cell,], aes(x,y, fill = PC1))+geom_raster()+ggtitle("MN/WI border Environmental PC1")

m7<-ggplot(df.b[df.b$cell %in% mn.wi.border$cell,], aes(PLSdensity))+geom_histogram()+ggtitle("PLS density histogram")
m5<- ggplot(df.b[df.b$cell %in% mn.wi.border$cell,], aes(Oak))+geom_histogram()+ggtitle("PLS % Oak histogram")
m6<- ggplot(df.b[df.b$cell %in% mn.wi.border$cell,], aes(Hemlock))+geom_histogram()+ggtitle("PLS % Hemlock histogram")

png(width = 8, height = 11, units = "in", res = 300, "outputs/cluster/mn_wi_border_rolling_bimodal_maps.png")
grid.arrange(m1,m2,m3,m4,m5, m6, m7, nrow=4, ncol = 2)
dev.off()


# Prairie-forest boundary--Minnesota
pf1 <- ggplot(df.b[df.b$cell %in% p.f.border$cell,], aes(x,y, fill = bimodal))+geom_raster()+ggtitle("Prairie-forest boundary, MN Bimodal Regions")
pf2 <- ggplot(df.b[df.b$cell %in% p.f.border$cell,], aes(x,y, fill = PLSdensity))+geom_raster()+ggtitle("Prairie-forest boundary, MN PLS Density")
pf3 <- ggplot(df.b[df.b$cell %in% p.f.border$cell,], aes(x,y, fill = Oak))+geom_raster()+ggtitle("Prairie-forest boundary, MN Oak Composition")
pf4 <- ggplot(df.b[df.b$cell %in% p.f.border$cell,], aes(x,y, fill = PC1))+geom_raster()+ggtitle("Prairie-forest boundary, MN Environmental PC1")

pf7 <- ggplot(df.b[df.b$cell %in% p.f.border$cell,], aes(PLSdensity))+geom_histogram()+ggtitle("PLS PLS tree density histogram")
pf5 <- ggplot(df.b[df.b$cell %in% p.f.border$cell,], aes(Oak))+geom_histogram()+ggtitle("PLS %Oak histogram")

pf6 <- ggplot(df.b[df.b$cell %in% p.f.border$cell,], aes(Elm))+geom_histogram()+ggtitle("PLS %Elm histogram")

png(width = 8, height = 11, units = "in", res = 300, "outputs/cluster/prairie_forest_boundary_rolling_bimodal_maps.png")
grid.arrange(pf1,pf2,pf3,pf4,pf5, pf6, pf7, nrow=4, ncol = 2)
dev.off()

# Hemlocak region--Wisconsin/Upper Mi
h1 <- ggplot(df.b[df.b$cell %in% hemlock.mtn$cell,], aes(x,y, fill = bimodal))+geom_raster()+ggtitle("Hemlock Region, Bimodal Regions")
h2 <- ggplot(df.b[df.b$cell %in% hemlock.mtn$cell,], aes(x,y, fill = PLSdensity))+geom_raster()+ggtitle("Hemlock Region, PLS Density")
h3 <- ggplot(df.b[df.b$cell %in% hemlock.mtn$cell,], aes(x,y, fill = Oak))+geom_raster()+ggtitle("Hemlock Region, Oak Composition")
h8 <- ggplot(df.b[df.b$cell %in% hemlock.mtn$cell,], aes(x,y, fill = Hemlock))+geom_raster()+ggtitle("Hemlock Region, Hemlock Composition")
h4 <- ggplot(df.b[df.b$cell %in% hemlock.mtn$cell,], aes(x,y, fill = PC1))+geom_raster()+ggtitle("Hemlock Region, Environmental PC1")

h7 <- ggplot(df.b[df.b$cell %in% hemlock.mtn$cell,], aes(PLSdensity))+geom_histogram()+ggtitle("PLS PLS tree density histogram")
h5 <- ggplot(df.b[df.b$cell %in% hemlock.mtn$cell,], aes(Oak))+geom_histogram()+ggtitle("PLS %Oak histogram")

h6 <- ggplot(df.b[df.b$cell %in% hemlock.mtn$cell,], aes(Hemlock))+geom_histogram()+ggtitle("PLS %Hemlock histogram")

png(width = 8, height = 11, units = "in", res = 300, "outputs/cluster/Hemlock_Region_boundary_rolling_bimodal_maps.png")
grid.arrange(h1, h2, h3, h4,h8, h5, h6, h7, nrow=4, ncol = 2)
dev.off()

# test code for identifying ranges:

test <- df2
test <- test[order(test$PC1),]
test$endPC <- "NA"

# find the cell for the next bimodal and next stable grid cell:
bims <- which(test$bimodal == "Bimodal")
stab <- which(test$bimodal == "Stable")

next_b <- sapply(bims, function(a) {
  diff <- stab-a
  if(all(diff < 0)) return(NA)
  stab[min(diff[diff > 0]) == diff]
})



next_a <- sapply(stab, function(a) {
  diff <- bims-a
  if(all(diff < 0)) return(NA)
  bims[min(diff[diff > 0]) == diff]
})


test$next_stable <- NA
test$next_bimod<- NA
test$next_stable[bims] <- test$PC1[next_b]
test$next_bimod[stab] <- test$PC1[next_a]



test <- test[rev(order(test$PC1)),]
test$endPC <- "NA"

# find the cell for the next bimodal and next stable grid cell:
bims <- which(test$bimodal == "Bimodal")
stab <- which(test$bimodal == "Stable")

prev_b <- sapply(bims, function(a) {
  diff <- stab-a
  if(all(diff < 0)) return(NA)
  stab[min(diff[diff > 0]) == diff]
})



prev_a <- sapply(stab, function(a) {
  diff <- bims-a
  if(all(diff < 0)) return(NA)
  bims[min(diff[diff > 0]) == diff]
})


test$prev_stable <- NA
test$prev_bimod<- NA
test$prev_stable[bims] <- test$PC1[prev_b]
test$prev_bimod[stab] <- test$PC1[prev_a]

test$range <- ifelse(test$bimodal == "Bimodal", paste0( test$prev_stable, " - ", test$next_stable), 
                     paste0( test$prev_bimod, " - ", test$next_bimod))


ggplot( test, aes(PLSdensity)) + geom_histogram(position = "identity", alpha = 0.2)+facet_wrap(~range, ncol = 8)

# now for each grid cell, print the end PCvalue for each of the 
for(i in 1:length(test$y)){
  if(test[i,]$bimodal =="Bimodal"){
    endpc <-  test[next_b[i]-1,]$PC1
    ifelse(class(endpc) == 'NULL', test[i,]$endPC <- 'NA', test[i,]$endPC <- as.numeric(endpc))
    
    test[i,]$endPC <- as.numeric(endpc)
    
  }else if (test[i,]$bimodal == "Stable"){
    endpc <-  test[next_a[i]-1,]$PC1
    ifelse(class(endpc) == 'NULL', test[i,]$endPC <- 'NA', test[i,]$endPC <- as.numeric(endpc))
    
    test[i,]$endPC <- as.numeric(endpc)
  }
}


library(data.table)
setDT(df)

df[Event=="B", .(time, nextb=time)][df, on = "time", roll = -Inf][Event != "A", nextb := NA][]

test$order <- as.numeric(row.names(test))
setDT(test)

ab <- test[bimodal=="Bimodal", .(order, nextb=order)][test, on="order", roll=-Inf][bimodal != "Stable", nextb := NA][]
