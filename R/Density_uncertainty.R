# uncertainty around grid cell estimates of tree density:
# want to sample randomly from point level estimates within each grid cell:

# get all points in the grid cell:
# sample randomly from the distribution: 
# get the sd and the mean from that random sampling?

version <- "1.7-5"
setwd( "/Users/kah/Documents/bimodality")
library(data.table)
library(reshape2)
library(ggplot2)
library(hexbin)
library(grid)
library(gridExtra)
library(sp)
library(raster)
library(rgdal)


#-----------------------------Load PLS data--------------------------------------

# read in pont level data
pls.inil <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_inilmi_v',version, '.csv'))
pls.umw <- readRDS(paste0("data/outputs/UMW_pointwise.ests_v1.7-5UMDW.RDS"))

# combine upper and lower MW:
pls.full <- rbind(pls.inil[,c("x", "y", "Pointx","Pointy", "cell",  "spec", "count", "point", "density", "basal", "diams")], pls.umw[!is.na(pls.umw$density),c("x", "y","Pointx","Pointy", "cell",  "spec", "count", "point", "density", "basal", "diams")])

# get the mean value for each species at each PLS point:
pls.spec <- dcast(pls.full, Pointx + Pointy + x + y + cell ~ spec, mean, na.rm = TRUE, value.var = 'density')

# get estimate of total tree density at each point:
pls.spec$density <- rowSums(pls.spec[,!names(pls.spec)%in% c("x", "y","Pointx", "Pointy", "cell", "Water", "wet")], na.rm=TRUE) # sum species density in the grid cell
pls <- pls.spec

pls <- pls[!is.na(pls$density),] # remove all NA values for density

# Option 1: finding mean and sd of each grid cell
pls.mean <- dcast(pls, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density') # we want to sum the densities of all the species in each cells, then divide by the # of pls points within the cell, so take the avg 
pls.sd <- dcast(pls, x + y + cell ~., sd, na.rm = TRUE, value.var = 'density') # we want to sum the densities of all the species in each cells, then divide by the # of pls points within the cell, so take the avg 

colnames(pls.mean) <- c('x', 'y', 'cell','PLSdensity')
colnames(pls.sd) <- c('x', 'y', 'cell','density_sd')
hist(pls.mean$PLSdensity, xlim = c(0, 600),breaks = 1000)

pls.basic <- merge(pls.mean, pls.sd, by = c("x", "y", "cell"))


label.breaks <- function(beg, end, splitby){
  labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
  labels.test <- paste (labels.test$first, '-' , labels.test$second)
  labels.test
}


pls.basic$sd_bins <- cut(pls.basic$density_sd, breaks = seq(-1,600, by = 20), labels = label.breaks(0,580, 20))

ggplot(pls.basic, aes(x,y, fill = sd_bins))+geom_raster()
ggplot(pls.basic, aes(x,y, fill = PLSdensity))+geom_raster()

ggplot(pls.basic, aes(x = PLSdensity))+geom_histogram()

png("outputs/full_MW_dens_histogram_by_sd.png")
ggplot(pls.basic, aes(PLSdensity, fill = sd_bins))+geom_histogram(position = "stack")+theme_bw()+xlim(0,600)+ylim(0,2000)
dev.off()

# Option 2: Bootstrapping mean and 95% CI of the data in each grid cell:
library(boot)

func.mean <- function(d, i){
  d2 <- d[i,]
  return(mean(d2$density, na.rm=TRUE))
}

bootcorr <- boot(pls[pls$cell %in% 40599,], func.mean, R=500)
bootcorr

# compare to regular mean:
mean.dens <- mean(pls[pls$cell %in% 40599,]$density, na.rm = TRUE)

boot.ci(bootcorr, type = "bca")

# compare to regular 
sd.dens <- sd(pls.inil[pls.inil$cell %in% 40599,]$density, na.rm = TRUE)

mean.dens + sd.dens
mean.dens - sd.dens

pls.inil2 <- pls.inil[1:100,c("x", "y", "cell", "density")]

density.samples <- list()
# create a function that does the bootstrapped CI intervals
boot.calcs <- function(x){
        func.mean <- function(d, indices){
          d2 <- d[indices]
          return(mean(d2, na.rm=TRUE))
        }
     
         bootcorr <- boot(x, stat = func.mean, R=1000)
     
      
      # compare to regular means
      
      bootci <- boot.ci(bootcorr, type = "perc")
      
      if(is.null(bootci$percent[4])){
        out <- data.frame(mean = bootcorr$t0, 
                          ci.low = NA, 
                          ci.high = NA)
      }else{
      out <- data.frame(mean =  bootcorr$t0,
                        ci.low = bootci$percent[4], 
                        ci.high = bootci$percent[5])
      
      }
      
      out
  }


boot.dens <- function(x){
  func.mean <- function(d, indices){
    d2 <- d[indices]
    return(mean(d2, na.rm=TRUE))
  }
  
  bootcorr <- boot(x, stat = func.mean, R=1000)
  
  
  
 density.samples <- bootcorr$t
 density.samples
}

# create a list of densities by each cell:
dens.by.cells <- split(pls$density, pls$cell)

# apply the "boot.calcs" function over all cells:
dens.ci.mean <- lapply( dens.by.cells, FUN = boot.calcs)

dens.ci.mean.df <- do.call(rbind, dens.ci.mean)
dens.ci.mean.df$cell<- row.names(dens.ci.mean.df)
dens.ci.df <- merge(dens.ci.mean.df, pls.mean[,c("x", "y", "cell", "PLSdensity")], by = "cell")

dens.boot.samples <- lapply( dens.by.cells, FUN = boot.dens)

# how does the above method compare to getting estimates "by hand" --i.e. not using default CI and bootstrap function:

# following Brett Larget's example: http://www.stat.wisc.edu/~larget/stat302/chap3.pdf
# A quick bootstrap function for a confidence interval for the mean
# x is a single quantitative sample
# B is the desired number of bootstrap samples to take # binwidth is passed on to geom_histogram()

boot.mean = function(x,binwidth=NULL) {
  B = 1000
  n = length(x)
  boot.samples = matrix( sample(x,size=n*B,replace=TRUE), B, n)
  boot.statistics = apply(boot.samples,1,mean)
  se = sd(boot.statistics)
  require(ggplot2)
  if ( is.null(binwidth) )
    binwidth = diff(range(boot.statistics))/30
  p = ggplot(data.frame(x=boot.statistics),aes(x=x)) +
    geom_histogram(aes(y=..density..),binwidth=binwidth) + geom_density(color="red")
  plot(p)
  interval = mean(x) + c(-1,1)*2*se
  print( interval )
  return( list(boot.statistics = boot.statistics, interval=interval, se=se, plot=p) )
}


boot.mean(dens.by.cells[[1]]) #, B = 500)
boot.calcs(dens.by.cells[[1]])
# comparing both of these methods yeilds very similar CI estimates

#-------------------- get the density uncertainty from the FIA data:
FIA <- read.csv('data/FIA_species_plot_parameters_paleongrid.csv') # read in FIA data
speciesconversion <- read.csv('data/FIA_conversion-SGD_remove_dups.csv') # conversion table for converting FIA nomeclature to Paleon taxa

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )

# how we would normally calculate density:
FIA.by.paleon <- dcast(FIA.pal, x + y+ cell + plt_cn ~ PalEON, sum, na.rm=TRUE, value.var = 'density') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,6:25], na.rm = TRUE) # sum the total density in each plot--This is what we will use to get bootstrapped average total density +ci
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn', 'Var.5')) # melt the dataframe
fia.by.plot <- dcast(fia.melt, x + y +cell+ plt_cn ~ variable, sum, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell
melted.fia <- melt(fia.by.plot[,c('x', "y", "cell", "plt_cn", "FIAdensity")], id.vars = c('x', "y", "cell", "plt_cn"))

fia.by.cell <- ddply(melted.fia,~ cell,summarise,mean=mean(value),total = sum(value),sd=sd(value), x = mean(x), y = mean(y))

# create a list of densities by cell for the FIA
fdens.by.cells <- split(FIA.by.paleon$FIAdensity,  FIA.by.paleon$cell)

# apply the "boot.calcs" function over all FIA cells:
fdens.ci.mean <- lapply( fdens.by.cells, FUN = boot.calcs )

fdens.ci.mean.df <- do.call( rbind, fdens.ci.mean )
fdens.ci.mean.df$cell <- row.names(fdens.ci.mean.df)
fdens.ci.df <- merge(fdens.ci.mean.df, test, by = "cell")
fdens.ci.df <- merge(fdens.ci.mean.df, fia.by.cell, by = "cell")
colnames(fdens.ci.df)<- c("cell", "mean.fia", "ci.low.fia", "ci.high.fia",  "FIAdensity","FIAdenssd","x", "y")

# merge FIA and PLS together
alldens <- merge(dens.ci.df, fdens.ci.df, by = c("x", "y", "cell"))
summary(alldens)

alldens.m <- melt(alldens[,c("x", "y", "cell","ci.low", "ci.high", "PLSdensity", "ci.low.fia", "ci.high.fia", "FIAdensity")], id.vars = c('x', "y", "cell"))

dens.with.ci <- alldens.m[complete.cases(alldens.m),]

# -----------------plot histograms of density and histograms of the low and high confidence intervals:------------
png(height = 6, width = 7, units = "in", res = 300, "outputs/density_unc/PLSdensity_hist_MW_with_ci.png")
ggplot()+geom_histogram(data = alldens.m[alldens.m$variable %in% c("PLSdensity"),], aes(value, fill = variable,position = "identity", binwidth = 18))+
  geom_density(data =alldens.m[alldens.m$variable %in% c("PLSdensity", "ci.low", "ci.high"),] ,aes(value, color = variable, 20 *..count.., linetype = variable), size = 1.2)+scale_color_manual(values = c("grey", "grey", "red"))+
  scale_linetype_manual(values=c("dashed", "dotted", "solid"))+theme_bw(base_size = 20)+xlim(0,600)
dev.off()

png(height = 4, width = 7, units = "in", res = 300, "outputs/density_unc/PLS_MW_histograms.png")
ggplot(dens.with.ci[dens.with.ci$variable %in% c("PLSdensity", "ci.low", "ci.high"),], aes(value, color = variable))+geom_histogram(position = "identity",alpha = 0.5)+theme_bw()+xlim(0,400)+facet_wrap(~variable)
dev.off()



# plotting the fia data makes less sense because we can only get ci from grid cells with more than 1 fia plot--which are sparse in in & IL
png(height = 6, width = 7, units = "in", res = 300, "outputs/density_unc/FIAdensity_hist_MW_with_ci.png")
ggplot() + geom_histogram(data = dens.with.ci[dens.with.ci$variable %in% c("FIAdensity"),], aes(value, fill = variable, position = "identity", binwidth = 18))+
  geom_density(data = dens.with.ci[dens.with.ci$variable %in% c("FIAdensity", "ci.low.fia", "ci.high.fia"),] ,aes(value, color = variable, 25 *..count.., linetype = variable), size = 1.2)+xlim(0,600)+scale_color_manual(values = c("grey", "grey", "red"))+
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) + theme_bw(base_size = 20)
dev.off()


  
# histogram of FIA + 95% ci
png(height = 4, width = 7, units = "in", res = 300, "outputs/density_unc/FIA_MW_histograms.png")
ggplot(dens.with.ci[dens.with.ci$variable %in% c("FIAdensity", "ci.low.fia", "ci.high.fia"),], aes(value, color = variable))+geom_histogram(position = "identity",alpha = 0.5)+theme_bw()+xlim(0,600)+facet_wrap(~variable)
dev.off()


# make a histogram with both FIA and PLS density + 95% CI (this looks really messy)
clean.dens.with.ci <- dens.with.ci[dens.with.ci$value <= 600, ]

density.hists <- ggplot() + geom_histogram(data = clean.dens.with.ci[clean.dens.with.ci$variable %in% c("FIAdensity", "PLSdensity"),], aes(value, fill = variable, alpha = 0.5 ), position = "identity", binwidth = 25)+
  geom_density(data = clean.dens.with.ci[clean.dens.with.ci$variable %in% c("FIAdensity", "ci.low.fia", "ci.high.fia"),] ,aes(value, color = variable, 30 *..count.., linetype = variable), size = 1.2)+xlim(0,600)#scale_color_manual(values = c("light.blue", "light.blue", "blue"))+xlim(0,600)+

png(height = 4, width = 7, units = "in", res = 300, "outputs/density_unc/FIA_PLS_MW_histograms.png")

density.hists + geom_density(data = clean.dens.with.ci[clean.dens.with.ci$variable %in% c("PLSdensity", "ci.low", "ci.high"),] ,aes(value, color = variable, 30 *..count.., linetype = variable), size = 1.2)+scale_color_manual(values = c("salmon", "dodgerblue", "salmon","dodgerblue", "blue", "red"))+xlim(0,600)+
  scale_linetype_manual(values=c("dashed", "dashed", "dotted","dotted", "solid", "solid"))+ theme_bw(base_size = 20)
dev.off()


# -----------------------mapping out density estimates, CI intervals, and classification:---------------------
dens.ci.df$uncertainty <- (dens.ci.df$ci.high - dens.ci.df$ci.low)/2
dens.ci.df.m <- melt(dens.ci.df[,c("x", "y", "cell", "PLSdensity", "ci.low", "ci.high")], id.vars = c("x", "y", "cell"))
dens.ci.df.m$ecoclass <- ifelse(dens.ci.df.m$value >= 47, "Forest", ifelse(dens.ci.df.m$value >= 1, "Savanna", "Prairie" ))

alldens$uncertainty <- (alldens$ci.high.fia - alldens$ci.low.fia)/2
fdens.ci.df.m <- melt(alldens[,c("x", "y", "cell", "FIAdensity", "ci.low.fia", "ci.high.fia")], id.vars = c("x", "y", "cell"))
fdens.ci.df.m$ecoclass <- ifelse(fdens.ci.df.m$value >= 47, "Forest", ifelse(fdens.ci.df.m$value >= 1, "Savanna", "Prairie" ))

# map out the density and CI estimates for PLS
# need to set up state outlines:
all_states <- map_data("state")
states <- subset(all_states, region %in% c( "wisconsin","minnesota" ,"michigan", "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)



sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbpal_unc <- c('white', '#fecc5c', '#fd8d3c','#f03b20', '#bd0026')

dens.pr <- read.csv("data/PLS_full_dens_pr_with_bins.csv")
 ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.pr, aes(x=x, y=y, fill = PLSdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,600), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 10)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank())

 ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
   geom_raster(data=dens.ci.df.m[dens.ci.df.m$variable %in% "PLSdensity",], aes(x=x, y=y, fill = value))+
   geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
   labs(x="easting", y="northing")+ #+ 
   scale_fill_gradientn(colours = cbpalette, limits = c(0,650), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
   coord_equal()+theme_bw(base_size = 10)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                 axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                 axis.title.x=element_blank(),
                                                 axis.title.y=element_blank())

pls.dens.ci.maps <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.ci.df.m, aes(x=x, y=y, fill = value))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,650), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 10)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank())+facet_wrap(~variable)#+ annotate("text", x=-90000, y=1486000,label= "A", size = 5)+ggtitle("")
png(height = 5, width = 9, units = "in", res = 300, "outputs/density_unc/map_density_ci_MW_pls.png")
pls.dens.ci.maps
dev.off()



# make a map of the assigned ecoclass based on using low and high CI values as density
pls.class.ci.maps<- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.ci.df.m, aes(x=x, y=y, fill = ecoclass))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_manual(values = c("#ffffcc",  "#78c679",  "#006837"), limits=c("Prairie", "Savanna", "Forest"), name ="Ecocode", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 10)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank())+facet_wrap(~variable)#+ annotate("text", x=-90000, y=1486000,label= "A", size = 5)+ggtitle("")

png(height = 5, width = 9, units = "in", res = 300, "outputs/density_unc/map_class_ci_MW_pls.png")
pls.class.ci.maps
dev.off()

# Map out density and CI intervals for FIA
fia.dens.ci.maps<- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fdens.ci.df.m, aes(x=x, y=y, fill = value))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,650), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 10)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank())+facet_wrap(~variable)#+ annotate("text", x=-90000, y=1486000,label= "A", size = 5)+ggtitle("")
png(height = 5, width = 9, units = "in", res = 300, "outputs/density_unc/map_density_ci_MW_fia.png")
fia.dens.ci.maps
dev.off()

# map out ecoclassificaiton based on density and CI intervals for FIA
fia.class.ci.maps<- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fdens.ci.df.m, aes(x=x, y=y, fill = ecoclass))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_manual(values = c("#ffffcc",  "#78c679",  "#006837"), limits=c("Prairie", "Savanna", "Forest"), name ="Ecocode", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 10)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank())+facet_wrap(~variable)#+ annotate("text", x=-90000, y=1486000,label= "A", size = 5)+ggtitle("")

png(height = 5, width = 9, units = "in", res = 300, "outputs/density_unc/map_class_ci_inil_fia.png")
fia.class.ci.maps
dev.off()

# map out density and uncertainty for the FIA era:
f.density.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=alldens, aes(x=x, y=y, fill = FIAdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+scale_fill_gradientn(colours = cbpalette, limits = c(0,650), name ="Tree Density", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 10)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank())

f.uncertainty.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=alldens, aes(x=x, y=y, fill = uncertainty))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+scale_fill_gradientn(colours = cbpal_unc, limits = c(0,500), name =" Uncertainty", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 10)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank())

png(height = 5, width = 9, units = "in", res = 300, "outputs/density_unc/map_density_uncertainty_MW_fia.png")
grid.arrange(f.density.map, f.uncertainty.map, ncol = 2)
dev.off()

# Map out the Density & Uncertainty for PLS era
density.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.ci.df, aes(x=x, y=y, fill = PLSdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+scale_fill_gradientn(colours = cbpalette, limits = c(0,650), name ="Tree Density", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 10)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank())

uncertainty.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.ci.df, aes(x=x, y=y, fill = uncertainty))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+scale_fill_gradientn(colours = cbpal_unc, limits = c(0,250), name =" Uncertainty", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 10)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank())

png(height = 5, width = 9, units = "in", res = 300, "outputs/density_unc/map_density_uncertainty_MW_pls.png")
grid.arrange(density.map, uncertainty.map, ncol = 2)
dev.off()

#-------------------------------------------------------------------------------------------------------
# option # 2part b: sample 1 value from grid cell distribution, do this for all grid cells-> is it bimodal? Do this several times

library(boot)

# this function samples 100 pls points from each grid cell & takes the mean of those samples
samp.dens <- function(x){ mean(sample(x,100,replace = TRUE),na.rm=TRUE)}
sample100 <- function(df){
    test <- lapply(df, function(x){ mean(sample(x,100,replace = TRUE),na.rm=TRUE)})
    test2 <- do.call("rbind", test)
    test2
}


#next we do the sample100 function 100 times, so we generate 100 histograms:
fpoint.dens.mat <- matrix(fdens.by.cells, nrow = length(fdens.by.cells), ncol = 100 ) # make 251 by 20 matrix

fcell.dens.mat <- apply(X = fpoint.dens.mat, FUN = sample100, MARGIN = 2)

ggplot(data.frame(fcell.dens.mat), aes(x = fcell.dens.mat[,10]))+geom_histogram()

#hist(cell.dens.mat[,10],  breaks = 100)$breaks
#hist(cell.dens.mat[,3],  breaks = 100)$count

# if we get the CI on histogram without removing the prairie ecoclass:
fbreaks <- apply(fcell.dens.mat,FUN = function(x) {hist(x, xlim = c(0,600), breaks = 100)$breaks[2:101]}, MARGIN = 2)
fcounts <- apply(fcell.dens.mat,FUN = function(x) {hist(x, xlim = c(0,600), breaks = 100)$count}, MARGIN = 2)
fcounts.df <- do.call("rbind", fcounts)
fcounts.df <- t(fcounts.df)

fcount.sd <- apply(fcounts.df, FUN = sd, MARGIN=1)
fcount.95 <- apply(fcounts.df, FUN = function(x){quantile(x,.95)}, MARGIN = 1)
fcount.5 <- apply(fcounts.df, FUN = function(x){quantile(x,.05)}, MARGIN = 1)
fcount.mean <- apply(fcounts.df, FUN = mean, MARGIN = 1)

#plot( breaks[1:100,1], count.mean[1:100])

count.sds <- data.frame(counts = count.mean[1:100], breaks = breaks[1:100,1], sd = count.sd[1:100], min.sd = count.mean[1:100] - count.sd[1:100], max.sd = count.mean[1:100] + count.sd[1:100], 
                        ci.95 = count.95[1:100], ci.5 = count.5[1:100])

# plot histogram bar plot with +/- SD
ggplot(count.sds, aes(breaks, counts))+geom_bar(stat = "identity")+geom_errorbar(data = count.sds, aes(ymin=min.sd, ymax=max.sd),width=1)
ggplot(count.sds, aes(breaks, ci.95))+geom_bar(stat = "identity")#+geom_errorbar(data = count.sds, aes(ymin=min.sd, ymax=max.sd),width=1)


#---------------------do this for cells with prairie cells removed---------------------
#next we do the sample100 function 100 times, so we generate 100 histograms:
point.dens.mat <- matrix(dens.by.cells, nrow = length(dens.by.cells),ncol = 100 ) # make 251 by 20 matrix

cell.dens.mat <- apply(X = point.dens.mat, FUN = sample100, MARGIN = 2)

cell.dens <- as.data.frame(cell.dens.mat)
cell.dens$cell <- names(dens.by.cells)

write.csv(cell.dens, "outputs/density_100samples_by_cell.csv")

cell.dens.mat[cell.dens.mat < 1 ] <- NA  # get rid of prairie cells
cell.dens.mat[cell.dens.mat > 1000 ] <- NA  # get rid of high density cells


#  get the CI on histogram after removing the prairie ecoclass:
# note, we are generatinge CI on counts for 40 bins here
breaks <- apply(cell.dens.mat,FUN = function(x) {hist(x, xlim = c(0,600), breaks = 40)$breaks[2:101]}, MARGIN = 2)
counts <- apply(cell.dens.mat,FUN = function(x) {hist(x, xlim = c(0,600), breaks = 40)$count}, MARGIN = 2)
counts.df <- do.call("rbind", counts)
counts.df <- t(counts.df)

count.sd <- apply(counts.df, FUN = sd, na.rm=TRUE, MARGIN=1)
count.95 <- apply(counts.df, FUN = function(x){quantile(x,.95,na.rm=TRUE)}, MARGIN = 1)
count.5 <- apply(counts.df, FUN = function(x){quantile(x,.05,na.rm=TRUE)}, MARGIN = 1)
count.mean <- apply(counts.df, FUN = mean,na.rm=TRUE, MARGIN = 1)

#plot( breaks[1:100,1], count.mean[1:100])

count.sds <- data.frame(counts = count.mean[1:100], breaks = breaks[1:100,1], sd = count.sd[1:100], min.sd = count.mean[1:100] - count.sd[1:100], max.sd = count.mean[1:100] + count.sd[1:100], 
                        ci.95 = count.95[1:100], ci.5 = count.5[1:100])

# plot histogram bar plot with +/- SD
ggplot(count.sds, aes(breaks, counts))+geom_bar(stat = "identity")+geom_errorbar(data = count.sds, aes(ymin=min.sd, ymax=max.sd),width=1)
ggplot(count.sds, aes(breaks, ci.95))+geom_bar(stat = "identity")#+geom_errorbar(data = count.sds, aes(ymin=min.sd, ymax=max.sd),width=1)
ggplot(count.sds, aes(breaks, ci.95))+geom_bar(stat = "identity")#+geom_errorbar(data = count.sds, aes(ymin=min.sd, ymax=max.sd),width=1)

png(width = 6, height = 4, units = "in", res = 300, "outputs/density_unc/PLS_counts_unc_barplot.png")
ggplot(count.sds, aes(breaks, counts) )+geom_bar(stat = "identity",fill = "blue")+geom_ribbon(aes(ymin=ci.5, ymax=ci.95),fill="darkgrey", alpha=0.9)+xlim(0,600)+theme_bw()+xlab("Tree Density")
dev.off()

png(width = 6, height = 4, units = "in", res = 300, "outputs/density_unc/PLS_counts_unc_barplot_errorbars.png")
ggplot(count.sds, aes(breaks, counts) )+geom_bar(stat = "identity",fill = "blue")+geom_errorbar(aes(ymin=ci.5, ymax=ci.95),width = 1)+xlim(0,600)+theme_bw()+xlab("Tree Density")
dev.off()

png(width = 6, height = 4, units = "in", res = 300, "outputs/density_unc/PLS_counts_unc_lineplot.png")
ggplot(count.sds, aes(breaks, counts) )+geom_line(color = "blue", width = 1)+geom_ribbon(aes(ymin=ci.5, ymax=ci.95),alpha = 0.5)+xlim(0,600)+theme_bw()+xlab("Tree Density")
dev.off()



#-------------------------CI estimation for FIA histogram data---------------------------------

library(boot)

# this function samples 100 pls points from each grid cell & takes the mean of those samples
samp.dens <- function(x){ mean(sample(x,100,replace = TRUE),na.rm=TRUE)}
sample100 <- function(df){
  test <- lapply(df, function(x){ mean(sample(x,100,replace = TRUE),na.rm=TRUE)})
  test2 <- do.call("rbind", test)
  test2
}


#next we do the sample100 function 100 times, so we generate 100 histograms:
fpoint.dens.mat <- matrix(fdens.by.cells, nrow = length(fdens.by.cells), ncol = 100 ) # make 251 by 20 matrix

fcell.dens.mat <- apply(X = fpoint.dens.mat, FUN = sample100, MARGIN = 2)

ggplot(data.frame(fcell.dens.mat), aes(x = fcell.dens.mat[,10]))+geom_histogram()


# if we get the CI on histogram without removing the prairie ecoclass:
fbreaks <- apply(fcell.dens.mat,FUN = function(x) {hist(x, xlim = c(0,600), breaks = 100)$breaks[2:101]}, MARGIN = 2)
fcounts <- apply(fcell.dens.mat,FUN = function(x) {hist(x, xlim = c(0,600), breaks = 100)$count}, MARGIN = 2)
fcounts.df <- do.call("rbind", fcounts)
fcounts.df <- t(fcounts.df)

fcount.sd <- apply(fcounts.df, FUN = sd, MARGIN=1)
fcount.95 <- apply(fcounts.df, FUN = function(x){quantile(x,.95)}, MARGIN = 1)
fcount.5 <- apply(fcounts.df, FUN = function(x){quantile(x,.05)}, MARGIN = 1)
fcount.mean <- apply(fcounts.df, FUN = mean, MARGIN = 1)

#plot( breaks[1:100,1], count.mean[1:100])

fcount.sds <- data.frame(counts = fcount.mean[1:100], breaks = fbreaks[1:100,1], sd = fcount.sd[1:100], min.sd = fcount.mean[1:100] - fcount.sd[1:100], max.sd = fcount.mean[1:100] + fcount.sd[1:100], 
                        ci.95 = fcount.95[1:100], ci.5 = fcount.5[1:100])

# plot histogram bar plot with +/- SD
ggplot(fcount.sds, aes(breaks, counts))+geom_bar(stat = "identity")+geom_errorbar(data = fcount.sds, aes(ymin=min.sd, ymax=max.sd),width=1)
ggplot(fcount.sds, aes(breaks, ci.95))+geom_bar(stat = "identity")#+geom_errorbar(data = count.sds, aes(ymin=min.sd, ymax=max.sd),width=1)

#---------------------do this for all FIA cells with corresponding PLS cells---------------------
#next we do the sample100 function 100 times, so we generate 100 histograms:
inPLS<- names(dens.by.cells)
UMW.by.cells <- fdens.by.cells[names(fdens.by.cells) %in% inPLS]
fpoint.dens.mat <- matrix(UMW.by.cells , nrow = length(UMW.by.cells ),ncol = 100 ) # make 251 by 20 matrix

fcell.dens.mat <- apply(X = fpoint.dens.mat, FUN = sample100, MARGIN = 2)

fcell.dens.mat[fcell.dens.mat > 1000 ] <- NA  # get rid of prairie cells
# if we get the CI on histogram without removing the prairie ecoclass:
fbreaks <- apply(fcell.dens.mat,FUN = function(x) {hist(x, xlim = c(0,600), breaks = 40)$breaks[2:101]}, MARGIN = 2)
fcounts <- apply(fcell.dens.mat,FUN = function(x) {hist(x, xlim = c(0,600), breaks = 40)$count}, MARGIN = 2)
fcounts.df <- do.call("rbind", fcounts)
fcounts.df <- t(fcounts.df)

fcount.sd <- apply(fcounts.df, FUN = sd, na.rm=TRUE, MARGIN=1)
fcount.95 <- apply(fcounts.df, FUN = function(x){quantile(x,.95,na.rm=TRUE)}, MARGIN = 1)
fcount.5 <- apply(fcounts.df, FUN = function(x){quantile(x,.05,na.rm=TRUE)}, MARGIN = 1)
fcount.mean <- apply(fcounts.df, FUN = mean,na.rm=TRUE, MARGIN = 1)

#plot( breaks[1:100,1], count.mean[1:100])

fcount.sds <- data.frame(counts = fcount.mean[1:100], breaks = fbreaks[1:100,1], sd = fcount.sd[1:100], min.sd = fcount.mean[1:100] - fcount.sd[1:100], max.sd = fcount.mean[1:100] + fcount.sd[1:100], 
                        ci.95 = fcount.95[1:100], ci.5 = fcount.5[1:100])

# plot histogram bar plot with +/- SD
ggplot(fcount.sds, aes(breaks, counts))+geom_bar(stat = "identity")+geom_errorbar(data = fcount.sds, aes(ymin=min.sd, ymax=max.sd),width=1)
ggplot(fcount.sds, aes(breaks, ci.95))+geom_bar(stat = "identity")#+geom_errorbar(data = count.sds, aes(ymin=min.sd, ymax=max.sd),width=1)
ggplot(fcount.sds, aes(breaks, ci.95))+geom_bar(stat = "identity")#+geom_errorbar(data = count.sds, aes(ymin=min.sd, ymax=max.sd),width=1)

png(width = 6, height = 4, units = "in", res = 300, "outputs/density_unc/FIA_counts_unc_barplot.png")
ggplot(fcount.sds, aes(breaks, counts) )+geom_bar(stat = "identity",fill = "blue")+geom_ribbon(aes(ymin=ci.5, ymax=ci.95),fill="darkgrey", alpha=0.9)+xlim(0,600)+theme_bw()+xlab("Tree Density")
dev.off()

png(width = 6, height = 4, units = "in", res = 300, "outputs/density_unc/FIA_counts_unc_barplot_errorbars.png")
ggplot(fcount.sds, aes(breaks, counts) )+geom_bar(stat = "identity",fill = "blue")+geom_errorbar(aes(ymin=ci.5, ymax=ci.95),width = 1)+xlim(0,600)+theme_bw()+xlab("Tree Density")
dev.off()

png(width = 6, height = 4, units = "in", res = 300, "outputs/density_unc/FIA_counts_unc_lineplot.png")
ggplot(fcount.sds, aes(breaks, counts) )+geom_line(color = "blue", width = 1)+geom_ribbon(aes(ymin=ci.5, ymax=ci.95),alpha = 0.5)+xlim(0,600)+theme_bw()+xlab("Tree Density")
dev.off()

# ---------------lets plot the PLS and FIA datasets together:
png(width = 6, height = 4, units = "in", res = 300, "outputs/density_unc/PLS_FIA_counts_unc_barplot_40bins.png")
ggplot(count.sds, aes(breaks, counts, fill ="PLS") )+geom_bar(stat = "identity",fill = 'blue', alpha = 0.3)+geom_ribbon(aes(ymin=ci.5, ymax=ci.95),fill="lightblue", alpha=0.9)+
  geom_bar(data = fcount.sds, aes(breaks, counts) ,stat = "identity",fill = "red", alpha = 0.3)+geom_ribbon(data = fcount.sds,aes(ymin=ci.5, ymax=ci.95),fill="pink", alpha=0.9)+xlim(0,600)+theme_bw()+xlab("Tree Density")
dev.off()



#-----------------Incorporating density uncertainty into evaluation of bimodality overall:--------------------
full <- read.csv("outputs/cluster/full_comp_dens_df.csv")

