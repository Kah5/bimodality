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


# Option 1: finding mean and sd of each grid cell
pls.mean <- dcast(pls.inil, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density') # we want to sum the densities of all the species in each cells, then divide by the # of pls points within the cell, so take the avg 
pls.sd <- dcast(pls.inil, x + y + cell ~., sd, na.rm = TRUE, value.var = 'density') # we want to sum the densities of all the species in each cells, then divide by the # of pls points within the cell, so take the avg 

colnames(pls.mean) <- c('x', 'y', 'cell','PLSdensity')
colnames(pls.sd) <- c('x', 'y', 'cell','density_sd')
hist(pls.mean$PLSdensity, xlim = c(0, 600),breaks = 100)

pls <- merge(pls.mean, pls.sd, by = c("x", "y", "cell"))


label.breaks <- function(beg, end, splitby){
  labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
  labels.test <- paste (labels.test$first, '-' , labels.test$second)
  labels.test
}


pls$sd_bins <- cut(pls$density_sd, breaks = seq(-1,400, by = 20), labels = label.breaks(0,380, 20))

ggplot(pls, aes(x,y, fill = sd_bins))+geom_raster()

png("outputs/IN_IL_dens_histogram_by_sd.png")
ggplot(pls, aes(PLSdensity, fill = sd_bins))+geom_histogram(position = "stack")+theme_bw()
dev.off()

# Option 2: Bootstrapping mean and 95% CI of the data in each grid cell:
library(boot)

func.mean <- function(d, i){
  d2 <- d[i,]
  return(mean(d2$density, na.rm=TRUE))
}

bootcorr <- boot(pls.inil[pls.inil$cell %in% 40599,], func.mean, R=500)
bootcorr

# compare to regular mean:
mean.dens <- mean(pls.inil[pls.inil$cell %in% 40599,]$density, na.rm = TRUE)

boot.ci(bootcorr, type = "bca")

# compare to regular 
sd.dens <- sd(pls.inil[pls.inil$cell %in% 40599,]$density, na.rm = TRUE)

mean.dens + sd.dens
mean.dens - sd.dens

pls.inil2 <- pls.inil[1:100,c("x", "y", "cell", "density")]

boot.calcs <- function(x){
        func.mean <- function(d, indices){
          d2 <- d[indices]
          return(mean(d2, na.rm=TRUE))
        }
     
         bootcorr <- boot(x, stat = func.mean, R=500)
     
      
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


# create a list of densities by each cell:
dens.by.cells <- split(pls.inil$density, pls.inil$cell)

# apply the "boot.calcs" function over all cells:
dens.ci.mean <- lapply( dens.by.cells, FUN = boot.calcs)

dens.ci.mean.df <- do.call(rbind, dens.ci.mean)
dens.ci.mean.df$cell<- row.names(dens.ci.mean.df)
dens.ci.df <- merge(dens.ci.mean.df, pls.mean[,c("x", "y", "cell", "PLSdensity")], by = "cell")

#-------------------- density uncertainty from the FIA data:
FIA <- read.csv('data/FIA_species_plot_parameters_paleongrid.csv')
speciesconversion <- read.csv('data/FIA_conversion-SGD_remove_dups.csv')

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )

# how we would normally calculate density
FIA.by.paleon <- dcast(FIA.pal, x + y+ cell+ plt_cn ~ PalEON, sum, na.rm=TRUE, value.var = 'density') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,6:25], na.rm = TRUE) # sum the total density in each plot
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn', 'Var.5')) # melt the dataframe
fia.by.plot <- dcast(fia.melt, x + y +cell+ plt_cn ~ variable, sum, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell
melted.fia <- melt(fia.by.plot[,c('x', "y", "cell", "plt_cn", "FIAdensity")], id.vars = c('x', "y", "cell", "plt_cn"))

fia.by.cell <- ddply(melted.fia,~ cell,summarise,mean=mean(value),sd=sd(value), x = mean(x), y = mean(y))

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


# plot histograms of density and histograms of the low and high confidence intervals:
png(height = 6, width = 7, units = "in", res = 300, "outputs/density_unc/PLSdensity_hist_inil_with_ci.png")
ggplot()+geom_histogram(data = alldens.m[alldens.m$variable %in% c("PLSdensity"),], aes(value, fill = variable,position = "identity", binwidth = 18))+
  geom_density(data =alldens.m[alldens.m$variable %in% c("PLSdensity", "ci.low", "ci.high"),] ,aes(value, color = variable, 20 *..count.., linetype = variable), size = 1.2)+scale_color_manual(values = c("grey", "grey", "red"))+
  scale_linetype_manual(values=c("dashed", "dotted", "solid"))+theme_bw(base_size = 20)
dev.off()

png(height = 4, width = 7, units = "in", res = 300, "outputs/density_unc/PLS_inil_histograms.png")
ggplot(dens.with.ci[dens.with.ci$variable %in% c("PLSdensity", "ci.low", "ci.high"),], aes(value, color = variable))+geom_histogram(position = "identity",alpha = 0.5)+theme_bw()+xlim(0,600)+facet_wrap(~variable)
dev.off()

dens.with.ci <- alldens.m[complete.cases(alldens.m),]

# plotting the fia data makes less sense because we can only get ci from grid cells with more than 1 fia plot--which are sparse in in & IL
png(height = 6, width = 7, units = "in", res = 300, "outputs/density_unc/FIAdensity_hist_inil_with_ci.png")
ggplot() + geom_histogram(data = dens.with.ci[dens.with.ci$variable %in% c("FIAdensity"),], aes(value, fill = variable,position = "identity", binwidth = 18))+
  geom_density(data = dens.with.ci[dens.with.ci$variable %in% c("FIAdensity", "ci.low.fia", "ci.high.fia"),] ,aes(value, color = variable, 25 *..count.., linetype = variable), size = 1.2)+scale_color_manual(values = c("grey", "grey", "red"))+
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) + theme_bw(base_size = 20)
dev.off()


png(height = 4, width = 7, units = "in", res = 300, "outputs/density_unc/FIA_inil_histograms.png")
ggplot(dens.with.ci[dens.with.ci$variable %in% c("FIAdensity", "ci.low.fia", "ci.high.fia"),], aes(value, color = variable))+geom_histogram(position = "identity",alpha = 0.5)+theme_bw()+xlim(0,600)+facet_wrap(~variable)
dev.off()


# -----------------------map out Indiana and Illinois with the CI intervals:
dens.ci.df$uncertainty <- dens.ci.df$ci.high - dens.ci.df$ci.low
dens.ci.df.m <- melt(dens.ci.df[,c("x", "y", "cell", "PLSdensity", "ci.low", "ci.high")], id.vars = c("x", "y", "cell"))

alldens$uncertainty <- alldens$ci.high.fia - alldens$ci.low.fia
fdens.ci.df.m <- melt(alldens[,c("x", "y", "cell", "FIAdensity", "ci.low.fia", "ci.high.fia")], id.vars = c("x", "y", "cell"))


# need to set up state outlines:
all_states <- map_data("state")
states <- subset(all_states, region %in% c(   "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)


# fia and pls plots
sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")


pls.dens.ci.maps<- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.ci.df.m, aes(x=x, y=y, fill = value))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,600), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 10)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank())+facet_wrap(~variable)#+ annotate("text", x=-90000, y=1486000,label= "A", size = 5)+ggtitle("")
png(height = 5, width = 9, units = "in", res = 300, "outputs/density_unc/map_density_ci_inil_pls.png")
pls.dens.ci.maps
dev.off()

fia.dens.ci.maps<- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=fdens.ci.df.m, aes(x=x, y=y, fill = value))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,600), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw(base_size = 10)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                axis.title.x=element_blank(),
                                                axis.title.y=element_blank())+facet_wrap(~variable)#+ annotate("text", x=-90000, y=1486000,label= "A", size = 5)+ggtitle("")
png(height = 5, width = 9, units = "in", res = 300, "outputs/density_unc/map_density_ci_inil_fia.png")
fia.dens.ci.maps
dev.off()
