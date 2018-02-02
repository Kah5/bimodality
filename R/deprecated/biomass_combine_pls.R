version <- "1.6-5"
setwd( "C:/Users/JMac/Documents/Kelly/biomodality")
library(data.table)
library(reshape2)
library(dtplyr)
library(ggplot2)
library(hexbin)
library(grid)
library(gridExtra)
library(sp)
library(raster)
library(rgdal)

#--------------------------------load data-----------------------------------
# read in pont level data
pls.inil <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_v',version, '.csv'))

# find the mean density by species
#pls.inil <- dcast(pls.inil, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density') # we want to sum the densities of all the species in each cells, then divide by the # of pls points within the cell, so take the avg 
#pls.inil <- pls.inil[, c("x", "y", "cell", ".")] # just keep mean 

#colnames(pls.inil) <- c('x', 'y', 'cell','PLSdensity')
#hist(pls.inil$PLSdensity, xlim = c(0, 600),breaks = 100)


# read in point level data
pls.spec <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_v',version, '.csv'))

# find mean denisty for all species in a grid cell
pls.spec <- dcast(pls.spec, x + y + cell ~spec, mean, na.rm = TRUE, value.var = 'biom')
pls.spec$total <- rowSums(pls.spec[,!names(pls.spec)%in% c("x", "y", "cell", "Water", "wet")], na.rm=TRUE) # sum species density in the grid cell
hist(pls.spec$total, breaks = 50)
pls.new <- pls.spec[,c('x', 'y', 'cell', 'total')]
colnames(pls.new) <- c('x', 'y', 'cell','PLSbiomass')

# read in Uppermidwest data at paleon grid scale
umdw <- read.csv('data/plss_biomass_alb_v0.9-10.csv')
#umdw.mean <- dcast(umdw, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density')
umdw$total <- rowSums(umdw[,4:32], na.rm= TRUE)
umdw.new <- umdw[,c('x', 'y', 'cell', 'total')]


colnames(umdw.new) <- c('x', 'y', 'cell', 'PLSbiomass')
umdw.n <- umdw.new[,c('x', 'y', 'PLSbiomass')]
coordinates(umdw.n) <- ~x+y
gridded(umdw.n) <- TRUE
umdw.rast <- raster(umdw.n)
plot(umdw.rast)
proj4string(umdw.rast) <- '+init=epsg:3175'

writeRaster(umdw.rast, "data/upper_midwest.ascii", overwrite = TRUE)



# -----------rename species and join upper and lower midwest spec. tables----------------

#this is to join the species tables
colnames(pls.spec) <- c("x" ,  "y" , "cell" ,"Alder","Ash",
                        "Bald cypress","Basswood","Beech","Birch", "Black.gum" ,         
                        "Black gum.sweet gum", "Buckeye" , "Cedar.juniper" ,"Cherry" ,"Chestnut" ,          
                        "Dogwood","Elm" , "Hackberry", "Hickory", "Ironwood",    
                        "Locust" ,"Maple" ,"Mulberry" ,"No.tree","Oak",                
                        "Other.hardwood","Pine","Poplar", "Poplar.tulip poplar", "Sweet gum" ,         
                        "Sycamore" ,"Tamarack" ,"Tulip.poplar" ,"Unknown.tree","Walnut" ,            
                        "Water","Wet" ,"Willow","total" )
umdw.names<- colnames(umdw)
pls.names<- colnames(pls.spec)


#create name vectors for species columns missing in the upper and lower midewst
to.add.umdw <-pls.names[!pls.names %in% umdw.names]
to.add.pls <- umdw.names[!umdw.names %in% pls.names]

#add these species columns to the respective dataframes, but with 0 for data values
pls.spec[,to.add.pls] <- 0
umdw[,to.add.umdw] <-0 



#reorder the columns so the pls.spec and umdw dataframes match
pls.spec<- pls.spec[ , order(names(pls.spec))]
umdw <- umdw[,order(names(umdw))]

full.spec <- rbind(pls.spec, umdw)
is.nan.data.frame<- function(x){do.call(cbind, lapply(x, is.nan))}
full.spec[is.nan.data.frame(full.spec)] <- 0
ggplot(full.spec, aes(x,y, fill = total))+geom_raster()

hist(full.spec$total, breaks = 1000, xlim = c(0,600))

# make a biomass histogram:
png('outputs/pls_biomass_full_hist.png')
ggplot(full.spec, aes(total))+geom_histogram(bins = 100)+xlim(-1,600)+ylim(0,600)+xlab("Total Biomass (mg/ha)")
dev.off()


dens.pr <- read.csv("data/PLS_full_dens_pr_with_bins.csv")

compositions <- c( "Alder" ,"Ash","Bald cypress"  ,     
                   "Basswood","Beech","Birch" ,        
                   "Black.gum" , "Black gum.sweet gum" ,"Buckeye" ,           
                   "Cedar.juniper" ,  "Cherry"    ,"Chestnut"  ,         
                   "Dogwood" ,  "Elm", "Fir",         
                   "Hackberry" , "Hemlock", "Hickory",            
                   "Ironwood","Locust", "Maple" ,           
                   "Mulberry" , "No.tree","Oak",                
                   "Other.hardwood", "Pine" ,              
                   "Poplar", "Poplar.tulip poplar", "Spruce" ,            
                   "Sweet gum" , "Sycamore" , "Tamarack"  ,         
                   "Tulip.poplar","Unknown.tree","Walnut" ,            
                   "Willow", "total" )              


full.spec <- merge(full.spec, dens.pr[, c("x","y","cell", "PC1bins", "PC1", "PLSdensity")], by = c("x","y", "cell"))
# make maps for all these compositions
for(i in 1:length(compositions)){
  
  map.comp <- ggplot(full.spec[full.spec$PLSdensity > 99, ], aes(x,y, fill=full.spec[full.spec$PLSdensity > 99,compositions[i]]))+geom_raster()+
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
    labs(x="easting", y="northing") +
    scale_fill_gradientn(colours = rev(terrain.colors(7)), limits = c(0,300), name =paste(compositions[i]," Biomass"), na.value = 'darkgrey')+
    coord_equal()+theme_bw(base_size = 10)+ theme(legend.position = "bottom",axis.line=element_blank(),axis.text.x=element_blank(),
                                                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                  axis.title.x=element_blank(),
                                                  axis.title.y=element_blank())#+facet_wrap(~period)
  
  map.comp
  
  ggsave(filename=paste0("outputs/Biomass/Maps/", compositions[i], "-maps.png"))
}

# make hexbin plots with all these compositions
for(i in 1:length(compositions)){
  
  hex.comp <- ggplot(full.spec[full.spec$PLSdensity > 99,], aes(PC1, full.spec[full.spec$PLSdensity > 99,compositions[i]]))+geom_hex() + 
    theme_bw(base_size = 10)+scale_fill_distiller(palette = "Spectral", limits = c(1,130))+ylim(0,1000)+
    
    xlab('Environmental PC1') +ylab(paste("%", compositions[i]," Biomass"))
  
  hex.comp
  
  ggsave(filename=paste0("outputs/Biomass/hexbin_plots/", compositions[i], "-hex.png"))
}


# make histograms for all these compositions
for(i in 1:length(compositions)){
  
  
  comphist<- ggplot(full.spec[full.spec$PLSdensity > 99,], aes(full.spec[full.spec$PLSdensity > 99,compositions[i]]))+geom_histogram(position = 'identity', alpha = 0.7, bins = 50)+xlim(0,300)+xlab(compositions[i]) # 
  
  comphist
  
  ggsave(filename=paste0("outputs/Biomass/full_hist/", compositions[i], "-hist.png"))
}

# Fix ordering of the PC1_bins factors
full.spec$PC1_bins_f2 <- factor(full.spec$PC1bins, levels = c("5 - 6", "4 - 5", "3 - 4",
                                                                "2 - 3", "1 - 2", "0 - 1", "-1 - 0",
                                                                "-2 - -1", "-3 - -2", "-4 - -3",
                                                                "-5 - -4"))

# plot the composition histograms by environment
for(i in 1:length(compositions)){
  
  
  
  comp.envt.hist <- ggplot(full.spec[full.spec$PLSdensity > 99,], aes(full.spec[full.spec$PLSdensity > 99,compositions[i]]))+geom_histogram(position = 'identity', alpha = 0.7)+xlim(-1,500)+facet_wrap(~PC1_bins_f2, nrow = 3, scales = "free_y")+xlab(paste("% ",compositions[i], " composition"))
  
  comp.envt.hist
  
  ggsave(filename=paste0("outputs/Biomass/envt_hist/", compositions[i], "-hist.png"))
}

