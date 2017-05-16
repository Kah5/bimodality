# This script is looking into the composition and density in bimodal areas 

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
pls.spec <- dcast(pls.spec, x + y + cell ~spec, mean, na.rm = TRUE, value.var = 'density')
pls.spec$total <- rowSums(pls.spec[,!names(pls.spec)%in% c("x", "y", "cell", "Water", "wet")], na.rm=TRUE) # sum species density in the grid cell
hist(pls.spec$total, breaks = 50)
pls.new <- pls.spec[,c('x', 'y', 'cell', 'total')]
colnames(pls.new) <- c('x', 'y', 'cell','PLSdensity')

# read in Uppermidwest data at paleon grid scale
umdw <- read.csv('data/plss_density_alb_v0.9-10.csv')
#umdw.mean <- dcast(umdw, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density')
umdw$total <- rowSums(umdw[,5:32], na.rm= TRUE)
umdw.new <- umdw[,c('x', 'y', 'cell', 'total')]


colnames(umdw.new) <- c('x', 'y', 'cell', 'PLSdensity')
umdw.n <- umdw.new[,c('x', 'y', 'PLSdensity')]
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

#move around the columns
require(dplyr)
full.spec<- full.spec %>%
  dplyr::select(cell, everything())

full.spec<- full.spec %>%
  dplyr::select(y, everything())

full.spec<- full.spec %>%
  dplyr::select(x, everything())

full.spec<- full.spec %>%
  dplyr:: select(X, everything())

full.spec<- full.spec %>%
  dplyr:: select(-total, everything())

#now add totals to the 'total columns
#full.spec$total <- rowSums(full.spec[,4:41], na.rm = TRUE)
summary(full.spec$total)
hist(full.spec$total, breaks = 1000, xlim = c(0,600))

colnames(full.spec)[42] <- 'PLSdensity'



#------------------------------map out pls density for some species----------------

spec.melt <- melt(full.spec[,1:41], id.vars = c("x", "y", "cell"))
ggplot(full.spec, aes(x=x, y=y, fill = Oak))+geom_raster()


cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
r2bpalette <- c('#ca0020',
  '#f4a582',
  '#92c5de',
  '#0571b0')

X11(width = 18, height = 12)
ggplot(spec.melt, aes(x=x, y=y, fill=value))+geom_raster()+coord_equal()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                axis.title.x=element_blank(),
                                                                                axis.title.y=element_blank())+facet_wrap(~variable, ncol = 10, scales = 'free')+scale_fill_gradientn(colours = rev(rainbow(3)))


X11(width = 12)
ggplot(spec.melt, aes(x = value))+geom_histogram(binwidth = 15)+facet_wrap(~variable, ncol=10, scales = 'free')


# -----------------merge with bimodality analysis----------------------------------

dens.pr <- read.csv("outputs/PLS_full_dens_pr_bins_with_bimodality_for_PC1.csv") 
dens.pr <- dens.pr[,c('x','y','cell','ecotype','bimodal', 'classification')]
spec <- merge(full.spec, dens.pr, by = c('x','y','cell'))

summary(spec[spec$bimodal== 'Bimodal',])
summary(spec[spec$bimodal== 'Bimodal',]$classification)

summary(spec[spec$bimodal== 'Bimodal' & spec$classification == "Bimodal Forest",])

# % of places that are bimodal and classified as forests
counts <- summary(spec[spec$bimodal== 'Bimodal',]$classification)
pcts <- (counts/1560)*100

#Bimodal Forest Bimodal Savanna         Prairie   Stable Forest  Stable Savanna 
#35.641026       59.871795        4.487179        0.000000        0.000000



#---------------------------look at the composition of bimodal forests-----------------

bi.forest <- spec[spec$bimodal== 'Bimodal' & spec$classification == "Bimodal Forest",]


ggplot(bi.forest, aes(x=x, y=y, fill = Oak))+geom_raster()
ggplot(bi.forest, aes(Oak))+geom_density(position = fill)
ggplot(bi.forest, aes(Maple))+geom_density()
ggplot(bi.forest, aes(Beech))+geom_density()
ggplot(bi.forest, aes(Hemlock))+geom_density()
ggplot(bi.forest, aes(Hickory))+geom_density()
ggplot(bi.forest, aes(Basswood))+geom_density()
ggplot(bi.forest, aes(Ironwood))+geom_density()
#bimodal <- spec[spec$bimodal== 'Bimodal' & !spec$PLSdensity == 0,]

# remove water, wet, no tree
bi.forest<- bi.forest[,!names(bi.forest) %in% c("Water", "Wet", "No.tree")]

f.melt <- melt(bi.forest, id.vars = c('x', 'y', 'cell', 'bimodal', 'classification', 'PLSdensity','ecotype'))

ggplot(f.melt, aes(x = value, fill = variable))+geom_density(position = 'fill')
ggplot(f.melt[f.melt$value > 0,], aes(x = value, fill = variable))+geom_histogram()



# caluclate the % of total denisty that the taxa makes up of the grid cell:

comps <- bi.forest
comps[,4:38] <- comps[,4:38]/comps[,39] # calculate the proportion of the total density that each species takes up
comp.melt <- melt(comps, id.vars = c('x', 'y', 'cell', 'bimodal', 'classification', 'PLSdensity','ecotype'))
#ggplot(comp.melt, aes(x = value, fill = variable))+geom_density(position = 'fill')
#ggplot(comp.melt[comp.melt$value > 0,], aes(x = value, fill = variable))+geom_histogram()

#X11(width = 12)
#ggplot(comp.melt, aes(x = x,y=y, fill = value))+geom_raster()+facet_wrap(~variable, ncol = 10) + scale_fill_gradient(low= 'blue', high='red')


# find the grid cell with highest % of denisty in all bimodal places
comps[is.na(comps)]<- 0
comps$highest <- colnames(comps[,4:38])[max.col(comps[,4:38],ties.method="first")]
taxa <- unique(comps$highest)


highest <- comps[names(comps) %in% taxa] 
highest$x <- comps$x
highest$y <- comps$y
highest$highest <- comps$highest

# plot with colors by mesophytic vs non mesophytic:
# blues => Oak, hickory, pines
# reds = > Maple, elm, Basswood, Beech, blackgum.sweet gum, Black.gum, Buckeye
# yellows => spruce, tamarack
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)



forest <- ggplot(highest, aes(x=x, y=y, fill = highest)) + geom_raster() + scale_fill_manual(values = c(
  "skyblue", "royalblue", "blue", 'forestgreen',"darkred", "red", "pink", "salmon", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", 'red','red','red','red',
  "yellow", "yellow", "yellow", "yellow", "yellow", "yellow"
  
), limits = c('Oak',"Hickory",'Pine', 'Walnut',"Maple", "Basswood", "Beech", "black gum.sweet gum", 
              'Black.gum', "Buckeye", "Cherry", 'Dogwood', "Elm", "Hackberry", 'Ironwood', "Alder", "Ash", "Locust", "Mulberry", "Other.hardwood",
              'Sweet gum', "Poplar", 'Poplar.tulip poplar', "Spruce", "Sycamore", "Tamarack", "Willow") )+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                    axis.text.y=element_blank(),axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank())+
  xlab("easting") + ylab("northing") +coord_equal()




#---------------------------look at the composition of bimodal savannas -----------------

bi.savanna <- spec[spec$bimodal== 'Bimodal' & spec$classification == "Bimodal Savanna",]


ggplot(bi.savanna, aes(x=x, y=y, fill = Oak))+geom_raster()
#ggplot(bi.savanna, aes(Oak))+geom_density(position = fill)
ggplot(bi.savanna, aes(Maple))+geom_density()
ggplot(bi.savanna, aes(Beech))+geom_density()
ggplot(bi.savanna, aes(Hemlock))+geom_density()
ggplot(bi.savanna, aes(Hickory))+geom_density()
ggplot(bi.savanna, aes(Basswood))+geom_density()
ggplot(bi.savanna, aes(Ironwood))+geom_density()
#bimodal <- spec[spec$bimodal== 'Bimodal' & !spec$PLSdensity == 0,]

# remove water, wet, no tree
bi.savanna<- bi.savanna[,!names(bi.savanna) %in% c("Water", "Wet", "No.tree")]

p.melt <- melt(bi.savanna, id.vars = c('x', 'y', 'cell', 'bimodal', 'classification', 'PLSdensity','ecotype'))

ggplot(p.melt, aes(x = value, fill = variable))+geom_density(position = 'fill')
ggplot(p.melt[p.melt$value > 0,], aes(x = value, fill = variable))+geom_histogram()



# caluclate the % of total denisty that the taxa makes up of the grid cell:

compss <- bi.savanna
compss[,4:38] <- compss[,4:38]/compss[,39] # calculate the proportion of the total density that each species takes up
comp.melt <- melt(compss, id.vars = c('x', 'y', 'cell', 'bimodal', 'classification', 'PLSdensity','ecotype'))
#ggplot(comp.melt, aes(x = value, fill = variable))+geom_density(position = 'fill')
#ggplot(comp.melt[comp.melt$value > 0,], aes(x = value, fill = variable))+geom_histogram()

#X11(width = 12)
#ggplot(comp.melt, aes(x = x,y=y, fill = value))+geom_raster()+facet_wrap(~variable, ncol = 10) + scale_fill_gradient(low= 'blue', high='red')


# find the grid cell with highest % of denisty in all bimodal places
compss[is.na(compss)]<- 0
compss$highest <- colnames(compss[,4:38])[max.col(compss[,4:38],ties.method="first")]
taxa <- unique(compss$highest)


highest <- compss[names(compss) %in% taxa] 
highest$x <- compss$x
highest$y <- compss$y
highest$highest <- compss$highest

# plot with colors by mesophytic vs non mesophytic:
# blues => Oak, hickory, pines
# reds = > Maple, elm, Basswood, Beech, blackgum.sweet gum, Black.gum, Buckeye
# yellows => spruce, tamarack

savanna <- ggplot(highest, aes(x=x, y=y, fill = highest)) + geom_raster() + scale_fill_manual(values = c(
  "skyblue", "royalblue", "blue", 'forestgreen',"darkred", "red", "pink", "salmon", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", 'red','red','red','red',
  "yellow", "yellow", "yellow", "yellow", "yellow", "yellow"
  
), limits = c('Oak',"Hickory",'Pine', 'Walnut',"Maple", "Basswood", "Beech", "black gum.sweet gum", 
              'Black.gum', "Buckeye", "Cherry", 'Dogwood', "Elm", "Hackberry", 'Ironwood', "Alder", "Ash", "Locust", "Mulberry", "Other.hardwood",
              'Sweet gum', "Poplar", 'Poplar.tulip poplar', "Spruce", "Sycamore", "Tamarack", "Willow") )+
  theme_bw()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                    axis.text.y=element_blank(),axis.ticks=element_blank(),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank())+
  xlab("easting") + ylab("northing") +coord_equal()


#---------------------------look at the composition of all the places -----------------

all <- spec


ggplot(all, aes(x=x, y=y, fill = Oak))+geom_raster()
#ggplot(all, aes(Oak))+geom_density(position = fill)
ggplot(all, aes(Maple))+geom_density()
ggplot(all, aes(Beech))+geom_density()
ggplot(all, aes(Hemlock))+geom_density()
ggplot(all, aes(Hickory))+geom_density()
ggplot(all, aes(Basswood))+geom_density()
ggplot(all, aes(Ironwood))+geom_density()
#bimodal <- spec[spec$bimodal== 'Bimodal' & !spec$PLSdensity == 0,]

# remove water, wet, no tree
all<- all[,!names(all) %in% c("Water", "Wet", "No.tree")]

p.melt <- melt(all, id.vars = c('x', 'y', 'cell', 'bimodal', 'classification', 'PLSdensity','ecotype'))

ggplot(p.melt, aes(x = value, fill = variable))+geom_density(position = 'fill')
ggplot(p.melt[p.melt$value > 0,], aes(x = value, fill = variable))+geom_histogram()



# caluclate the % of total denisty that the taxa makes up of the grid cell:

compss <- all
compss[,4:38] <- compss[,4:38]/compss[,39] # calculate the proportion of the total density that each species takes up
comp.melt <- melt(compss, id.vars = c('x', 'y', 'cell', 'bimodal', 'classification', 'PLSdensity','ecotype'))
#ggplot(comp.melt, aes(x = value, fill = variable))+geom_density(position = 'fill')
#ggplot(comp.melt[comp.melt$value > 0,], aes(x = value, fill = variable))+geom_histogram()

#X11(width = 12)
#ggplot(comp.melt, aes(x = x,y=y, fill = value))+geom_raster()+facet_wrap(~variable, ncol = 10) + scale_fill_gradient(low= 'blue', high='red')


# find the grid cell with highest % of denisty in all bimodal places
compss[is.na(compss)]<- 0
compss$highest <- colnames(compss[,4:38])[max.col(compss[,4:38],ties.method="first")]
taxa <- unique(compss$highest)


highest <- compss[names(compss) %in% taxa] 
highest$x <- compss$x
highest$y <- compss$y
highest$highest <- compss$highest

# plot with colors by mesophytic vs non mesophytic:
# blues => Oak, hickory, pines
# reds = > Maple, elm, Basswood, Beech, blackgum.sweet gum, Black.gum, Buckeye
# yellows => spruce, tamarack

all <- ggplot(highest, aes(x=x, y=y, fill = highest)) + geom_raster() + scale_fill_manual(values = c(
  "skyblue", "royalblue", "blue", 'cyan4','forestgreen',"darkred", "red", "pink", "coral", 'chocolate1',"red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red",'red','red','red','red',
  "yellow", "yellow", "yellow", "yellow","yellow", "yellow", "yellow", "yellow"
  
), limits = c('Oak',"Hickory",'Pine', 'Walnut',"Cedar.juniper","Maple", "Basswood", "Beech","Hemlock", 'Birch',"black gum.sweet gum", 
              'Black.gum', "Buckeye", "Cherry", 'Dogwood', "Elm", "Hackberry", "Chestnut",'Ironwood', "Alder", "Ash", "Locust", "Mulberry", "Other.hardwood","Unknown.tree",
              'Sweet gum', "Poplar", 'Poplar.tulip poplar', "Tulip.poplar" ,"Spruce", "Sycamore", "Tamarack", "Willow") )+
  theme_bw()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank())+
  xlab("easting") + ylab("northing") +coord_equal()

X11(width =12)
a <- all + ggtitle('All grid cells (species with highest density)')
b <- savanna + ggtitle('Bimodal savanna (species with highest density)')
c <- forest + ggtitle('Bimodal forest (species with highest density)')

source("R/grid_arrange_shared_legend.R")

png(height = 4, width = 18, units = "in", res = 300, "outputs/v1.6-5/full/density_highest_species.png")
grid_arrange_shared_legend(a,b,c, nrow = 1, ncol = 3)
dev.off()


#-------------------Lets look at FIA now-------------------------------
FIA <- read.csv('data/FIA_species_plot_parameters_paleongrid.csv')
speciesconversion <- read.csv('data/FIA_conversion-SGD_remove_dups.csv')

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )
FIA.by.paleon <- dcast(FIA.pal, x + y+ cell+ plt_cn ~ PalEON, sum, na.rm=TRUE, value.var = 'density') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,6:25], na.rm = TRUE) # sum the total density in each plot
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn', 'Var.5')) # melt the dataframe
fia.by.cell <- dcast(fia.melt, x + y+ cell ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell


# grid cells with PLS 
pls.cells <- spec[,c('x','y','cell','bimodal','classification')]
fia.inpls<- merge(pls.cells, fia.by.cell, by = c('x','y','cell'))
ggplot(fia.inpls, aes(x=x,y=y, fill=Oak))+geom_raster()



# -------------------look at all species in fia----------------------
all <- fia.inpls


# caluclate the % of total denisty that the taxa makes up of the grid cell:

compss <- all
compss[,6:25] <- compss[,6:25]/compss[,26] # calculate the proportion of the total density that each species takes up
comp.melt <- melt(compss, id.vars = c('x', 'y', 'cell', 'bimodal', 'classification', 'FIAdensity'))

# find the grid cell with highest % of denisty in all bimodal places
compss[is.na(compss)]<- 0
compss$highest <- colnames(compss[,6:25])[max.col(compss[,6:25],ties.method="first")]
taxa <- unique(compss$highest)


highest <- compss[names(compss) %in% taxa] 
highest$x <- compss$x
highest$y <- compss$y
highest$highest <- compss$highest

# plot with colors by mesophytic vs non mesophytic:
# blues => Oak, hickory, pines
# reds = > Maple, elm, Basswood, Beech, blackgum.sweet gum, Black.gum, Buckeye
# yellows => spruce, tamarack

allf <- ggplot(highest, aes(x=x, y=y, fill = highest)) + geom_raster() + scale_fill_manual(values = c(
  "skyblue", "royalblue", "blue", 'cyan4','forestgreen',"darkred", "red", "pink", "coral", 'chocolate1',"red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red",'red','red','red','red',
  "yellow", "yellow", "yellow", "yellow","yellow", "yellow", "yellow", "yellow"
  
), limits = c('Oak',"Hickory",'Pine', 'Walnut',"Cedar.juniper","Maple", "Basswood", "Beech","Hemlock", 'Birch',"black gum.sweet gum", 
              'Black.gum', "Buckeye", "Cherry", 'Dogwood', "Elm", "Hackberry", "Chestnut",'Ironwood', "Alder", "Ash", "Locust", "Mulberry", "Other.hardwood","Unknown.tree",
              'Sweet gum', "Poplar", 'Poplar.tulip poplar', "Tulip.poplar" ,"Spruce", "Sycamore", "Tamarack", "Willow") )+
  theme_bw()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank())+
  xlab("easting") + ylab("northing") +coord_equal()


# -------------------look at all species in historical bimodal forests -----------------------
all <- fia.inpls[fia.inpls$classification== "Bimodal Forest",]


# caluclate the % of total denisty that the taxa makes up of the grid cell:

compss <- all
compss[,6:25] <- compss[,6:25]/compss[,26] # calculate the proportion of the total density that each species takes up
comp.melt <- melt(compss, id.vars = c('x', 'y', 'cell', 'bimodal', 'classification', 'FIAdensity'))

# find the grid cell with highest % of denisty in all bimodal places
compss[is.na(compss)]<- 0
compss$highest <- colnames(compss[,6:25])[max.col(compss[,6:25],ties.method="first")]
taxa <- unique(compss$highest)


highest <- compss[names(compss) %in% taxa] 
highest$x <- compss$x
highest$y <- compss$y
highest$highest <- compss$highest

# plot with colors by mesophytic vs non mesophytic:
# blues => Oak, hickory, pines
# reds = > Maple, elm, Basswood, Beech, blackgum.sweet gum, Black.gum, Buckeye
# yellows => spruce, tamarack

bif <- ggplot(highest, aes(x=x, y=y, fill = highest)) + geom_raster() + scale_fill_manual(values = c(
  "skyblue", "royalblue", "blue", 'cyan4','forestgreen',"darkred", "red", "pink", "coral", 'chocolate1',"red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red",'red','red','red','red',
  "yellow", "yellow", "yellow", "yellow","yellow", "yellow", "yellow", "yellow"
  
), limits = c('Oak',"Hickory",'Pine', 'Walnut',"Cedar.juniper","Maple", "Basswood", "Beech","Hemlock", 'Birch',"black gum.sweet gum", 
              'Black.gum', "Buckeye", "Cherry", 'Dogwood', "Elm", "Hackberry", "Chestnut",'Ironwood', "Alder", "Ash", "Locust", "Mulberry", "Other.hardwood","Unknown.tree",
              'Sweet gum', "Poplar", 'Poplar.tulip poplar', "Tulip.poplar" ,"Spruce", "Sycamore", "Tamarack", "Willow") )+
  theme_bw()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank())+
  xlab("easting") + ylab("northing") +coord_equal()

# -------------------look at all species in historical bimodal forests -----------------------
all <- fia.inpls[fia.inpls$classification== "Bimodal Savanna",]


# caluclate the % of total denisty that the taxa makes up of the grid cell:

compss <- all
compss[,6:25] <- compss[,6:25]/compss[,26] # calculate the proportion of the total density that each species takes up
comp.melt <- melt(compss, id.vars = c('x', 'y', 'cell', 'bimodal', 'classification', 'FIAdensity'))

# find the grid cell with highest % of denisty in all bimodal places
compss[is.na(compss)]<- 0
compss$highest <- colnames(compss[,6:25])[max.col(compss[,6:25],ties.method="first")]
taxa <- unique(compss$highest)


highest <- compss[names(compss) %in% taxa] 
highest$x <- compss$x
highest$y <- compss$y
highest$highest <- compss$highest

# plot with colors by mesophytic vs non mesophytic:
# blues => Oak, hickory, pines
# reds = > Maple, elm, Basswood, Beech, blackgum.sweet gum, Black.gum, Buckeye
# yellows => spruce, tamarack

sif <- ggplot(highest, aes(x=x, y=y, fill = highest)) + geom_raster() + scale_fill_manual(values = c(
  "skyblue", "royalblue", "blue", 'cyan4','forestgreen',"darkred", "red", "pink", "coral", 'chocolate1',"red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red", "red",'red','red','red','red',
  "yellow", "yellow", "yellow", "yellow","yellow", "yellow", "yellow", "yellow"
  
), limits = c('Oak',"Hickory",'Pine', 'Walnut',"Cedar.juniper","Maple", "Basswood", "Beech","Hemlock", 'Birch',"black gum.sweet gum", 
              'Black.gum', "Buckeye", "Cherry", 'Dogwood', "Elm", "Hackberry", "Chestnut",'Ironwood', "Alder", "Ash", "Locust", "Mulberry", "Other.hardwood","Unknown.tree",
              'Sweet gum', "Poplar", 'Poplar.tulip poplar', "Tulip.poplar" ,"Spruce", "Sycamore", "Tamarack", "Willow") )+
  theme_bw()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank())+
  xlab("easting") + ylab("northing") +coord_equal()

# plot fia
X11(width =12)
d <- allf + ggtitle('All grid cells (species with highest density) FIA')
e <- sif + ggtitle('Bimodal savanna (species with highest density) FIA')
f <- bif + ggtitle('Bimodal forest (species with highest density) FIA')

source("R/grid_arrange_shared_legend.R")

png(height = 4, width = 18, units = "in", res = 300, "outputs/v1.6-5/full/density_highest_species_FIA.png")
grid_arrange_shared_legend(d,e,f, nrow = 1, ncol = 3)
dev.off()


#--------------------------------species density PCA------------------------
# note this is working with density, but I think I want species composition
full.spec[is.na(full.spec)]<- 0


drops <- c("x","y", 'cell', "No.tree", "Water", "Wet", "PLSdensity")
# need to remove the no tree density estimates, water, and wet from the df
scale.dens <- scale(full.spec[,!(names(full.spec)) %in% drops]) #PC all but ksat and diff
#dens.dens <- dens.rm[, c('PLSdensity')] # pls density

# apply PCA - scale. = TRUE 
#dens.pca <- princomp(scale.dens) # the scaled dataset doesnt work

dens.pca <- princomp(scale.dens) 
plot(dens.pca)

biplot(dens.pca)


#dens.rm$PC1 <- dens.pca[,1]
#dens.rm$PC2 <- scores[,2]
#loadings <-dens.pca$rotation
#head(loadings)


#output for prcomp
#outputPCA <- list(summary(dens.pca), loadings)
#outputPCA


#scores
#scores <- dens.pca$x
#head(scores,10)

# add pc1 and pc2 to df
df <- full.spec
df$pc1 <- scores[,1]
df$pc2 <- scores[,2]

ggplot(df, aes(x=pc1, y = pc2, color = PLSdensity))+geom_point()
ggplot(df, aes(x=x, y=y, fill = pc1))+geom_raster()

plot(dens.pca, type = "l")
print(dens.pca)


# using ggbiplot
library(ggbiplot)
source("R/newggbiplot.R")



g <- newggbiplot(dens.pca, obs.scale = 1, var.scale = 1, labels.size
                 = 25,alpha = 0,color = "blue",  alpha_arrow = 1, line.size = 1.5, scale = TRUE)

# this plot looks like it has not been scaled
g + ylim(-3, 1)+xlim(-3, 1)


#----------------------cluster analysis---------------------------------
# we want to cluster