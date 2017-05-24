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
ggplot(full.spec, aes(x=x, y=y, fill = Beech))+geom_raster()


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
ggplot(spec.melt, aes(x = value))+geom_histogram(binwidth = 15)+facet_wrap(~variable, ncol=10, scales = 'free<- ')
full.spec[is.na(full.spec)]<- 0
density.full <- full.spec
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

#--------------------------------species composition PCA------------------------
# read in the data of the counts of species in each grid cell
comp.inil <- read.csv("data/outputs/plss_inil_composition.csv_v1.csv")
comp.inil <- comp.inil[!names(comp.inil) %in% c("X","Water", "Wet")] # get rid of water and wet columns

comp.umw <-read.csv("data/plss_composition_alb_v0.9-10.csv")

colnames(comp.inil) <- c("x" ,  "y" , "cell" ,"Alder","Ash",
                        "Bald cypress","Basswood","Beech","Birch", "Black.gum" ,         
                        "Black gum.sweet gum", "Buckeye" , "Cedar.juniper" ,"Cherry" ,"Chestnut" ,          
                        "Dogwood","Elm" , "Hackberry", "Hickory", "Ironwood",    
                        "Locust" ,"Maple" ,"Mulberry" ,"No.tree","Oak",                
                        "Other.hardwood","Pine","Poplar", "Poplar.tulip poplar", "Sweet gum" ,         
                        "Sycamore" ,"Tamarack" ,"Tulip.poplar" ,"Unknown.tree","Walnut" ,            
                         "Willow" )
umdw.names<- colnames(comp.umw)
pls.names<- colnames(comp.inil)


#create name vectors for species columns missing in the upper and lower midewst
to.add.umdw <-pls.names[!pls.names %in% umdw.names]
to.add.pls <- umdw.names[!umdw.names %in% pls.names]

#add these species columns to the respective dataframes, but with 0 for data values
comp.inil[,to.add.pls] <- 0
comp.umw[,to.add.umdw] <-0 



#reorder the columns so the comp.inil and comp.umw dataframes match
comp.inil<- comp.inil[ , order(names(comp.inil))]
comp.umw <- comp.umw[,order(names(comp.umw))]

full.spec <- rbind(comp.inil, comp.umw)

#move around the columns
require(dplyr)
full.spec<- full.spec %>%
  dplyr::select(cell, everything())

full.spec<- full.spec %>%
  dplyr::select(y, everything())

full.spec<- full.spec %>%
  dplyr::select(x, everything())

write.csv(full.spec, "outputs/full_midwest_composition.csv")


# note this is working with density, but I think I want species composition
full.spec[is.na(full.spec)]<- 0


drops <- c("x","y", 'cell', "No.tree", "Water", "Wet", "PLSdensity")

# plot out composition of the full datasets
ggplot(full.spec, aes(x=x, y=y, fill = Oak)) + geom_raster()
ggplot(full.spec, aes(x=x, y=y, fill = Beech)) + geom_raster()

# melt the dataframe:
comp.m <- melt(full.spec, id.vars = c("x", "y", "cell"))

# plot all the species composition at once:
X11(width = 12)
ggplot(comp.m, aes(x = x, y = y, fill = value))+geom_raster()+coord_equal()+theme_bw()+scale_fill_gradientn(colors = rev(terrain.colors(5)))+facet_wrap(~ variable, ncol=9)+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                                                                                                                                                                                             axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                                                                                                                                                                             axis.title.x=element_blank(),
                                                                                                                                                                                                                                                                             axis.title.y=element_blank())+
  xlab("easting") + ylab("northing")




# need to remove the no tree density estimates, water, and wet from the df
scale.dens <- scale(full.spec[,!(names(full.spec)) %in% drops]) #PC all but ksat and diff
#dens.dens <- dens.rm[, c('PLSdensity')] # pls density


# apply PCA - scale. = TRUE 
#dens.pca <- princomp(scale.dens) # the scaled dataset doesnt work

dens.pca <- princomp(scale.dens, scale = TRUE) 
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
scores <- dens.pca$scores

#head(scores,10)

# add pc1 and pc2 to df
df <- full.spec
df$comp.pc1 <- scores[,1]
df$comp.pc2 <- scores[,2]

#ggplot(df, aes(x=pc1, y = pc2, color = PLSdensity))+geom_point()
#ggplot(df, aes(x=x, y=y, fill = pc1))+geom_raster()

plot(dens.pca, type = "l")
print(dens.pca)


# using ggbiplot
library(ggbiplot)
source("R/newggbiplot.R")
source("R/ggbiplot2.R")
p<- ggbiplot(dens.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)

# I dont know why the background points disappear with ggbiplot2--I didnt change that part
g <- ggbiplot2(dens.pca, obs.scale = 1, var.scale = 1, labels.size
                 = 25, alpha = 0, color = "blue",  alpha_arrow = 1, line.size = 1.5, scale = TRUE)

g

png("outputs/pls_counts_PCA_nopoints.png")
g
dev.off()

png("outputs/pls_counts_PCA_with_points.png")
p
dev.off()

# now add the scores to the overall pls bimodality data and see if these PCA scores result in bimodality?
dens.pr <- read.csv("outputs/PLS_full_dens_pr_bins_with_bimodality_for_PC1.csv") 
dens.pr <- merge(dens.pr, df[,c("x", "y", "cell", "comp.pc1", "comp.pc2")])

ggplot(dens.pr, aes(x = x, y=y, fill= comp.pc1))+geom_raster()
ggplot(dens.pr, aes(x = x, y=y, fill= comp.pc2))+geom_raster()

ggplot(dens.pr, aes(x = comp.pc1, y=PLSdensity))+geom_point()
ggplot(dens.pr, aes(x = comp.pc2, y=PLSdensity))+geom_point()

ggplot(dens.pr, aes(x = MAP1910, y=comp.pc1, color = bimodal))+geom_point()
ggplot(dens.pr, aes(x = PC1, y=comp.pc1,  color = bimodal))+geom_point()
ggplot(dens.pr, aes(x = PC1, y=comp.pc2,  color = bimodal))+geom_point()

write.csv(dens.pr, "outputs/PLS_full_dens_with_species_comp_PCvals.csv")
#----------------------cluster analysis---------------------------------
# we want to cluster the data based on % species composition: based on tree density, not the counts
# using clusters similar to simons mediod clustering scheme: 
library(cluster)
library(fpc)

comps <- density.full[!names(density.full) %in% c("Water", "Wet", "No Tree")]
#comps <- comps[!is.na(comps),]
comps[,4:39] <- comps[,4:39]/comps[,40] # calculate the proportion of the total density that each species takes up
comps <- comps[,1:39]

# remove prairie cells:
comps <- data.frame(comps[complete.cases(comps),])
# write as a csv so we don't have to keep doing this:
write.csv(comps, "data/outputs/plss_pct_density_composition_v1.6.csv")


classes.3 <- pam(comps[,4:ncol(comps)], k = 3)
classes.4 <- pam(comps[,4:ncol(comps)], k = 4)
classes.5 <- pam(comps[,4:ncol(comps)], k = 5)
classes.6 <- pam(comps[,4:ncol(comps)], k = 6)
classes.7 <- pam(comps[,4:ncol(comps)], k = 7)
classes.8 <- pam(comps[,4:ncol(comps)], k = 8)

plot(classes.8)
plot(classes.7)
plot(classes.6)
plot(classes.5)
plot(classes.4)
plot(classes.3)

#summary(classes.8) # Avg. Silhouette width = 
summary(classes.7) # Avg. Silhouette width = 0.2506271
summary(classes.6) # Avg. Silhouette width = 0.2677659
summary(classes.5) # Avg. Silhouette width = 0.2610493
summary(classes.4) # Avg. Silhouette width = 0.2006347
summary(classes.3) # Avg. Silhouette width = 0.2393605



# 5 classes:


mediods <- comps$cell [classes.5$id.med]


df5 <- comps[comps$cell %in% mediods,] # look at the rows that have the mediods

old_classes <- classes.5
#[1] 49221 29369 17193 16954 11274# mediods
rem_class5 <- factor(old_classes$clustering,
                    labels=c('Maple/Elm/Hickory/Oak/Basswood', # 1,
                             'Oak', # 2
                             'Poplar',#3
                             "Pine/Tamarack/Poplar/Spruce/Birch", # 4,
                             'PineTamarack' 
                            
                             
                    ))

clust_plot5 <- data.frame(comps, 
                         cluster = rem_class5,
                         clustNum = as.numeric(rem_class5))

ggplot(clust_plot5, aes(x = x, y=y, fill=cluster))+geom_raster()




# 6 classes
mediods <- comps$cell [classes.6$id.med]
#mediods
#[1] 35637 29369 20805 19885  7144 19029

df6 <- comps[comps$cell %in% mediods,] # look at the rows that have the mediods
write.csv(df, "outputs/species_comp_clusters_6_class_mediods.csv")

old_classes <- classes.6
rem_class <- factor(old_classes$clustering,
                   labels=c('Elm/Maple/Hickory/Oak/Beech', # 1,
                            'Oak', # 2
                            'Hemlock/Beech/Cedar/Birch/Maple',#3
                            #'Oak/Poplar/Basswood/Maple',
                            "Poplar/Oak", # 4,
                            'Tamarack/Spruce/Birch/Pine/Spruce/Poplar', #5,
                            'Pine/Tamarack/Poplar' #6
                           
                            
                       ))

clust_plot6 <- data.frame(comps, 
                         speciescluster = rem_class,
                         clustNum = as.numeric(rem_class))


# merge the clusters with denisty estimates:
dens <- merge(dens.pr, clust_plot6[,c('x', "y", "cell", "speciescluster", "clustNum")], by = c("x","y","cell"),keep = all)

write.csv(dens, "outputs/cluster/density_pls_with_clusters.csv")

# map out the clusters in space:
png(width = 6, height = 6, units= 'in',res=300,"outputs/cluster/six_cluster_map.png")
ggplot(clust_plot6, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank())+xlab("easting") + ylab("northing") +coord_equal()
dev.off()

ggplot(dens, aes(x = MAP1910, y=comp.pc1, color = speciescluster))+geom_point(size = 0.5)

# plot by bimodality
bi <- ggplot(dens, aes(x = PC1, y=comp.pc1,  color = bimodal))+geom_point(size = 1)+theme_bw()+ylab("Species Composition PC1")+xlab("Environmental Space PC1")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(),
        legend.key.size = unit(2,'lines'), legend.position = c(0.205, 0.125)
        ,legend.background = element_rect(fill=alpha('transparent', 0.4)),panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))+ annotate("text", x=-4, y=4,label= "A", size = 5)

# plot by clusters
cl <-ggplot(dens, aes(x = PC1, y=comp.pc1,  color = speciescluster))+geom_point(size = 1)+theme_bw()+ylab("Species Composition PC1")+xlab("Environmental Space PC1")+
  theme(axis.text = element_blank(), axis.ticks=element_blank(), legend.key.size = unit(2,'lines'), legend.position = c(0.205, 0.325) ,legend.background = element_rect(fill=alpha('transparent', 0.4)),panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))+ annotate("text", x=-4, y=4,label= "B", size = 5)

png(width = 8, height =10, units= 'in', res= 300, "outputs/cluster/speciescluster_vs_environment.png")
grid.arrange(arrangeGrob(bi, cl, heights=c(1/2, 1/2), widths=c(8), ncol=1))
dev.off()

#with pc2
bi2 <- ggplot(dens, aes(x = PC1, y=comp.pc2,  color = bimodal))+geom_point(size = 0.8)+theme_bw()+ylab("Species Composition PC2")+xlab("Environmental Space PC1")
cl2 <-ggplot(dens, aes(x = PC1, y=comp.pc2,  color = speciescluster))+geom_point(size = 0.8)+theme_bw()+ylab("Species Composition PC2")+xlab("Environmental Space PC1")


#---------------------Ordination of the species composition data---------------------
#NMDS:
library(vegan)
comps <- read.csv("data/outputs/plss_pct_density_composition_v1.6.csv")
comps<- as.matrix(comps[5:ncol(comps)])
NMDS <- metaMDS(comps[1:100,],distance = "bray",k=2)


NMDS 

stressplot(NMDS)

plot(NMDS)

ordiplot(NMDS,type="n")
orditorp(NMDS,display="species",col="red",air=0.01)
#orditorp(NMDS,display="sites",cex=1.25,air=0.01)
variableScores <- NMDS$species
sampleScores <- NMDS$points


#-----------------------PCA of full dataset----------------------
dens.pca <- princomp(comps) 
plot(dens.pca)

biplot(dens.pca)

library(ggbiplot)
source("R/newggbiplot.R")

ggbiplot(dens.pca, pc.biplot = TRUE)

g <- newggbiplot(dens.pca, obs.scale = 1, var.scale = 1, labels.size
                 = 25,alpha = 0,color = "blue",  alpha_arrow = 1, line.size = 1.5, scale = TRUE)

g
