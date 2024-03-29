# combine_umw_pls_fia.R
# this script takes density estimates by species from upper midwest (previously compiled + estimated by Goring et al. 2016),
# adds on the density from Indiana and Illinois, as well as southern michigan


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

###############################################################################################
#                        PLS density and composition combining data
###############################################################################################


#........................................PLS density data.......................................


# read in pont level density data
#pls.inil <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_inilmi','_v',version, '.csv'))
#pls.inil <- read.csv(paste0('outputs/density_biomass_pointwise.ests_inilmi_v1.7-5.csv'))
pls.inil <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_full_MW_v1.7-5.csv'))
pls.inil <- pls.inil[!is.na(pls.inil$spec),]


# find the mean species density for each grid cell, then 

# find mean denisty for all species in a grid cell
pls.spec <- dcast(pls.inil, x + y + cell ~ spec, sum, na.rm = TRUE, value.var = 'density')

## This is to normalize.  Diameters need to be averaged by the tree number, otherwise, by
##  the point number.
count.table <- dcast(pls.inil, x + y + cell ~ spec, sum, na.rm = TRUE, value.var = 'count')
points.by.cell <- rowSums(count.table[,4:ncol(count.table)], na.rm = TRUE)
trees.by.cell  <- rowSums(count.table[,!colnames(count.table) %in% c('x', 'y', 'cell', 'No tree', 'Water')], na.rm = TRUE)

#  The function averages the estimates to a point level estimate from the aggregated sum.
#  Why is the multiplier * 2? Because there are two trees per point and we would underestimate otherwise.
normalize <- function(x, mult = 2, value = points.by.cell) {
  x[,4:ncol(x)] <-  x[,4:ncol(x)] / value * mult
  x
}

pls.spec <- normalize(pls.spec)

pls.spec$PLSdensity <- rowSums(pls.spec[,!names(pls.spec)%in% c("x", "y", "cell", "Water", "wet", "No tree")], na.rm=TRUE) # sum species density in the grid cell
hist(pls.spec$PLSdensity, breaks = 50)
pls.new <- pls.spec[,c('x', 'y', 'cell', 'PLSdensity')]
colnames(pls.new) <- c('x', 'y', 'cell','PLSdensity')

#keep only the 99th percentile of densitys---this is also what simon does
#nine.nine.pct <- quantile(pls.spec$PLSdensity, probs = 0.995, na.rm=TRUE )
#pls.spec$PLSdensity[pls.spec$PLSdensity > nine.nine.pct['99.5%']] <- nine.nine.pct['99.5%']

summary(pls.spec)

# -----------------------read in Uppermidwest data at paleon grid scale
umdw <- read.csv('data/plss_density_alb_v0.9-10.csv')
#umdw.mean <- dcast(umdw, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density')
umdw$PLSdensity <- rowSums(umdw[,4:32], na.rm= TRUE)
umdw.new <- umdw[,c('x', 'y', 'cell', "PLSdensity")]
colnames(umdw.new) <- c('x', 'y', 'cell', 'PLSdensity')

# combine density data: 
#densitys <- rbind(pls.new[,c("x", "y", "cell", "PLSdensity")], umdw.new)
densitys <- pls.spec

#note that for some reason, 1 grid cell is duplicated
nodups <- densitys[!duplicated(densitys$cell),] # remove dups
dup <- densitys[duplicated(densitys$cell),] # what is the duplicated row?
#test <- rbind(nodups, dup)
densitys <- nodups

hist(densitys$PLSdensity, breaks = 50)
#map out density:
ggplot(densitys, aes(x,y,color = PLSdensity))+geom_point()

# merge with simons estimates to see if they match
test.merge <- merge(umdw.new, densitys, by = c("x", "y", "cell"))
ggplot(test.merge, aes(PLSdensity.x, PLSdensity.y))+geom_point(size = 0.1)

test.merge$diff<- test.merge$PLSdensity.x - test.merge$PLSdensity.y
ggplot(test.merge, aes(x,y, fill = diff))+geom_raster()#+scale_fill_gradientn(colours = cbpalette, limits = c(0,600), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  

hist(densitys$PLSdensity, breaks = 50)
hist(densitys[densitys$PLSdensity > 0.5,]$PLSdensity, breaks = 100)

ggplot(densitys, aes(x, y, fill = PLSdensity))+geom_raster()


# ----------------------mapping out tree density-------------------------------
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata.sp<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata.sp)



us.alb <- readOGR("/Users/kah/Documents/bimodality/data/us_alb/us_alb.shp", layer = "us_alb" )

mw <- us.alb[us.alb@data$STATE_NAME %in% c("Indiana", "Illinois", "Minnesota", "Michigan", "Wisconsin"),]


base.rast <- raster(xmn = -71000, xmx = 2297000, ncols=296,
                    ymn = 58000,  ymx = 1498000, nrows = 180,
                    crs = '+init=epsg:3175')



numbered.rast <- setValues(base.rast, 1:ncell(base.rast))
numbered.cell <- raster::extract(numbered.rast, mw)


cells <- unlist(numbered.cell)
full.cells <- data.frame(cell = cells)
full.cells[2:3]<- xyFromCell(numbered.rast, cells)
colnames(full.cells) <- c("cell","x", "y")

cells2add <- full.cells[!full.cells$cell %in% densitys$cell, ]
cells2add$PLSdensity <- NA
nacells <- cells2add[,c("x", "y", "cell", "PLSdensity")]

#densitys <- rbind(nacells, densitys)
# Map out tree density below: 

sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=densitys, aes(x=x, y=y, fill = PLSdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,600), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw()
pls.map

workingdir <- "/Users/kah/Documents/bimodality"

png(paste0(workingdir,"/outputs/v",version,"/PLS_full_tree_density_map.png"))
pls.map
dev.off()

# write out combinded datasets to use later:
write.csv(densitys, paste0("data/midwest_pls_full_density_alb",version,".csv"), row.names = FALSE)
 


# -----------Get species composition PLS data------------------------------------

#this is to join the species tables
colnames(pls.spec) <- c("x" ,  "y" , "cell" ,"Alder","Ash",
                        "Bald cypress","Basswood","Beech","Birch", "Black.gum" ,         
                        "Black gum.sweet gum", "Buckeye" , "Cedar.juniper" ,"Cherry" ,"Chestnut" ,          
                        "Dogwood","Elm" ,"Fir", "Hackberry", "Hemlock","Hickory", "Ironwood",    
                        "Locust" ,"Maple" ,"Mulberry" ,"No.tree","Oak",                
                        "Other.hardwood","Pine","Poplar", "Poplar.tulip poplar","Spruce", "Sweet gum" ,         
                        "Sycamore" ,"Tamarack" ,"Tulip.poplar" ,"Unknown.tree","Walnut" ,            
                        "Water","Willow","PLSdensity" )
umdw.names <- colnames(umdw)
pls.names <- colnames(pls.spec)


#create name vectors for species columns missing in the upper and lower midewst
to.add.umdw <- pls.names[!pls.names %in% umdw.names]
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
full.spec <- full.spec %>%
  dplyr::select(cell, everything())

full.spec <- full.spec %>%
  dplyr::select(y, everything())

full.spec <- full.spec %>%
  dplyr::select(x, everything())

#full.spec<- full.spec %>%
# dplyr:: select(X, everything())

full.spec<- full.spec %>%
  dplyr:: select(-PLSdensity, everything())

#now add totals to the 'total columns
#full.spec$total <- rowSums(full.spec[,4:41], na.rm = TRUE)
#summary(full.spec$total)
#hist(full.spec$total, breaks = 1000, xlim = c(0,600))
#full.spec[is.na(full.spec)] <- 0
#colnames(full.spec)[42] <- 'PLSdensity'

# create a density.full data.frame



comps <- full.spec[!names(full.spec) %in% c("Water", "Wet", "No Tree", "No.tree")]
comps <- comps[!is.na(comps),]
#comps$total2 <- rowSums(comps[,4:38], na.rm=TRUE)
comps[,4:38] <- comps[,4:38]/comps[,39] # calculate the proportion of the total density that each species takes up
comps <- comps[,1:38]

# remove prairie cells:
#comps <- data.frame( comps[ complete.cases(comps),] )

# write as a csv so we don't have to keep doing this:
write.csv(comps, "data/outputs/plss_pct_density_composition_v1.6.csv", row.names = FALSE)



###############################################################################################
#                        FIA density and composition combining data
###############################################################################################

#-------------------------------------------FIA density ------------------------------------------------
#read in FIA from Sean's repository

library(data.table)
library(reshape2)
library(dtplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(modes)

FIA <- read.csv('data/FIA_species_plot_parameters_paleongrid.csv')
speciesconversion <- read.csv('data/fia_conversion_v02-sgd.csv')

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )
FIA.by.paleon <- dcast(FIA.pal, x + y+ cell+ plt_cn ~ PalEON, sum, na.rm=TRUE, value.var = 'density') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,6:35], na.rm = TRUE) # sum the total density in each plot
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn')) # melt the dataframe
fia.by.cell <- dcast(fia.melt, x + y+ cell ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell

density.FIA.table <- fia.by.cell


hist(density.FIA.table$FIAdensity, breaks = 100)


#------merge upper midwest FIA and PLS ------------------------------------------------
densitys <- merge(densitys[,c('x', 'y', 'cell', 'PLSdensity')], density.FIA.table[,c('x', 'y', 'cell', 'FIAdensity')],
by = c('x', 'y', 'cell'))

#note that for some reason, 1 grid cell is duplicated
nodups <- densitys[!duplicated(densitys$cell),] # remove dups
dup <- densitys[duplicated(densitys$cell),] # what is the duplicated row?
#test <- rbind(nodups, dup)
densitys <- nodups



#map out density basic plots of density:

ggplot(densitys, aes(x,y,color = FIAdensity))+geom_point()

#keep only the 99th percentile of densitys---this is also what simon does
nine.nine.pct <- apply(densitys[,4:ncol(densitys)], 2, quantile, probs = 0.995, na.rm=TRUE)
#densitys$PLSdensity[densitys$PLSdensity > nine.nine.pct['PLSdensity']] <- nine.nine.pct['PLSdensity']
densitys$FIAdensity[densitys$FIAdensity > nine.nine.pct['FIAdensity']] <- nine.nine.pct['FIAdensity']

summary(densitys)

hist(densitys$PLSdensity, breaks = 25)
hist(densitys$FIAdensity, breaks = 25)
#print out the histograms here
png(height=4, width=8, units = 'in',res = 300, paste0("outputs/v",version,"/FIA_PLS_hists.png"), type="cairo")
pushViewport(viewport(layout = grid.layout(1, 2)))
print(ggplot(densitys, aes(PLSdensity)) +geom_histogram(fill= "#D55E00",color = "black") +xlim(0, 700)+ xlab("PLS tree density (stems/ha)")+ ylab('# grid cells')+ 
theme_bw(base_size = 10), vp = viewport(layout.pos.row = 1, layout.pos.col = 1))#+ facet_wrap(~plsprbins)
print(ggplot(densitys, aes(FIAdensity)) +geom_histogram(binwidth = 30,fill ="#0072B2",  color = 'black') +xlim(0, 700)+xlab('Modern Tree density (stems/ha)')+ylab("# grid cells")+
        theme_bw(base_size = 10),vp = viewport(layout.pos.row = 1, layout.pos.col = 2))#+ facet_wrap(~fiaprbins)

dev.off()

#make difference plot with ggplot:
dens <- densitys
dens$diff <- dens$FIAdensity - dens$PLSdensity

png(width = 4, height = 3,units = 'in', res = 300, paste0('outputs/v', version, '/density_difference_plot.png'))
ggplot(dens, aes(x = PLSdensity, y = diff))+ geom_point()+geom_density_2d() +geom_smooth(method = 'lm', color = 'red')+xlim(0,600)+
  theme_bw()+ ylab('increase in density \n since PLS (trees/ha)') + xlab('PLS tree density (trees/ha)') + annotate("text", x=400, y=900,label= paste("R-squared =", round(summary(lm(dens$PLSdensity ~ dens$diff))$adj.r.squared,2)), size = 5)
dev.off()


#plot maps of tree density

#map out with polygon overlay
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)



sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=densitys, aes(x=x, y=y, fill = PLSdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw()

#save to png
png(paste0('outputs/v',version,'/PLS_tree_density_map_FIA_region.png'))
pls.map
dev.off()

fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=densitys, aes(x=x, y=y, fill = FIAdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="FIA tree density") + 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw()

#save to png
png(paste0('outputs/v',version,'/FIA_tree_density_map.png'))
fia.map
dev.off()

#map PLS and fia side by side
png(height=400, width=800,paste0('outputs/v', version, '/tree_density_maps_PLS_FIA.png'))
pushViewport(viewport(layout = grid.layout(1, 2)))
print(pls.map, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(fia.map, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
dev.off()

write.csv(densitys, paste0("data/midwest_pls_fia_density_alb", version,".csv"))


#------------------------------------FIA Species Composiition-------------------------------------

FIA <- read.csv('data/FIA_species_plot_parameters_paleongrid.csv')
speciesconversion <- read.csv('data/fia_conversion_v02-sgd.csv')

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )
FIA.by.paleon <- dcast(FIA.pal, x + y+ cell+ plt_cn ~ PalEON, sum, na.rm=TRUE, value.var = 'density') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,6:35], na.rm = TRUE) # sum the total density in each plot
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn')) # melt the dataframe
fia.by.cell <- dcast(fia.melt, x + y+ cell ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell

fcomps <- fia.by.cell
fcomps <- fcomps[fcomps$cell %in% densitys$cell, ]

fcomps[,4:35] <- fcomps[,4:34]/fcomps[,35] # calculate the proportion of the total density that each species takes up
fcomps <- fcomps[,1:34]

# remove prairie cells:
fcomps <- data.frame(fcomps[complete.cases(fcomps),])
fcomps <- fcomps[! names(fcomps) %in% "plt_cn"]


library(cluster)
library(fpc)

# need to match up the species with pls and fia
colnames(fcomps) <- c("x" , "y" , "cell"  ,"Alder",       
                      "Ash" ,"Basswood" ,"Beech", "Birch" ,     
                      "Black.gum", "Buckeye"    ,    "Cedar.juniper" , "Cherry" ,       
                      "Dogwood" , "Douglas fir" ,   "Elm"  ,  "Fir",           
                      "Hackberry"  ,"Hemlock"   ,  "Hickory"  ,   "Ironwood",      
                      "Maple"   , "Oak"     ,  "Other.hardwood" ,"Other.softwood",
                      "Pine"   ,  "Poplar"  ,  "Spruce" ,   "Sweet.gum",     
                      "Sycamore"    ,   "Tamarack"     ,  "Tulip.poplar"  , "Unknown.tree",  
                      "Walnut","Willow")

# add douglas fir to fir
fcomps$Fir <- rowSums(fcomps[,c("Fir", "Douglas fir")], na.rm=TRUE)
fcomps <- fcomps[,-14] # get rid of douglas fir

plscols <- colnames(comps)
fiacols <- colnames(fcomps)

notinfia <- plscols[ !plscols %in% fiacols ]
notinpls <- fiacols[ !fiacols %in% plscols ] 

comps[,notinpls] <- 0
fcomps[,notinfia] <-0 


#reorder the columns so the comp.inil and comp.umw dataframes match
comps <- comps[ ,order(names(comps))]
fcomps <- fcomps[ ,order(names(fcomps))]


# reorganize fcomps:

fcomps <- fcomps %>%
  dplyr::select(cell, everything())

fcomps <- fcomps %>%
  dplyr::select(y, everything())

fcomps <- fcomps %>%
  dplyr::select(x, everything())

# write as a csv so we don't have to keep doing this:
write.csv(fcomps, "data/outputs/FIA_pct_density_composition.csv", row.names = FALSE)


# add and fia vs. pls flag:
comps$period <- "Past"
fcomps$period<- "Modern"

fullcomps <- rbind( comps, fcomps )

#move around the columns
require(dplyr)
fullcomps <- fullcomps %>%
  dplyr::select(period, everything())

fullcomps <- fullcomps %>%
  dplyr::select(cell, everything())

fullcomps <- fullcomps %>%
  dplyr::select(y, everything())

fullcomps <- fullcomps %>%
  dplyr::select(x, everything())

#fullcomps <- fullcomps[complete.cases(fullcomps),]

write.csv(fullcomps, "outputs/cluster/fullcomps.csv", row.names = FALSE)


#-----------------------PCA of full dataset (PLS and FIA)----------------------
fullcomps <- read.csv("outputs/cluster/fullcomps.csv")
cells <- fullcomps[fullcomps$period %in% "Past",]$cell
fullcomps <- fullcomps[fullcomps$cell %in% cells, ]
fullcomps <- fullcomps[! duplicated(fullcomps),]
fullcomps <-fc <- na.omit(fullcomps) 

fullcomps <- fullcomps[!names(fullcomps) %in% c("No.tree", "Other.softwood", "period", "FIAdensity", "PLSdensity", "Sweet.gum.1")]

ggplot(fc, aes(Oak, fill = period))+geom_histogram()+facet_wrap(~period)

# pca on the scaled data
full.pca <- princomp(scale(fullcomps[,4:ncol(fullcomps)], center = TRUE))#scale to 0 variance
plot(full.pca)

#biplot(full.pca)
scores <- full.pca$scores

fc$pc1 <- scores[,1]
fc$pc2 <- scores[,2]



source("R/newggbiplot.R")

# make generic biplot
#ggbiplot(full.pca, pc.biplot = TRUE)+geom_point(data= fc, aes(x=pc1, y=pc2, color = period))



g <- newggbiplot(full.pca, obs.scale = 1, var.scale = 1, labels.size
                 = 25,alpha = 0,color = "blue",  alpha_arrow = 1, line.size = 1.5, scale = TRUE)
g$layers <- c(geom_point(data = fc, aes(x = pc1, y = pc2, color = period)), g$layers)

png("outputs/cluster/full_composition_PCA_biplot_w_somi.png")
g + theme_bw()
dev.off()

png("outputs/cluster/full_composition_PCA1_maps_w_somi.png")
ggplot(data = fc, aes(x = x, y=y, fill = pc1))+geom_raster()+facet_wrap(~period)+theme_bw()+coord_equal()
dev.off()

png("outputs/cluster/full_composition_PCA2_maps.png")
ggplot(data = fc, aes(x = x, y=y, fill = pc2))+geom_raster()+facet_wrap(~period)+theme_bw()+coord_equal()
dev.off()



fia.pcs <- fc[fc$period %in% "Modern",]
pls.pcs <- fc[fc$period %in% "PLS",]

colnames(pls.pcs)[43:44] <- c("pls_pc1", "pls_pc2")

colnames(fia.pcs)[43:44] <- c("fia_pc1", "fia_pc2")

fc.m <- merge(pls.pcs[, c("x", "y", "cell","pls_pc1", "pls_pc2")], fia.pcs[,c("x", "y", "cell","fia_pc1", "fia_pc2")], by = c("x", "y", "cell"))

write.csv(fc, "outputs/full_comp_pcs.csv", row.names = FALSE)









#--------------------------------FIA data from 1980s and 1990s------------------------------

# read in FIA data from 1980s and early 1990s
fia.old.by.cell <- read.csv("data/FIA_plot_data/fia.by.cell.out_1980_1990.csv")
density.FIA.table <- fia.old.by.cell

# add on the modern survey data:


hist(density.FIA.table$FIAdensity, breaks = 100)


#------merge upper midwest FIA and PLS ------------------------------------------------
densitys.old <- merge(densitys[,c('x', 'y', 'cell', 'PLSdensity')], density.FIA.table[,c('x', 'y', 'cell', 'FIAdensity', "INVYRcd")],
                  by = c('x', 'y', 'cell'), keep = all.y)

#note that for some reason, 1 grid cell is duplicated
#nodups <- densitys.old[!duplicated(densitys.old$cell),] # remove dups
#dup <- densitys.old[duplicated(densitys.old$cell),] # what is the duplicated row?
#test <- rbind(nodups, dup)
#densitys.old <- nodups



#map out density basic plots of density:

ggplot(densitys.old, aes(x,y,color = FIAdensity))+geom_point()

#keep only the 99th percentile of densitys---this is also what simon does
nine.nine.pct <- apply(densitys.old[,4:ncol(densitys.old)], 2, quantile, probs = 0.995, na.rm=TRUE)
#densitys$PLSdensity[densitys$PLSdensity > nine.nine.pct['PLSdensity']] <- nine.nine.pct['PLSdensity']
densitys.old$FIAdensity[densitys.old$FIAdensity > nine.nine.pct['FIAdensity']] <- nine.nine.pct['FIAdensity']

summary(densitys.old)

hist(densitys.old$PLSdensity, breaks = 25)
hist(densitys.old$FIAdensity, breaks = 25)

#make difference plot with ggplot:
dens <- densitys.old
dens$diff <- dens$FIAdensity - dens$PLSdensity

ggplot(dens, aes(x = PLSdensity, y = diff))+ geom_point()+geom_density_2d() +geom_smooth(method = 'lm', color = 'red')+xlim(0,600)+
  theme_bw()+ ylab('increase in density \n since PLS (trees/ha)') + xlab('PLS tree density (trees/ha)') + annotate("text", x=400, y=900,label= paste("R-squared =", round(summary(lm(dens$PLSdensity ~ dens$diff))$adj.r.squared,2)), size = 5)



#plot maps of tree density

#map out with polygon overlay
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)



sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=densitys, aes(x=x, y=y, fill = PLSdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw()

#save to png
png(paste0('outputs/v',version,'/PLS_tree_density_map_FIA_region.png'))
pls.map
dev.off()

fia.map.old <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=densitys.old, aes(x=x, y=y, fill = FIAdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="FIA tree density") + 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw()+facet_wrap(~INVYRcd)

#save to png
png(paste0('outputs/v',version,'/FIA_tree_density_map_old_surveys.png'))
fia.map.old
dev.off()

#map PLS and fia side by side

write.csv(densitys.old, paste0("data/midwest_pls_fia_density_old_surveys_alb", version,".csv"))


#------------------------------------FIA Species Composiition-------------------------------------


fcomps <- fia.old.by.cell
fcomps <- fcomps[fcomps$cell %in% densitys$cell, ]

fcomps[,5:33] <- fcomps[,5:33]/fcomps[,34] # calculate the proportion of the total density that each species takes up
#fcomps <- fcomps[,1:34]

# remove prairie cells:
fcomps <- data.frame(fcomps[complete.cases(fcomps),])
#fcomps <- fcomps[! names(fcomps) %in% "plt_cn"]


library(cluster)
library(fpc)

# need to match up the species with pls and fia
colnames(fcomps) <- c("x" , "y" , "cell"  ,"INVYR",       
                      "Ash" ,"Basswood" ,"Beech", "Birch" ,     
                      "Black.gum", "Buckeye"    ,    "Cedar.juniper" , "Cherry" ,       
                      "Dogwood" , "Douglas fir" ,   "Elm"  ,  "Fir",           
                      "Hackberry"  ,"Hemlock"   ,  "Hickory"  ,   "Ironwood",      
                      "Maple"   , "Oak"     ,  "Other.hardwood" ,
                      "Pine"   ,  "Poplar"  ,  "Spruce" ,   "Sweet.gum",     
                      "Sycamore"    ,   "Tamarack"     ,  "Tulip.poplar"  , "Unknown.tree",  
                      "Walnut","Willow", "FIAdensity", "INVYRcd")

# add douglas fir to fir
fcomps$Fir <- rowSums(fcomps[,c("Fir", "Douglas fir")], na.rm=TRUE)
fcomps <- fcomps[,-14] # get rid of douglas fir
fc.full <- fcomps
fcomps <- fcomps[!names(fcomps) %in% c("FIAdensity", "INVYR", "INVYRcd")]
plscols <- colnames(comps)
fiacols <- colnames(fcomps)

notinfia <- plscols[ !plscols %in% fiacols ]
notinpls <- fiacols[ !fiacols %in% plscols ] 

comps[,notinpls] <- 0
fcomps[,notinfia] <-0 


#reorder the columns so the comp.inil and comp.umw dataframes match
comps <- comps[ ,order(names(comps))]
fcomps <- fcomps[ ,order(names(fcomps))]


# reorganize fcomps:

fcomps <- fcomps %>%
  dplyr::select(cell, everything())

fcomps <- fcomps %>%
  dplyr::select(y, everything())

fcomps <- fcomps %>%
  dplyr::select(x, everything())

# write as a csv so we don't have to keep doing this:
write.csv(fcomps, "data/outputs/FIA_pct_density_composition_oldsurvey.csv", row.names = FALSE)


# add and fia vs. pls flag:
comps$period <- "Past"
fcomps$period<- ifelse(fc.full$INVYRcd %in% "1980s", "Modern-1980s", "Modern-1990s")

fullcomps <- rbind( comps, fcomps )

#move around the columns
require(dplyr)
fullcomps <- fullcomps %>%
  dplyr::select(period, everything())

fullcomps <- fullcomps %>%
  dplyr::select(cell, everything())

fullcomps <- fullcomps %>%
  dplyr::select(y, everything())

fullcomps <- fullcomps %>%
  dplyr::select(x, everything())

#fullcomps <- fullcomps[complete.cases(fullcomps),]

write.csv(fullcomps, "outputs/cluster/fullcomps_oldsurvey.csv", row.names = FALSE)


#-----------------------PCA of full dataset (PLS and FIA)----------------------
fullcomps <- read.csv("outputs/cluster/fullcomps_oldsurvey.csv")
cells <- fullcomps[fullcomps$period %in% "Past",]$cell
fullcomps <- fullcomps[fullcomps$cell %in% cells, ]
fullcomps <- fullcomps[! duplicated(fullcomps),]
fullcomps <-fc <- na.omit(fullcomps) 
fullcomps.old <- fc.old <- fullcomps[!fullcomps$period %in% "Past",]
fullcomps.old <- fullcomps.old[!names(fullcomps.old) %in% c("No.tree", "Other.softwood", "period", "FIAdensity", "PLSdensity", "Sweet.gum.1")]

ggplot(fc, aes(Oak, fill = period))+geom_histogram()+facet_wrap(~period)

# pca on the scaled data

cc <- scale(fullcomps.old[,4:ncol(fullcomps.old)])
cc[is.na(cc)]<- 0
newscores <- predict(full.pca, newdata=cc)


#biplot(full.pca)
scores <- newscores

fc.old$pc1 <- scores[,1]
fc.old$pc2 <- scores[,2]



source("R/newggbiplot.R")

# make generic biplot
#ggbiplot(full.pca, pc.biplot = TRUE)+geom_point(data= fc, aes(x=pc1, y=pc2, color = period))



g <- newggbiplot(full.pca, obs.scale = 1, var.scale = 1, labels.size
                 = 25,alpha = 0,color = "blue",  alpha_arrow = 1, line.size = 1.5, scale = TRUE)
g$layers <- c(geom_point(data = fc.old, aes(x = pc1, y = pc2, color = period)), g$layers)

png("outputs/cluster/full_composition_PCA_biplot_w_somi_old_fia.png")
g + theme_bw()
dev.off()

png("outputs/cluster/full_composition_PCA1_maps_w_somi_old_fia.png")
ggplot(data = fc.old, aes(x = x, y=y, fill = pc1))+geom_raster()+facet_wrap(~period)+theme_bw()+coord_equal()
dev.off()

png("outputs/cluster/full_composition_PCA2_maps_oldfia.png")
ggplot(data = fc.old, aes(x = x, y=y, fill = pc2))+geom_raster()+facet_wrap(~period)+theme_bw()+coord_equal()
dev.off()




write.csv(fc.old, "outputs/full_comp_pcs_old_surveys.csv", row.names = FALSE)

