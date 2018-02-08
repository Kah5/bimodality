# combine_umw_pls_fia.R
# this script takes density estimates by species from upper midwest (previously compiled + estimated by Goring et al. 2016),
# adds on the density from Indiana and Illinois, as well as southern michigan

# -----------------------------------------PLS density-----------------------------------------
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


#........................................Load PLS data.......................................

# read in pont level data
pls.inil <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_inilmi_v',version, '.csv'))


# find the mean density by species
pls.inil <- dcast(pls.inil, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density') # we want to sum the densities of all the species in each cells, then divide by the # of pls points within the cell, so take the avg 
pls.inil <- pls.inil[, c("x", "y", "cell", ".")] # just keep mean 

colnames(pls.inil) <- c('x', 'y', 'cell','PLSdensity')
hist(pls.inil$PLSdensity, xlim = c(0, 600),breaks = 100)


# read in point level data
pls.spec <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_inilmi_v',version, '.csv'))

# find mean denisty for all species in a grid cell
pls.spec <- dcast(pls.spec, x + y + cell ~spec, mean, na.rm = TRUE, value.var = 'density')
pls.spec$total <- rowSums(pls.spec[,!names(pls.spec)%in% c("x", "y", "cell", "Water", "wet")], na.rm=TRUE) # sum species density in the grid cell

# write to a csv:
write.csv(pls.spec, "data/plss_gridded_density_inil_v1.6.csv")

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

hist(umdw.new$PLSdensity, breaks = 25)

densitys <- rbind(pls.inil, umdw.new)
#coordinates(pls.inil)<- ~x+y

# read in lower mi data
#note that for some reason, 1 grid cell is duplicated
nodups <- densitys[!duplicated(densitys$cell),] # remove dups
dup <- densitys[duplicated(densitys$cell),] # what is the duplicated row?
#test <- rbind(nodups, dup)
densitys <- nodups

hist(densitys$PLSdensity, breaks = 50)
#map out density:
ggplot(densitys, aes(x,y,color = PLSdensity))+geom_point()


#keep only the 99th percentile of densitys---this is also what simon does
nine.nine.pct <- quantile(densitys[,4], probs = 0.995, na.rm=TRUE )
densitys$PLSdensity[densitys$PLSdensity > nine.nine.pct['99.5%']] <- nine.nine.pct['99.5%']

summary(densitys)

hist(densitys$PLSdensity, breaks = 50)
hist(densitys[densitys$PLSdensity > 0.5,]$PLSdensity, breaks = 100)

# ----------------------mapping out tree density-------------------------------
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)

# Map out tree density below: 

sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=densitys, aes(x=x, y=y, fill = PLSdensity))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="PLS tree density") + 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,700), name ="Tree \n Density \n (trees/hectare)", na.value = 'darkgrey') +
  coord_equal()+theme_bw()

workingdir <- "/Users/kah/Documents/bimodality"

png(paste0(workingdir,"/outputs/v",version,"/PLS_full_tree_density_map.png"))
pls.map
dev.off()

# write out combinded datasets:
write.csv(densitys, paste0("data/midwest_pls_full_density_alb",version,".csv"), row.names = FALSE)



#-------------------------------------------FIA density ------------------------------------------------
#read in FIA from Sean's repository


setwd( "C:/Users/JMac/Documents/Kelly/biomodality")
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
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn', 'Var.5')) # melt the dataframe
fia.by.cell <- dcast(fia.melt, x + y+ cell ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell

density.FIA.table <- fia.by.cell


hist(density.FIA.table$FIAdensity, breaks = 100)


#------merge upper midwest FIA and PLS 
densitys <- merge(densitys[,c('x', 'y', 'cell', 'PLSdensity')], density.FIA.table[,c('x', 'y', 'cell', 'FIAdensity')],
by = c('x', 'y', 'cell'))

#note that for some reason, 1 grid cell is duplicated
nodups <- densitys[!duplicated(densitys$cell),] # remove dups
dup <- densitys[duplicated(densitys$cell),] # what is the duplicated row?
#test <- rbind(nodups, dup)
densitys <- nodups

hist(densitys$PLSdensity, breaks = 25)
#map out density basic plots of density:
ggplot(densitys, aes(x,y,color = PLSdensity))+geom_point()
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

#################################
#plot maps of tree density
#################################

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
png(paste0('outputs/v',version,'/PLS_tree_density_map.png'))
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


#------------------------------------Species Composiition-------------------------------------

#this is to join the species tables (Upper midwest and INIL pls data have some species that only occur in one time period, so we add NA values for the other region)
pls.names <- colnames(pls.spec)
umdw.names <- colnames(umdw)
colnames(pls.spec)[6] <- 'Bald.cypress'
colnames(pls.spec)[10] <- 'Black.gum'
colnames(pls.spec)[24] <- "No.tree"
colnames(pls.spec)[13] <- 'Cedar.juniper'
colnames(pls.spec)[26] <- 'Other.hardwood'
colnames(pls.spec)[33] <- 'Tulip.poplar'
colnames(pls.spec)[34] <- 'Unknown.tree'

pls.names<- colnames(pls.spec)
umdw.names<- colnames(umdw)

#create name vectors for species columns missing in the upper and lower midewst
to.add.umdw <-pls.names[!pls.names %in% umdw.names]
to.add.pls <- umdw.names[!umdw.names %in% pls.names]

#add these species columns to the respective dataframes, but with 0 for data values
pls.spec[,to.add.pls] <- NA
umdw[,to.add.umdw] <- NA


FIA <- read.csv('data/FIA_species_plot_parameters_paleongrid.csv')
speciesconversion <- read.csv('data/FIA_conversion-SGD_remove_dups.csv')

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )
FIA.by.paleon <- dcast(FIA.pal, x + y+ cell+ plt_cn ~ PalEON, sum, na.rm=TRUE, value.var = 'density') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,6:25], na.rm = TRUE) # sum the total density in each plot
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn', 'Var.5')) # melt the dataframe
fia.by.cell <- dcast(fia.melt, x + y+ cell ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell


# get the grid cells from FIA where we have PLS records
pls.cells <- full.spec[,c('x','y','cell')]
fia.inpls <- merge(pls.cells, fia.by.cell, by = c('x','y','cell'))
ggplot(fia.inpls, aes(x=x,y=y, fill=Oak))+geom_raster()



# -------------------look at all species in fia----------------------
all <- fia.inpls


# caluclate the % of total denisty that the taxa makes up of the grid cell:

compss <- all
compss[,4:24] <- compss[,4:23]/compss[,24] # calculate the proportion of the total density that each species takes up
comp.melt <- melt(compss, id.vars = c('x', 'y', 'cell', 'FIAdensity'))

# find the grid cell with highest % of denisty in all bimodal places
compss[is.na(compss)]<- 0
compss$highest <- colnames(compss[,4:23])[max.col(compss[,4:23],ties.method="first")]
taxa <- unique(compss$highest)


highest <- compss[names(compss) %in% taxa] 
highest$x <- compss$x
highest$y <- compss$y
highest$highest <- compss$highest

# plot the highest density with colors by mesophytic vs non mesophytic:
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








#now I need to add columns to FIA dataframe for species not seen in FIA, but seen in PLS:

#reorder the columns so the pls.spec and umdw dataframes match
pls.spec<- pls.spec[ , order(names(pls.spec))]
umdw <- umdw[,order(names(umdw))]

full.spec <- rbind(pls.spec, umdw)

#move around the  columns so that x and y are at the front
require(dplyr)
full.spec <- full.spec %>%
  dplyr::select(cell, everything())

full.spec <- full.spec %>%
  dplyr::select(y, everything())

full.spec <- full.spec %>%
  dplyr::select(x, everything())

full.spec <- full.spec %>%
  dplyr:: select(-total, everything())

#now add totals to the 'total columns
full.spec$total <- rowSums(full.spec[,!names(full.spec) %in% c('x','y','cell',"No.tree", "Water", "Wet", "total")], na.rm = TRUE)
summary(full.spec$total)
hist(full.spec$total, breaks = 1000, xlim = c(0,600))

colnames(full.spec)[42] <- 'PLSdensity'

#now I need to add columns to FIA dataframe for species not seen in FIA, but seen in PLS:

pls.full.n <- colnames(full.spec)
fia.names <- colnames(density.FIA.table)
fia.names[!fia.names %in% pls.full.n]
pls.full.n[!pls.full.n %in% fia.names]

colnames(density.FIA.table)[10] <- 'Cedar.juniper'
colnames(density.FIA.table)[18] <- 'Other.hardwood'
fia.names[!fia.names %in% pls.full.n]
pls.full.n[!pls.full.n %in% fia.names]


density.FIA.table<- density.FIA.table[,-5]# remove atlantic white cedar
density.FIA.table<- density.FIA.table[,-23] #remove total density

pls.names<- colnames(full.spec)
fia.names<- colnames(density.FIA.table)

#create name vectors for species columns missing in the upper and lower midewst
to.add.fia <-pls.names[!pls.names %in% fia.names]
#to.add.pls <- fia.names[!fia.names %in% pls.names]

#add these species columns to the respective dataframes, but with 0 for data values
#pls.spec[,to.add.pls] <- NA
density.FIA.table[,to.add.fia] <- NA
density.FIA.table<- density.FIA.table[,-42] #remove plsdensity columns

FIA.full <- density.FIA.table[,order(names(density.FIA.table))]

FIA.full<- FIA.full %>%
  dplyr:: select(cell, everything())

FIA.full<- FIA.full %>%
  dplyr:: select(y, everything())

FIA.full<- FIA.full %>%
  dplyr::select(x, everything())


#now add totals to the 'total columns
FIA.full$FIAdensity <- rowSums(FIA.full[,!names(FIA.full) %in% c('x','y','cell',"No.tree", "Water", "Wet", "total")], na.rm = TRUE)
summary(FIA.full$FIAdensity)
hist(FIA.full$FIAdensity, breaks = 50, xlim = c(0,600))
dens.pr <- dens.pr[dens.pr$FIAdensity < 650,]


write.csv(dens.pr, paste0("data/midwest_pls_full_density_alb",version,".csv"), row.names = FALSE)

# -----------------------------FIA species composistion----------------------------------

FIA <- read.csv('data/FIA_species_plot_parameters_paleongrid.csv')
speciesconversion <- read.csv('data/fia_conversion_v02-sgd.csv')

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )
FIA.by.paleon <- dcast(FIA.pal, x + y+ cell+ plt_cn ~ PalEON, sum, na.rm=TRUE, value.var = 'density') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,6:35], na.rm = TRUE) # sum the total density in each plot
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn')) # melt the dataframe
fia.by.cell <- dcast(fia.melt, x + y+ cell ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell

fcomps <- fia.by.cell
fcomps <- fcomps[fcomps$cell %in% density.full$cell, ]

fcomps[,4:34] <- fcomps[,4:34]/fcomps[,35] # calculate the proportion of the total density that each species takes up
fcomps <- fcomps[,1:34]

# remove prairie cells:
fcomps <- data.frame(fcomps[complete.cases(fcomps),])
# write as a csv so we don't have to keep doing this:
write.csv(fcomps, "data/outputs/FIA_pct_density_composition.csv")

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

# add and fia vs. pls flag:
comps$period <- "Past"
fcomps$period<- "Modern"

fullcomps <- rbind( comps, fcomps )

#move around the columns
require(dplyr)
fullcomps<- fullcomps %>%
  dplyr::select(period, everything())

fullcomps<- fullcomps %>%
  dplyr::select(cell, everything())

fullcomps<- fullcomps %>%
  dplyr::select(y, everything())

fullcomps<- fullcomps %>%
  dplyr::select(x, everything())

fullcomps <- fullcomps[complete.cases(fullcomps),]

write.csv(fullcomps, "outputs/cluster/fullcomps.csv")


#-----------------------PCA of full dataset (PLS and FIA)----------------------
fullcomps <- read.csv("outputs/cluster/fullcomps.csv")
fc <- fullcomps
fullcomps <- fullcomps[!names(fullcomps) %in% c("No.tree", "Other.softwood", "period", "FIAdensity")]


# pca on the scaled data
full.pca <- princomp(scale(fullcomps[,6:ncol(fullcomps)])) #scale to 0 variance
plot(full.pca)

biplot(full.pca)
scores <- full.pca$scores

fc$pc1 <- scores[,1]
fc$pc2 <- scores[,2]


library(ggbiplot)
source("R/newggbiplot.R")

#png("outputs/cluster/pca_scree_plot.png")
ggbiplot(full.pca, pc.biplot = TRUE)+geom_point(data= fc, aes(x=pc1, y=pc2, color = period))
#dev.off()


g <- newggbiplot(full.pca, obs.scale = 1, var.scale = 1, labels.size
                 = 25,alpha = 0,color = "blue",  alpha_arrow = 1, line.size = 1.5, scale = TRUE)
g$layers <- c(geom_point(data = fc, aes(x = pc1, y = pc2, color = period)), g$layers)

png("outputs/cluster/full_composition_PCA_biplot.png")
g + theme_bw()
dev.off()

png("outputs/cluster/full_composition_PCA1_maps.png")
ggplot(data = fc, aes(x = x, y=y, fill = pc1))+geom_raster()+facet_wrap(~period)+theme_bw()+coord_equal()
dev.off()

png("outputs/cluster/full_composition_PCA2_maps.png")
ggplot(data = fc, aes(x = x, y=y, fill = pc2))+geom_raster()+facet_wrap(~period)+theme_bw()+coord_equal()
dev.off()




