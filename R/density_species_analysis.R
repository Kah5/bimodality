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

#move around the columns to that x and y are the first columns
require(dplyr)
full.spec<- full.spec %>%
  dplyr::select(cell, everything())

full.spec<- full.spec %>%
  dplyr::select(y, everything())

full.spec<- full.spec %>%
  dplyr::select(x, everything())

#full.spec<- full.spec %>%
 # dplyr:: select(X, everything())

full.spec<- full.spec %>%
  dplyr:: select(-total, everything())

#now add totals to the 'total columns
#full.spec$total <- rowSums(full.spec[,4:41], na.rm = TRUE)
summary(full.spec$total)
hist(full.spec$total, breaks = 1000, xlim = c(0,600))

colnames(full.spec)[42] <- 'PLSdensity'



#------------------------------map out pls density for some species----------------
# melt data fram to be able to plot using ggplot
spec.melt <- melt(full.spec[,1:41], id.vars = c("x", "y", "cell"))
ggplot(full.spec, aes(x=x, y=y, fill = Beech))+geom_raster()


cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
r2bpalette <- c('#ca0020',
  '#f4a582',
  '#92c5de',
  '#0571b0')

#X11(width = 18, height = 12)
# map out species desities
ggplot(spec.melt, aes(x=x, y=y, fill=value))+geom_raster()+coord_equal()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                axis.title.x=element_blank(),
                                                                                axis.title.y=element_blank())+facet_wrap(~variable, ncol = 10, scales = 'free')+scale_fill_gradientn(colours = rev(rainbow(3)))


full.spec[is.na(full.spec)]<- 0
density.full <- full.spec



#-------------------Lets look at FIA now-------------------------------
FIA <- read.csv('data/FIA_species_plot_parameters_paleongrid.csv')
speciesconversion <- read.csv('data/FIA_conversion-SGD_remove_dups.csv')

FIA.pal <- merge(FIA, speciesconversion, by = 'spcd' )
FIA.by.paleon <- dcast(FIA.pal, x + y+ cell+ plt_cn ~ PalEON, sum, na.rm=TRUE, value.var = 'density') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,6:25], na.rm = TRUE) # sum the total density in each plot
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn', 'Var.5')) # melt the dataframe
fia.by.cell <- dcast(fia.melt, x + y+ cell ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell


# get the grid cells from FIA where we have PLS records
pls.cells <- full.spec[,c('x','y','cell')]
fia.inpls<- merge(pls.cells, fia.by.cell, by = c('x','y','cell'))
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







#---------------------Ordination of the species composition data---------------------
#NMDS: run on the CRC
library(vegan)
fullcomps<- read.csv("outputs/cluster/fullcomps.csv")
pls.comps <- fullcomps[fullcomps$period %in% "Past",]
s.scores <- readRDS("outputs/cluster/NMDS.samp.scores.PLS_trymax100.rds")
v.scores <- readRDS("outputs/cluster/NMDS.var.scores.PLS_trymax100.rds")
pls.comps2 <- pls.comps[!names(pls.comps) %in% c("No.tree", "Other.softwood", "period", "FIAdensity")]
samps <- sample(1:8792, size = 50, replace = FALSE)
NMDS <- metaMDS(pls.comps2[samps,5:39], distance = 'bray',trymax = 50, k=2,smin = 0.1, sratmax = 0.9999999999) 
v.scores <- data.frame(v.scores)
v.scores$species <- row.names(v.scores)
s.scores <- data.frame(s.scores)
stressplot(NMDS)

plot(NMDS)
ggplot(v.scores, aes(MDS1, MDS2))+geom_point()+geom_text(data=v.scores,aes(x=MDS1,y=MDS2,label=species),alpha=0.5)
ggplot(v.scores, aes(MDS1, MDS2))+geom_point()+geom_text(data=v.scores,aes(x=MDS1,y=MDS2,label=species),alpha=0.5)+xlim(-0.1,0.1)
ggplot(v.scores, aes(MDS1, MDS2))+geom_point()+geom_text(data=v.scores,aes(x=MDS1,y=MDS2,label=species),alpha=0.5)+xlim(-0.05,0.1)+ylim(-0.05,0.05)

ordiplot(NMDS,type="n")
orditorp(NMDS,display="species",col="red",air=0.01)

#orditorp(NMDS,display="sites",cex=1.25,air=0.01)

# sycamore and blackgum are outliers outline in terms of MDS1--I removed these to get a better look at species we are interested in:
png(height = 4, width = 8, units = "in",res = 200,"outputs/cluster/NMDS_full_excluding_outliers.png")
full <- ggplot(v.scores, aes(MDS1, MDS2))+geom_point()+geom_text(data=v.scores,aes(x=MDS1,y=MDS2,label=species),alpha=0.5)
zoom <- ggplot(v.scores, aes(MDS1, MDS2))+geom_point()+geom_text(data=v.scores,aes(x=MDS1,y=MDS2,label=species),alpha=0.5)+xlim(-0.1, 0.1)
grid.arrange(full, zoom, nrow = 1, ncol = 2)
dev.off()

# MDS2 seems to be the dominant separation of species

pls.comps$MDS1 <- s.scores$MDS1
pls.comps$MDS2 <- s.scores$MDS2

# it seems like the Oak separates well from pin, hemlock, alder on MDS1
# but MDS2 separates Oak from cherry, hickory, dogwood, other speices
ggplot(pls.comps, aes(MDS1, MDS2, color = period))+geom_point(alpha = 0.5)+xlim(-0.01, 0.015)+ylim(-0.0075,0.005)+facet_wrap(~period)
ggplot(pls.comps, aes(x=x, y=y, fill = MDS1))+geom_raster()+facet_wrap(~period)+scale_fill_gradient(low = 'blue', high='red', limits= c(-0.0025, 0.0025))+facet_wrap(~period)
ggplot(pls.comps, aes(x=x, y=y, fill = MDS2))+geom_raster()+scale_fill_gradient(low = 'blue', high='red', limits= c(-0.0025, 0.0025))+facet_wrap(~period)

ggplot(pls.comps, aes(MDS1, Maple))+geom_point()
ggplot(pls.comps, aes(MDS1))+geom_histogram(bins = 50)
ggplot(pls.comps, aes(MDS2))+geom_histogram()

# -----------------------Clustering of FIA data----------------------

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

#-------is the horeshoe shape present in the species density data as well?----------
density.full <- full.spec[,!names(full.spec) %in% c("Water", "No Tree", "Wet", "PLSdensity")]

plscells <- density.full$cell

density.fia <- fia.by.cell[fia.by.cell$cell %in% plscells, ]
density.fia <- density.fia[,1:34]

colnames(density.fia) <- c("x" , "y" , "cell"  ,"Alder",       
                      "Ash" ,"Basswood" ,"Beech", "Birch" ,     
                      "Black.gum", "Buckeye"    ,    "Cedar.juniper" , "Cherry" ,       
                      "Dogwood" , "Douglas fir" ,   "Elm"  ,  "Fir",           
                      "Hackberry"  ,"Hemlock"   ,  "Hickory"  ,   "Ironwood",      
                      "Maple"   , "Oak"     ,  "Other.hardwood" ,"Other.softwood",
                      "Pine"   ,  "Poplar"  ,  "Spruce" ,   "Sweet.gum",     
                      "Sycamore"    ,   "Tamarack"     ,  "Tulip.poplar"  , "Unknown.tree",  
                      "Walnut","Willow")

density.fia$Fir <- rowSums(density.fia[,c("Douglas fir", "Fir")], na.rm=TRUE)

density.fia <- density.fia[,!names(density.fia) %in% "Douglas fir"]

plscols <- colnames(density.full)
fiacols <- colnames(density.fia)

notinfia <- plscols[ !plscols %in% fiacols ]
notinpls <- fiacols[ !fiacols %in% plscols ] 

density.full[,notinpls] <- 0
density.fia[,notinfia] <-0 


#reorder the columns so the comp.inil and comp.umw dataframes match
density.full <- density.full[ ,order(names(density.full))]
density.fia <- density.fia[ ,order(names(density.fia))]

# add and fia vs. pls flag:
density.full$period <- "PLS"
density.fia$period<- "FIA"

fulldens <- rbind( density.full, density.fia )

#move around the columns
require(dplyr)
fulldens<- fulldens %>%
  dplyr::select(period, everything())

fulldens <- fulldens %>%
  dplyr::select(cell, everything())

fulldens <- fulldens %>%
  dplyr::select(y, everything())

fulldens <- fulldens %>%
  dplyr::select(x, everything())

fulldens <- fulldens[!duplicated(fulldens[,c('x',"y","cell","period")]),] # remove duplicated cells
fd <- fulldens

fulldens <- fulldens[,!names(fulldens) %in% c("Other.softwood", "No.tree", "Douglas fir", "Sweet.gum")]
summary(scale(fulldens[,5:39]))

standardize <- function(x) {(x - mean(x))/sd(x)}
X <- apply(fulldens[,5:39], MARGIN=2, FUN=standardize) 


dens.pca <- princomp(scale(fulldens[,5:39])) #need to scale to 0 variance
plot(dens.pca)

biplot(dens.pca)
scores <- dens.pca$scores

#######fix this below:
fulldens$pc1 <- scores[,1]
fulldens$pc2 <- scores[,2]


library(ggbiplot)
source("R/newggbiplot.R")

#png("outputs/cluster/pca_scree_plot.png")
ggbiplot(dens.pca, pc.biplot = TRUE)+geom_point(data= fulldens, aes(x=pc1, y=pc2, color = period))
#dev.off()

g <- newggbiplot(dens.pca, obs.scale = 1, var.scale = 1, labels.size
                 = 25,alpha = 0,color = "blue",  alpha_arrow = 1, line.size = 1.5, scale = TRUE)
g$layers <- c(geom_point(data = fulldens, aes(x = pc1, y = pc2, color = period)), g$layers)

png("outputs/cluster/PCA_biplot_unscaled_species_density_data.png")
g
dev.off()


ggplot(fulldens, aes(pc1))+geom_histogram()+facet_wrap(~period)
ggplot(fulldens, aes(pc2))+geom_histogram()+facet_wrap(~period)
#---------------------------------------------------------------------------------                                                               
# Is community composition bimodal across the environmental space?
dens.pr <- read.csv("outputs/PLS_full_dens_pr_bins_with_bimodality_for_PC1.csv") 
dens.pr <- dens.pr[,c('x','y','cell','ecotype','bimodal', "PC1", "PC2")]#, "PC1bins", "PC2bins")]
fc.m <- merge(fc, dens.pr, by = c('x','y','cell'))
PLSbins<- read.csv('data/PLS_full_dens_pr_with_bins.csv')
FIAbins <- read.csv('outputs/FIA_pls_density_with_bins.csv')

pls.fc <- fc.m[fc.m$period %in% "Past",]
fia.fc <- fc.m[fc.m$period %in% "Modern",]

plsmerged <- merge(pls.fc[!names(pls.fc) %in% c("PC1", "PC2")], PLSbins[,c("x", "y", "cell", "PC1","PC2","PC1bins", "PC2bins")], by = c("x", "y", "cell"))

fiamerged <- merge(fia.fc[!names(pls.fc) %in% c("PC1", "PC2")], FIAbins[,c("x", "y", "cell", "PC1fia","PC2fia","PC1fiabins", "PC2fiabins")], by = c("x", "y", "cell"))
colnames(fiamerged)[47:50] <- c("PC1","PC2","PC1bins", "PC2bins") # rename to match the PLS data

fc.m <- rbind(fiamerged, plsmerged)

# now PC1 is the PC for modern and pls data respectively and pc1 is the species principal component 1
ggplot(fc.m, aes(x=PC1, y = pc1,color=period))+geom_point()
ggplot(fc.m, aes(x=PC2, y = pc1,color=period))+geom_point()

png(width = 6, height = 4, units = 'in', res = 300, "outputs/cluster/species_pc2_envt_pc1.png")
ggplot(fc.m, aes(x=PC1, y = pc2,color=period))+geom_point(size = 0.5)+theme_bw()+ #ylim(-2.5, 2.5)+
  ylab("species composition pc2")+xlab("Environmental data pc1")+geom_density2d()+facet_wrap(~period)
dev.off()

png(width = 6, height = 4, units = 'in', res = 300, "outputs/cluster/species_pc2_envt_pc1_panels.png")
ggplot(fc.m, aes(x=PC1, y = pc2,color=period))+geom_point(size = 0.5)+theme_bw()+ 
  ylab("species composition pc2")+xlab("Environmental data pc1")+facet_wrap(~period)
dev.off()

#------------- Is the composition data bimodal in PLS and FIA?:----------------------
hist(fc.m$pc1) # the overall pc1 of composition does not seem bimodal
hist(fc.m$pc2) # same with pc2

# how about FIA or PLS by itself?
png("outputs/cluster/Composition_PC1_histograms.png")
ggplot(data = fc, aes(pc2, fill = period))+geom_histogram()+facet_wrap(~period,ncol=1)
dev.off()

png(height = 6, width = 5, units = 'in',res = 300, "outputs/cluster/Composition_pc2_histogram_overlay.png")
ggplot(data = fc, aes(pc2, fill = period))+geom_histogram(position = "identity", alpha = 0.7)+theme_bw(base_size = 15)+xlab("Species PC2")+
  coord_flip()+xlim(2.5,-5)+theme(legend.title = element_blank())
dev.off()

png(height = 4, width = 7, units = 'in',res = 300, "outputs/cluster/Composition_pc2_histogram_overlay_horizontal.png")
ggplot(data = fc, aes(pc2, fill = period))+geom_histogram(position = "identity", alpha = 0.7)+theme_bw(base_size = 15)+xlab("Species PC2")+
  xlim(2.5,-5)
dev.off()

library(modes)
#test for significance of species pc1
bimodality_coefficient(density(fc.m[fc.m$period %in% "Past",]$pc1)$y)
#[1] 0.7789676
#test for significance of species pc1
diptest::dip.test(density(fc.m[fc.m$period %in% "Past",]$pc1)$y)
# p valueu =0.4296



png("outputs/cluster/Composition_PC2_histograms.png")
ggplot(data = fc, aes(pc2, fill = period))+geom_histogram()+facet_wrap(~period,ncol=1)
dev.off()

#test for significance of species pc2
bimodality_coefficient(density(fc.m[fc.m$period %in% "Past",]$pc2)$y)
#[1] 0.8892039
diptest::dip.test(density(fc.m[fc.m$period %in% "Past",]$pc2)$y)
# pvalue < 2.2e-16

# Using the PC1 bins of with add the data -- FIA or PLS:
library(modes)
comp.bimodal <- function(data = fc.m, binby, density, time){


    data <- data[data[,"period"] %in% time,]
    bins <- as.character(unique(data[,binby]))
    coeffs <- matrix(NA, length(bins), 2)
    for (i in 1:length(bins)){
      coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
      coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
    }
    coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
    coef.bins<- data.frame(cbind(coeffs, bins))
    coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
    coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
    coef.new <- strsplit(as.character(coef.bins$bins), " - ")
    library(plyr)
    coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
    colnames(coef.new) <- c("low", "high")
    coef.bins <- cbind(coef.bins, coef.new)
    
    #merge bins with the "binby" column
    merged <- merge(coef.bins, data, by.x = "bins", by.y = binby)
    
    
    #define bimodality
    #merged$bimodal <- "Unimodal"
    #criteria for bimodality
    
    bi <- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05, "Bimodal", "Unimodal")
    merged$bimodal <- bi
    
    
    unique(merged$bimodal)
    
    
    ggplot()+ # geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
      geom_raster(data = merged, aes(x = x, y = y, fill = bimodal))+
      theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                        axis.text.y=element_blank(),axis.ticks=element_blank(),
                        axis.title.x=element_blank(),
                        axis.title.y=element_blank())+
      scale_fill_manual(values = c("red", 'blue'), limits= c("Bimodal", "Unimodal"))+
      xlab("easting") + ylab("northing") +coord_equal() 

}

png(width = 10, height = 4, units='in',res=300,'outputs/cluster/PLS_composition_pca_maps.png')
ppc1 <-comp.bimodal(data=fc.m, binby = "PC1bins", density = "pc1", time= "Past")+ ggtitle("Modes for PLS pc1 of composition")
ppc2 <-comp.bimodal(data=fc.m, binby = "PC1bins", density = "pc2", time= "Past")+ ggtitle("Modes for PLS pc2 of composition")
grid.arrange(arrangeGrob(ppc1, ppc2,  widths=c(1.1,1.1), ncol=2))
dev.off()


png(width = 10, height = 4, units='in',res=300,'outputs/cluster/FIA_composition_pca_maps.png')
fpc1 <- comp.bimodal(data=fc.m, binby = "PC1bins", density = "pc1", time= "Modern")+ ggtitle("Modes for FIA pc1 of composition")
fpc2 <- comp.bimodal(data=fc.m, binby = "PC1bins", density = "pc2", time= "Modern")+ ggtitle("Modes for FIA pc2 of composition")
grid.arrange(arrangeGrob(fpc1, fpc2,  widths=c(1.1,1.1), ncol=2))
dev.off()


#data = fc.m
#binby = "PC1bins"
#density = "pc2"
#time = "Modern"


comp.bimodal.df <- function(data = fc.m, binby, density, time){
  
  
  data <- data[data[,"period"] %in% time,]
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 2)
  for (i in 1:length(bins)){
    coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
  coef.new <- strsplit(as.character(coef.bins$bins), " - ")
  library(plyr)
  coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
  colnames(coef.new) <- c("low", "high")
  coef.bins <- cbind(coef.bins, coef.new)
  
  #merge bins with the "binby" column
  merged <- merge(coef.bins, data, by.x = "bins", by.y = binby)
  
  
  #define bimodality
  #merged$bimodal <- "Unimodal"
  #criteria for bimodality
  
  bi <- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05, "Bimodal", "Unimodal")
  merged[,c(paste0('bimodal_',density))] <- bi
  
  
  unique(merged$bimodal)
  
  
  merged
}

pls.pc1 <- comp.bimodal.df(data=fc.m, binby = "PC1bins", density = "pc1", time= "Past")
pls.pc2 <- comp.bimodal.df(data=fc.m, binby = "PC1bins", density = "pc2", time= "Past")

fia.pc1 <- comp.bimodal.df(data=fc.m, binby = "PC1bins", density = "pc1", time= "Modern")
fia.pc2 <- comp.bimodal.df(data=fc.m, binby = "PC1bins", density = "pc2", time= "Modern")


# using the same criteria as density, there are no significantly bimodal places
# if you only evaluate on the BC being > 0.55, then the bimodal density places have bimodal composition


#----Are the bimodal places in composition the same as those in density?

# read in bimodality file from density:
#dens.bi <- read.csv('data/PLS_full_dens_pr_with_bins.csv')
dens.pr <- read.csv("outputs/PLS_full_dens_pr_bins_with_bimodality_for_PC1.csv")
colnames(dens.pr)[1:12] <- c("X2", "densbins", "V1dens", "V2dens", "BCdens", "dipPdens", "lowdens", "highdens", "NA.", "x", "y", "cell")
colnames(dens.pr)[85] <- c("bimodaldensity")
plspc2.m <- merge(pls.pc2[,c("x", "y", "cell", "BC", "dipP", "pc1","pc2",'bimodal_pc2')], dens.pr, by = c("x", "y", "cell"))


# map out the places that are bimodal density, bimodal comp, stable both, bimodal both:

comp.bi <- ifelse (plspc2.m$BC > 0.55 & plspc2.m$dipP <= 0.05, "Bimodal Composition", "Unimodal Composition")
plspc2.m$bimodaldensity <- paste(plspc2.m$bimodaldensity, "Density")
plspc2.m$bimodalcomp <- comp.bi

plspc2.m$bimodalboth <- paste(plspc2.m$bimodaldensity, "&", plspc2.m$bimodalcomp)

unique(plspc2.m$bimodalboth)

all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota','wisconsin','michigan',"illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)

png(height=6, width = 6, units="in", res=300, "outputs/cluster/map_pls_comp_and_dens_bimodality.png")
ggplot(plspc2.m, aes(x, y, fill= bimodalboth))+geom_raster()+theme_bw()+
  scale_fill_manual(values = c('#ca0020',
    '#f4a582',
    '#92c5de',
    '#0571b0'))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  coord_equal() +xlab(" ")+ ylab(" ")+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.4,'lines'),legend.background = element_rect(fill=alpha('transparent', 0.4)),legend.position = "bottom", legend.direction = "vertical",legend.title = element_blank(),
                      panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))

dev.off()

# plot bimodal in terms of tree density
png(height=6, width = 6, units="in", res=300, "outputs/cluster/map_pls_dens_bimodality.png")
ggplot(plspc2.m, aes(x, y, fill= bimodaldensity))+geom_raster()+theme_bw()+
  scale_fill_manual(values = c('#ca0020',
                               #'#f4a582',
                               #'#92c5de',
                               '#0571b0'))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  coord_equal() +xlab(" ")+ ylab(" ")+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                                            panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1), legend.title = element_blank())+ggtitle("") 
dev.off()

png(height=6, width = 6, units="in", res=300, "outputs/cluster/map_pls_comp_bimodality.png")
ggplot(plspc2.m, aes(x, y, fill= bimodalcomp))+geom_raster()+theme_bw()+
  scale_fill_manual(values = c('#ca0020',
                               #'#f4a582',
                               #'#92c5de',
                               '#0571b0'))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  coord_equal() +xlab(" ")+ ylab(" ")+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                                            panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1), legend.title = element_blank())+ggtitle("") 
dev.off()


#------------- make the same map of both bimodalities for the modern landscape:-----
fia.dens <- read.csv("outputs/FIA_pls_density_with_bins.csv")

dens.prfia <- merge(fia.dens[,c("x", "y", "cell","FIAdensity")], dens.pr, by = c("x","y", "cell"))
ggplot(dens.prfia, aes(x,y,fill = FIAdensity))+geom_raster()

# get bimodal places in FIA
fia.bim <- bimodal.df(data = fia.dens, binby = "PC1fiabins",density = "FIAdensity", binby2 = "PC1fiabins")

colnames(fia.bim)[1:8] <- c("fiadensbins", "fiaV1dens", "fiaV2dens", "fiaBCdens", "fiadipPdens", "fialowdens", "fiahighdens", "fiaNA.")
colnames(fia.bim)[93] <- c("fiabimodaldensity")
fiapc2.m <- merge(fia.pc2[,c("x", "y", "cell", "BC", "dipP", "pc1","pc2",'bimodal_pc2')], fia.bim, by.x = c("x", "y", "cell"), by.y = c("x", "y", "cell"))


# map out the places that are bimodal density, bimodal comp, stable both, bimodal both:

fcomp.bi <- ifelse (fiapc2.m$BC > 0.55 & fiapc2.m$dipP <= 0.05, "Bimodal Composition", "Unimodal Composition")
fiapc2.m$bimodaldensity <- paste(fiapc2.m$fiabimodaldensity, "Density")
fiapc2.m$bimodalcomp <- fcomp.bi

fiapc2.m$bimodalboth <- paste(paste0(fiapc2.m$fiabimodaldensity, " Density"), "&", fiapc2.m$bimodalcomp)

unique(fiapc2.m$bimodalboth)

png(height=6, width = 6, units="in", res=300, "outputs/cluster/map_FIA_comp_and_dens_bimodality.png")

ggplot(fiapc2.m, aes(x, y, fill= bimodalboth))+geom_raster()+theme_bw()+
  scale_fill_manual(values = c('#ca0020',
                               #'#f4a582',
                               '#92c5de',
                               '#0571b0'))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  coord_equal() +xlab(" ")+ ylab(" ")+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.4,'lines'),legend.background = element_rect(fill=alpha('transparent', 0.4)),legend.position = "bottom", legend.direction = "vertical",legend.title = element_blank(),
                                            panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))
dev.off()

png(height=6, width = 6, units="in", res=300, "outputs/cluster/map_FIA_dens_bimodality.png")

ggplot(fiapc2.m, aes(x, y, fill= fiabimodaldensity))+geom_raster()+theme_bw()+
  scale_fill_manual(values = c('#ca0020',
                               #'#f4a582',
                               #'#92c5de',
                               '#0571b0'))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  coord_equal() +xlab(" ")+ ylab(" ")+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                                             panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1), legend.title = element_blank())+ggtitle("") 
dev.off()

png(height=6, width = 6, units="in", res=300, "outputs/cluster/map_FIA_comp_bimodality.png")

ggplot(fiapc2.m, aes(x, y, fill= bimodalcomp))+geom_raster()+theme_bw()+
  scale_fill_manual(values = c('#ca0020',
                               #'#f4a582',
                               #'#92c5de',
                               '#0571b0'))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  coord_equal() +xlab(" ")+ ylab(" ")+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.6,'lines'), legend.position = c(0.205, 0.125),legend.background = element_rect(fill=alpha('transparent', 0.4)),
                                            panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1), legend.title = element_blank())+ggtitle("") 
dev.off()
##--------------------------------------------------------------------------

png(height=6, width = 6, units="in", res=300, 'outputs/cluster/map_pls_comp_bimodality.png')
ggplot(plspc2.m, aes(x, y, fill= bimodal_pc2))+geom_raster()+theme_bw()+
  scale_fill_manual(values = c('#762a83','#1b7837'))+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  coord_equal()+theme(axis.text = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.8,'lines'),legend.background = element_rect(fill=alpha('transparent', 0.4)), legend.position = c(0.2,0.1),legend.title=element_blank(),
                      panel.grid.major = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1))
dev.off()

ggplot(plspc2.m, aes(PLSdensity, pc2))+geom_point()+geom_density2d()

#----------------Bimodality criteria over all the data--------------------
#comp.bimodal(data=fc.m, binby = "PC1bins", density = "pc1", time= "FIA")
#comp.bimodal(data=fc.m, binby = "PC1bins", density = "pc1", time= "PLS") 
comp.bimodal.full <- function(data = fc.m, binby, density){
  
  bins <- as.character(unique(data[,binby]))
  coeffs <- matrix(NA, length(bins), 2)
  for (i in 1:length(bins)){
    coeffs[i,1]<- bimodality_coefficient(na.omit(data[data[,binby] %in% bins[i], c(density)]))
    coeffs[i,2] <- diptest::dip.test(na.omit(density(data[data[,binby] %in% bins[i], c(density)])$y))$p
  }
  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
  coef.new <- strsplit(as.character(coef.bins$bins), " - ")
  library(plyr)
  coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
  colnames(coef.new) <- c("low", "high")
  coef.bins <- cbind(coef.bins, coef.new)
  
  #merge bins with the "binby" column
  merged <- merge(coef.bins, data, by.x = "bins", by.y = binby)
  
  
  #define bimodality
  #merged$bimodal <- "Unimodal"
  #criteria for bimodality
  
  bi <- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05, "Bimodal", "Unimodal")
  merged$bimodal <- bi
  
  
  unique(merged$bimodal)
  ggplot(merged, aes(pc2, fill = period))+geom_histogram(alpha=0.6,position = 'identity')+facet_wrap(~bins)
  ggplot(merged, aes(PC1, pc2, color = bimodal, shape = period))+geom_point()
  
  ggplot()+ # geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), color = 'black', fill = 'white')+
    geom_raster(data = merged, aes(x = x, y = y, fill = bimodal))+
    theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                      axis.title.x=element_blank(),
                      axis.title.y=element_blank())+
    scale_fill_manual(values = c("red", 'blue'), limits= c("Bimodal", "Unimodal"))+
    xlab("easting") + ylab("northing") +coord_equal() +facet_grid(~period)
  
}

comp.bimodal.full(data = fc.m, binby="PC1bins", density = "pc2")

fc.m$PC1_bins_f = factor(fc.m$PC1bins, levels=c('-5 - -4','-4 - -3',
                                                '-3 - -2','-2 - -1',
                                                '-1 - 0', '0 - 1',
                                                '1 - 2', '2 - 3', 
                                                '3 - 4', '4 - 5', 
                                                '5 - 6'))

# plot the composition histograms by bin and period:
png(height = 6, width =8,units = 'in',res=300,"outputs/cluster/composition_pc1_hists_by_PC1bins.png")
ggplot(fc.m, aes(pc1, fill = period)) + geom_histogram(alpha = 0.5, position = 'identity')+
  facet_wrap(~PC1_bins_f, ncol = 4 )
dev.off()

# get the cells that have data in PLS and FIA
PLS.cell <- fc.m[fc.m$period %in% "Past",]
FIA.cell <- fc.m[fc.m$period %in% "Modern",]

common <- merge(PLS.cell[,c("x", "y","cell")], FIA.cell[,c("x", "y", "cell")], by = c("x", "y", "cell"))
both <- common$cell

png(height = 6, width =8,units = 'in',res=300,"outputs/cluster/composition_pc2_hists_by_PC1bins.png")
ggplot(fc.m[fc.m$cell %in% both,], aes(pc2, fill = period,alpha = period)) + geom_histogram( position = 'identity')+ scale_alpha_discrete(limits=c("Past", "Modern"),range=c(0.4, 1))
  facet_wrap(~PC1_bins_f, ncol = 4 )+ xlab("Species composition PC2")
dev.off()

# zoom in on intermediate environment space:
png(height = 4.5, width = 5.5, units = 'in', res = 300, "outputs/cluster/comphist_by_period_envtpc1_mid.png")

fc.m$PC1_bins_f2 <- factor(fc.m$PC1_bins_f, levels = c("5 - 6", "4 - 5", "3 - 4",
                                                       "2 - 3", "1 - 2", "0 - 1", "-1 - 0",
                                                       "-2 - -1", "-3 - -2", "-4 - -3",
                                                       "-5 - -4"))

ggplot(fc.m[fc.m$cell %in% both & fc.m$PC1_bins_f2 %in% c('0 - 1', '1 - 2','-1 - 0'),], aes(pc2, fill = period)) + geom_histogram(alpha = 0.5, position = 'identity', bins = 26)+xlim(2.5, -3)+
  facet_wrap(~PC1_bins_f2, ncol = 4 )+ xlab("Species composition PC2")+#scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+
  coord_flip()+theme_bw(base_size =15)+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5), legend.position = "bottom", legend.title=element_blank())
dev.off()

png(height = 3, width = 5, units = 'in', res = 300, "outputs/cluster/comp_smooth_by_period_envtpc1_mid_ghostfigs.png")
fia.ghost.c<- ggplot(fc.m[fc.m$cell %in% both & fc.m$PC1_bins_f %in% c('-1 - 0', '0 - 1', '1 - 2'),], aes(pc2, fill = period, alpha = period, linetype = period))  + geom_density( position = 'identity')+ scale_alpha_discrete(limits=c("Past", "Modern"),range=c(0.1, 1), guide = FALSE)+xlim(-3,2)+
  facet_wrap(~PC1_bins_f, ncol = 4 )+theme_bw()+scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+coord_flip()+theme(axis.title.x=element_blank(),
                                                                                                                                              axis.text.x=element_blank(),
                                                                                                                                              axis.ticks.x=element_blank())+xlab("Species composition PC2")
fia.ghost.c
dev.off()

png(height = 3, width = 5, units = 'in', res = 300, "outputs/cluster/comp_smooth_PLSonly_envtpc1_mid_ghostfigs.png")
pls.only.c<- ggplot(fc.m[fc.m$cell %in% both & fc.m$PC1_bins_f %in% c('-1 - 0', '0 - 1', '1 - 2') & fc.m$period %in% "Past",], aes(pc2, fill = period))  + geom_density( position = 'identity')+ xlim(-3,2)+#scale_alpha_discrete(limits=c("Past", "Modern"),range=c(1, 1), guide = FALSE)+
  facet_wrap(~PC1_bins_f, ncol = 4 )+theme_bw()+scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+coord_flip()+theme(axis.title.x=element_blank(),
                                                                                                                                              axis.text.x=element_blank(),
                                                                                                                                              axis.ticks.x=element_blank())+xlab("Species composition PC2")
pls.only.c
dev.off()

png("outputs/cluster/overlaid_PC2_hists.png")
ggplot(fc.m[fc.m$cell %in% both,], aes(pc2, fill = period)) + geom_histogram(alpha = 0.5, position = 'identity')+
   xlab("Species composition PC2")+theme_bw(base_size = 10)
dev.off()

png("outputs/cluster/overlaid_PC1_hists.png")
ggplot(fc.m[fc.m$cell %in% both,], aes(pc1, fill = period)) + geom_histogram(alpha = 0.5, position = 'identity')+
  xlab("Species composition PC1")+theme_bw(base_size = 10)
dev.off()

# digging into the bimodal places in envtPC1 from -1 to 2:
ggplot(fc.m[fc.m$PC1bins %in% "0 - 1",], aes(pc2, Oak, color = period))+geom_point()
ggplot(fc.m[fc.m$PC1bins %in% "0 - 1",], aes(pc2, Hemlock, color = period))+geom_point()
ggplot(fc.m[fc.m$PC1bins %in% "0 - 1",], aes(pc2, Maple, color = period))+geom_point()
ggplot(fc.m[fc.m$PC1bins %in% "0 - 1",], aes(pc2, Elm, color = period))+geom_point()


fc.bim <- fc.m[fc.m$PC1bins %in% c("0 - 1", "-1 - 0", "1 - 2", "2-3"),]

fc.bim.m <- melt(fc.bim, id.vars = c('x', 'y','cell','X', 'period','pc1', 'pc2',
                         'ecotype','bimodal', 'PC1', "PC2", "PC1bins", "PC2bins", "PC1_bins_f"))

impt.spec <- fc.bim.m[fc.bim.m$variable %in% c('Oak', "Hemlock", "Maple", "Beech"),] 
impt.spec$variable_f  = factor(impt.spec$variable, levels=c('Oak','Maple','Hemlock','Beech'))

  
png(height = 3, width = 9, units = 'in',res=300,'outputs/cluster/species_composition_nonzero_mid_hists.png')
ggplot(impt.spec[ impt.spec$value > 0 & impt.spec$PC1_bins_f %in% c("-1 - 0", "0 - 1", "1 - 2") & impt.spec$cell %in% both,], aes(value*100, fill = period))+geom_histogram(position = 'identity', alpha = 0.5)+facet_wrap(~variable_f, ncol = 4)+
  xlab("Species Percent Composition")+theme_bw(base_size = 12)
dev.off()

#ggplot(fc.bim.m[fc.bim.m$variable %in% c('Oak', "Hemlock", "Maple", "Beech") & fc.bim.m$value > 0 & fc.bim.m$PC1_bins_f %in% c("-1 - 0", "0 - 1", "1 - 2"),], aes(value*100,pc2, fill = period))+geom_point()+facet_wrap(~variable)+
 # xlab("Species Percent Composition")

ggplot(fc.bim.m[fc.bim.m$variable %in% c('Oak', "Hemlock", "Maple", "Beech") & fc.bim.m$value > 0,], aes(PC1,value))+geom_point()+facet_wrap(~variable)



# ploting the environmental bins
ggplot(fc.bim.m, aes(pc2, value, color = variable))+geom_point()+stat_smooth(position = 'identity')+facet_wrap(~period)
ggplot(fc.bim.m, aes(PC1, value, color = variable))+geom_point()+facet_wrap(~period)

ggplot(fc.bim.m[fc.bim.m$PC1bins %in% "0 - 1" ,], aes(pc2, value, color = variable))+geom_smooth(position = 'identity', method = 'loess')+facet_wrap(~period)

png("outputs/cluster/PLS_envt_bins_map.png")
pls.envt <- ggplot(fc.m[fc.m$period %in% "Past",], aes(x,y, fill= PC1_bins_f))+geom_raster()+coord_equal()+theme_bw()+
 geom_polygon( data = mapdata,aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                      axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                      axis.title.x=element_blank(),
                                                                      axis.title.y=element_blank())+xlab("easting") + ylab("northing") +coord_equal()+scale_fill_manual(values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c',
    '#fb9a99','#e31a1c','#fdbf6f','#66c2a5','#cab2d6','#6a3d9a', "#878787"))
pls.envt
dev.off()

png("outputs/cluster/FIA_envt_bins_map.png")
fia.envt<- ggplot(fc.m[fc.m$period %in% "Modern",], aes(x,y, fill= PC1_bins_f))+geom_raster()+coord_equal()+theme_bw()+
  geom_polygon( data = mapdata,aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                   axis.title.x=element_blank(),
                                                                                                   axis.title.y=element_blank())+xlab("easting") + ylab("northing") +coord_equal()+
  scale_fill_manual(name = "envPC1 bins",values = c('#a6cee3','#1f78b4','#b2df8a','#33a02c',
                               '#fb9a99','#e31a1c','#fdbf6f','#66c2a5','#cab2d6','#6a3d9a', '#543005',"#878787"))
fia.envt
dev.off()

png(height = 4, width = 8, units ="in",res=200,"outputs/cluster/PC1_envt_bins_maps.png")
grid_arrange_shared_legend(fia.envt + ggtitle("FIA"), pls.envt + ggtitle("PLS"), ncol = 2, nrow=1, position = 'right')
dev.off()

# select species with higest compositions to look at in these graphs

summary(fc.bim.m)
spec <- c("Oak","Maple","Hemlock", "Birch", "Beech", "Ash", "Cherry", "Poplar", "Pine", "Elm")

#ggplot(fc.bim.m[fc.bim.m$PC1bins %in% "0 - 1" & fc.bim.m$variable %in% spec & fc.bim.m$cell %in% both,], aes(pc2, value, color = variable))+geom_smooth(position = 'identity', method = 'loess')+geom_point()+facet_wrap(~period)

env.PC0.1 <- ggplot(fc.bim.m[fc.bim.m$PC1bins %in% c("0 - 1") & fc.bim.m$variable %in% spec & fc.bim.m$cell %in% both,], aes(pc2, value, color = variable))+
            geom_smooth(position = 'identity', method = 'loess')+
            scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33','#a65628','#f781bf','#999999', "black"), limits = spec)+facet_wrap(~period) + theme_bw() + ylab("Composition") + xlab("composition pc2 values")+ggtitle ("Composition over environmental PC1 range 0-1")

env.PCn1.0 <- ggplot(fc.bim.m[fc.bim.m$PC1bins %in% c("-1 - 0") & fc.bim.m$variable %in% spec  & fc.bim.m$cell %in% both,], aes(pc2, value, color = variable))+
            geom_smooth(position = 'identity', method = 'loess')+
            scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33','#a65628','#f781bf','#999999', "black"), limits = spec)+facet_wrap(~period) + theme_bw() + ylab("Composition") + xlab("composition pc2 values")+ggtitle ("Composition over environmental PC1 range -1 - 0")

env.PC1.2 <- ggplot(fc.bim.m[fc.bim.m$PC1bins %in% c("1 - 2") & fc.bim.m$variable %in% spec  & fc.bim.m$cell %in% both,], aes(pc2, value, color = variable))+
            geom_smooth(position = 'identity', method = 'loess')+
            scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33','#a65628','#f781bf','#999999', "black"), limits = spec)+xlim(-3,2)+facet_wrap(~period) + theme_bw() + ylab("Composition") + xlab("composition pc2 values")+ggtitle ("Composition over environmental PC1 range 1- 2")

source("R/grid_arrange_shared_legend.R")
png(height = 9, width = 6, units = 'in', res = 300, "outputs/cluster/composition_by_species_bimodal_bins.png")
grid_arrange_shared_legend(env.PCn1.0, env.PC0.1,env.PC1.2, ncol = 1, nrow = 3)
dev.off()



#ggplot(fc.bim.m[fc.bim.m$PC1bins %in% c("0 - 1") & fc.bim.m$variable %in% spec & fc.bim.m$cell %in% both,], aes(pc2, value, color = variable))+
 # geom_density2d(position = 'stack')+
  #scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33','#a65628','#f781bf','#999999', "black"), limits = spec)+facet_wrap(~period) + theme_bw() + ylab("Composition") + xlab("composition pc2 values")+ggtitle ("Composition over environmental PC1 range 0-1")


env.PC0.1.point <- ggplot(fc.bim.m[fc.bim.m$PC1bins %in% c("0 - 1") & fc.bim.m$variable %in% spec  & fc.bim.m$cell %in% both,], aes(pc2, value, color = variable))+
  geom_point()+
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33','#a65628','#f781bf','#999999', "black"), limits = spec)+facet_wrap(~period) + theme_bw() + ylab("Composition") + xlab("composition pc2 values")+ggtitle ("Composition over environmental PC1 range 0-1")

env.PCn1.0.point <- ggplot(fc.bim.m[fc.bim.m$PC1bins %in% c("-1 - 0") & fc.bim.m$variable %in% spec  & fc.bim.m$cell %in% both,], aes(pc2, value, color = variable))+
  geom_point()+
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33','#a65628','#f781bf','#999999', 'black'), limits = spec)+facet_wrap(~period) + theme_bw() + ylab("Composition") + xlab("composition pc2 values")+ggtitle ("Composition over environmental PC1 range -1 - 0")

env.PC1.2.point <- ggplot(fc.bim.m[fc.bim.m$PC1bins %in% c("1 - 2") & fc.bim.m$variable %in% spec & fc.bim.m$cell %in% both,], aes(pc2, value, color = variable))+
  geom_point()+
  scale_color_manual(values = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00', '#ffff33','#a65628','#f781bf','#999999','black'), limits = spec)+facet_wrap(~period) + theme_bw() + ylab("Composition") + xlab("composition pc2 values")+ggtitle ("Composition over environmental PC1 range -1 - 0")


png(height = 9, width = 6, units = 'in', res = 300, "outputs/cluster/composition_by_species_bimodal_bins_points.png")
grid_arrange_shared_legend(env.PCn1.0.point, env.PC0.1.point, env.PC1.2.point, ncol = 1, nrow = 3)
dev.off()



# lets find how much composition of each species has changed:
PLS.oak <- fc.m[fc.m$period %in% "Past",]
PLS.oak <- merge(PLS.oak, dens.pr[,c("x", 'y', "cell", "PLSdensity")], by = c("x", "y", 'cell'))

FIA.oak <- fc.m[fc.m$period %in% "Modern",]
fcomps <- fia.by.cell
fcomps <- fcomps[fcomps$cell %in% density.full$cell, ]
fcomps$FIAdensity <- rowSums(fcomps[,4:34], na.rm=TRUE)
FIA.oak <- merge(FIA.oak, fcomps[c("x", 'y', 'cell', "FIAdensity")], by = c('x', "y", "cell"))

# look at the places that had more than 50% oak composition
oak50 <- PLS.oak[PLS.oak$Oak >= 0.50,]$cell
pls.oak50 <- PLS.oak[PLS.oak$cell %in% oak50,]
fia.oak50 <- FIA.oak[FIA.oak$cell %in% oak50,]
full.oak50 <- rbind(pls.oak50[,1:51], fia.oak50[,1:51])


scatter <- ggplot(full.oak50, aes(PC1, pc2, color = period))+geom_point()+theme_bw()+ylab("Species composition PC2")+xlab('Environmental PC1')
histo <- ggplot(full.oak50, aes(pc2, fill = period))+geom_histogram(alpha=0.5, position = 'identity')+theme_bw()
source("R/grid_arrange_shared_legend.R")

png("outputs/cluster/PLS_oak_50pct_hist_scatter.png")
grid_arrange_shared_legend(scatter, histo, nrow=2, ncol=1)
dev.off()

oaks <- merge(PLS.oak[,c("x", "y","cell", "Oak","Maple","Pine", "Beech", "Hemlock", "Cherry")], FIA.oak[,c("x", "y", "cell", "Oak", "Maple","Pine", "Beech", "Hemlock", "Cherry", "FIAdensity")], by = c("x", "y", "cell"))

oaks$oakdiff <- oaks$Oak.y - oaks$Oak.x
oaks$maplediff <- oaks$Maple.y - oaks$Maple.x
oaks$pinediff <- oaks$Pine.y - oaks$Pine.x
oaks$beechdiff <- oaks$Beech.y - oaks$Beech.x
oaks$hemlockdiff <- oaks$Hemlock.y - oaks$Hemlock.x
oaks$cherrydiff <- oaks$Cherry.y - oaks$Cherry.x
# negative values = a complete removal of oak on the modern landscape
# positive values = a 100% increase in oak on the modern landscape, 0 = no change


oak.diff <- ggplot(oaks, aes(x, y, fill = oakdiff)) + geom_raster()+coord_equal()+scale_fill_gradient2()+
            geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                   axis.title.x=element_blank(),
                                                                                                   axis.title.y=element_blank())+xlab("easting") + ylab("northing") +coord_equal()



maple.diff <- ggplot(oaks, aes(x, y, fill = maplediff)) + geom_raster()+coord_equal()+scale_fill_gradient2()+
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                     axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                     axis.title.x=element_blank(),
                                                                                                     axis.title.y=element_blank())+
    xlab("easting") + ylab("northing") +coord_equal()


pine.diff<- ggplot(oaks, aes(x, y, fill = pinediff)) + geom_raster()+coord_equal()+scale_fill_gradient2()+
    geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                   axis.title.x=element_blank(),
                                                                                                   axis.title.y=element_blank())+xlab("easting") + ylab("northing") +coord_equal()

hemlock.diff<- ggplot(oaks, aes(x, y, fill = hemlockdiff)) + geom_raster()+coord_equal()+scale_fill_gradient2()+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                   axis.title.x=element_blank(),
                                                                                                   axis.title.y=element_blank())+xlab("easting") + ylab("northing") +coord_equal()




beech.diff<- ggplot(oaks, aes(x, y, fill = beechdiff)) + geom_raster()+coord_equal()+scale_fill_gradient2()+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                   axis.title.x=element_blank(),
                                                                                                   axis.title.y=element_blank())+xlab("easting") + ylab("northing") +coord_equal()


cherry.diff<- ggplot(oaks, aes(x, y, fill = cherrydiff)) + geom_raster()+coord_equal()+scale_fill_gradient2()+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                   axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                   axis.title.x=element_blank(),

                                                                                                                                                                                                    axis.title.y=element_blank())+xlab("easting") + ylab("northing") +coord_equal()

#----------Does the species bimodality correspond to density bimodality?

# add the density to the dataframe to evaluate:
colnames(FIA.oak)[53] <- "Density"
colnames(PLS.oak)[53] <- "Density"

full <- rbind(FIA.oak, PLS.oak)

png(height = 6, width = 4, units = "in", res = 300, "outputs/cluster/density_histogram_masked.png")
ggplot(full[full$cell %in% both, ], aes(Density, fill = period)) + geom_histogram(alpha = 0.5, position = 'identity', bins = 25)+xlim(0,700)+coord_flip()+theme_bw(base_size = 12)+xlab("Tree Density (stems/ha)")
dev.off()

full$PC1_bins_f2 <- factor(full$PC1_bins_f, levels = c("5 - 6", "4 - 5", "3 - 4",
                                                       "2 - 3", "1 - 2", "0 - 1", "-1 - 0",
                                                       "-2 - -1", "-3 - -2", "-4 - -3",
                                                       "-5 - -4"))

png(height = 6, width =8,units = 'in',res=300, 'outputs/cluster/density_by_period_envtpc1.png')
ggplot(full[full$cell %in% both, ], aes(Density, fill = period)) + geom_histogram(alpha = 0.5, position = 'identity')+xlim(0,800)+
  facet_wrap(~PC1_bins_f2, ncol = 4 )
dev.off()


png(height = 4.5, width = 5.5, units = 'in', res = 300, "outputs/cluster/densityhist_by_period_envtpc1_mid.png")
ggplot(full[full$cell %in% both & full$PC1_bins_f %in% c('-1 - 0', "0 - 1", "1 - 2"), ], aes(Density, fill = period)) + geom_histogram(alpha = 0.5, position = 'identity', bins = 26)+xlim(0,700)+
  facet_wrap(~PC1_bins_f2, ncol = 4 )+theme_bw(base_size = 12)+#scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+
  coord_flip()+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5), legend.title = element_blank(), legend.position = "bottom")+xlab("Tree Density (stems/ha)")
dev.off()

png(height = 3, width = 5, units = 'in', res = 300, "outputs/cluster/densitysmooth_by_period_envtpc1_mid.png")
ggplot(full[full$cell %in% both & full$PC1_bins_f %in% c('-1 - 0', "0 - 1", "1 - 2"), ], aes(Density, fill = period)) + geom_density(alpha = 0.5, position = 'identity')+xlim(0,700)+
  facet_wrap(~PC1_bins_f, ncol = 4 )+theme_bw()+scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+coord_flip()+theme(axis.title.x=element_blank(),
                                                                                                                                               axis.text.x=element_blank(),
                                                                                                                                               axis.ticks.x=element_blank())
dev.off()

# make the ghost figures the intemediate envrionment for figure two:
png(height = 3, width = 5, units = 'in', res = 300, "outputs/cluster/density_smooth_by_period_envtpc1_mid_ghostfigs.png")
fia.ghost.d<- ggplot(full[full$cell %in% both & full$PC1_bins_f %in% c('-1 - 0', '0 - 1', '1 - 2'),], aes(Density, fill = period, alpha = period, linetype = period))  + geom_density( position = 'identity')+ scale_alpha_discrete(limits=c("Past", "Modern"),range=c(0.2, 1), guide = FALSE)+xlim(0,700)+
  facet_wrap(~PC1_bins_f, ncol = 4 )+theme_bw()+scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+coord_flip()+theme(axis.title.x=element_blank(),
                                                                                                                                              axis.text.x=element_blank(),
                                                                                                                                              axis.ticks.x=element_blank())+xlab("Tree Density (trees/ha)")
fia.ghost.d
dev.off()

# make a figure with the PLS only (no modern ghost) for figure 2
png(height = 3, width = 5, units = 'in', res = 300, "outputs/cluster/density_smooth_PLSonly_envtpc1_mid_ghostfigs.png")
pls.only.d<- ggplot(full[full$cell %in% both & full$PC1_bins_f %in% c('-1 - 0', '0 - 1', '1 - 2') & full$period %in% "Past",], aes(Density, fill = period))  + geom_density( position = 'identity')+ xlim(0,700)+#scale_alpha_discrete(limits=c("Past", "Modern"),range=c(1, 1), guide = FALSE)+
  facet_wrap(~PC1_bins_f, ncol = 4 )+theme_bw()+scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+coord_flip()+theme(axis.title.x=element_blank(),
                                                                                                                                              axis.text.x=element_blank(),
                                                                                                                                              axis.ticks.x=element_blank())+xlab("Tree Density (trees/ha)")
pls.only.d
dev.off()

# combine Density and compositon into the same figure for figure 2 and 3 
source("R/grid_arrange_shared_legend.R")
png(height = 5, width = 5, units = 'in', res = 300, "outputs/paper_figs/fig2_pls_density_comp.png")
grid_arrange_shared_legend(pls.only.d, pls.only.c, ncol = 1, nrow = 2)
dev.off()

# combine composition and density for the FIA ghost figures
png(height = 5, width = 5, units = 'in', res = 300, "outputs/paper_figs/fig3_fia_plsghost_density_comp.png")
grid_arrange_shared_legend(fia.ghost.d, fia.ghost.c, ncol = 1, nrow = 2)
dev.off()

# map out the intemediate places on modern landscape, and the two modes on the pls land scape:
plot(density(full[full$cell %in% both & full$period %in% "Past",]$Density))
plot(density(full[full$cell %in% both & full$period %in% "Modern",]$Density))

# find width at half maximum for modern data:

d <- density(full[full$cell %in% both & full$period %in% "Past",]$Density)
plot(d)
xmax <- d$x[d$y==max(d$y)]

x1 <- d$x[d$x < xmax][which.min(abs(d$y[d$x < xmax]-max(d$y)/2))]
x2 <- d$x[d$x > xmax][which.min(abs(d$y[d$x > xmax]-max(d$y)/2))]

points(c(x1, x2), c(d$y[d$x==x1], d$y[d$x==x2]), col="red")

# the total width is x2-x1
width <- x2-x1

# range of the "Modern Intermediate state" should be 149.9-(1/2)*224.1
min.int <- xmax-abs(0.5*width)
max.int <- xmax+abs(0.5*width)
full$distn.range <- "test"
full$distn.range <- ifelse(full$Density >= min.int & full$Density <= max.int, "Intermediate", ifelse(full$Density < min.int, "Low", "High"))

X11(width = 12)
ggplot(full[full$cell %in% both,], aes(x,y, fill = distn.range))+geom_raster()+facet_wrap(~period)+theme_bw()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                                                                                                                               axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                                                                                                               axis.title.x=element_blank(),
                                                                                                                                                                                                               axis.title.y=element_blank())+
  xlab("easting") + ylab("northing") +coord_equal()

ggplot(full[full$cell %in% both, ], aes(Density, fill = period)) + geom_histogram(alpha = 0.5, position = 'identity', bins = 25)+xlim(0,700)+coord_flip()+theme_bw(base_size = 12)+xlab("Tree Density (stems/ha)")+geom_vline(aes(xintercept=53))+geom_vline(aes(xintercept=69))+geom_vline(aes(xintercept=172))


# Altenative methods: find where the kernal density estimates intersect:
df.pls <- density(full[full$cell %in% both & full$period %in% "Past",]$Density, bw=15)
df.fia <- density(full[full$cell %in% both & full$period %in% "Modern",]$Density, bw=15)

# find points where 
max(df.pls$y[df.pls$x < 150])
peak1x <- which (df.pls$y == max(df.pls$y[df.pls$x < 150]))
peak2x <- which (df.pls$y == max(df.pls$y[df.pls$x > 75]))


df.pls$x[peak1x]
df.pls$x[peak2x]

xminima <- d$x[d$y==min(d$y[which(d$x >75 & d$x <150)])]
xminimay <- d$y[d$y==min(d$y[which(d$x >75 & d$x <150)])]




# the total width is x2-x1
width1 <- xminima-peak1x
width2 <- peak2x-xminima
# range of the "Modern Intermediate state" should be 149.9-(1/2)*224.1
min.int <- xminima-abs(0.5*width1)
max.int <- xminima+abs(0.5*width2)

d <- density(full[full$cell %in% both & full$period %in% "Past",]$Density)
plot(d)
xminima <- d$x[d$y==min(d$y[which(d$x >75 & d$x <150)])]

x1 <- min.int
x2 <- max.int
points(c(x1, x2), c(0, 0), col="red")

full$distn.class <- ifelse(full$Density >= min.int & full$Density <= max.int, "Intermediate", ifelse(full$Density < min.int, "Low", "High"))


# of these, 53 and 172 seem to capture the intemediate peak well and separate the PLS peaks
png("outputs/cluster/hist_distn_classes.png")
ggplot(full[full$cell %in% both, ], aes(Density, fill = period)) + geom_histogram(alpha = 0.5, position = 'identity', bins = 25)+xlim(0,700)+coord_flip()+theme_bw(base_size = 12)+xlab("Tree Density (stems/ha)")+geom_vline(aes(xintercept=82.3))+geom_vline(aes(xintercept=155))
dev.off()

#classify the full dataset based on these criteria


# map out the classes for the full dataset (not exculding points not in FIA):

png(height = 4, width = 8, units = "in", res=300,"outputs/cluster/distn_class_maps.png")
ggplot(full, aes(x,y, fill = distn.class))+geom_raster()+coord_equal()+facet_wrap(~period)+theme_bw()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                                                                                                                                             axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                                                                                                                                             axis.title.x=element_blank(),
                                                                                                                                                                                                                             axis.title.y=element_blank())+
  xlab("easting") + ylab("northing")

dev.off()

#--------------------Does the bimodality occur at the same place?-------------
# use the comp.bimodal.df function to get the bimodal designations:
PLSdens <- comp.bimodal.df(full, binby = "PC1bins", density = "Density", time = "PLS")
FIAdens <- comp.bimodal.df(full, binby = "PC1bins", density = "Density", time = "FIA")

PLSpc2 <- comp.bimodal.df(full, binby = "PC1bins", density = "pc2", time = "PLS")
FIApc2 <- comp.bimodal.df(full, binby = "PC1bins", density = "pc2", time = "FIA")


ggplot(PLSdens, aes(x=x,y=y,fill = bimodal_Density))+geom_raster()
ggplot(FIAdens, aes(x=x, y=y, fill = bimodal_Density))+geom_raster()

ggplot(PLSpc2, aes(x=x,y=y,fill = bimodal_pc2))+geom_raster()
ggplot(FIApc2, aes(x=x, y=y, fill = bimodal_pc2))+geom_raster()

# need to merge back into the same df:
bi.comp <- rbind(PLSpc2[,c("x", "y", 'cell','period','bimodal_pc2')], FIApc2[,c("x", "y", 'cell','period','bimodal_pc2')])
bi.comp$bimodal_pc2 <- paste0(bi.comp$bimodal_pc2, " Composition")
bi.dens <- rbind(PLSdens[,c("x", "y", 'cell','period','bimodal_Density')], FIAdens[,c("x", "y", 'cell','period','bimodal_Density')])
bi.dens$bimodal_Density <- paste0(bi.dens$bimodal_Density, " Density")

bi.df <-merge(bi.comp, bi.dens, by = c('x','y','cell','period'))

full <- merge(full, bi.df, by = c('x','y','cell','period'))
full$biboth <- paste(full$bimodal_Density," & ",full$bimodal_pc2)

library(ggExtra)
# make plots of density vs composition with marginal histograms
# for PLS
p <- ggplot(full[full$period %in% "Past",], aes(pc2, Density, color = biboth))+geom_point(size = 0.75, alpha = 0.75)+scale_color_manual(values = c('#ca0020',
                                                                                                                                    '#f4a582',
                                                                                                                                    '#92c5de',
                                                                                                                                    '#0571b0'), limits =c("Bimodal Density  &  Bimodal Composition" ,"Bimodal Density  &  Unimodal Composition","Unimodal Density  &  Bimodal Composition" ,"Unimodal Density  &  Unimodal Composition"))+ylab("Tree Density (stems/hectare)")+xlab("Species Composition PC2")+theme(legend.position = "bottom")+xlim(-5, 2)+ggtitle('PLS')+guides(color=guide_legend(nrow=2,byrow=TRUE))
q <- ggMarginal(p, type = "histogram")

# plot out with colors for which is bimodal compositions and density
by.stabilityp<- ggplot(full[full$period %in% "Past",], aes(pc2, Density, color = biboth))+geom_point(size = 0.75)+scale_color_manual(values = c('#ca0020',
                                                                                                                                              '#f4a582',
                                                                                                                                              '#92c5de',
                                                                                                                                              '#0571b0'), limits =c("Bimodal Density  &  Bimodal Composition" ,"Bimodal Density  &  Unimodal Composition","Unimodal Density  &  Bimodal Composition" ,"Unimodal Density  &  Unimodal Composition"))+
  ylab("Tree Density (stems/hectare)")+xlab("Species Composition PC2")+xlim(-5, 2)+ggtitle('PLS')+facet_wrap(~biboth, ncol = 1)

by.stabilityf<- ggplot(full[full$period %in% "Modern",], aes(pc2, Density, color = biboth))+geom_point(size = 0.75)+scale_color_manual(values = c('#ca0020',
                                                                                                                                               '#f4a582',
                                                                                                                                               '#92c5de',
                                                                                                                                               '#0571b0'), limits =c("Bimodal Density  &  Bimodal Composition" ,"Bimodal Density  &  Unimodal Composition","Unimodal Density  &  Bimodal Composition" ,"Unimodal Density  &  Unimodal Composition"))+ylab("Tree Density (stems/hectare)")+xlab("Species Composition PC2")+xlim(-5, 2)+ggtitle('FIA')+facet_wrap(~biboth, ncol=1)

#png(height = 6, width = 5)
X11(width = 6)
full$label <- sample.int(13595, 13595, replace = TRUE)
full<- full[order(full$label,decreasing=TRUE),]

a<- ggplot(full[full$period %in% "Past",], aes(pc2, Density, color = biboth))+geom_point(alpha= 0.5)+scale_color_manual(values = c('#ca0020',
                                                                                                                               '#f4a582',
                                                                                                                               '#92c5de',
                                                                                                                               '#0571b0'), limits =c("Bimodal Density  &  Bimodal Composition" ,"Bimodal Density  &  Unimodal Composition","Unimodal Density  &  Bimodal Composition" ,"Unimodal Density  &  Unimodal Composition"))+
  ylab("Tree Density (stems/hectare)")+xlab("Species Composition PC2")+xlim(-5, 2)+ylim(0,1000)+ggtitle('PLS')

# use bins 
png("outputs/cluster/density_v_comp_by_bimodal_pls.png")
a <- a + geom_density2d(mapping = aes(pc2, Density), bins = 15, size =0.5)+theme_bw()+facet_wrap(~biboth, ncol=2)+theme(legend.position="bottom")
a
dev.off()

# do the same for FIA
b <- ggplot(full[full$period %in% "Modern",], aes(pc2, Density, color = biboth))+geom_point(alpha= 0.5)+scale_color_manual(values = c('#ca0020',
                                                                                                                                  '#f4a582',
                                                                                                                                  '#92c5de',
                                                                                                                                  '#0571b0'), limits =c("Bimodal Density  &  Bimodal Composition" ,"Bimodal Density  &  Unimodal Composition","Unimodal Density  &  Bimodal Composition" ,"Unimodal Density  &  Unimodal Composition"))+
  ylab("Tree Density (stems/hectare)")+xlab("Species Composition PC2")+xlim(-5, 2)+ylim(0,1000)+ggtitle('FIA')

# use bins 
png("outputs/cluster/density_v_comp_by_bimodal_fia.png")
b <- b + geom_density2d(mapping = aes(pc2, Density), bins = 15, size =0.5)+theme_bw()+facet_wrap(~biboth, ncol=2)+theme(legend.position="bottom")
b
dev.off()

# plot pls and fia together
png(width = 8, height = 10, units = "in", res = 300, "outputs/cluster/density_v_comp_by_bimodal_both.png")
grid_arrange_shared_legend(a, b, ncol = 1, nrow = 2, position = c("bottom"))
dev.off()


# for FIA
b <- ggplot(full[full$period %in% "Modern",], aes(pc2, Density, color = biboth))+geom_point(size = 0.75)+scale_color_manual(values = c('#ca0020',
                                                                                                                                    '#f4a582',
                                                                                                                                    '#92c5de',
                                                                                                                                    '#0571b0'), limits =c("Bimodal Density  &  Bimodal Composition" ,"Bimodal Density  &  Unimodal Composition","Unimodal Density  &  Bimodal Composition" ,"Unimodal Density  &  Unimodal Composition"))+theme(legend.position = "bottom")+ylab("Tree Density (stems/hectare)")+xlab("Species Composition PC2")+xlim(-5, 2)+ggtitle("FIA")+guides(color=guide_legend(nrow=2,byrow=TRUE))
f <- ggMarginal(b, type = "histogram")

png(width = 5, height = 10, units = "in", res = 200, "outputs/cluster/all_density_vs_comp.png")
grid.arrange(f,q, ncol=1)
dev.off()

#---------------- subsets of plots for only bimodal places-----------------------------
# for the PLS places that were significantly bimodal
p2 <- ggplot(full[full$period %in% "Past" & full$PC1bins %in% c("0 - 1", "1 - 2", "-5 - -4", "-4 - -3"),], aes(pc2, Density))+geom_point()+ylab("Tree Density (stems/hectare)")+xlab("Species Composition PC2")+xlim(-5, 2)+ggtitle("PLS")
q2 <- ggMarginal(p2, type = "histogram")


b2 <- ggplot(full[full$period %in% "Modern" & full$PC1bins %in% c("0 - 1", "1 - 2", "-5 - -4", "-4 - -3"),], aes(pc2, Density))+geom_point()+ylab("Tree Density (stems/hectare)")+xlab("Species Composition PC2")+xlim(-5, 2)+ggtitle("FIA")
f2 <- ggMarginal(b2, type = "histogram")

png(width = 5, height = 10, units = "in", res = 200, "outputs/cluster/sig_bimodal_density_vs_comp.png")
grid.arrange(f2, q2, ncol=1, top = "Compositional bimodal regions")
dev.off()



# defining the number of grid cells that support PLS composition bimodality:
summary(full[full$period %in% "Past",] )


#------------What is the support for/strength of the bimodality?--------------
# define ecotype for both fia and pls
ecotype  <- ifelse(full$Density == 0,  "prairie", 
                             ifelse(full$Density <= 47, "Savanna",
                                    ifelse(full$Density > 47, "Forest", "Check")))
full$ecotype <- factor(ecotype)

# for each environmental bin, calculate the ratio of forest to savanna:
bins <- as.character(unique(full[full$period %in% "Past",]$PC1_bins_f))
ratio <- data.frame(bins = bins, 
                    ratio = NA, 
                    count = NA,
                    savanna = NA,
                    forest = NA)

coeffs <- matrix(NA, length(bins), 2)

library(modes)
for(i in 1:length(bins)){
  df <- summary(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i],]$ecotype)
  ratio[i,]$ratio <- df[3]/df[1]
  ratio[i,]$count<- sum(df, na.rm=TRUE)
  ratio[i,]$savanna <- df[3]
  ratio[i,]$forest <- df[1]
    coeffs[i,1]<- bimodality_coefficient(na.omit(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i], 'pc2']))
    coeffs[i,2] <- diptest::dip.test(na.omit(density(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i], 'pc2'])$y))$p
    
    }

  coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
  coef.bins<- data.frame(cbind(coeffs, bins))
  coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
  coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
  coef.new <- strsplit(as.character(coef.bins$bins), " - ")
  library(plyr)
  coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
  colnames(coef.new) <- c("low", "high")
  coef.bins <- cbind(coef.bins, coef.new)
  
  #merge bins with the "bins" column
  merged <- merge(coef.bins, ratio, by = "bins")
  #criteria for bimodality
  bi <- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05, "Bimodal", "Unimodal")
  merged$bimodal <- bi
  
  
  
saveRDS(merged, "outputs/cluster/PLS_ratio_sav_forest.rds")

merged$bins_factor = factor(merged$bins, levels=c('-5 - -4','-4 - -3',
                                                '-3 - -2','-2 - -1',
                                                '-1 - 0', '0 - 1',
                                                '1 - 2', '2 - 3', 
                                                '3 - 4', '4 - 5', 
                                                '5 - 6'))

# get the values for whether the bins are bimodal from above to color by significance:
merg.m <- melt(merged[,c("bins_factor", "bimodal", "savanna", "forest")], id.vars = c("bins_factor", "bimodal"))

png("outputs/cluster/ratio_sav_for_comp_bimodality_PLS.png")
ggplot(merg.m, aes(bins_factor,value, fill = variable))+geom_bar(stat='identity', position = position_dodge())+scale_fill_manual(values = c("forestgreen", "tan"), 
                                                                                                                                  limits = c("forest", "savanna"))+geom_text(aes(label=bimodal), vjust=0.5)+ylab('ratio of savanna to forest in PLS')+xlab("Environmental PC1 bins")
dev.off()

png("outputs/cluster/counts_sav_for_comp_bimodality_PLS.png")
ggplot(merged, aes(bins_factor,count, fill = bimodal))+geom_bar(stat='identity')+ylab('number of grid cells in PLS')+xlab("Environmental PC1 bins")
dev.off()



# do the same for FIA:
bins <- as.character(unique(full[full$period %in% "Modern",]$PC1_bins_f))
ratiofia <- data.frame(bins = bins, 
                    ratio = NA, 
                    count=NA,
                    savanna= NA, 
                    forest = NA)

coeffs <- matrix(NA, length(bins), 2)

for(i in 1:length(bins)){
  df <- summary(full[full$period %in% "Modern" & full$PC1_bins_f %in% bins[i],]$ecotype)
  ratiofia[i,]$ratio <- df[3]/df[1]
  ratiofia[i,]$count <- sum(df, na.rm=TRUE)
  ratiofia[i,]$savanna <- df[3]
  ratiofia[i,]$forest <- df[1]
  coeffs[i,1]<- bimodality_coefficient(na.omit(full[full$period %in% "Modern" & full$PC1_bins_f %in% bins[i], 'pc2']))
  coeffs[i,2] <- diptest::dip.test(na.omit(density(full[full$period %in% "Modern" & full$PC1_bins_f %in% bins[i], 'pc2'])$y))$p
  #coeffs[i,3]<- bimodality_coefficient(na.omit(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i], 'Density']))
  #coeffs[i,4] <- diptest::dip.test(na.omit(density(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i], 'Density'])$y))$p
  
}

coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
coef.bins<- data.frame(cbind(coeffs, bins))
coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
coef.new <- strsplit(as.character(coef.bins$bins), " - ")
library(plyr)
coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
colnames(coef.new) <- c("low", "high")
coef.bins <- cbind(coef.bins, coef.new)

#merge bins with the "bins" column
merged <- merge(coef.bins, ratiofia, by = "bins")
#criteria for bimodality
bi <- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05, "Bimodal", "Unimodal")
merged$bimodal <- bi

saveRDS(merged, "outputs/cluster/FIA_ratio_sav_forest.rds")

merged$bins_factor = factor(merged$bins, levels=c('-5 - -4','-4 - -3',
                                                  '-3 - -2','-2 - -1',
                                                  '-1 - 0', '0 - 1',
                                                  '1 - 2', '2 - 3', 
                                                  '3 - 4', '4 - 5', 
                                                  '5 - 6'))
merg.m <- melt(merged[,c("bins_factor", "bimodal", "savanna", "forest")], id.vars = c("bins_factor", "bimodal"))

png("outputs/cluster/ratio_sav_for_comp_bimodality_FIA.png")
ggplot(merg.m, aes(bins_factor,value, fill = variable))+geom_bar(stat='identity', position = position_dodge())+scale_fill_manual(values = c("forestgreen", "tan"), 
                                                                                                                                 limits = c("forest", "savanna"))+geom_text(aes(label=bimodal), vjust=0.5)+ylab('ratio of savanna to forest in PLS')+xlab("Environmental PC1 bins")
dev.off()

png("outputs/cluster/count_sav_for_comp_bimodality_FIA.png")
ggplot(merged, aes(bins_factor,count, fill=bimodal))+geom_bar(stat='identity')+ylab('count of points in FIA')+xlab("Environmental PC1 bins")
dev.off()

#------------Is there a bimodality between FIA and PLS?----------------
# overall, the pc2 is not significantly bimoal
png('outputs/cluster/pc2_FIA_PLS_histogram.png')
a <- ggplot(full, aes(pc2, fill = period)) + geom_histogram(alpha = 0.5, position = "identity")+xlab("Environmental PC2")+ggtitle("All, colored by period")
b <- ggplot(full, aes(pc2)) + geom_histogram(alpha = 0.5, position = "identity")+xlab("Environmental PC2")+ggtitle("All (PLS and FIA)")
grid.arrange(a, b, ncol = 1, top = "species pc2 histogram")
dev.off()

ggplot(full, aes(pc2, Density, fill = period))+geom_point()

bimodality_coefficient(na.omit(full[, 'pc2']))
# 0.3428977
diptest::dip.test(na.omit(density(full[, 'pc2'])$y))$p
# 0.009320621

#overall, pc1 is not significantly bimodal:
png("outputs/cluster/pc1_FIA_PLS_histogram.png")
c<- ggplot(full, aes(pc1, fill = period)) + geom_histogram(alpha = 0.5, position = "identity")+xlab("Environmental PC1")+ggtitle("All, colored by period")
d<- ggplot(full, aes(pc1)) + geom_histogram(alpha = 0.5, position = "identity")+xlab("Environmental PC1")+ggtitle("All (PLS and FIA)")
grid.arrange(c, d, ncol = 1, top = "Species pc1 histograms")
dev.off()

bimodality_coefficient(na.omit(full[, 'pc1']))
#0.3765603
diptest::dip.test(na.omit(density(full[, 'pc1'])$y))$p
#0.0

#overall, density is not significantly bimodal
png("outputs/cluster/density_FIA_PLS_histogram.png")
e <- ggplot(full, aes(Density, fill = period)) + geom_histogram(alpha = 0.5, position = "identity")+ggtitle("All, colored by period")
f <- ggplot(full, aes(Density)) + geom_histogram(alpha = 0.5, position = "identity")+ggtitle("All, PLS and FIA combined")
grid.arrange(e,f, ncol = 1, top = "Density histograms")
dev.off()

bimodality_coefficient(na.omit(full[,'Density']))
#0.3798221
diptest::dip.test(na.omit(density(full[,'Density'])$y))$p
#0

# now lets look at bimodality of composition within environmental bins:
bins <- as.character(unique(full$PC1_bins_f))
ratiofia <- data.frame(bins = bins, 
                       ratio = NA, 
                       count=NA)
coeffs <- matrix(NA, length(bins), 2)

for(i in 1:length(bins)){
  df <- summary(full[ full$PC1_bins_f %in% bins[i],]$ecotype)
  ratiofia[i,]$ratio <- df[3]/df[1]
  ratiofia[i,]$count <- sum(df, na.rm=TRUE)
  coeffs[i,1]<- bimodality_coefficient(na.omit(full[ full$PC1_bins_f %in% bins[i], 'pc2']))
  coeffs[i,2] <- diptest::dip.test(na.omit(density(full[ full$PC1_bins_f %in% bins[i], 'pc2'])$y))$p
  #coeffs[i,3]<- bimodality_coefficient(na.omit(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i], 'Density']))
  #coeffs[i,4] <- diptest::dip.test(na.omit(density(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i], 'Density'])$y))$p
  
}

coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
coef.bins<- data.frame(cbind(coeffs, bins))
coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
coef.new <- strsplit(as.character(coef.bins$bins), " - ")
library(plyr)
coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
colnames(coef.new) <- c("low", "high")
coef.bins <- cbind(coef.bins, coef.new)

#merge bins with the "bins" column
merged <- merge(coef.bins, ratiofia, by = "bins")
#criteria for bimodality
bi <- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05, "Bimodal", "Unimodal")
merged$bimodal <- bi

saveRDS(merged, "outputs/cluster/full_ratio_sav_forest.rds")

merged$bins_factor = factor(merged$bins, levels=c('-5 - -4','-4 - -3',
                                                  '-3 - -2','-2 - -1',
                                                  '-1 - 0', '0 - 1',
                                                  '1 - 2', '2 - 3', 
                                                  '3 - 4', '4 - 5', 
                                                  '5 - 6'))


ggplot(merged, aes(bins_factor,ratio, fill=bimodal))+geom_bar(stat='identity')+ylab('ratio of savanna to forest in FIA')+xlab("Environmental PC1 bins")

png("outputs/cluster/density_histogram_by_envt_full.png")
ggplot(full, aes(Density, fill = period))+geom_histogram()+facet_wrap(~PC1_bins_f)
dev.off()

png("outputs/cluster/pc1_histogram_by_envt_full.png")
ggplot(full, aes(pc1, fill = period))+geom_histogram()+facet_wrap(~PC1_bins_f)+xlab("species composition pc1")
dev.off()

png("outputs/cluster/pc2_histogram_by_envt_full.png")
ggplot(full, aes(pc2, fill = period))+geom_histogram()+facet_wrap(~PC1_bins_f)+xlab("species composition pc2")
dev.off()


#------------Define the bimodality in termes of axes of environmetna and composition-------

# maps of environmental PCA axis:
png("outputs/cluster/maps_environmental_space.png")
ggplot(full, aes(x, y, fill = PC1_bins_f))+geom_raster()+coord_equal()+facet_wrap(~period)+theme_bw()
dev.off()

# plots of environmental space over the bimodal regions of PC1
a <-ggplot(dens.pr, aes(PC1, MAP1910))+geom_point()+ylab("Mean Annual Precip. (mm/yr)")
b <- ggplot(dens.pr, aes(PC1, pasttmean))+geom_point()+ylab("Mean annual Temp. DegC")
c <- ggplot(dens.pr, aes(PC1, pastdeltaP))+geom_point()+ylab("Precipitation seasonality")
d <- ggplot(dens.pr, aes(PC1, deltaT))+geom_point()+ylab("Temperature Seasonality")
e <- ggplot(dens.pr, aes(PC1, sandpct))+geom_point()+ylab('% sand')
f <- ggplot(dens.pr, aes(PC1, ksat))+geom_point()+ylab("ksat")

png(height = 9, width = 6, units = 'in', res=200, "outputs/cluster/environmental_pca_summary_fig.png")
grid.arrange(a,b,c,d,e,f, ncol=2, top = "Enviromental variable relationships to PC1")
dev.off()

# plot composition vs environment:

ggplot(full[full$PC1bins %in% c("0 - 1", "1 - 2", "-1 - 0"),], aes(PC1, Oak))+geom_point()+facet_wrap(period~PC1bins)

ggplot(full[], aes(PC1, Maple))+geom_point()+facet_wrap(~period)
ggplot(full[], aes(PC1, Beech))+geom_point()+facet_wrap(~period)
ggplot(full[], aes(PC1, Hemlock))+geom_point()+facet_wrap(~period)


fullspec.m <- melt(full[,c('x', "y", "cell", "PC1", "PC2", "PC1_bins_f","period",
                          'Oak', "Hemlock", "Maple", "Beech", "Pine", "Poplar", "Fir", "Spruce", "Tamarack", "Cherry", "Maple")], id.vars = c('x', "y", "cell", "PC1", "PC2", "PC1_bins_f", "period"))

png(height = 10, width = 7, units = 'in', res= 300, "outputs/cluster/species_composition_changes_PLS.png")
ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("0 - 1","1 - 2","-1 - 0") & fullspec.m$variable %in% c('Oak', "Hemlock", "Maple", "Beech", "Pine", "Poplar"),], aes(PC1, value, color = variable))+geom_point()+stat_smooth(method = "gam", color = 'black')+xlab("PC1 environment")+
  facet_wrap(variable~PC1_bins_f, scales = "free_x", ncol = 3)+ggtitle("PLS species composition -1 to 2 PC1")
dev.off()



# I have to manually add histgrams marginally because this doesnt work in facet_wrap..annoying
oak <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("0 - 1") & fullspec.m$variable %in% "Oak" ,], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS Oak composition 0 to 1 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
oak<- ggMarginal(oak, type = "histogram")

hem <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("0 - 1") & fullspec.m$variable %in% "Hemlock",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS hemlock composition 0 to 1 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
hem<- ggMarginal(hem, type = "histogram")

maple <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("0 - 1") & fullspec.m$variable %in% "Maple",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS maple composition 0 to 1 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
maple<- ggMarginal(maple, type = "histogram")

beech <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("0 - 1") & fullspec.m$variable %in% "Beech",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS beech composition 0 to 1 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
beech<- ggMarginal(beech, type = "histogram")

pine <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("0 - 1") & fullspec.m$variable %in% "Pine",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS pine composition 0 to 1 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
pine<- ggMarginal(pine, type = "histogram")

poplar <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("0 - 1") & fullspec.m$variable %in% "Poplar",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS poplar composition 0 to 1 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
poplar <- ggMarginal(poplar, type = "histogram")

# plot the other places:
oakn1 <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("-1 - 0") & fullspec.m$variable %in% "Oak",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS Oak composition -1 to 0 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
oakn1<- ggMarginal(oakn1, type = "histogram")

hemn1 <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("-1 - 0") & fullspec.m$variable %in% "Hemlock",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS hemlock composition -1 to 0 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
hemn1<- ggMarginal(hemn1, type = "histogram")

maplen1 <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("-1 - 0") & fullspec.m$variable %in% "Maple",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS maple composition -1 to 0 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
maplen1<- ggMarginal(maplen1, type = "histogram")

beechn1 <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("-1 - 0") & fullspec.m$variable %in% "Beech",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS beech composition -1 to 0 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
beechn1<- ggMarginal(beechn1, type = "histogram")

pinen1 <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("-1 - 0") & fullspec.m$variable %in% "Pine",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS pine composition -1 to 0 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
pinen1<- ggMarginal(pinen1, type = "histogram")

poplarn1 <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("-1 - 0") & fullspec.m$variable %in% "Poplar",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS poplar composition -1 to 0 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
poplarn1 <- ggMarginal(poplarn1, type = "histogram")

oak2 <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("1 - 2") & fullspec.m$variable %in% "Oak",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS Oak composition 1 - 2 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
oak2<- ggMarginal(oak2, type = "histogram")

hem2 <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("1 - 2") & fullspec.m$variable %in% "Hemlock",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS hemlock composition 1 - 2 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
hem2<- ggMarginal(hem2, type = "histogram")

maple2 <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("1 - 2") & fullspec.m$variable %in% "Maple",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS maple composition 1 - 2 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
maple2<- ggMarginal(maple2, type = "histogram")

beech2 <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("1 - 2") & fullspec.m$variable %in% "Beech",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS beech composition 1 - 2 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
beech2<- ggMarginal(beech2, type = "histogram")

pine2 <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("1 - 2") & fullspec.m$variable %in% "Pine",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS pine composition 1 - 2 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
pine2<- ggMarginal(pine2, type = "histogram")

poplar2 <- ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("1 - 2") & fullspec.m$variable %in% "Poplar",], aes(PC1, value))+geom_point()+xlab("PC1 environment")+ggtitle("PLS poplar composition 1 - 2 PC1")+
  theme(plot.title = element_text(hjust = 0.5))
poplar2 <- ggMarginal(poplar2, type = "histogram")


png(height = 12, width = 12, units = 'in', res = 200, "outputs/cluster/comp_v_envt_with_marginal_hists.png")
grid.arrange(oakn1,oak,oak2, hemn1,hem,hem2, 
             maplen1,maple,maple2, beechn1,beech,beech2, 
             pinen1,pine, pine2, poplarn1,poplar,poplar2, ncol = 3, nrow = 6)
dev.off()


# plot the rest of the species clusteres
png(height = 10, width = 7, units = 'in', res= 300, "outputs/cluster/species_composition_changes_FIA.png")
ggplot(fullspec.m[fullspec.m$period %in% "Modern" & fullspec.m$PC1_bins_f %in% c("0 - 1","1 - 2","-1 - 0") & fullspec.m$variable %in% c('Oak', "Hemlock", "Maple", "Beech", "Pine", "Poplar") ,], aes(PC1, value, color = variable))+geom_point()+stat_smooth(method = 'gam', color = 'black')+xlab("PC1 environment")+facet_wrap(variable~PC1_bins_f, scales = "free_x", ncol = 3)+ggtitle("FIA species composition -1 to 2 PC1")
dev.off()

png(height = 10, width = 7, units = 'in', res= 300, "outputs/cluster/species_composition_changes_both_0_1.png")
ggplot(fullspec.m[fullspec.m$PC1_bins_f %in% c("0 - 1") & fullspec.m$variable %in% c('Oak', "Hemlock", "Maple", "Beech", "Pine", "Poplar") ,], aes(PC1, value, color = variable))+geom_point()+stat_smooth(method = 'gam', color = 'black')+xlab("PC1 environment")+facet_wrap(variable~period, scales = "free", ncol = 2)+ggtitle("species composition 0 to 1 PC1")
dev.off()


png(height = 12, width = 6, units = 'in', res= 300, "outputs/cluster/species_composition_changes_PLS_northern_mn.png")
ggplot(fullspec.m[fullspec.m$period %in% "Past" & fullspec.m$PC1_bins_f %in% c("-4 - -3","-5 - -4") &  
                    fullspec.m$variable %in% c('Oak', "Fir", "Maple", "Spruce", "Pine", "Poplar", "Tamarack") ,], aes(PC1, value, color = variable))+geom_point()+xlab("PC1 environment")+facet_wrap(variable~PC1_bins_f, scales = "free_x", ncol = 2)+ggtitle("PLS species composition -5 to -3 PC1")
dev.off()

png(height = 12, width = 6, units = 'in', res= 300, "outputs/cluster/species_composition_changes_FIA_northern_mn.png")
ggplot(fullspec.m[fullspec.m$period %in% "Modern" & fullspec.m$PC1_bins_f %in% c("-4 - -3","-5 - -4") &
                    fullspec.m$variable %in% c('Oak', "Fir", "Maple", "Spruce", "Pine", "Poplar", "Tamarack") ,], aes(PC1, value, color = variable))+geom_point()+xlab("PC1 environment")+facet_wrap(variable~PC1_bins_f, scales = "free_x", ncol = 2)+ggtitle("FIA species composition -5 to -3 PC1")
dev.off()


# plot for all of the facets of environment:
ggplot(fullspec.m[fullspec.m$period %in% "Past"  ,], aes(PC1, value, color = variable))+geom_point()+stat_smooth(method = "loess")+facet_wrap(~PC1bins, scales = "free")

ggplot(fullspec.m[fullspec.m$period %in% "Past"  ,], aes(PC1, value, color = variable))+geom_point()+stat_smooth(method = "loess")



a1.2 <- ggplot(full[full$PC1bins %in% "1 - 2",], aes(pc2, Density, color = Oak))+geom_point()+facet_wrap(~period)+ylab("Tree Density (stems/hectare)")+xlab("Species Composition PC2")+ggtitle('environmental PC 1-2')
b0.1 <- ggplot(full[full$PC1bins %in% "0 - 1",], aes(pc2, Density, color = Oak))+geom_point()+facet_wrap(~period)+ylab("Tree Density (stems/hectare)")+xlab("Species Composition PC2")+ggtitle('environmental PC 0-1')
c.1.0 <- ggplot(full[full$PC1bins %in% "-1 - 0",], aes(pc2, Density, color = Oak))+geom_point()+facet_wrap(~period)+ylab("Tree Density (stems/hectare)")+xlab("Species Composition PC2")+ggtitle('environmental PC -1 - 0')

png(height = 9, width = 6, units = 'in', res=300,"outputs/cluster/species_comp_vs_density_all_bimodal_by_pc1bins.png")
grid_arrange_shared_legend(a1.2, b0.1, c.1.0, ncol=1, nrow=3)
dev.off()

write.csv(fc.m, "outputs/cluster/fullcomps_dataset.csv")
write.csv(full, "outputs/cluster/full_comp_dens_df.csv")
