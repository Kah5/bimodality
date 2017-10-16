version <- "1.6-5"

library(data.table)
library(reshape2)
library(dtplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(sp)
library(raster)
library(rgdal)

#--------------------------------load data-----------------------------------
# read in pont level density data
pls.inil <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_v',version, '.csv'))


# read in point level density data
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


full.spec[is.na(full.spec)]<- 0
density.full <- full.spec
full.spec[is.na(full.spec)]<- 0




#----------------------cluster analysis---------------------------------
# we want to cluster the data based on % species composition: based on tree density, not the counts
# using clusters similar to simons mediod clustering scheme: 
library(cluster)
#library(fpc)

comps <- density.full[!names(density.full) %in% c("Water", "Wet", "No Tree", "No.tree")]
#comps <- comps[!is.na(comps),]
comps[,4:38] <- comps[,4:38]/comps[,39] # calculate the proportion of the total density that each species takes up
comps <- comps[,1:38]

comp2 <- comps
# remove prairie cells:
comps <- data.frame( comps[ complete.cases(comps),] )
# write as a csv so we don't have to keep doing this:
write.csv(comps, "data/outputs/plss_pct_density_composition_v1.6.csv")
set.seed(11)
# use Pam for the k-mediods clustering algorithm. These take ~30 seconds to a minute each
#classes.3 <- pam(comps[,4:ncol(comps)], k = 3, diss = FALSE, keep.diss = TRUE)
#classes.4 <- pam(comps[,4:ncol(comps)], k = 4, diss = FALSE)
classes.5 <- pam(comps[,4:ncol(comps)], k = 5, diss = FALSE,  keep.diss = TRUE)
classes.6 <- pam(comps[,4:ncol(comps)], k = 6, diss = FALSE, keep.diss = TRUE)
#classes.7 <- pam(comps[,4:ncol(comps)], k = 7, diss = FALSE, keep.diss = TRUE)
#classes.8 <- pam(comps[,4:ncol(comps)], k = 8, diss = FALSE)

diss.6 <- as.matrix(classes.6$diss)
diss.6

brays <- vegdist(comps[,4:ncol(comps)], method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
        na.rm = FALSE) 
brays2<- as.matrix(brays)

comp2 <- comp2[complete.cases(comp2),]
brays <- vegdist(comp2[,4:ncol(comp2)], method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
                 na.rm = FALSE) 
brays2 <- as.matrix(brays)

# Use Avg. Silhouette width to evaluate the clusters:  
# SIlhouette width close to 1 indicates the cluster clusters very well with itself. Silhoutte widith that is negative or low indicates low clustering with itself
summary(classes.7) # Avg. Silhouette width = 0.2506271
summary(classes.6) # Avg. Silhouette width = 0.2677659
summary(classes.5) # Avg. Silhouette width = 0.2610493
summary(classes.4) # Avg. Silhouette width = 0.2006347
summary(classes.3) # Avg. Silhouette width = 0.2393605
# these sihouette widths are low, but this is likely due to the large amount of data and noise in composition
#plot(classes.5)
#plot(classes.6)

# both 5 and 6 classes have the highest average silhouetted widths:

# lets assign groups based on 5 clusters (similar to simon's groups)
mediods <- comps$cell [classes.5$id.med]
index <- rownames(comps[comps$cell %in% mediods,])
#diss.5 <- as.matrix(classes.5$diss)
diss.5.dissimilarity <- diss.6[,index]
df5 <- comps[comps$cell %in% mediods,] # look at the rows that have the mediods

old_classes <- classes.5
#[1] 49221 29369 17193 16954 11274# mediods
# had to look at the compositoin  of the mediod values to determine the speces
rem_class5 <- factor(old_classes$clustering,
                     labels=c('Maple/Elm/Hickory/Oak/Basswood', # 1,
                              'Oak', # 2
                              'Poplar',#3
                              "Pine/Tamarack/Poplar/Spruce/Birch", # 4,
                              'PineTamarack' #5
                              
                              
                     ))

clust_plot5 <- data.frame(comps, 
                          cluster = rem_class5,
                          clustNum = as.numeric(rem_class5),
                          diss1 = diss.5.dissimilarity[,1],
                          diss2 = diss.5.dissimilarity[,2],
                          diss3 = diss.5.dissimilarity[,3],
                          diss4 = diss.5.dissimilarity[,4],
                          diss5 = diss.5.dissimilarity[,5]
                         )

ggplot(clust_plot5, aes(x = x, y=y, fill=diss1))+geom_raster()
ggplot(clust_plot5, aes(x = x, y=y, fill=diss2))+geom_raster()
ggplot(clust_plot5, aes(x = x, y=y, fill=diss3))+geom_raster()
ggplot(clust_plot5, aes(x = x, y=y, fill=diss4))+geom_raster()
ggplot(clust_plot5, aes(x = x, y=y, fill=diss5))+geom_raster()


# Now lets look at the clustering of 6 classes
mediods <- comps$cell [classes.6$id.med]
index <- rownames(comps[comps$cell %in% mediods,])
brays <- vegdist(comps[,4:ncol(comps)], method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
                 na.rm = FALSE) 
brays2 <- as.matrix(brays)

diss.6.dissimilarity <- brays2[,index]

#mediods
#[1] 

df6 <- comps[comps$cell %in% mediods,] # look at the rows that have the mediods
write.csv(df6, "outputs/species_comp_clusters_6_class_mediods.csv")

old_classes <- classes.6
rem_class <- factor(old_classes$clustering,
                    labels=c('Elm/Maple/Hickory/Oak/Beech', #mediod 1
                             "Oak", # medoid 2
                             'Hemlock/Beech/Cedar/Birch/Maple', # mediod 3
                             "Poplar/Oak", # mediod 4
                             'Tamarack/Spruce/Birch/Pine/Spruce/Poplar', # mediod5
                             'Pine/Tamarack/Poplar'# mediod 6
                      ))

classes.6$silinfo$clus.avg.widths

# note the the clus.avg.widths is not in the same order as df6,had to manually assign
ranked_class <- factor(old_classes$clustering,
                    labels=c("Low.0.10", #mediod 1
                             "High.0.71", # mediod 2
                             "Mod.0.19", # mediod 3
                             "High.0.40", #mediod 4
                             "Mod.0.16", # mediod 5
                             "High.0.42" #mediod 6
               
                    ))



clust_plot6 <- data.frame(comps, 
                          speciescluster = rem_class,
                          rank = ranked_class,
                          clustNum = as.numeric(rem_class),
                          diss1 = diss.6.dissimilarity[,1],
                          diss2 = diss.6.dissimilarity[,2],
                          diss3 = diss.6.dissimilarity[,3],
                          diss4 = diss.6.dissimilarity[,4],
                          diss5 = diss.6.dissimilarity[,5],
                          diss6 = diss.6.dissimilarity[,6])


# merge the clusters with denisty estimates:
dens <- merge(dens.pr, clust_plot6[,c('x', "y", "cell", "speciescluster", "clustNum",'rank', 'diss1','diss2','diss3','diss4','diss5','diss6')], by = c("x","y","cell"),keep = all)

write.csv(dens, "outputs/cluster/density_pls_with_clusters.csv")

# map out the clusters in space:
png(width = 6, height = 6, units= 'in',res=300,"outputs/paper_figs/six_cluster_map_pls.png")
pls.clust <- ggplot(clust_plot6, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c('#beaed4','#386cb0','#ffff99','#f0027f', '#7fc97f','#fdc086'))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                            axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(),legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.position = c(0.205, 0.32),legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+ annotate("text", x=-90000, y=1486000,label= "B", size = 5)+ggtitle("")
pls.clust 
dev.off()

ggplot(clust_plot6, aes(x = x, y=y, fill=diss3))+geom_raster()+
   geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),
                                                                                                              axis.title.y=element_blank(),legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.position = c(0.205, 0.32),legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+ annotate("text", x=-90000, y=1486000,label= "B", size = 5)+ggtitle("")
colnames(clust_plot6)[42:47] <- c('Elm.Maple.Hickory.Oak.Beech.diss', #mediod 1
                                  'Tamarack.Spruce.Birch.Pine.Spruce.Poplar.diss', # mediod5
                                  'Pine.Tamarack.Poplar.diss', # mediod 6
                                  
                                  "Poplar.Oak.diss", # mediod 4
                                  
                                  'Hemlock.Beech.Cedar.Birch.Maple.diss', # mediod 3
                                  "Oak.diss" )# medoid 2) #6)                                                                                             


ggplot(clust_plot6, aes(x = x, y=y, fill=rank))+geom_raster()+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 6, name = "Greys"))+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                              axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),
                                                                                                           axis.title.y=element_blank(),legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.position = c(0.205, 0.32),legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+ annotate("text", x=-90000, y=1486000,label= "B", size = 5)+ggtitle("")
#map out the dissimilarities in space
clust6.m <- melt(clust_plot6[,c("x", "y", "cell", "Elm.Maple.Hickory.Oak.Beech.diss",
                                "Oak.diss",
                                "Hemlock.Beech.Cedar.Birch.Maple.diss",
                                "Poplar.Oak.diss",
                                "Tamarack.Spruce.Birch.Pine.Spruce.Poplar.diss",
                                "Pine.Tamarack.Poplar.diss")], id.vars = c('x',"y","cell"))



# create relabeller
composition_names <- list(
  'Elm.Maple.Hickory.Oak.Beech.diss'="Elm/Maple/Hickory/ \n Oak/Beech",
  'Oak.diss'="Oak",
  'Hemlock.Beech.Cedar.Birch.Maple.diss'="Hemlock/Beech/Cedar/ \n Birch/Maple",
  'Poplar.Oak.diss'="Poplar/Oak",
  'Tamarack.Spruce.Birch.Pine.Spruce.Poplar.diss' = 'Tamarack/Spruce/Birch/ \n Pine/Spruce/Poplar',
  'Pine.Tamarack.Poplar.diss' = 'Pine/Tamarack/Poplar'
)

composition_labeller <- function(variable,value){
  return(composition_names[value])
}

png(width = 10, height = 6, units="in",res=300,"outputs/Composition/six_cluster_dissimilarity_maps.png")
dis.maps <- ggplot(clust6.m, aes(x,y, fill = value))+geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 6, name = "YlGnBu"))+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                                                                                                                                                                  axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),
                                                                                                                                                                                                                                                  axis.title.y=element_blank(),legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.background = element_rect(fill=alpha('transparent', 0)))+
                                                                                                                                                                                                                                                  xlab("easting") + ylab("northing") +coord_equal()+ggtitle("")+facet_wrap(~variable, ncol = 3, labeller = composition_labeller)
dis.maps
dev.off()

# lets look at the histograms of these overall
png(width = 10, height = 6, units = "in", res=300, 'outputs/Composition/six_cluster_dissimilarity_hists.png')
dis.hist <- ggplot(clust6.m, aes(value))+geom_histogram(bw = 35)+theme_bw()+facet_wrap(~variable, ncol = 3, labeller = composition_labeller)
dis.hist
dev.off()

# write as csv for future 
write.csv(clust_plot6, "outputs/six_clust_pls_dissimilarity.csv", row.names = FALSE)


#do the same clustering for FIA data and plot:
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
fcomps <- fcomps[ ,order(names(fcomps))]

# add and fia vs. pls flag:
comps$period <- "PLS"
fcomps$period<- "FIA"


#move around the columns
require(dplyr)
fcomps<- fcomps %>%
  dplyr::select(period, everything())

fcomps<- fcomps %>%
  dplyr::select(cell, everything())

fcomps<- fcomps %>%
  dplyr::select(y, everything())

fcomps<- fcomps %>%
  dplyr::select(x, everything())


#fcomps classifcation only

classes.3 <- pam(fcomps[,5:ncol(fcomps)], k = 3, diss = FALSE,  keep.diss = TRUE)
classes.4 <- pam(fcomps[,5:ncol(fcomps)], k = 4, diss = FALSE,  keep.diss = TRUE)
classes.5 <- pam(fcomps[,5:ncol(fcomps)], k = 5, diss = FALSE,  keep.diss = TRUE)
classes.6 <- pam(fcomps[,5:ncol(fcomps)], k = 6, diss = FALSE,  keep.diss = TRUE)
classes.7 <- pam(fcomps[,5:ncol(fcomps)], k = 7, diss = FALSE,  keep.diss = TRUE)
classes.8 <- pam(fcomps[,5:ncol(fcomps)], k = 8, diss = FALSE,  keep.diss = TRUE)

plot(classes.8)
plot(classes.7)
plot(classes.6)
plot(classes.5)
plot(classes.4)
plot(classes.3)

#summary(classes.8) # Avg. Silhouette width = 
summary(classes.7) # Avg. Silhouette width = 0.2553684
summary(classes.6) # Avg. Silhouette width = 0.2348445
summary(classes.5) # Avg. Silhouette width = 0.2350457
summary(classes.4) # Avg. Silhouette width = 
summary(classes.3) # Avg. Silhouette width = 
fcomps$idvar <- 1:nrow(fcomps)
mediods <- classes.5$medoids
#mediods <- fcomps[fcomps$idvar %in%  classes.5$medoids,]
#mediods <- fcomps$idvar [classes.5$id.med]
mediods <- fcomps$cell [classes.6$id.med]
index <- rownames(fcomps[fcomps$cell %in% mediods,])

#index <- fcomps[fcomps$cell %in% mediods,]$idvar
brays.f <- vegdist(fcomps[,5:39], method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
                 na.rm = FALSE) 
brays.f2 <- as.matrix(brays.f)

diss.f5.dissimilarity <- brays.f2[,index]

df5 <- fcomps[fcomps$cell %in% mediods,] # look at the rows that have the mediods

old_classes <- classes.5
#[1] 1292 2201 4618 4978 4604# idvars of the mediods
rem_class5 <- factor(old_classes$clustering,
                    labels=c('Oak/OtherHardwood/Elm', # 1,
                            'Maple/Ash/Birch', # 2
                            'Poplar/Spruce/Maple',#3
                           "Pine/Poplar", # 4,
                          'Cedar.juniper/Tamarack' 


  ))

rem_class5 <- factor(old_classes$clustering,
                     labels=c('Ash/Beech/Elm/Hickory/Oak', # 1,
                              'Poplar/Spruce/Tamarack', # 2
                              'Hemlock/Maple/Birch/Cedar.juniper',#3
                              "Tamarack/Cedar.juniper/Spruce", # 4,
                              'Cedar.juniper/Hemlock/Spruce' 
                              
                              
                     ))

clust_plot5 <- data.frame(fcomps, 
                          cluster = rem_class5,
                          clustNum = as.numeric(rem_class5),
                          diss1 = diss.f5.dissimilarity[,1],
                          diss2 = diss.f5.dissimilarity[,2],
                          diss3 = diss.f5.dissimilarity[,3],
                          diss4 = diss.f5.dissimilarity[,4],
                          diss5 = diss.f5.dissimilarity[,5])

ggplot(clust_plot5, aes(x = x, y=y, fill=cluster))+geom_raster()

classes.6$silinfo$clus.avg.widths
# they are all equally dissimilar
#0.40663776 -0.04516793  0.51842603  0.47118307  0.46843040  0.33582488

# 6 classes
mediods <- fcomps$cell [classes.6$id.med]
#mediods
#[1] 45094 27585 14273  9203 22622 15808
index <- rownames(fcomps[fcomps$cell %in% mediods,])

#index <- fcomps[fcomps$cell %in% mediods,]$idvar
brays.f <- vegdist(fcomps[,5:39], method="bray", binary=FALSE, diag=FALSE, upper=FALSE,
                   na.rm = FALSE) 
brays.f2 <- as.matrix(brays.f)

diss.f6.dissimilarity <- brays.f2[,index]
df6 <- fcomps[fcomps$cell %in% mediods,] # look at the rows that have the mediods
write.csv(df6, "outputs/fia_species_comp_clusters_6_class_mediods.csv")

old_classes <- classes.6
rem_class <- factor(old_classes$clustering,
                    labels=c('Oak/Maple', # 1,
                             'Oak/Hickory/Otherhardwood/Maple/Birch/Ash', # 2
                             'Maple',#3
                             #'Oak/Poplar/Basswood/Maple',
                             "Poplar/Spruce/Maple/Fir", # 4,
                             'Pine/Poplar', #5,
                             'Cedar.juniper/Poplar/Maple' #6
                             
                             
                    ))

clust_plot6f <- data.frame(fcomps, 
                          speciescluster = rem_class,
                          clustNum = as.numeric(rem_class),
                          diss1 = diss.f6.dissimilarity[,1],
                          diss2 = diss.f6.dissimilarity[,2],
                          diss3 = diss.f6.dissimilarity[,3],
                          diss4 = diss.f6.dissimilarity[,4],
                          diss5 = diss.f6.dissimilarity[,5],
                          diss6 = diss.f6.dissimilarity[,6])

#ggplot(clust_plot6f, aes(x = x, y=y, fill=diss1))+geom_raster()

png(width = 6, height = 6, units= 'in',res=300,"outputs/paper_figs/six_cluster_map_fia.png")
fia.clust<- ggplot(clust_plot6f, aes(x = x, y=y, fill=speciescluster))+geom_raster()+
  scale_fill_manual(values = c('#386cb0','#beaed4','#e41a1c','#ffff33', '#7fc97f','#fdc086'))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                             axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                              axis.title.x=element_blank(),
                                                                                                             axis.title.y=element_blank(), legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.position = c(0.205, 0.32),legend.background = element_rect(fill=alpha('transparent', 0)))+xlab("easting") + ylab("northing") +coord_equal()+ annotate("text", x=-90000, y=1486000,label= "D", size = 5)+ggtitle("")
fia.clust 

dev.off()

# reassign names
colnames(clust_plot6f)[44:49] <- c('Oak/Maple', # 1,
                                   'Oak/Hickory/Otherhardwood/Maple/Birch/Ash', # 2
                                   'MapleComp',#3
                                   "Poplar/Spruce/Maple/Fir", # 4,
                                   'Pine/Poplar', #5,
                                   'Cedar.juniper/Poplar/Maple')
# save the fia clusters to a csv:
#map out the dissimilarities in space
clust6f.m <- melt(clust_plot6f[,c("x", "y", "cell", 'speciescluster',"clustNum",'Oak/Maple', # 1,
                                  'Oak/Hickory/Otherhardwood/Maple/Birch/Ash', # 2
                                  'MapleComp',#3
                                  "Poplar/Spruce/Maple/Fir", # 4,
                                  'Pine/Poplar', #5,
                                  'Cedar.juniper/Poplar/Maple')], id.vars = c('x',"y","cell", "speciescluster","clustNum"))



# create relabeller
composition_names <- list(
  'Oak/Maple'="Oak/Maple",
  'Oak/Hickory/Otherhardwood/Maple/Birch/Ash'="Oak/Hickory/ \n Other hardwood/ \n Maple/Birch/Ash",
  'MapleComp'="Maple",
  'Poplar/Spruce/Maple/Fir'="Poplar/Spruce/ \n Maple/Fir",
  'Pine/Poplar' = 'Pine/Poplar',
  'Cedar.juniper/Poplar/Maple' = 'Cedar juniper/Poplar/Maple'
)

composition_labeller <- function(variable,value){
  return(composition_names[value])
}

png(width = 10, height = 6, units="in",res=300,"outputs/Composition/six_cluster_dissimilarity_fia_maps.png")
dis.fia.maps <- ggplot(clust6f.m, aes(x,y, fill = value))+geom_raster()+geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+theme_bw()+scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 6, name = "YlGnBu"))+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                                                                                                                                                                                axis.text.y=element_blank(),axis.ticks=element_blank(),axis.title.x=element_blank(),
                                                                                                                                                                                                                                                                axis.title.y=element_blank(),legend.key.size = unit(0.6,'lines'),legend.title=element_text(size=10),legend.background = element_rect(fill=alpha('transparent', 0)))+
  xlab("easting") + ylab("northing") +coord_equal()+ggtitle("")+facet_wrap(~variable, ncol = 3, labeller = composition_labeller)
dis.fia.maps
dev.off()

# lets look at the histograms of these overall
png(width = 10, height = 6, units = "in", res=300, 'outputs/Composition/six_cluster_dissimilarity_hists.png')
dis.fia.hist <- ggplot(clust6f.m, aes(value))+geom_histogram()+theme_bw()+facet_wrap(~variable, ncol = 3, labeller = composition_labeller)
dis.fia.hist
dev.off()

# write as csv for future 
write.csv(clust_plot6, "outputs/six_clust_pls_dissimilarity.csv", row.names = FALSE)

# plot pls and fia cluster figures together:
png(width = 10, height=4, units="in", res=300, "outputs/paper_figs/Fig_S1CD.png")
grid.arrange(pls.clust, fia.clust, ncol = 2)
dev.off()
