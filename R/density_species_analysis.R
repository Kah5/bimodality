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
ggplot(comp.melt, aes(x = value, fill = variable))+geom_density(position = 'fill')
ggplot(comp.melt[comp.melt$value > 0,], aes(x = value, fill = variable))+geom_histogram()

DF<- comps[,4:38]
colnames(DF)[apply(DF,1,which.max)]
comps$highest <- colnames(comps[,4:38])[max.col(comps[,4:38],ties.method="first")]
taxa <- unique(comps$highest)

highest <- comps[names(comps) %in% taxa] 
highest$x <- comps$x
highest$y <- comps$y
highest$highest <- comps$highest

ggplot(highest, aes(x=x, y=y, fill = highest))+geom_raster()
