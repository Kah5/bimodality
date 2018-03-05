library(ggplot2)
library(reshape2)
# read in FIA data to look at from WI

wiplot <- read.csv("data/FIA_plot_data/WI_PLOT.csv")
wicond <- read.csv("data/FIA_plot_data/WI_COND.csv")
witree <- read.csv("data/FIA_plot_data/WI_TREE.csv")

miplot <- read.csv("data/FIA_plot_data/MI_PLOT.csv")
micond <- read.csv("data/FIA_plot_data/MI_COND.csv")
mitree <- read.csv("data/FIA_plot_data/MI_TREE.csv")

mnplot <- read.csv("data/FIA_plot_data/MN_PLOT.csv")
mncond <- read.csv("data/FIA_plot_data/MN_COND.csv")
mntree <- read.csv("data/FIA_plot_data/MN_TREE.csv")

inplot <- read.csv("data/FIA_plot_data/IN_PLOT.csv")
incond <- read.csv("data/FIA_plot_data/IN_COND.csv")
intree <- read.csv("data/FIA_plot_data/IN_TREE.csv")

ilplot <- read.csv("data/FIA_plot_data/IL_PLOT.csv")
ilcond <- read.csv("data/FIA_plot_data/IL_COND.csv")
iltree <- read.csv("data/FIA_plot_data/IL_TREE.csv")

wiplot.old <- wiplot[wiplot$INVYR < 1999,]
wicond.old <- wicond[wicond$INVYR < 1999,] 
witree.old <- witree[witree$INVYR < 1999,] 

miplot.old <- miplot[miplot$INVYR < 1999,]
micond.old <- micond[micond$INVYR < 1999,] 
mitree.old <- mitree[mitree$INVYR < 1999,] 

mnplot.old <- mnplot[mnplot$INVYR < 1999,]
mncond.old <- mncond[mncond$INVYR < 1999,] 
mntree.old <- mntree[mntree$INVYR < 1999,] 

inplot.old <- inplot[inplot$INVYR < 1999,]
incond.old <- incond[incond$INVYR < 1999,] 
intree.old <- intree[intree$INVYR < 1999,] 

ilplot.old <- ilplot[ilplot$INVYR < 1999,]
ilcond.old <- ilcond[ilcond$INVYR < 1999,] 
iltree.old <- iltree[iltree$INVYR < 1999,] 

# merge plot and cond tables by CN

plot.old <- rbind(inplot.old, ilplot.old, miplot.old, mnplot.old, wiplot.old)
cond.old <- rbind(incond.old, ilcond.old, micond.old, mncond.old, wicond.old)
tree.old <- rbind(intree.old, iltree.old, mitree.old, mntree.old, witree.old)


    
hist(cond.old$STDAGE)

# get only live trees
tree.old <- tree.old[tree.old$STATUSCD == 1,]
tree.old$DIA <- tree.old$DIA*2.54
tree.old <- tree.old[tree.old$DIA >= 20.32,]
test <- merge(plot.old, tree.old[,c( "STATECD","PLT_CN","PLOT","COUNTYCD", "INVYR", "TREE", "DIA", "TPA_UNADJ", "SPCD","SUBP")], by = c(  "INVYR", "PLOT", "COUNTYCD", "STATECD"))


merged.tree <- test


ggplot(merged.tree, aes(PLOT, fill = INVYR))+geom_histogram()+facet_wrap(~INVYR)

#cn <- unique(miplot.old$CN)
#test <- mitree.old[mitree.old$PLT_CN %in% cn,]


tree.count <- merged.tree %>% dplyr::count(PLT_CN, SPCD, INVYR, STATECD, PLOT,LAT,LON)

# select trees > 8 inches (>=20.32)
ac2ha   <- 0.404686 

merged.tree <- tree.count#<- merge(merged.tree, tree.count, by = c("PLT_CN", "SPCD", "INVYR", "PLOT","STATECD"))

# need to matach tree count up with overall merged data
#merged.tree$DENS <- merged.tree$n * merged.tree$TPA_UNADJ * (1/ac2ha)
merged.tree$DENS <- merged.tree$n * 6.018046 * (1/ac2ha)

hist(merged.tree$DENS)
ggplot(merged.tree, aes(DENS))+geom_histogram()+facet_wrap(~INVYR)+xlim(0,500)

# convert to paleon coordinates (roughly b/c these are fuzzed + swapped)

head(merged.tree)
merged.tree <- merged.tree[complete.cases(merged.tree[,c("LON", "LAT")]),]

coordinates(merged.tree) <- ~ LON + LAT
proj4string(merged.tree)=CRS("+proj=longlat +datum=WGS84") #define: WGS-84 lon,lat projection
tree.albers <- spTransform(merged.tree,CRS("+init=epsg:3175")) #convert to: NAD83/Great Lakes and St Lawrence Albers projection


numbered.rast <- setValues(base.rast, 1:ncell(base.rast))
numbered.cell <- raster::extract(numbered.rast, tree.albers)

xys <- raster::xyFromCell(numbered.rast, cell = numbered.cell)

tree.albers <- data.frame(tree.albers)
tree.albers$x <- xys[,1]
tree.albers$y <- xys[,2]
tree.albers$cell <- numbered.cell


head(tree.albers)
speciesconversion <- read.csv('data/fia_conversion_v02-sgd.csv')

FIA.pal <- merge(tree.albers, speciesconversion, by.x = 'SPCD', by.y = "spcd" )
FIA.by.paleon <- dcast(FIA.pal, LON + LAT + PLT_CN+ x + y + cell + INVYR ~ PalEON, sum, na.rm=TRUE, value.var = 'DENS') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,8:length(FIA.by.paleon)], na.rm = TRUE) # sum the total density in each plot
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y',"LON", "LAT", 'cell', "PLT_CN",  "INVYR")) # melt the dataframe
fia.by.cell <- dcast(fia.melt, x + y+ cell + INVYR ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell
#fia.by.cell$total <- rowSums(fia.by.cell[,4:28], na.rm = TRUE)

ggplot(fia.by.cell, aes(x, y, fill = FIAdensity)) + geom_raster()
ggplot(fia.by.cell, aes(x, y, fill = FIAdensity)) + geom_raster() + facet_wrap(~INVYR)
ggplot(fia.by.cell, aes(FIAdensity)) + geom_histogram() + xlim(0,500)+facet_wrap(~INVYR)
#ggplot(FIA.by.paleon, aes(Hemlock)) + geom_histogram() + facet_wrap(~INVYR)

fia.by.cell$INVYRcd<- ifelse(fia.by.cell$INVYR < 1990, "1980s", ifelse(fia.by.cell$INVYR >= 1990 & fia.by.cell$INVYR <= 1999, "1990s", "1999-2017") )
ggplot(fia.by.cell, aes(FIAdensity)) + geom_histogram() + xlim(0,500)+facet_wrap(~INVYRcd)

ggplot(fia.by.cell, aes(x,y, fill = FIAdensity)) + geom_raster() + facet_wrap(~INVYRcd)

# need to check this method but it gives reasonable vals

write.csv(fia.by.cell, "data/FIA_plot_data/fia.by.cell.out_1980_1990.csv", row.names = FALSE)
