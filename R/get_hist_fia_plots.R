library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(sp)
library(raster)

# read in FIA plot, condition, and tree data to look at state level FIA tables
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

# select the datasets for all surveys before 1999
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

# Join the all statewide plot, cond, and tree tables together in one big df:
plot.old <- rbind(inplot.old, ilplot.old, miplot.old, mnplot.old, wiplot.old)


# issues with the condition table names not matching up:
con.col.names <- colnames(wicond.old[,colnames(wicond.old) %in% colnames(incond.old)])

tree.col.names <- colnames(mitree.old[,colnames(mitree.old) %in% colnames(intree.old)])

cond.old <- rbind(incond.old[,con.col.names], ilcond.old[,con.col.names], micond.old[,con.col.names],  mncond.old[,con.col.names], wicond.old[,con.col.names]) # note the MN condition table now has extra columns
tree.old <- rbind(intree.old[,tree.col.names], iltree.old[,tree.col.names], mitree.old[,tree.col.names], mntree.old[,tree.col.names], witree.old[,tree.col.names])




hist(cond.old$STDAGE)

# get only the live trees
tree.old <- tree.old[tree.old$STATUSCD == 1,]
tree.old$DIA <- tree.old$DIA*2.54 # convert DBH to cm
#tree.old <- tree.old[tree.old$DIA >= 20.32,] # select trees > 8 inches (>=20.32)
tree.old <- tree.old[tree.old$DIA >= 2.54,] # select trees > 1 inches (>=20.32)

# join the plot table to the tree dable using INVYR, PLOT, COUNTYCD, and STATECD
merged.tree <- merge(plot.old, tree.old[,c( "STATECD","PLT_CN","PLOT","COUNTYCD", "INVYR", "TREE", "DIA", "TPA_UNADJ", "SPCD","SUBP", "TPA_UNADJ")], by = c(  "INVYR", "PLOT", "COUNTYCD", "STATECD"))



# plot histograms of inventory yearsL
ggplot(merged.tree, aes(PLOT, fill = INVYR))+geom_histogram()+facet_wrap(~INVYR)

#cn <- unique(miplot.old$CN)
#test <- mitree.old[mitree.old$PLT_CN %in% cn,]

# summarise the number of trees in each plot
tree.count <- merged.tree %>% dplyr::count(PLT_CN, SPCD, INVYR, STATECD, PLOT,LAT,LON, TPA_UNADJ)

merged.tree$DIA_class <- ifelse(merged.tree$DIA >= 8*2.54, ">8 in", 
                                ifelse(merged.tree$DIA < 8*2.54 & merged.tree$DIA >= 4*2.54, "4-8 in",
                                       ifelse(merged.tree$DIA < 4*2.54, "1-4 in", NA)))

tree.count.byDBH <- merged.tree %>% dplyr::count(PLT_CN, SPCD, INVYR, STATECD, PLOT,LAT,LON,DIA_class, TPA_UNADJ)


ac2ha   <- 0.404686 # acre to hectare conversion factor

merged.tree <- tree.count#<- merge(merged.tree, tree.count, by = c("PLT_CN", "SPCD", "INVYR", "PLOT","STATECD"))
merged.tree.dbh <- tree.count.byDBH#<- merge(merged.tree, tree.count, by = c("PLT_CN", "SPCD", "INVYR", "PLOT","STATECD"))

# need to matach tree count up with overall merged data
merged.tree$DENS_TPA <- merged.tree$n * merged.tree$TPA_UNADJ * (1/ac2ha) # for the old surveys, we need to use TPA_UNAJ to calculate the tree density
# note that DENS is based on: https://github.com/PalEON-Project/PalEON-FIA/blob/master/R_scripts/Plot_parameters_agg.R
merged.tree$DENS <- merged.tree$n * 6.018046 * (1/ac2ha) # to get trees per ha, multiple the number trees in the plot * the TPA factor (6.018046) * 1/ ac2ha 

hist(merged.tree$DENS)
hist(merged.tree$DENS_TPA)
ggplot(merged.tree, aes(DENS))+geom_histogram()+facet_wrap(~INVYR)+xlim(0,500)

# tree count by diameter class:
# need to matach tree count up with overall merged data
merged.tree.dbh$DENS_TPA <- merged.tree.dbh$n * merged.tree.dbh$TPA_UNADJ * (1/ac2ha) # for the old surveys, we need to use TPA_UNAJ to calculate the tree density
# note that DENS is based on: https://github.com/PalEON-Project/PalEON-FIA/blob/master/R_scripts/Plot_parameters_agg.R
merged.tree.dbh$DENS <- merged.tree.dbh$n * 6.018046 * (1/ac2ha) # to get trees per ha, multiple the number trees in the plot * the TPA factor (6.018046) * 1/ ac2ha 

hist(merged.tree.dbh$DENS)
hist(merged.tree.dbh$DENS_TPA)
ggplot(merged.tree.dbh, aes(DENS))+geom_histogram()+facet_grid(INVYR~DIA_class)+xlim(0,500)


# ALL data convert to paleon coordinates (roughly b/c these are fuzzed + swapped)

head(merged.tree)
merged.tree <- merged.tree[complete.cases(merged.tree[,c("LON", "LAT")]),]

coordinates(merged.tree) <- ~ LON + LAT
proj4string(merged.tree)=CRS("+proj=longlat +datum=WGS84") #define: WGS-84 lon,lat projection
tree.albers <- spTransform(merged.tree,CRS("+init=epsg:3175")) #convert to: NAD83/Great Lakes and St Lawrence Albers projection

base.rast <- raster(xmn = -71000, xmx = 2297000, ncols=296,
                    ymn = 58000,  ymx = 1498000, nrows = 180,
                    crs = '+init=epsg:3175')

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
FIA.by.paleon <- dcast(FIA.pal, LON + LAT + PLT_CN+ x + y + cell + INVYR ~ PalEON, sum, na.rm=TRUE, value.var = 'DENS_TPA') #sum all species in common taxa in FIA grid cells
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

ggplot(fia.by.cell, aes( FIAdensity, fill = INVYRcd))+geom_histogram(position = "identity", alpha = 0.5)+xlim(0,500)
ggplot(fia.by.cell, aes(Hemlock, fill = INVYRcd))+geom_histogram(position = "identity", alpha = 0.5)+xlim(0,500)
ggplot(fia.by.cell, aes(Beech, fill = INVYRcd))+geom_histogram(position = "identity", alpha = 0.5)+xlim(0,500)

# need to check this method but it gives reasonable vals


write.csv(fia.by.cell, "data/FIA_plot_data/fia.by.cell.out_1980_1990_TPA_1in.csv", row.names = FALSE)
fia.by.cell<- read.csv( "data/FIA_plot_data/fia.by.cell.out_1980_1990_TPA1in.csv")
fia.by.cell.v1<- read.csv( "data/FIA_plot_data/fia.by.cell.out_1980_19901in.csv")
density.compare <- merge(fia.by.cell[,c("x", "y", "INVYR", "FIAdensity")], fia.by.cell.v1[,c("x", "y", "INVYR", "FIAdensity")], by = c("x", "y", "INVYR"))

ggplot(density.compare, aes(FIAdensity.x, FIAdensity.y))+geom_point()+geom_abline(a = 0, b = 1, col = "red")


# for DIA classes: convert to paleon coordinates (roughly b/c these are fuzzed + swapped)

head(merged.tree.dbh)
merged.tree.dbh <- merged.tree.dbh[complete.cases(merged.tree.dbh[,c("LON", "LAT")]),]

coordinates(merged.tree.dbh) <- ~ LON + LAT
proj4string(merged.tree.dbh)=CRS("+proj=longlat +datum=WGS84") #define: WGS-84 lon,lat projection
tree.albers <- spTransform(merged.tree.dbh,CRS("+init=epsg:3175")) #convert to: NAD83/Great Lakes and St Lawrence Albers projection

base.rast <- raster(xmn = -71000, xmx = 2297000, ncols=296,
                    ymn = 58000,  ymx = 1498000, nrows = 180,
                    crs = '+init=epsg:3175')

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
FIA.by.paleon <- dcast(FIA.pal, LON + LAT + PLT_CN+ x + y + cell + INVYR + DIA_class ~ PalEON, sum, na.rm=TRUE, value.var = 'DENS_TPA') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,9:length(FIA.by.paleon)], na.rm = TRUE) # sum the total density in each plot
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y',"LON", "LAT", 'cell', "PLT_CN",  "INVYR", "DIA_class")) # melt the dataframe
fia.by.cell.dbh <- dcast(fia.melt, x + y+ cell + INVYR +DIA_class ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell
#fia.by.cell$total <- rowSums(fia.by.cell[,4:28], na.rm = TRUE)

ggplot(fia.by.cell.dbh, aes(x, y, fill = FIAdensity)) + geom_raster()+facet_wrap(~DIA_class)
ggplot(fia.by.cell.dbh, aes(x, y, fill = DIA_class)) + geom_raster() + facet_wrap(~INVYR)
ggplot(fia.by.cell.dbh, aes(FIAdensity, fill = DIA_class, alpha = 0.3)) + geom_histogram(position = "identity") + xlim(0,500)+facet_wrap(~INVYR)
#ggplot(FIA.by.paleon, aes(Hemlock)) + geom_histogram() + facet_wrap(~INVYR)

fia.by.cell.dbh$INVYRcd<- ifelse(fia.by.cell.dbh$INVYR < 1990, "1980s", ifelse(fia.by.cell.dbh$INVYR >= 1990 & fia.by.cell.dbh$INVYR <= 1999, "1990s", "1999-2017") )
ggplot(fia.by.cell.dbh, aes(FIAdensity, fill = DIA_class)) + geom_histogram() + xlim(0,500)+facet_wrap(~INVYRcd)

ggplot(fia.by.cell.dbh, aes(x,y, fill = FIAdensity)) + geom_raster() + facet_grid(DIA_class~INVYRcd)

ggplot(fia.by.cell.dbh, aes( FIAdensity, fill = INVYRcd))+geom_histogram(position = "identity", alpha = 0.5)+xlim(0,500)
ggplot(fia.by.cell.dbh, aes(Hemlock, fill = INVYRcd))+geom_histogram(position = "identity", alpha = 0.5)+xlim(0,500)
ggplot(fia.by.cell.dbh, aes(Beech, fill = INVYRcd))+geom_histogram(position = "identity", alpha = 0.5)+xlim(0,500)

# need to check this method but it gives reasonable vals


write.csv(fia.by.cell.dbh, "data/FIA_plot_data/fia.by.cell.dbh.out_1980_1990_TPA_1in.csv", row.names = FALSE)
fia.by.cell.dbh<- read.csv( "data/FIA_plot_data/fia.by.cell.dbh.out_1980_1990_TPA_1in.csv")
fia.by.cell.dbh.v1<- read.csv( "data/FIA_plot_data/fia.by.cell.dbh.out_1980_1990_TPA_1in.csv")
density.compare <- merge(fia.by.cell.dbh[,c("x", "y", "INVYR", "DIA_class","FIAdensity")], fia.by.cell.dbh.v1[,c("x", "y", "INVYR", "DIA_class","FIAdensity")], by = c("x", "y", "INVYR", "DIA_class"))

ggplot(density.compare, aes(FIAdensity.x, FIAdensity.y))+geom_point()+geom_abline(a = 0, b = 1, col = "red")



#---------------------------------------------------------------------
# Which types of grid cells are being logged??

wiplot.new <- wiplot[wiplot$INVYR > 1999,]
wicond.new <- wicond[wicond$INVYR > 1999,] 
witree.new <- witree[witree$INVYR > 1999,] 

miplot.new <- miplot[miplot$INVYR > 1999,]
micond.new <- micond[micond$INVYR > 1999,] 
mitree.new <- mitree[mitree$INVYR > 1999,] 

mnplot.new <- mnplot[mnplot$INVYR > 1999,]
mncond.new <- mncond[mncond$INVYR > 1999,] 
mntree.new <- mntree[mntree$INVYR > 1999,] 

inplot.new <- inplot[inplot$INVYR > 1999,]
incond.new <- incond[incond$INVYR > 1999,] 
intree.new <- intree[intree$INVYR > 1999,] 

ilplot.new <- ilplot[ilplot$INVYR > 1999,]
ilcond.new <- ilcond[ilcond$INVYR > 1999,] 
iltree.new <- iltree[iltree$INVYR > 1999,] 


plot.new <- rbind(inplot.new, ilplot.new, miplot.new, mnplot.new, wiplot.new)
cond.new <- rbind(incond.new, ilcond.new, micond.new, mncond.new, wicond.new)
tree.new <- rbind(intree.new, iltree.new, mitree.new, mntree.new, witree.new)



tree.new <- tree.new[tree.new$STATUSCD == 1,]
tree.new$DIA <- tree.new$DIA*2.54
tree.new <- tree.new[tree.new$DIA >= 20.32,]
test1 <- merge(plot.new, cond.new[,c( "STATECD","PLT_CN","PLOT","COUNTYCD", "INVYR","STDAGE", "TRTCD1", "DSTRBCD1")], by = c(  "INVYR", "PLOT", "COUNTYCD", "STATECD"))
test <- merge(test1, tree.new[,c( "STATECD","PLT_CN","PLOT","COUNTYCD", "INVYR", "TREE", "DIA", "TPA_UNADJ", "SPCD","SUBP")], by = c(  "INVYR", "PLOT", "COUNTYCD", "STATECD","PLOT"))


merged.tree <- test

merged.tree$TRTcode <- as.character(merged.tree$TRTCD1)
ggplot(merged.tree, aes(LON, LAT, color = TRTcode))+geom_point(size = 0.5)#+facet_wrap(~INVYR)
unique(merged.tree$STATECD)
# get the unique plot name, the inventory year of the treatement, and the type

# there are 3270 plots with some treatment code 10, 20, 30:
cut.plots <- unique(merged.tree[merged.tree$TRTcode %in% c("10", "20", "30"),c("PLT_CN.x","PLOT","INVYR", "TRTcode", "PREV_PLT_CN", "STATECD")])
uncut.plots <- merged.tree[!merged.tree$TRTcode %in% c("10", "20", "30"),]

unique(cut.plots$STATECD)
# need to find the previous density of the cut.plots in the previous inventory year:
# need to select cells by lat, lon, state, and  plot

unique(cut.plots[,c("STATECD")])

merged.tree[merged.tree$PLT_CN.x == 3.755549e+14,]


yrs <- unique(merged.tree[merged.tree$PLOT %in% cut.plots[2951,]$PLOT & merged.tree$STATECD %in% cut.plots[2951,]$STATECD & ! merged.tree$INVYR == cut.plots[2951,]$INVYR,]$INVYR)

prev_year <- yrs[which(abs(yrs-cut.plots[2951,]$INVYR) == min(abs(yrs-cut.plots[2951,]$INVYR)))]

merged.tree[merged.tree$PLOT %in% cut.plots[2951,]$PLOT & merged.tree$STATECD %in% cut.plots[2951,]$STATECD & merged.tree$INVYR %in% yrs[6],]
#merged.tree[merged.tree$PLT_CN == 2.471089e+14,]

# for each plot in cut.plots, find the survey year before logging treatement:

# loop works, but is slow:
prev.year <- 1:length(cut.plots$PLOT)

for(i in 1:length(cut.plots$PLOT)){

  yrs <- unique(merged.tree[merged.tree$PLOT %in% cut.plots[i,]$PLOT & merged.tree$STATECD %in% cut.plots[i,]$STATECD & ! merged.tree$INVYR == cut.plots[i,]$INVYR,]$INVYR)

  if(length(unique(merged.tree[merged.tree$PLOT %in% cut.plots[i,]$PLOT & merged.tree$STATECD %in% cut.plots[i,]$STATECD &  !merged.tree$INVYR == cut.plots[i,]$INVYR,]$INVYR)) < 1){ # if there are no previous plots have prev_year == NA
    prev_year[i] <- NA
    
  }else{ # if there is a previous year, save in prev_year:
    
  prev_year[i] <- yrs[which(abs(yrs-cut.plots[i,]$INVYR) == min(abs(yrs-cut.plots[i,]$INVYR)))]
  
  }
}

cut.plots$prev_yr <- prev_year


# changing to apply function: This still isnot working
#get_prev_year <- function(cuts2plot){
  plt<- cuts2plot['PLOT']
  st <- cuts2plot['STATECD']
  invyr <- cuts2plot['INVYR']
  
  yrs <- as.numeric(unique(merged.tree[merged.tree$PLOT %in% plt & merged.tree$STATECD %in% st & ! merged.tree$INVYR == invyr,]$INVYR))
  
  if(length(unique(merged.tree[merged.tree$PLOT %in% plt & merged.tree$STATECD %in% st &  merged.tree$INVYR < as.numeric(invyr),]$INVYR)) < 1){ # if there are no previous plots have prev_year == NA
    prev_year <- NA
    
  }else{ # if there is a previous year, save in prev_year:
    
    prev_year <- yrs[which((yrs-as.numeric(invyr)) == min(yrs-as.numeric(invyr)))]
    
  }
  return(prev_year)
}



# apply function over the whole cut.plots dataset:
prev_yrs <- list()

cut.2000 <- cut.plots[!cut.plots$INVYR == 2000,]
# this still takes awhile, but is shorter than the loop
#prev_yrs <- apply(X = as.matrix(cut.2000), MARGIN=1, FUN = get_prev_year)

write.csv(merged.tree,"outputs/merged.tree.new.csv", row.names = FALSE)

# save the previous year for each cut plot
#cut.2000$prev_year <- prev_yrs


get_prev_survey <- function(df.prev){
  plt<- df.prev['PLOT']
  st <- df.prev['STATECD']
  invyr <- df.prev['prev_year']
  
  prev_survey <- merged.tree[merged.tree$PLOT %in% plt & merged.tree$STATECD %in% st &  merged.tree$INVYR == invyr,]
  
  
  return(prev_survey)
}

prev_yr_survey <- list()
prev_yr_survey <- apply(X = as.matrix(cut.2000), MARGIN=1, FUN = get_prev_survey)

# write cut.2000 to csv
write.csv(cut.2000, "outputs/logged_prev_years.csv",row.names = FALSE)

# need to get the data from each of these plots in previous years and put into a new df:
prev_yr_survey <- merge(cut.2000[,c("STATECD", "PLOT", "INVYR", "TRTcode","PLT_CN.x")], merged.tree[,c("INVYR","PLOT", "STATECD","PLT_CN.x", "COUNTYCD", "CN", "PLOT_STATUS_CD", "LAT", "LON", "TREE", "TPA_UNADJ", "SPCD", "SUBP")], by.x = c(  "INVYR", "PLOT",  "STATECD"), by.y = c(  "INVYR", "PLOT", "STATECD"))
#prev_yr_survey <- read.csv("outputs/logged_prev_years_data.csv")
#merged.tree[merged.tree$PLT_CN %in% 6.670160e+13 ,]

# now estimate density for these past surveys:
#prev_yr_survey <- prev_yr_survey[!names(prev_yr_survey) %in% "PREV_PLT_CN", ]
tree.count <- prev_yr_survey %>% dplyr::count( SPCD, INVYR, STATECD, PLOT,LAT,LON)

unique(cut.2000$STATECD)


# select trees > 8 inches (>=20.32)
ac2ha   <- 0.404686 

merged.tree.new <- tree.count#<- merge(merged.tree, tree.count, by = c("PLT_CN", "SPCD", "INVYR", "PLOT","STATECD"))

# need to matach tree count up with overall merged data
#merged.tree$DENS <- merged.tree$n * merged.tree$TPA_UNADJ * (1/ac2ha)
merged.tree.new$DENS <- merged.tree.new$n * 6.018046 * (1/ac2ha)

hist(merged.tree.new$DENS)
ggplot(merged.tree.new, aes(DENS))+geom_histogram()+facet_wrap(~INVYR)+xlim(0,500)
#ggplot(merged.tree.new, aes(DENS))+geom_histogram()+facet_wrap(~INVYR)+xlim(0,500)
# convert to paleon coordinates (roughly b/c these are fuzzed + swapped)

head(merged.tree.new)
merged.tree.new <- merged.tree.new[complete.cases(merged.tree.new[,c("LON", "LAT")]),]

coordinates(merged.tree.new) <- ~ LON + LAT
proj4string(merged.tree.new)=CRS("+proj=longlat +datum=WGS84") #define: WGS-84 lon,lat projection
tree.albers <- spTransform(merged.tree.new,CRS("+init=epsg:3175")) #convert to: NAD83/Great Lakes and St Lawrence Albers projection


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
FIA.by.paleon <- dcast(FIA.pal, LON + LAT  +STATECD + x + y + cell + INVYR ~ PalEON, sum, na.rm=TRUE, value.var = 'DENS') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,9:length(FIA.by.paleon)], na.rm = TRUE) # sum the total density in each plot
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y',"LON", "LAT", 'cell',   "INVYR","STATECD", "TR")) # melt the dataframe
fia.by.cell <- dcast(fia.melt, x + y+ cell + INVYR ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell
#fia.by.cell$total <- rowSums(fia.by.cell[,4:28], na.rm = TRUE)

ggplot(fia.by.cell, aes(x, y, fill = FIAdensity)) + geom_raster()
ggplot(fia.by.cell, aes(x, y, fill = FIAdensity)) + geom_raster() + facet_wrap(~INVYR)

ggplot(fia.by.cell, aes( FIAdensity)) + geom_histogram() + facet_wrap(~INVYR)

ggplot(fia.by.cell, aes(FIAdensity))+geom_histogram()+

write.csv(fia.by.cell, "data/FIA_plot_data/fia.by.cell.treated.2000_2017.csv", row.names = FALSE)
summary(fia.by.cell)


# want to get an estimate of the porportion of total plots in each density class that are logged between 1999-2017:
# actually want to do this at aggregate grid cell level or by plot level?

# lets do by grid cell first:
# get # of plots in each density classes of all the plots in the recent surveys:
# using bins of width 30 to classify density:
# need to get all unlogged places:


tree.count.uncut <- uncut.plots %>% dplyr::count(SPCD, INVYR, STATECD, PLOT,LAT,LON)


# select trees > 8 inches (>=20.32)
ac2ha   <- 0.404686 

merged.tree.uncut <- tree.count.uncut#<- merge(merged.tree, tree.count, by = c("PLT_CN", "SPCD", "INVYR", "PLOT","STATECD"))

# need to matach tree count up with overall merged data
#merged.tree$DENS <- merged.tree$n * merged.tree$TPA_UNADJ * (1/ac2ha)
merged.tree.uncut$DENS <- merged.tree.uncut$n * 6.018046 * (1/ac2ha)

hist(merged.tree.uncut$DENS)
ggplot(merged.tree.uncut, aes(DENS))+geom_histogram()+facet_wrap(~INVYR)+xlim(0,500)

# convert to paleon coordinates (roughly b/c these are fuzzed + swapped)

head(merged.tree.uncut)
merged.tree.uncut <- merged.tree.uncut[complete.cases(merged.tree.uncut[,c("LON", "LAT")]),]

coordinates(merged.tree.uncut) <- ~ LON + LAT
proj4string(merged.tree.uncut)=CRS("+proj=longlat +datum=WGS84") #define: WGS-84 lon,lat projection
tree.albers <- spTransform(merged.tree.uncut,CRS("+init=epsg:3175")) #convert to: NAD83/Great Lakes and St Lawrence Albers projection


numbered.rast <- setValues(base.rast, 1:ncell(base.rast))
numbered.cell <- raster::extract(numbered.rast, tree.albers)

xys <- raster::xyFromCell(numbered.rast, cell = numbered.cell)

tree.albers <- data.frame(tree.albers)
tree.albers$x <- xys[,1]
tree.albers$y <- xys[,2]
tree.albers$cell <- numbered.cell


head(tree.albers)
speciesconversion <- read.csv('data/fia_conversion_v02-sgd.csv')

FIA.pal.uncut <- merge(tree.albers, speciesconversion, by.x = 'SPCD', by.y = "spcd" )
FIA.by.paleon.uncut <- dcast(FIA.pal.uncut, LON + LAT +STATECD + x + y + cell + INVYR ~ PalEON, sum, na.rm=TRUE, value.var = 'DENS') #sum all species in common taxa in FIA grid cells
FIA.by.paleon.uncut$FIAdensity <- rowSums(FIA.by.paleon.uncut[,8:length(FIA.by.paleon.uncut)], na.rm = TRUE) # sum the total density in each plot
fia.melt.uncut <- melt(FIA.by.paleon.uncut, id.vars = c('x', 'y',"LON", "LAT", 'cell',  "INVYR","STATECD")) # melt the dataframe
fia.by.cell.uncut <- dcast(fia.melt.uncut, x + y+ cell + INVYR ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell
#fia.by.cell$total <- rowSums(fia.by.cell[,4:28], na.rm = TRUE)

ggplot(fia.by.cell.uncut, aes(x, y, fill = FIAdensity)) + geom_raster()


label.breaks <- function(beg, end, splitby){
  labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
  labels.test <- paste (labels.test$first, '-' , labels.test$second)
  labels.test
}


fia.by.cell.uncut$dens_bins <- cut(fia.by.cell.uncut$FIAdensity, breaks = seq(0,600, by = 30), labels = label.breaks(0,570, 30))
fia.by.cell.uncut$dens_binsx <- ifelse(fia.by.cell.uncut$FIAdensity <= 47, "0 - 47", 
                                 ifelse(fia.by.cell.uncut$FIAdensity > 47 & fia.by.cell.uncut$FIAdensity <= 100, "48 - 100",
                                        ifelse(fia.by.cell.uncut$FIAdensity > 100 & fia.by.cell.uncut$FIAdensity <= 200, "100 - 200",
                                               ifelse(fia.by.cell.uncut$FIAdensity > 200 & fia.by.cell.uncut$FIAdensity <= 300, "200 - 300",
                                                      ifelse(fia.by.cell.uncut$FIAdensity > 300 & fia.by.cell.uncut$FIAdensity <= 400, "300 - 400", 
                                                             ifelse(fia.by.cell.uncut$FIAdensity > 400 & fia.by.cell.uncut$FIAdensity <= 500,"400 - 500", "500 +"))))))


n_uncut_by_dens <- fia.by.cell.uncut %>% dplyr::count( dens_binsx)
n_uncut_by_dens_invyr <- fia.by.cell.uncut %>% dplyr::count(INVYR, dens_binsx)
ggplot(n_uncut_by_dens, aes(dens_binsx, n))+geom_bar(stat = "identity")

ggplot(n_uncut_by_dens_invyr, aes(dens_binsx, n))+geom_bar(stat = "identity")+facet_wrap(~INVYR)


# get # of plots in each density classes of the logged plots in the recent surveys:
label.breaks <- function(beg, end, splitby){
  labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
  labels.test <- paste (labels.test$first, '-' , labels.test$second)
  labels.test
}


fia.by.cell$dens_bins <- cut(fia.by.cell$FIAdensity, breaks = seq(0,600, by = 30), labels = label.breaks(0,570, 30))
fia.by.cell$dens_binsx <- ifelse(fia.by.cell$FIAdensity <= 47, "0 - 47", 
                                 ifelse(fia.by.cell$FIAdensity > 47 & fia.by.cell$FIAdensity <= 100, "48 - 100",
                                 ifelse(fia.by.cell$FIAdensity > 100 & fia.by.cell$FIAdensity <= 200, "100 - 200",
                                        ifelse(fia.by.cell$FIAdensity > 200 & fia.by.cell$FIAdensity <= 300, "200 - 300",
                                               ifelse(fia.by.cell$FIAdensity > 300 & fia.by.cell$FIAdensity <= 400, "300 - 400", 
                                               ifelse(fia.by.cell$FIAdensity > 400 & fia.by.cell$FIAdensity <= 500,"400 - 500", "500 +"))))))

n_logged_by_dens <- fia.by.cell %>% dplyr::count( dens_binsx)
n_logged_by_dens_invyr <- fia.by.cell %>% dplyr::count(INVYR, dens_binsx)


ggplot(n_logged_by_dens, aes(dens_binsx, n))+geom_bar(stat = "identity")

# get the ratio of plots logged in each density class
n_uncut_by_dens
n_logged_by_dens

logged_counts<- merge(n_uncut_by_dens, n_logged_by_dens, by = "dens_binsx")
colnames(logged_counts) <- c("dens_bins", "unlogged", "logged")

logged_counts$ratio <- logged_counts$logged/(logged_counts$logged+logged_counts$unlogged)*100
# need to replace NA factor with "600+"
logged_counts$dens_bins<- as.character(logged_counts$dens_bins) 
#logged_counts[21,]$dens_bins <- "600+"
logged_counts$dens_bins <- as.factor(logged_counts$dens_bins)

# need to reorder the dens_bins:
#logged_counts$dens_bins = factor(logged_counts$dens_bins,levels = c("0 - 30", "30 - 60", "60 - 90", 
 #                                                                    "90 - 120", "120 - 150", "150 - 180",
  #                                                                   "180 - 210", "210 - 240" ,"240 - 270", "270 - 300",
   #                                                                  "300 - 330", "330 - 360", "360 - 390", "390 - 420" ,"420 - 450", "450 - 480", "480 - 510", "510 - 540", "540 - 570", "570 - 600","600+"))

logged_counts$dens_bins = factor(logged_counts$dens_bins,levels = c( "0 - 47", "48 - 100", "100 - 200", "200 - 300", "300 - 400",
                                                                     "400 - 500", "500 +"))


png("outputs/paper_figs/pct_FIA_dens_class_logged.png")
ggplot(logged_counts, aes(dens_bins, ratio))+geom_bar(stat = "identity")+ylab("% of density class logged")+xlab("Density Class")+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()


# get ratio of grid cells logged by invyr:
logged_counts_invyr <- merge(n_uncut_by_dens_invyr, n_logged_by_dens_invyr, by = c("dens_binsx", "INVYR"))
colnames(logged_counts_invyr) <- c("dens_bins", "INVYR","unlogged", "logged")

logged_counts_invyr$ratio <- logged_counts_invyr$logged/(logged_counts_invyr$logged+logged_counts_invyr$unlogged)*100
# need to replace NA factor with "600+"
logged_counts_invyr$dens_bins<- as.character(logged_counts_invyr$dens_bins) 
#logged_counts_invyr[logged_counts_invyr$dens.bins %in% NA,]$dens_bins <- "600+"
logged_counts_invyr$dens_bins <- as.factor(logged_counts_invyr$dens_bins)

# need to reorder the dens_bins:
logged_counts_invyr$dens_bins = factor(logged_counts_invyr$dens_bins, levels = c("0 - 30", "30 - 60", "60 - 90", 
                                                                    "90 - 120", "120 - 150", "150 - 180",
                                                                    "180 - 210", "210 - 240" ,"240 - 270", "270 - 300",
                                                                    "300 - 330", "330 - 360", "360 - 390", "390 - 420" ,"420 - 450", "450 - 480", "480 - 510", "510 - 540", "540 - 570", "570 - 600","600+"))

logged_counts_invyr$dens_bins = factor(logged_counts_invyr$dens_bins,levels = c( "0 - 47", "48 - 100", "100 - 200", "200 - 300", "300 - 400",
                                                                     "400 - 500", "500 +"))

png(height = 8, width = 12, units = "in",res = 250,"outputs/paper_figs/pct_FIA_dens_class_logged_by_INVYR.png")
ggplot(logged_counts_invyr, aes(dens_bins, ratio))+geom_bar(stat = "identity")+facet_wrap(~INVYR)+ylab("% of density class logged")+xlab("Density Class")+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

