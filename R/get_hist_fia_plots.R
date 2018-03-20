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

ggplot(fia.by.cell, aes( FIAdensity, fill = INVYRcd))+geom_histogram(position = "identity", alpha = 0.5)+xlim(0,500)
ggplot(fia.by.cell, aes(Hemlock, fill = INVYRcd))+geom_histogram(position = "identity", alpha = 0.5)+xlim(0,500)
ggplot(fia.by.cell, aes(Beech, fill = INVYRcd))+geom_histogram(position = "identity", alpha = 0.5)+xlim(0,500)

# need to check this method but it gives reasonable vals


write.csv(fia.by.cell, "data/FIA_plot_data/fia.by.cell.out_1980_1990.csv", row.names = FALSE)

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
test <- merge(test1, tree.new[,c( "STATECD","PLT_CN","PLOT","COUNTYCD", "INVYR", "TREE", "DIA", "TPA_UNADJ", "SPCD","SUBP")], by = c(  "INVYR", "PLOT", "COUNTYCD", "STATECD", "PLT_CN"))


merged.tree <- test

merged.tree$TRTcode <- as.character(merged.tree$TRTCD1)
ggplot(merged.tree, aes(LON, LAT, color = TRTcode))+geom_point(size = 0.5)+facet_wrap(~INVYR)

# get the unique plot name, the inventory year of the treatement, and the type

# there are 3270 plots with some treatment code 10, 20, 30:
cut.plots <- unique(merged.tree[merged.tree$TRTcode %in% c("10", "20", "30"),c("PLT_CN", "PLOT","INVYR", "TRTcode", "PREV_PLT_CN", "STATECD")])
#uncut.plots <- merged.tree[merged.tree$TRTcode %in% c("10", "20", "30"),]

# need to find the previous density of the cut.plots in the previous inventory year:
# need to select cells by lat, lon, state, and  plot

unique(cut.plots[,c("PLT_CN", "PLOT", "STATECD")])

merged.tree[merged.tree$PLT_CN == 3.755549e+14,]


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
get_prev_year <- function(cuts2plot){
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
prev_yrs <- apply(X = as.matrix(cut.2000), MARGIN=1, FUN = get_prev_year)

write.csv(merged.tree,"outputs/merged.tree.new.csv", row.names = FALSE)

# save the previous year for each cut plot
cut.2000$prev_year <- prev_yrs


get_prev_survey <- function(df.prev){
  plt<- df.prev['PLOT']
  st <- df.prev['STATECD']
  invyr <- df.prev['prev_year']
  
  prev_survey <- merged.tree[merged.tree$PLOT %in% plt & merged.tree$STATECD %in% st &  merged.tree$INVYR == invyr,]
  
  
  return(prev_survey)
}

prev_yr_survey <- list()
prev_yr_survey <- apply(X = as.matrix(cut.2000[1:20,]), MARGIN=1, FUN = get_prev_survey)

# write cut.2000 to csv
write.csv(cut.2000, "outputs/logged_prev_years.csv",row.names = FALSE)

# need to get the data from each of these plots in previous years and put into a new df:
prev_yr_survey <- merge(cut.2000[,c("STATECD", "PLOT", "INVYR", "TRTcode","PLT_CN")], merged.tree[,c("INVYR","PLOT", "STATECD","PLT_CN", "COUNTYCD", "CN", "PLOT_STATUS_CD", "LAT", "LON", "TREE", "TPA_UNADJ", "SPCD", "SUBP")], by.x = c(  "INVYR", "PLOT",  "STATECD", "PLT_CN"), by.y = c(  "INVYR", "PLOT", "STATECD", "PLT_CN"))
prev_yr_survey <- read.csv("outputs/logged_prev_years_data.csv")
#merged.tree[merged.tree$PLT_CN %in% 6.670160e+13 ,]

# now estimate density for these past surveys:
#prev_yr_survey <- prev_yr_survey[!names(prev_yr_survey) %in% "PREV_PLT_CN", ]
tree.count <- prev_yr_survey %>% dplyr::count(PLT_CN, SPCD, INVYR, STATECD, PLOT,LAT,LON)


# select trees > 8 inches (>=20.32)
ac2ha   <- 0.404686 

merged.tree.new <- tree.count#<- merge(merged.tree, tree.count, by = c("PLT_CN", "SPCD", "INVYR", "PLOT","STATECD"))

# need to matach tree count up with overall merged data
#merged.tree$DENS <- merged.tree$n * merged.tree$TPA_UNADJ * (1/ac2ha)
merged.tree.new$DENS <- merged.tree.new$n * 6.018046 * (1/ac2ha)

hist(merged.tree.new$DENS)
ggplot(merged.tree.new, aes(DENS))+geom_histogram()+facet_wrap(~INVYR)+xlim(0,500)

# convert to paleon coordinates (roughly b/c these are fuzzed + swapped)

head(merged.tree.new)
merged.tree <- merged.tree.new[complete.cases(merged.tree.new[,c("LON", "LAT")]),]

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
FIA.by.paleon <- dcast(FIA.pal, LON + LAT + PLT_CN +STATECD + x + y + cell + INVYR ~ PalEON, sum, na.rm=TRUE, value.var = 'DENS') #sum all species in common taxa in FIA grid cells
FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,9:length(FIA.by.paleon)], na.rm = TRUE) # sum the total density in each plot
fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y',"LON", "LAT", 'cell', "PLT_CN",  "INVYR","STATECD")) # melt the dataframe
fia.by.cell <- dcast(fia.melt, x + y+ cell + INVYR ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell
#fia.by.cell$total <- rowSums(fia.by.cell[,4:28], na.rm = TRUE)

ggplot(fia.by.cell, aes(x, y, fill = FIAdensity)) + geom_raster()
ggplot(fia.by.cell, aes(x, y, fill = FIAdensity)) + geom_raster() + facet_wrap(~INVYR)

ggplot(fia.by.cell, aes( FIAdensity)) + geom_histogram() + facet_wrap(~INVYR)

ggplot(fia.by.cell, aes(FIAdensity))+geom_histogram()

write.csv(fia.by.cell, "data/FIA_plot_data/fia.by.cell.treated.2000_2017.csv", row.names = FALSE)
summary(prev_yr_survey)

