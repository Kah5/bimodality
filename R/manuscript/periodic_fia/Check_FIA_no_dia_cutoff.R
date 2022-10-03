
## Download data from FIA DataMart
library(assertthat)
states = c("MN", "WI", "MI", "IN", "IL")
raw_data_dir <- "data/FIA_plot_data"
for(state in states){
  if (file.exists(file.path(raw_data_dir, paste0(state, "_PLOT.csv"))) &
      file.exists(file.path(raw_data_dir, paste0(state, "_TREE.csv"))) &
      file.exists(file.path(raw_data_dir, paste0(state, "_COND.csv"))) & !redownload_FIA_data) {
    print("Files already downloaded!")
    next
  } else {
    download.file(paste0("https://apps.fs.usda.gov/fia/datamart/CSV/", state, "_PLOT.csv"), 
                  file.path(raw_data_dir, paste0(state, "_PLOT.csv")), 
                  quiet = FALSE, 
                  mode = "w",
                  cacheOK = TRUE)
    download.file(paste0("https://apps.fs.usda.gov/fia/datamart/CSV/", state, "_TREE.csv"), 
                  file.path(raw_data_dir, paste0(state, "_TREE.csv")), 
                  quiet = FALSE, 
                  mode = "w",
                  cacheOK = TRUE)
    download.file(paste0("https://apps.fs.usda.gov/fia/datamart/CSV/", state, "_COND.csv"), 
                  file.path(raw_data_dir, paste0(state, "_COND.csv")), 
                  quiet = FALSE, 
                  mode = "w",
                  cacheOK = TRUE)
  }
}

## Save info on files into VERSIONS file in data directory.
## file.info(list.files()) not working so this code is not cross-platform
# if(.Platform$OS.type == "unix") {
#   info <- system(paste0("ls -l ", raw_data_dir, "/"), intern=TRUE)
#   assert_that(length(grep("\\.csv", info)) == length(states)*3,
#               msg = "incorrect number of files downloaded")
#   info <- c(paste0("Files downloaded ", date() , " with redownloading set to ", 
#                    redownload_FIA_data), info)
#   write(info, file = file.path(raw_data_dir, 'VERSIONS'), append = TRUE)
# }


## Extract tree-level data for all FIA plots satisfying the PalEON criteria,
## including that the data is from the most recent of the surveys for a given plot

library(dplyr)
library(readr)
library(assertthat)

## see here for FIA metadata
## https://www.fia.fs.fed.us/library/database-documentation/index.php

## Enforce column type information. See notes in fia_cols.R for why this is important.
source(file.path('preprocessing','fia_cols.R'))

## Variables to keep for our analyses:
vars = c('PLT_CN', 'PLOT', 'UNITCD', 'COUNTYCD', 'STATECD', 'SUBP', 'LAT', 'LON', 'INVYR', 
         'TREE', 'SPCD', 'DIA', 'STATUSCD', 'TPA_UNADJ', 'DRYBIO_AG')
fia = data.frame(matrix(NA, nrow = 0, ncol = length(vars)))

#state = "MN"
for(state in states){
  cat("Processing state ", state, " ... ")
  
  state_plot <- read_csv(file.path(raw_data_dir, paste0(state, "_PLOT.csv")),
                         col_types = cols_plot, progress = FALSE)
  
  state_cond <- read_csv(file.path(raw_data_dir, paste0(state, "_COND.csv")),
                         col_types = cols_cond, progress = FALSE)
  state_cond <- state_cond %>% dplyr::select(PLT_CN, STDORGCD, CONDID, COND_STATUS_CD, CONDPROP_UNADJ)
  
  if(exclude_plantation) {
    homogeneous_forested_plots <- state_plot %>% left_join(state_cond, by = c('CN' = 'PLT_CN')) %>%
      group_by(CN) %>% summarize(n = n(), avg_status = mean(COND_STATUS_CD), avg_origin = mean(STDORGCD)) %>%
      filter(avg_status == 1, avg_origin == 0)
  } else homogeneous_forested_plots <- state_plot %>% left_join(state_cond, by = c('CN' = 'PLT_CN')) %>%
    group_by(CN) %>% summarize(n = n(), avg_status = mean(COND_STATUS_CD)) %>%
    filter(avg_status == 1)
  
  ## only sampled, forested plots with surveys since changeover in survey design
  state_plot <- state_plot %>% filter(PLOT_STATUS_CD == 1 & INVYR >= earliest_fia_year & INVYR <= latest_fia_year)
  
  ## Only homogeneous condition plots; most mixed plots seem to have
  ## various amounts of non-forested
  
  ## Note: if we want to use plots that are partially forested, we need to retain 'COND:::CONDPROP_UNADJ'
  ## and then divide biomass/density at plot level by that number for correct scaling to area surveyed.
  ## see Github issue #2
  
  state_plot <- state_plot %>% filter(CN %in% homogeneous_forested_plots$CN)
  
  ## PLOTxSTATECDxUNITCDxCOUNTYCD should be a unique plot identifier
  ## select plot info only for most recent survey
  state_plot <- state_plot %>% group_by(PLOT, UNITCD, COUNTYCD) %>%
    mutate(INVYR_MAX = max(INVYR)) %>%
    filter(INVYR == INVYR_MAX)
  
  state_tree <- read_csv(file.path(raw_data_dir, paste0(state, "_TREE.csv")),
                         col_types = cols_tree, progress = FALSE)
  
  ## Only live trees with non-NA diameters, then match to plot info
  ## note that only CN is needed for matching but use of others prevents duplicated fields in result
  state_recent <- state_tree %>% filter(STATUSCD == 1 & !is.na(DIA)) %>%
    inner_join(state_plot, by = c("PLT_CN" = "CN", "PLOT"="PLOT", "INVYR" = "INVYR",
                                  "UNITCD" = "UNITCD", "COUNTYCD" = "COUNTYCD",
                                  "STATECD" = "STATECD"))
  
  state_recent <- state_recent[ , vars]
  
  cat(nrow(state_recent), " live trees in the interval ", earliest_fia_year, "-", latest_fia_year, ".\n")
  fia <- rbind(fia, state_recent)
}

assert_that(nrow(fia) == 1097212, msg = "unexpected number of trees before exclusions")

nNA <- sum(is.na(fia$SPCD))
if(nNA)
  warning("Found ", nNA, " missing taxon codes.")

## Only trees > 8 inches DBH


fia.8in <- fia %>% filter(DIA >= diameter_cutoff_inches)
fia.8in <- fia.8in %>% mutate(DIA_CM = DIA / cm_to_inch)



fia. <- fia %>% filter(DIA >= 1)
fia <- fia %>% mutate(DIA_CM = DIA / cm_to_inch)
conversions_data_dir = "data/conversions"

## add PalEON level3s taxon info
fia_to_l3a <- read_csv(file.path(conversions_data_dir, fia_to_level3a_file)) %>%
  dplyr::select('fia_spcd','level3a')
l3a_to_l3s <- read_csv(file.path(conversions_data_dir, level3a_to_level3s_file)) %>%
  dplyr::select('level3a','level3s')
fia <- fia %>% left_join(fia_to_l3a, by = c("SPCD" = "fia_spcd")) %>%
  left_join(l3a_to_l3s, by = c("level3a" = "level3a"))

## Too few of certain taxa to treat separately
fia <- fia %>% mutate(level3s = ifelse(level3s %in% excluded_level3s, "Other hardwood", level3s))

assert_that(nrow(fia) == 399316, msg = "unexpected number of trees after exclusions")
interim_results_dir = "data/fia_1inch_cutoff"

save(fia, file = file.path(interim_results_dir, 'full_trees.Rda'))



library(raster)
library(dplyr)
library(readr)
library(ncdf4)
library(tidyr)

## if doing only composition or density analysis, this could be changed to
## use full_trees.Rda
fn <- 'full_trees_1cm_diacutoff'
if(use_agb)
  fn <- paste0(fn, '_agb')
fn <-  paste0(fn, '.Rda')
load(file.path(interim_results_dir, fn))

load(file.path(interim_results_dir, fn))
## create base raster in Albers projection -- see PalEON Wiki
## for info on the PalEON Albers-based grid.
base_raster <- raster(xmn = -71000, xmx = 2297000, ncols = 296,
                      ymn = 58000,  ymx = 1498000, nrows = 180,
                      crs = '+init=epsg:3175')
## create PalEON cell ID for all the grid cells.
base_raster <- setValues(base_raster, 1:ncell(base_raster))

## Add PalEON cell info to the dataset.
# doing it with the fuzzed and swapped data
  x <- SpatialPointsDataFrame(coords = data.frame(fia = fia$LON, y = fia$LAT),
                              data = data.frame(CN = fia$PLT_CN, STATECD = fia$STATECD),
                              proj4string = CRS("+init=epsg:4269"))
  ## Transform to PalEON Albers projection
  x <-spTransform(x, CRS("+init=epsg:3175"))

  test <- extract(base_raster,x, cellnumbers = TRUE, df = TRUE) 
  xy <- raster::xyFromCell(base_raster, cell = test$cells, spatial = FALSE)
  
  # Convert to df and add cellnumber
  xy <- as.data.frame(xy)
  xy$cell <- test$cells
  
  fia.grid <- cbind(fia, xy)

  
  write.csv(fia.grid, "FIA_species_plot_parameters_paleongrid_1incutoff.csv")
  fia.grid<- read.csv( "FIA_species_plot_parameters_paleongrid_1incutoff.csv")
  # now summaries by grid and estimate density:

  
  cm_to_inch=0.39370
  fia_subplot_radius_in_feet=24 
  fia_subplots_per_plot=4
  squ_feet_per_acre=43560
  acres_per_ha=2.47105
  kg_per_Mg=1000
  
  # estimate density:
  area_conversion <- acres_per_ha * squ_feet_per_acre /
    (pi * fia_subplot_radius_in_feet^2 * fia_subplots_per_plot)
  
  ## total number of fia points per grid cell
  plots_per_cell <- fia.grid %>% group_by(PLT_CN, x, y, cell) %>%
    dplyr::summarize(n_trees = n()) %>% 
    group_by(cell) %>% summarize(points_total = n()) 
  
  
  
  ## Total density per taxon per plot only for taxa represented in a plot.
  ## Currently grouping taxa by level3s but this could change.
  density_taxon_plot <- fia.grid %>% group_by(PLT_CN, level3s, cell, x, y) %>%
    dplyr::summarize(total_density = n() * area_conversion) 
  
  # to do: bin by dia_inch classes, then  summarise:
  fia.grid$DIA_classes <- ifelse(fia.grid$DIA >= 8, ">8 in", 
                                 ifelse(fia.grid$DIA >= 4 & fia.grid$DIA < 8, "4-8 in", 
                                 "1-4 in"))
  
  
  
  density_taxon_plot.dbh <- fia.grid %>% group_by(PLT_CN, DIA_classes, level3s, cell, x, y) %>%
    dplyr::summarize(total_density = n() * area_conversion) 
  
  density_plot.dbh <- fia.grid %>% group_by(PLT_CN, DIA_classes,  cell, x, y) %>%
    dplyr::summarize(total_density = n() * area_conversion) 
  
  density_plot <- fia.grid %>% group_by(PLT_CN, DIA_classes,  cell, x, y) %>%
    mutate(total_density_dbhclass = n() * area_conversion) 
  ## after conversion, density is stems/Ha
  
  ggplot(density_taxon_plot, aes(x = x, y = y, fill = total_density))+geom_raster()+
    facet_wrap(~level3s)
  
  ## Average density per taxon per cell only for taxa represented in a cell,
  ## and only for plots in which taxon is present, so that potential density
  ## modeling ignores occupancy.
  density_taxon_cell <- density_taxon_plot %>% group_by(cell, x, y, level3s) %>%
    dplyr::summarize(avg = mean(total_density),
              geom_avg = mean(log(total_density)), points_occ = n()) %>% ungroup() %>% group_by(cell, x, y) %>% mutate(total = sum(avg))
  
  
  density_taxon_cell.dia <- density_taxon_plot.dbh %>% group_by(cell, x, y, level3s, DIA_classes) %>%
    dplyr::summarize(avg = mean(total_density),
              geom_avg = mean(log(total_density)), points_occ = n()) %>% ungroup() %>% group_by(cell, x, y, DIA_classes) %>% mutate(total = sum(avg))
  
  
  density_taxon_cell.dia_all <- density_plot.dbh  %>% group_by(cell, x, y, DIA_classes) %>%
    dplyr::summarize(avg = mean(total_density),
              geom_avg = mean(log(total_density)), points_occ = n()) %>% ungroup() %>% group_by(cell, x, y, DIA_classes) %>% mutate(totaldia = sum(avg)) %>% 
              ungroup() %>% group_by(cell, x, y) %>% mutate(total = sum(avg)) 
  
  total.dens.1in<- unique(density_taxon_cell[,c("x", "y", "cell", "level3s", "total")])
  ggplot(density_taxon_cell, aes(x = x, y = y, fill = avg))+geom_raster()+
    facet_wrap(~level3s)
  ggplot( total.dens.1in, aes(x = x, y = y, fill = total))+geom_raster()
  ggplot( total.dens.1in, aes( total))+geom_histogram()
  
  total.dens.dia.classes<- unique(density_taxon_cell.dia[,c("x", "y", "cell", "level3s","DIA_classes", "total")])

  
  ggplot(   total.dens.dia.classes, aes(x = x, y = y, fill = total))+geom_raster()+facet_wrap(~DIA_classes)
  ggplot(  total.dens.dia.classes, aes( total, fill = level3s))+geom_histogram()+facet_wrap(~DIA_classes, nrow = 3)
  
  ggplot( na.omit( total.dens.dia.classes), aes( total, fill = DIA_classes))+geom_histogram()+facet_wrap(~level3s)+theme_bw()+xlab("Average Tree Density (stems/ha)")
  ggplot(  total.dens.dia.classes, aes( x = total, fill = DIA_classes, alpha = 0.5))+geom_density()+facet_wrap(~level3s)
  
  
  ggplot(  data = density_taxon_cell.dia_all, aes( x = total, fill = DIA_classes, alpha = 0.5))+geom_density()
  ggplot(  data = density_taxon_cell.dia_all, aes( x = total, fill = DIA_classes))+geom_histogram()+facet_wrap(~DIA_classes, nrow = 3)+theme_bw()+xlab("Average Tree Density (stems/ha)")
  
  
  density_taxon_cell.dia_all$DIA_classes <- factor(density_taxon_cell.dia_all$DIA_classes, levels = c("1-4 in", "4-8 in", ">8 in"))
  total.only.hist <- ggplot(  data = unique(density_taxon_cell.dia_all[,c("cell", "x", "y", "total")]), aes( x = total))+geom_histogram()+theme_bw()+xlab("Average Tree Density (stems/ha)")
  total.by.dbh.class.hist <-ggplot(  data = unique(density_taxon_cell.dia_all[,c("cell", "x", "y", "totaldia", "DIA_classes")]), aes( x = totaldia, fill = DIA_classes))+geom_histogram()+theme_bw()+
    facet_wrap(~DIA_classes, nrow = 3)+xlab("Average Tree Density (stems/ha)")
  
  png(height = 4, width = 5, units = "in", res = 200, "outputs/totalFIA.by.dbh.class.hist_1in_cutoff.png")
  total.by.dbh.class.hist
  dev.off()
  
  png(height = 4, width = 5, units = "in", res = 200, "outputs/FIAdensity.hist_1in_cutoff.png")
  total.only.hist
  dev.off()
  
  
  write.csv(density_taxon_cell.dia_all, "outputs/FIA_density_taxon_cell.dia_all.csv")
  # want to make a set of bar plots showing how much of all densities in a grid cell are in each diam class:
  
  # X is density bins from zero to 1500
  # need the amount of tree density that is in each diam class
  density_taxon_cell.dia_all <- density_taxon_cell.dia_all[!density_taxon_cell.dia_all$total == density_taxon_cell.dia_all$avg,]
  
  density_taxon_cell.dia_props <-  density_taxon_cell.dia_all %>% mutate(DensityBins = cut(total, breaks = c(seq(1, 1200, by = 100)))) %>% 
    ungroup() %>% group_by(DIA_classes, DensityBins) %>% 
    summarise(total_dbhclass  = mean(avg)) %>% ungroup()%>%
    group_by(DensityBins) %>%
   mutate(total_density_bins = sum(total_dbhclass)) %>% ungroup() %>%
   group_by(DIA_classes, DensityBins) %>% 
   summarise(prop.dia.class = total_dbhclass /total_density_bins) 
    #group_by( DIA_classes, DensityBins) %>%
    #summarise( prop.dia.class = total_dbhclass /total_density_bins) #%>% ungroup()%>% group_by(DensityBins, DIA_classes) %>% summarise(avg.prop = mean(prop.dia.class))
  
  unique(density_taxon_cell.dia_props[,c("DIA_classes", "DensityBins", "prop.dia.class")])
  #ggplot(  data = density_taxon_cell.dia_all, aes(x=x, y = y, fill = total))+geom_raster()+facet_wrap(~DIA_classes)
  
 dens.bins.df <-  data.frame(DensityBins = unique(density_taxon_cell.dia_props$DensityBins), 
             Density = c("0 - 100", "101 - 200", "201 - 300", "301 - 400", "401- 500",
                         "501-600", "501 - 700", "701 - 800", "801 - 900", "901 - 1000", "1001 - 1100", ">1100"))
  demsity.dia.props <- left_join(dens.bins.df, density_taxon_cell.dia_props)
  
  demsity.dia.props$Density <- factor(demsity.dia.props$Density, levels =  c("0 - 100", "101 - 200", "201 - 300", "301 - 400", "401- 500",
                                                                             "501-600", "501 - 700", "701 - 800", "801 - 900", "901 - 1000", "1001 - 1100", ">1100"))
  prop.figure <- ggplot(demsity.dia.props, aes(x = Density, y = prop.dia.class, fill = DIA_classes))+
    geom_bar(stat = "identity")+theme_bw()+theme(axis.text.x = element_text(hjust = 1, angle = 45))+xlab("Density (stems/ha)")+ylab("Proportion of Density made up by each Diameter Class")
  
  png(height = 4, width = 5, units = "in", res = 200, "outputs/Proportion_diameter_class_FIA_1in_cutoff.png")
  prop.figure 
  dev.off()
  
  FIA.by.paleon <- dcast(fia.grid, x + y+ cell + plt_cn ~ level3a, sum, na.rm=TRUE, value.var = 'density') #sum all species in common taxa in FIA grid cells
  FIA.by.paleon$FIAdensity <- rowSums(FIA.by.paleon[,6:35], na.rm = TRUE) # sum the total density in each plot
  fia.melt <- melt(FIA.by.paleon, id.vars = c('x', 'y', 'cell', 'plt_cn')) # melt the dataframe
  fia.by.cell <- dcast(fia.melt, x + y+ cell ~ variable, mean, na.rm=TRUE, value.var = 'value') # average species densities and total density within each grid cell
  
  density.FIA.table <- fia.by.cell
  
  lowdens <- FIA.by.paleon[FIA.by.paleon$FIAdensity < 47,]
  summary(lowdens) # note there seems to be alot of density grid cells at 44.6...
  low.densm <- melt(lowdens, id.vars = c("x", "y", "cell", "plt_cn", "Var.5"))
  
  ggplot(low.densm, aes(value))+geom_histogram()+facet_wrap(~variable)
  summary(density.FIA.table$FIAdensity)
  hist(density.FIA.table$FIAdensity, breaks = 100)
  
 