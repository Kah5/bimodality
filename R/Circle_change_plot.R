# creating a fun visualization
library(migest)
library(circlize)

detach(package:plyr)  # summarise function doesnt work well with plyr
demo(cfplot_reg, package = "migest", ask = FALSE)
# basics come from this tutorial: https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html#basic-usage-1

# 
veg.data <- read.csv("data/PLS_FIA_density_climate_full.csv")

head(veg.data)
veg.data$start <- ifelse(veg.data$PLSdensity <= 0.5, "Prairie", ifelse(veg.data$PLSdensity <= 47, "Savanna", "Forest"))
veg.data$end <- ifelse(veg.data$FIAdensity <= 0.5, "Prairie", ifelse(veg.data$FIAdensity <= 47, "Savanna", ifelse(veg.data$FIAdensity >= 47,"Forest","Agriculture/Urban"))) # if na assume agricultural
veg.data[is.na(veg.data$FIAdensity),]$end <- "Agriculture/Urban"

# create a tibble table with the modern vegtype as columns and the past as rows:
veg.tibble <- veg.data[,c("start", "end")] %>% group_by(start) %>% summarise("Agriculture/Urban" = sum(end %in% "Agriculture/Urban"), "Forest" = sum(end %in% "Forest"), "Savanna" = sum(end %in% "Savanna"), "Prairie" = sum(end %in% "Prairie"))
veg.df <- data.frame(veg.tibble)
rownames(veg.df) <- veg.df$start # rename

as.matrix(veg.df[,2:5])

# assign colors to each vegtype:
grid.col = c(Forest = "forestgreen", Savanna = "brown", Prairie = "yellow",
             Agriculture.Urban = "grey")

# make the basic chord diagram for structure, with directionality
chordDiagram(as.matrix(veg.df[,2:5]) , grid.col, transparency = 0.4, annotationTrack = "grid", 
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(as.matrix(veg.df[,2:5])))))),
             directional = 1, diffHeight = uh(2, "mm"))

# add labesl to chord diagram:
circos.track(track.index = 1, panel.fun = function(x, y) {
       circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
                                    facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
   }, bg.border = NA) # here set bg.border to NA is important

# output to png
png(height = 5, width = 5, units = "in", res = 300,"outputs/circle_migration_density_class.png")
chordDiagram(as.matrix(veg.df[,2:5]) , grid.col, transparency = 0.4, annotationTrack = "grid", 
            preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(as.matrix(veg.df[,2:5])))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA) # here set bg.border to NA is important

dev.off()


# -------------------same figure, but with arrows to delineate directions:
# assign colors to each vegtype:
grid.col.dens = c(Forest = "forestgreen", Savanna = "brown", Prairie = "yellow",
             Agriculture.Urban = "grey")

# assign the links to have arrows:
arr.col.dens = data.frame(c("Prairie", "Forest", "Savanna", "Savanna"), c("Agriculture.Urban", "Agriculture.Urban", "Agriculture.Urban", "Forest"), 
                     c("grey12", "grey12", "grey12", "darkolivegreen"))
# make the basic chord diagram for structure, with directionality
chordDiagram(as.matrix(veg.df[,2:5]) , grid.col.dens, transparency = 0.4, #annotationTrack = "grid", 
             #preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(as.matrix(veg.df[,2:5])))))),
             directional = 1,direction.type = c("diffHeight","arrows") , diffHeight = uh(2, "mm"), link.arr.col = arr.col.dens, link.arr.length = 0.2)

chordDiagram(as.matrix(veg.df[,2:5]) , grid.col, transparency = 0.4, #annotationTrack = "grid", 
             preAllocateTracks = 3,
             directional = 1, direction.type = c("diffHeight","arrows") , diffHeight = uh(2, "mm"), link.arr.col = arr.col, link.arr.length = 0.2, bg.border = NA)


# add labesl to chord diagram:

# output to png
png(height = 5, width = 5, units = "in", res = 300,"outputs/circle_migration_density_class_directional.png")
chordDiagram(as.matrix(veg.df[,2:5]) , grid.col.dens, transparency = 0.4, annotationTrack = "grid", 
             preAllocateTracks = list(track.height = 0.2), directional = 1,direction.type = c("diffHeight","arrows") , diffHeight = uh(2, "mm"), link.arr.col = arr.col.dens, link.arr.length = 0.2)

circos.track(track.index = 2, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "bending.inside", niceFacing = TRUE, cex = 0.8,  adj = c(0.5, 0))#, adj = c(0, 0.5))
}, bg.border = NA) # here set bg.border to NA is important
# here set bg.border to NA is important

dev.off()



# -----------------------------next make these figures for changes in composition + density:

#->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>Combining vegetataion<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#read in specices composition:
clust_plot7 <- read.csv("outputs/seven_clust_pls_dissimilarity.csv")
clust_plot7$foresttype<- plyr::revalue(clust_plot7$speciescluster, c("Poplar"="Poplar", "Elm/Maple/Hickory/Oak/Beech"="Oak-Hickory", "Oak" = "Oak", 
                                                       "Hemlock/Beech/Cedar/Birch/Maple" = "N. Mixed Forest", 
                                                       "Pine/Tamarack/Poplar" = "Pine", "Tamarack/Spruce/Birch/Pine/Poplar" = "Boreal/Sub-boreal", "Beech/Maple/Hemlock" = "Beech-Maple"))

clust_plot7$pastforesttype<- factor(clust_plot7$foresttype, c("Oak", "Pine", "Poplar", "N. Mixed Forest", "Boreal/Sub-boreal", "Oak-Hickory", "Beech-Maple"))

# merge the clusters and pls density data: 
clust_7 <- merge(clust_plot7[,c("x", "y", "cell", "pastforesttype")], veg.data, by = c("x", "y", "cell"))

# read in species composition from FIA data:
species.clust <- read.csv("outputs/cluster/density_fia_with_clusters.csv")

species.clust$modforesttype <- plyr::revalue(species.clust$speciescluster,c("Oak/Maple" = "Oak/Maple", 
                                                                         "Maple/Ash/Birch/Aspen" = "Mixed Hardwoods",
                                                                         
                                                                         "Pine/Poplar"="Pine", "Aspen" = "Poplar"))


clust.data <- merge(species.clust[,c("x", "y", "cell", "modforesttype")], clust_7, by = c("x", "y", "cell"))



#->>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>   Making cluster circles  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
detach(package:plyr)
clust.tibble <- clust.data[,c("pastforesttype", "modforesttype")] %>% group_by(pastforesttype) %>% summarise("Mixed Hardwoods" = sum(modforesttype %in% "Mixed Hardwoods"), "Oak/Maple" = sum(modforesttype %in% "Oak/Maple"), "Pine" = sum(modforesttype %in% "Pine"), "Poplar" = sum(modforesttype %in% "Poplar"))
clust.df <- data.frame(clust.tibble)
rownames(clust.df) <- clust.df$pastforesttype # rename

clust.df <- as.matrix(clust.df[,2:5])
 clust.df<- clust.df[c(3,1,6,4,7,5,2),c(3,1,4,2)]


# assign colors to each vegtype:
 
colors <- c("Oak", "Pine", "Poplar", "N. Mixed Forest", "Boreal/Sub-boreal", "Oak-Hickory", "Beech-Maple", "Oak.Maple", "Mixed.Hardwoods")
 values <- c('#386cb0', '#f0027f','#fdc086','#ffff99','#7fc97f','#beaed4', '#bf5b17', '#35978f','#a6cee3') #,'#4daf4a','#984ea3','#ff7f00')
#grid.col = c("Oak" = "#386cb0", "Pine" = "brown", "Aspen" = "yellow", "N. Mixed Forest" = ""
 #            Agriculture.Urban = "grey")
grid.col <- c( values)
names(grid.col) <- colors
# make the basic chord diagram for structure, with directionality
chordDiagram(clust.df , grid.col, transparency = 0.4, annotationTrack = "grid", 
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(as.matrix(veg.df[,2:5])))))),
             directional = 1, diffHeight = uh(2, "mm"))

# add labesl to chord diagram:
circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
}, bg.border = NA) # here set bg.border to NA is important

# output to png
  png(height = 5, width = 5, units = "in", res = 300,"outputs/circle_migration_composition_class.png")
  chordDiagram(clust.df , grid.col, transparency = 0.4, annotationTrack = "grid", 
               preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(as.matrix(veg.df[,2:5])))))),
               directional = 1, diffHeight = uh(4, "mm"))
  
  # add labesl to chord diagram:
  circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
                facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  }, bg.border = NA) # here set bg.border to NA is important
  
  dev.off()

  
# assign arrows:

  png(height = 5, width = 5, units = "in", res = 300,"outputs/circle_migration_composition_class.png")
  # assign the links to have arrows:
  arr.col = data.frame(c("N. Mixed Forest", "Beech-Maple", "Boreal/Sub-boreal", "Oak-Hickory", "Oak", "Oak", "Boreal/Sub-Boreal"), c("Mixed.Hardwoods", "Mixed.Hardwoods", "Mixed.Hardwoods", "Mixed.Hardwoods", "Mixed.Hardwoods", "Oak-Maple", "Poplar"), 
                       c("grey12", "grey12", "grey12", "grey12", "grey12", "grey12", "grey12"))
  
  chordDiagram(clust.df , grid.col, transparency = 0.4, annotationTrack = "grid", 
               preAllocateTracks = list(track.height = 0.05), directional = 1,direction.type = c("diffHeight","arrows") , diffHeight = uh(3, "mm"), link.arr.col = arr.col, link.arr.length = 0.2)
  
  circos.track(track.index = 2, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
                facing = "bending.inside", niceFacing = TRUE, cex = 0.7,  adj = c(0.5, 0))#, adj = c(0, 0.5))
  }, bg.border = NA) # here set bg.border to NA is important
  # here set bg.border to NA is important
  dev.off()
  
  
# now plot both composition and density change plots together + annotate:

png(height = 4, width = 8, units = "in", res = 300, "outputs/paper_figs/circle_change_plots.png")
        par(mfrow=c(1,2))
        
       
        
        
        # PLS structure
        chordDiagram(as.matrix(veg.df[,2:5]) , grid.col.dens, transparency = 0.4, annotationTrack = "grid", 
                     preAllocateTracks = list(track.height = 0.05), directional = 1,direction.type = c("diffHeight","arrows") , diffHeight = uh(2, "mm"), link.arr.col = arr.col.dens, link.arr.length = 0.2)
        
        circos.track(track.index = 2, panel.fun = function(x, y) {
          circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
                      facing = "bending.inside", niceFacing = TRUE, cex = 0.8,  adj = c(0.5, 0))#, adj = c(0, 0.5))
        }, bg.border = NA) # here set bg.border to NA is important
        # here set bg.border to NA is important
        mtext(text = "A",  side = 3, adj = 0.05 )
        arr.col = data.frame(c("N. Mixed Forest", "Beech-Maple", "Boreal/Sub-boreal", "Oak-Hickory", "Oak", "Oak", "Boreal/Sub-Boreal"), c("Mixed.Hardwoods", "Mixed.Hardwoods", "Mixed.Hardwoods", "Mixed.Hardwoods", "Mixed.Hardwoods", "Oak-Maple", "Poplar"), 
                             c("grey12", "grey12", "grey12", "grey12", "grey12", "grey12", "grey12"))
        
        chordDiagram(clust.df , grid.col, transparency = 0.4, annotationTrack = "grid", 
                     preAllocateTracks = list(track.height = 0.05), directional = 1,direction.type = c("diffHeight","arrows") , diffHeight = uh(3, "mm"), link.arr.col = arr.col, link.arr.length = 0.2)
        
        circos.track(track.index = 2, panel.fun = function(x, y) {
          circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
                      facing = "bending.inside", niceFacing = TRUE, cex = 0.7,  adj = c(0.5, 0))#, adj = c(0, 0.5))
        }, bg.border = NA)
        mtext(text = "B",  side = 3, adj = 0.05 )
        
dev.off()