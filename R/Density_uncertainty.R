# uncertainty around grid cell estimates of tree density:
# want to sample randomly from point level estimates within each grid cell:

# get all points in the grid cell:
# sample randomly from the distribution: 
# get the sd and the mean from that random sampling?

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


#-----------------------------Load PLS data--------------------------------------

# read in pont level data
pls.inil <- read.csv(paste0('outputs/biomass_no_na_pointwise.ests_inilmi_v',version, '.csv'))


# Option 1: finding mean and sd of each grid cell
pls.mean <- dcast(pls.inil, x + y + cell ~., mean, na.rm = TRUE, value.var = 'density') # we want to sum the densities of all the species in each cells, then divide by the # of pls points within the cell, so take the avg 
pls.sd <- dcast(pls.inil, x + y + cell ~., sd, na.rm = TRUE, value.var = 'density') # we want to sum the densities of all the species in each cells, then divide by the # of pls points within the cell, so take the avg 

colnames(pls.mean) <- c('x', 'y', 'cell','PLSdensity')
colnames(pls.sd) <- c('x', 'y', 'cell','density_sd')
hist(pls.mean$PLSdensity, xlim = c(0, 600),breaks = 100)

pls <- merge(pls.mean, pls.sd, by = c("x", "y", "cell"))


label.breaks <- function(beg, end, splitby){
  labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
  labels.test <- paste (labels.test$first, '-' , labels.test$second)
  labels.test
}


pls$sd_bins <- cut(pls$density_sd, breaks = seq(-1,400, by = 20), labels = label.breaks(0,380, 20))

ggplot(pls, aes(x,y, fill = sd_bins))+geom_raster()

png("outputs/IN_IL_dens_histogram_by_sd.png")
ggplot(pls, aes(PLSdensity, fill = sd_bins))+geom_histogram(position = "stack")+theme_bw()
dev.off()

# Option 2: Bootstrapping mean and 95% CI of the data in each grid cell:
library(boot)

func.mean <- function(d, i){
  d2 <- d[i,]
  return(mean(d2$density, na.rm=TRUE))
}

bootcorr <- boot(pls.inil[pls.inil$cell %in% 40599,], func.mean, R=500)
bootcorr

# compare to regular mean:
mean.dens <- mean(pls.inil[pls.inil$cell %in% 40599,]$density, na.rm = TRUE)

boot.ci(bootcorr, type = "bca")

# compare to regular 
sd.dens <- sd(pls.inil[pls.inil$cell %in% 40599,]$density, na.rm = TRUE)

mean.dens + sd.dens
mean.dens - sd.dens

pls.inil2 <- pls.inil[1:100,c("x", "y", "cell", "density")]

boot.calcs <- function(x){
        func.mean <- function(d, indices){
          d2 <- d[indices]
          return(mean(d2, na.rm=TRUE))
        }
     
         bootcorr <- boot(x, stat = func.mean, R=500)
     
      
      # compare to regular means
      
      bootci <- boot.ci(bootcorr, type = "bca")
      out <- data.frame(mean =  bootcorr$t0,
                        ci.low = bootci$bca[4], 
                        ci.high = bootci$bca[5])

      out
  }

test<- pls.inil[1:500,]
as.list(test)

# create a list of densities by each cell:
dens.by.cells <- split(pls.inil$density, pls.inil$cell)

# apply the "boot.calcs" function over all cells:
dens.ci.mean <- lapply( dens.by.cells, FUN = boot.calcs)

dens.ci.mean.df <- do.call(rbind, dens.ci.mean)
