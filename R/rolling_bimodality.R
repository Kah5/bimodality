# finding the overall environmental space where the bimodality occures
# the script uses output from density_species_analysis.R 
library(modes)
library(ggplot2)
library(diptest)
library(grid)
library(gridExtra)
library(cowplot)
# Objective: calculate bimodality on a rolling bin basis and narrow in on the environmental space where data is signifcantly bimodal

full <- read.csv("outputs/cluster/full_comp_dens_df.csv")

pls.full <- full[full$period %in% "PLS",]

#rolling BC Function (this only works if the data are ordered by the environment)


rollBC_r = function(x, y, xout, width, df) { # x and y are the environment val and the density/comp that we want to determin bimodality with
  
  out <- data.frame(xout = xout,
                    bc = NA,
                    pval = NA,
                    n = NA)
  
  for( i in seq_along(xout) ){
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i,]$bc <-  bimodality_coefficient( na.omit(y[ window ] ) )# what is the BC for places with less than 300 trees per hectare
    out[i,]$pval <- ifelse(length(na.omit(y[window])) > 2,diptest::dip.test(na.omit(density(na.omit(y[window]))$y))$p, NA)
    out[i,]$n <- length(na.omit( y[ window ]))
  }
  
  df2 <- merge(df, out, by.x = "PC1", by.y = "xout")
  df2$bimodal <- ifelse(df2$bc >= 0.55 & df2$pval < 0.05, "Bimodal", "Stable")
  bim2 <- ifelse(y == ordered$Density, "Density", "species pc2")
  
  ggplot(df2, aes(x= PC1, y = n, color = bimodal))+geom_point()+ggtitle(paste0(bim2, " samples binwidth = ", width))
}



ordered <- pls.full[order(pls.full$PC1),]
ordered$rownum <- 1:length(ordered$PC1)

a<- rollBC_r(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 1, df = ordered)
b<- rollBC_r(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 0.5, df = ordered)
c<- rollBC_r(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 0.25, df= ordered)
d<- rollBC_r(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 0.15, df= ordered)

#ordered <- pls.full[order(pls.full$PC1),]
#ordered$rownum <- 1:length(ordered$PC1)

e <- rollBC_r(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 1, df = ordered)
f <- rollBC_r(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.5, df = ordered)
g <-rollBC_r(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.25, df = ordered)
h <-rollBC_r(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.15, df = ordered)

png(height = 12, width = 7, units = "in", res = 200, "outputs/cluster/nsamples_rolling_bimodality.png")
grid.arrange(a,e,b,f,c,g,d,h, ncol = 2, nrow=4)
dev.off()


# make a function that maps out the bimodal area:

rollBC_map = function(x, y, xout, width, df) { # x and y are the environment val and the density/comp that we want to determin bimodality with
  
  out <- data.frame(xout = xout,
                    bc = NA,
                    pval = NA,
                    n = NA)
  
  for( i in seq_along(xout) ){
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i,]$bc <-  bimodality_coefficient( na.omit(y[ window ] ) )# what is the BC for places with less than 300 trees per hectare
    out[i,]$pval <- ifelse(length(na.omit(y[window])) > 2,diptest::dip.test(na.omit(density(na.omit(y[window]))$y))$p, NA)
    out[i,]$n <- length(na.omit( y[ window ]))
  }
  
   df2 <- merge(df, out, by.x = "PC1", by.y = "xout")
   df2$bimodal <- ifelse(df2$bc >= 0.55 & df2$pval < 0.05, "Bimodal", "Stable")
   bim2 <- ifelse(y == ordered$Density, "Density", "species pc2")
  
   bim <-ggplot(df2, aes(x = x, y = y, fill = bimodal ))+geom_raster()+coord_equal()+theme()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                                    axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                                    axis.title.x=element_blank(),
                                                                                                    axis.title.y=element_blank())+ggtitle(paste0("bimodality ", bim2, " width =", width))
   samp <- ggplot(df2, aes(x = x, y = y, fill = n ))+geom_raster()+coord_equal()+ theme(axis.line=element_blank(),axis.text.x=element_blank(),
                                                                                        axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                                                        axis.title.x=element_blank(),
                                                                                        axis.title.y=element_blank())+ggtitle(paste0("N samples ", bim2, " width =", width))
   #samp2 <- ggplot(df2, aes(x= PC1, y = n, color = bimodal))+geom_point()
    plot_grid(bim, samp, ncol = 2, nrow = 1, align = 'hv')
   }

a <- rollBC_map(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 1, df = ordered)
b <- rollBC_map(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.5, df = ordered)
c <- rollBC_map(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.25, df = ordered)
d <- rollBC_map(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.15, df = ordered)

# density: 
e <- rollBC_map(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 1, df = ordered)
f <- rollBC_map(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 0.5, df = ordered)
g <- rollBC_map(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 0.25, df = ordered)
h <- rollBC_map(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 0.15, df = ordered)



png(width = 8, height = 11, units = "in", res = 300, "outputs/cluster/rolling_bimodal_maps_species_pc2_pls.png")
grid.arrange(a,b,c,d, nrow=4, ncol = 1)
dev.off()

png(width = 8, height = 11, units = "in", res = 300, "outputs/cluster/rolling_bimodal_maps_density_pls.png")
grid.arrange(e,f,g,h, nrow=4, ncol = 1)
dev.off()


# the previous functions were for the purpuse of visualizing the datasets, now lets output the df:
