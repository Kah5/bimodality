# finding the overall environmental space where the bimodality occures
# the script uses output from density_species_analysis.R 
library(modes)
library(ggplot2)
library(diptest)
# Objective: calculate bimodality on a rolling bin basis and narrow in on the environmental space where data is signifcantly bimodal

full <- read.csv("outputs/cluster/full_comp_dens_df.csv")

pls.full <- full[full$period %in% "PLS",]

#rolling BC Function (this only works if the data are ordered by the environment)


rollBC_r = function(x, y, xout, width) { # x and y are the environment val and the density/comp that we want to determin bimodality with
  
  out <- data.frame(xout = xout,
                   bc = NA,
                   pval = NA)
  
  for( i in seq_along(xout) ){
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i,]$bc <-  bimodality_coefficient( na.omit(y[ window ] ) )# what is the BC for places with less than 300 trees per hectare
    #out[i,]$pval <- diptest::dip.test(na.omit(density(na.omit(y[window]))$y))$p
    }
  #out$bimodal <- ifelse(out$bc >= 0.55 & out$pval <= 0.05, "Bimodal", "Stable")
  
  ggplot(out, aes(x = xout, y = bc))+geom_point()+
    geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
    xlab('interval center') + ylab('Bimodality Coefficient') +ggtitle(paste0( 'Bimodality coefficient for binwidth = ', width))
  
}



ordered <- pls.full[order(pls.full$PC1),]
ordered$rownum <- 1:length(ordered$PC1)

rollBC_r(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 1)
rollBC_r(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 0.5)
rollBC_r(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 0.25)

ordered <- pls.full[order(pls.full$PC1),]
ordered$rownum <- 1:length(ordered$PC1)

rollBC_r(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 1)
rollBC_r(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.5)
rollBC_r(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.25)




# make a function that maps out the bimodal area:

rollBC_map = function(x, y, xout, width, df) { # x and y are the environment val and the density/comp that we want to determin bimodality with
  
  out <- data.frame(xout = xout,
                    bc = NA,
                    pval = NA)
  
  for( i in seq_along(xout) ){
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i,]$bc <-  bimodality_coefficient( na.omit(y[ window ] ) )# what is the BC for places with less than 300 trees per hectare
    #out[i,]$pval <- diptest::dip.test(na.omit(density(na.omit(y[window]))$y))$p
  }
   df2 <- merge(df, out, by.x = "PC1", by.y = "xout")
   df2$bimodal <- ifelse(df2$bc >= 0.55, "Bimodal", "Stable")
   
  ggplot(df2, aes(x = x, y = y, fill = bimodal ))+geom_raster()
}

rollBC_map(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 1, df = ordered)
rollBC_map(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.5, df = ordered)
rollBC_map(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.25, df = ordered)
rollBC_map(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.25, df = ordered)

# density: 
rollBC_map(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 1, df = ordered)
rollBC_map(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 0.5, df = ordered)
rollBC_map(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 0.25, df = ordered)
rollBC_map(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 0.25, df = ordered)
