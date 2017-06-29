# finding the overall environmental space where the bimodality occures
# the script uses output from density_species_analysis.R 

# Objective: calculate bimodality on a rolling bin basis and narrow in on the environmental space where data is signifcantly bimodal

full <- read.csv("outputs/cluster/full_comp_dens_df.csv")

pls.full <- full[full$period %in% "PLS",]

#rolling BC Function (this only works if the data are ordered by the environment)


rollBC_r = function(x, y, xout, width) { # x and y are the environment val and the density/comp that we want to determin bimodality with
  
  out = numeric(length(xout))
  
  for( i in seq_along(xout) ){
    window = x >= (xout[i]-width) & x <= (xout[i]+width)
    out[i] = bimodality_coefficient( na.omit(y[ window ] ) )# what is the BC for places with less than 300 trees per hectare
  }
  
  ggplot()+geom_point(aes(x = xout, y = out))+
    geom_hline( yintercept = 5/9)+ylim(0,1)+theme_bw()+
    xlab('interval center') + ylab('Bimodality Coefficient') +ggtitle(paste0( 'Bimodality coefficient for binwidth = ', width))
  
}



ordered <- pls.full[order(pls.full$PC1),]
ordered$rownum <- 1:length(ordered$PC1)

rollBC_r(x = ordered$PC1, y = ordered$Density, xout = ordered$PC1, width = 1)

ordered <- pls.full[order(pls.full$PC1),]
ordered$rownum <- 1:length(ordered$PC1)

rollBC_r(x = ordered$PC1, y = ordered$pc2, xout = ordered$PC1, width = 0.5)
