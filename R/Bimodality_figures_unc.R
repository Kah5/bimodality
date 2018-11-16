# make figures for the paper & coduct bimodality analyses:
# migrated some of this code from bimodality_draft_3.1.17

library(gridExtra)
library(grid)
library(ggplot2)
library(maps)
library(sp)
library(plyr)
library(maps)
library(cowplot)
library(dplyr)
library(tidyr)
library(ks)
library(ggplotify)

# read in old PLS density + climate data:
dens.pr <- read.csv("data/PLS_FIA_density_climate_full.csv")

# read in draws of total PLS density:
total.m <- read.csv("data/extracted_total_PLS_density_draws.csv")

dens.summary <- total.m %>% group_by(x, y) %>% dplyr::summarize(mean_dens = mean(value, na.rm=TRUE),
                                                                media_dens = median(value, na.rm=TRUE),
                                                         ci.low_dens = quantile(value, 0.025, na.rm=TRUE), 
                                                         ci.high_dens = quantile(value, 0.975, na.rm=TRUE))

ggplot(dens.summary, aes(x,y, fill =  media_dens))+geom_raster()+ scale_fill_distiller(palette = "Spectral")

dens <- merge(dens.pr, dens.summary, by = c("x", "y"), all.y = TRUE)

dens <- dens[!is.na(dens$mean_dens), ]

ggplot(dens[dens$mean_dens <= 0.5,], aes(x,y, fill =  mean_dens))+geom_raster()+ scale_fill_distiller(palette = "Spectral")
write.csv(dens, "outputs/density_full_unc.csv", row.names = FALSE)
# -------------------figure 1 A: Map of pls bimodality

# need to set up state outlines:
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota', 'wisconsin', 'michigan', "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)



# fia and pls plots
sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens, aes(x=x, y=y, fill = mean_dens))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,775), name ="Tree \n Density", na.value = 'darkgrey') +
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                                legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                                
                                                axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png("/Users/kah/Documents/bimodality/outputs/paper_figs_unc/PLS_density_map_full.png")
pls.map
dev.off()



dens$density_discrete <- ifelse(dens$mean_dens <= 0.5, "Prairie", 
                                   ifelse(dens$mean_dens <= 47, "Savanna",
                                          ifelse(dens$mean_dens > 47 & dens$mean_dens <= 100, "47-100",
                                                 ifelse(dens$mean_dens > 100 & dens$mean_dens <= 200, "100-200", 
                                                        ifelse(dens$mean_dens > 200 & dens$mean_dens <= 300, "200-300", 
                                                               ifelse(dens$mean_dens > 300 & dens$mean_dens <= 400, "300-400",
                                                                      ifelse(dens$mean_dens > 400 & dens$mean_dens <= 500, "400-500",
                                                                             ifelse(dens$mean_dens > 500 , "500+", "No data"))))))))

dens$density_discrete<- factor(dens$density_discrete, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))




pls.map.alt.color <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens, aes(x=x, y=y, fill = density_discrete))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_manual(values = c('#dfc27d',
    '#8c510a',
    '#d9f0a3',
    '#addd8e',
    '#78c679',
    '#41ab5d',
    '#238443',
    '#005a32',"darkgrey"), name ="Tree Density", na.value = 'darkgrey', drop = F) +
  
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.3, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png(height = 4, width = 3, units = "in", res = 300,"/Users/kah/Documents/bimodality/outputs/paper_figs_unc/PLS_smooth_density_map_full_alt_colors.png")
pls.map.alt.color+ggtitle("PLS mean density smoothed")
dev.off()

# plot median density

dens$density_discrete_median <- ifelse(dens$media_dens <= 0.5, "Prairie", 
                                ifelse(dens$media_dens <= 47, "Savanna",
                                       ifelse(dens$media_dens > 47 & dens$media_dens <= 100, "47-100",
                                              ifelse(dens$media_dens > 100 & dens$media_dens <= 200, "100-200", 
                                                     ifelse(dens$media_dens > 200 & dens$media_dens <= 300, "200-300", 
                                                            ifelse(dens$media_dens > 300 & dens$media_dens <= 400, "300-400",
                                                                   ifelse(dens$media_dens > 400 & dens$media_dens <= 500, "400-500",
                                                                          ifelse(dens$media_dens > 500 , "500+", "No data"))))))))

dens$density_discrete_median<- factor(dens$density_discrete_median, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))

dens$density_discrete_low <- ifelse(dens$ci.low_dens <= 0.5, "Prairie", 
                                       ifelse(dens$ci.low_dens <= 47, "Savanna",
                                              ifelse(dens$ci.low_dens > 47 & dens$ci.low_dens <= 100, "47-100",
                                                     ifelse(dens$ci.low_dens > 100 & dens$ci.low_dens <= 200, "100-200", 
                                                            ifelse(dens$ci.low_dens > 200 & dens$ci.low_dens <= 300, "200-300", 
                                                                   ifelse(dens$ci.low_dens > 300 & dens$ci.low_dens <= 400, "300-400",
                                                                          ifelse(dens$ci.low_dens > 400 & dens$ci.low_dens <= 500, "400-500",
                                                                                 ifelse(dens$ci.low_dens > 500 , "500+", "No data"))))))))

dens$density_discrete_low <- factor(dens$density_discrete_low, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))




pls.map.alt.color.low <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens, aes(x=x, y=y, fill =density_discrete_low))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_manual(values = c('#dfc27d',
                               '#8c510a',
                               '#d9f0a3',
                               '#addd8e',
                               '#78c679',
                               '#41ab5d',
                               '#238443',
                               '#005a32',"darkgrey"), name ="Tree Density", na.value = 'darkgrey', drop = F) +
  
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.3, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png(height = 4, width = 3, units = "in", res = 300,"/Users/kah/Documents/bimodality/outputs/paper_figs_unc/PLS_smooth_density_map_full_alt_colors.png")
pls.map.alt.color+ggtitle("PLS mean density smoothed")
dev.off()


ggplot()+geom_errorbar(data= dens, aes(ymin = ci.low_dens, ymax = ci.high_dens), color = "grey")+
  geom_point(data = dens, aes(PLSdensity, mean_dens), size = 0.2)#+geom_abline(intercept = 0, slope = 1, color = "red")+theme_bw()


pred.old.plot <- ggplot(dens, aes(PLSdensity, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(PLSdensity, mean_dens), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+theme_bw()+ylab("Smoothed Density")+xlab("Previous grid cell density")

png(height = 5, width = 5, units = "in", res = 200, "outputs/chris_estimates_vs_previous_estimates.png")
pred.old.plot
dev.off()



pc1_unc <- ggplot(dens, aes(PC1, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(PC1, mean_dens), color = "black", size = 0.05)+theme_bw()+ylab("PLS Density (trees/ha)")+xlab("PC1")

ppet_unc <- ggplot(dens, aes(GS_ppet, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(GS_ppet, mean_dens), color = "black", size = 0.05)+theme_bw()+ylab("PLS Density (trees/ha)")+xlab("growing season P-PET")


soil_unc <- ggplot(dens, aes(mean_GS_soil, mean_dens))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens, ymax=ci.high_dens), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(mean_GS_soil, mean_dens), color = "black", size = 0.05)+theme_bw()+ylab("PLS Density (trees/ha)")+xlab("Growing season soil moisture")


# save plots of environment vs density estimates + CI:
png(height = 10, width = 4, units = "in", res = 300, "outputs/paper_figs_unc/smooth_dens_vs_envts.png")
cowplot::plot_grid(pc1_unc, ppet_unc, soil_unc, ncol = 1)
dev.off()

ggplot()+geom_histogram(data = dens, aes(mean_dens), fill = "red")+geom_histogram(data= dens, aes(ci.high_dens), aes = 2, color = "grey")+geom_histogram(data= dens, aes(ci.low_dens), alpha = 0.2, fill = "blue")



# <<<<<<<<<<<<<<< map bimodality evaluated on samples from  esimates: >>>>>>>>>>>>>>>>>>>>>>>>>>>>

# map bimodal based on PC1:
out.df <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PC1_stat.rds")
pvalues <- out.df  %>% group_by(pc1_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                              median.p = median(pvalue, na.rm = TRUE),
                                                              ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                              ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                              mean.d = mean(dip, na.rm = TRUE),
                                                              median.d = median(dip, na.rm = TRUE),
                                                              ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                              ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


dens$pc1_bins <- cut(dens$PC1, breaks=seq(-5.5, 4.5, by = 0.25))

# also make density bins:
dens$dens.bins <- cut(bimod.pc.pls$mean_dens, breaks=seq(0, 775, by = 25))
ordered.bins <- data.frame(dens.bins = unique(cut(dens[order(dens$mean_dens),]$mean_dens, breaks=seq(0, 775, by = 25))),
                           mids = seq(0, 775, by = 25))


hist.summary<- dens %>% group_by(dens.bins) %>% dplyr::summarise(mean = count(mean_dens),
                                                    ci.high = count(ci.high_dens), 
                                                  ci.low =count(ci.low_dens))


ordered.cuts <- data.frame(pc1_bins = unique(cut(pls.df[order(pls.df$PC1),]$PC1, breaks=seq(-5.5, 4.5, by = 0.25))),
                           mids = seq(-5.375, 4.5, by = 0.25))
pvalues <- left_join(ordered.cuts, pvalues, by = "pc1_bins")
bimod.pc.pls <- left_join(dens, pvalues, by = c("pc1_bins"))


# merge with the envt + pc data

bimod.pc.pls$bimclass <- ifelse(bimod.pc.pls$mean.p <= 0.05, "bimodal", "unimodal")

bimod.pc.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc.pls, aes(x=x, y=y, fill = bimclass))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027',"unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")



pc1.dip.pls <- ggplot(pvalues, aes(mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS PC1")+ylab("DIP value")+xlim(-6.4, 4.5)
pc1.pval.pls <- ggplot(pvalues, aes(mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS PC1")+ylab("P value")+xlim(-6.4, 4.5)


png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/pls_dip_pvalues_unc_ppet.png")
plot_grid(pc1.dip.pls, pc1.pval.pls)
dev.off()

# pased on P-PET:

out.df <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PPET_stat.rds")
pvalues <- out.df  %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                               median.p = median(pvalue, na.rm = TRUE),
                                                               ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                               ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                               mean.d = mean(dip, na.rm = TRUE),
                                                               median.d = median(dip, na.rm = TRUE),
                                                               ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                               ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


dens$ppet_bins <- cut(dens$GS_ppet, breaks=seq(-170, 205, by = 15))

ordered.cuts <- data.frame(ppet_bins = unique(cut(pls.df[order(pls.df$GS_ppet),]$GS_ppet, breaks=seq(-170, 205, by = 15))),
                           mids = seq(-167.5, 205, by = 15))
pvalues <- left_join(ordered.cuts, pvalues, by = "ppet_bins")

bimod.ppet.pls <- left_join(dens, pvalues, by = c("ppet_bins"))


# merge with the envt + pc data

bimod.ppet.pls$bimclass_ppet <- ifelse(bimod.ppet.pls$mean.p <= 0.05, "bimodal", "unimodal")
bimod.ppet.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.pls, aes(x=x, y=y, fill = bimclass_ppet))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal" = '#d73027', "unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")



ppet.dip.pls <- ggplot(pvalues, aes(mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS P-PET")+ylab("DIP value")
ppet.pval.pls <- ggplot(pvalues, aes(mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS P-PET")+ylab("P value")

png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/pls_dip_pvalues_unc_ppet.png")
plot_grid(ppet.dip.pls, ppet.pval.pls)
dev.off()

# based on soil moisture estimates:
out.df <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_soil_15bins_kde_stat.rds")

pvalues <- out.df  %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                median.p = median(pvalue, na.rm = TRUE),
                                                                ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                mean.d = mean(dip, na.rm = TRUE),
                                                                median.d = median(dip, na.rm = TRUE),
                                                                ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


dens$soil_bins <- cut(dens$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05))

ordered.cuts <- data.frame(soil_bins = levels(unique(cut(pls.df[order(pls.df$mean_GS_soil),]$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05)))),
                           mids = seq(0.025, 1.8, by = 0.05))
pvalues <- left_join(ordered.cuts, pvalues, by = "soil_bins")

bimod.sm.pls <- left_join(dens, pvalues, by = c("soil_bins"))


# merge with the envt + pc data

bimod.sm.pls$bimclass_soil <- ifelse(bimod.sm.pls$mean.p <= 0.05, "bimodal", "unimodal")



bimod.sm.pls.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.pls, aes(x=x, y=y, fill = bimclass_soil))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")




soil.dip.pls <- ggplot(pvalues, aes(mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS Soil moisture")+ylab("DIP value")
soil.pval.pls <- ggplot(pvalues, aes(mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("PLS Soil moisture")+ylab("P value")


png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/pls_dip_pvalues_unc_ppet.png")
plot_grid(soil.dip.pls, soil.pval.pls)
dev.off()


png(height = 10, width = 10, units = "in", res = 300, "outputs/paper_figs_unc/bimodal_maps_with_pvalues.png")
plot_grid(bimod.pc.pls.map, bimod.ppet.pls.map, bimod.sm.pls.map,
          pc1_unc, ppet_unc, soil_unc,
          pc1.dip.pls + ylim(0,0.1), ppet.dip.pls+ylim(0,0.1), soil.dip.pls+ylim(0,0.1),
          pc1.pval.pls, ppet.pval.pls, soil.pval.pls, 
          ncol = 3, rel_heights = c(1,1,0.5, 0.5))
dev.off()



# -------------------------Read in FIA density statistical estimates-----------------------


total.m <- read.csv("data/extracted_total_FIA_density_draws.csv")

dens.summary.fia <- total.m %>% group_by(x, y) %>% dplyr::summarize(mean_dens_fia = mean(value, na.rm=TRUE),
                                                                media_dens_fia = median(value, na.rm=TRUE),
                                                                ci.low_dens_fia = quantile(value, 0.025, na.rm=TRUE), 
                                                                ci.high_dens_fia = quantile(value, 0.975, na.rm=TRUE))

ggplot(dens.summary.fia, aes(x,y, fill = mean_dens_fia))+geom_raster()+ scale_fill_distiller(palette = "Spectral")

dens <- merge(dens, dens.summary.fia, by = c("x", "y"), all.x = TRUE)

dens <- dens[!is.na(dens.full$mean_dens_fia), ]

ggplot(dens, aes(x,y, fill =  mean_dens_fia))+geom_raster()+ scale_fill_distiller(palette = "Spectral")
write.csv(dens, "outputs/density_full_FIA_PLS_unc.csv", row.names = FALSE)
# -------------------figure 1 A: Map of FIA bimodality

# need to set up state outlines:
all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota', 'wisconsin', 'michigan', "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)



# fia and FIA plots
sc <- scale_colour_gradientn(colours = rev(terrain.colors(8)), limits=c(0, 16))
cbpalette <- c("#ffffcc", "#c2e699", "#78c679", "#31a354", "#006837")
cbPalette <- c("#999999","#009E73", "#E69F00", "#56B4E9",  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


FIA.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens, aes(x=x, y=y, fill = mean_dens_fia))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_gradientn(colours = cbpalette, limits = c(0,775), name ="Tree \n Density", na.value = 'darkgrey') +
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.5, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png("/Users/kah/Documents/bimodality/outputs/paper_figs_unc/FIA_density_map_full.png")
FIA.map
dev.off()



dens$density_discrete <- ifelse(dens$mean_dens_fia <= 0.5, "Prairie", 
                                ifelse(dens$mean_dens_fia <= 47, "Savanna",
                                       ifelse(dens$mean_dens_fia > 47 & dens$mean_dens_fia <= 100, "47-100",
                                              ifelse(dens$mean_dens_fia > 100 & dens$mean_dens_fia <= 200, "100-200", 
                                                     ifelse(dens$mean_dens_fia > 200 & dens$mean_dens_fia <= 300, "200-300", 
                                                            ifelse(dens$mean_dens_fia > 300 & dens$mean_dens_fia <= 400, "300-400",
                                                                   ifelse(dens$mean_dens_fia > 400 & dens$mean_dens_fia <= 500, "400-500",
                                                                          ifelse(dens$mean_dens_fia > 500 , "500+", "No data"))))))))

dens$density_discrete<- factor(dens$density_discrete, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))




FIA.map.alt.color <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens, aes(x=x, y=y, fill = density_discrete))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_manual(values = c('#dfc27d',
                               '#8c510a',
                               '#d9f0a3',
                               '#addd8e',
                               '#78c679',
                               '#41ab5d',
                               '#238443',
                               '#005a32',"darkgrey"), name ="Tree Density", na.value = 'darkgrey', drop = F) +
  
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.3, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png(height = 4, width = 3, units = "in", res = 300,"/Users/kah/Documents/bimodality/outputs/paper_figs_unc/FIA_smooth_density_map_full_alt_colors.png")
FIA.map.alt.color+ggtitle("FIA mean density smoothed")
dev.off()

# plot median density

dens$density_discrete_median <- ifelse(dens$media_dens <= 0.5, "Prairie", 
                                       ifelse(dens$media_dens <= 47, "Savanna",
                                              ifelse(dens$media_dens > 47 & dens$media_dens <= 100, "47-100",
                                                     ifelse(dens$media_dens > 100 & dens$media_dens <= 200, "100-200", 
                                                            ifelse(dens$media_dens > 200 & dens$media_dens <= 300, "200-300", 
                                                                   ifelse(dens$media_dens > 300 & dens$media_dens <= 400, "300-400",
                                                                          ifelse(dens$media_dens > 400 & dens$media_dens <= 500, "400-500",
                                                                                 ifelse(dens$media_dens > 500 , "500+", "No data"))))))))

dens$density_discrete_median<- factor(dens$density_discrete_median, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))

dens$density_discrete_low <- ifelse(dens$ci.low_dens_fia <= 0.5, "Prairie", 
                                    ifelse(dens$ci.low_dens_fia <= 47, "Savanna",
                                           ifelse(dens$ci.low_dens_fia > 47 & dens$ci.low_dens_fia <= 100, "47-100",
                                                  ifelse(dens$ci.low_dens_fia > 100 & dens$ci.low_dens_fia <= 200, "100-200", 
                                                         ifelse(dens$ci.low_dens_fia > 200 & dens$ci.low_dens_fia <= 300, "200-300", 
                                                                ifelse(dens$ci.low_dens_fia > 300 & dens$ci.low_dens_fia <= 400, "300-400",
                                                                       ifelse(dens$ci.low_dens_fia > 400 & dens$ci.low_dens_fia <= 500, "400-500",
                                                                              ifelse(dens$ci.low_dens_fia > 500 , "500+", "No data"))))))))

dens$density_discrete_low <- factor(dens$density_discrete_low, c("Prairie", "Savanna","47-100", "100-200", "200-300", "300-400", "400-500",  "500+", "No data"))




FIA.map.alt.color.low <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens, aes(x=x, y=y, fill =density_discrete_low))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ #+ 
  scale_fill_manual(values = c('#dfc27d',
                               '#8c510a',
                               '#d9f0a3',
                               '#addd8e',
                               '#78c679',
                               '#41ab5d',
                               '#238443',
                               '#005a32',"darkgrey"), name ="Tree Density", na.value = 'darkgrey', drop = F) +
  
  theme_bw(base_size = 8)+ theme(legend.position=c(0.2, 0.25),legend.background = element_rect(fill=alpha('transparent', 0)) ,axis.line=element_blank(),axis.text=element_blank(),
                                 legend.key.size = unit(0.3, "lines"),legend.title = element_text(size = 5),axis.text.y=element_blank(),axis.ticks=element_blank(),
                                 
                                 axis.title=element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ggtitle("")+coord_equal()


png(height = 4, width = 3, units = "in", res = 300,"/Users/kah/Documents/bimodality/outputs/paper_figs_unc/FIA_smooth_density_map_full_alt_colors.png")
FIA.map.alt.color+ggtitle("FIA mean density smoothed")
dev.off()



pred.old.plot <- ggplot(dens, aes(FIAdensity, mean_dens_fia))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens_fia, ymax=ci.high_dens_fia), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(FIAdensity, mean_dens_fia), color = "black", size = 0.5)+
  geom_abline(aes(slope = 1, intercept = 0), color = "red", linetype = "dashed")+theme_bw()+ylab("Smoothed Density")+xlab("Previous grid cell density")

png(height = 5, width = 5, units = "in", res = 200, "outputs/chris_estimates_vs_previous_estimates_FIA.png")
pred.old.plot
dev.off()



pc1_unc_fia <- ggplot(dens, aes(PC1fia, mean_dens_fia))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens_fia, ymax=ci.high_dens_fia), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(PC1fia, mean_dens_fia), color = "black", size = 0.05)+theme_bw()+ylab("FIA Density (trees/ha)")+xlab("PC1")+geom_smooth()

ppet_unc_fia <- ggplot(dens, aes(GS_ppet_mod, mean_dens_fia))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens_fia, ymax=ci.high_dens_fia), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(GS_ppet_mod, mean_dens_fia), color = "black", size = 0.05)+theme_bw()+ylab("FIA Density (trees/ha)")+xlab("growing season P-PET")+geom_smooth()


soil_unc_fia <- ggplot(dens, aes(mean_GS_soil_m, mean_dens_fia))+geom_point(color = "black", size = 0.5)+
  geom_errorbar(data = dens, aes(ymin=ci.low_dens_fia, ymax=ci.high_dens_fia), color = "grey", alpha = 0.5)+
  geom_point(data = dens, aes(mean_GS_soil_m, mean_dens_fia), color = "black", size = 0.05)+theme_bw()+ylab("FIA Density (trees/ha)")+xlab("Growing season soil moisture")+geom_smooth()


# save plots of environment vs density estimates + CI:
png(height = 10, width = 4, units = "in", res = 300, "outputs/paper_figs_unc/smooth_dens_vs_envts_FIA.png")
cowplot::plot_grid(pc1_unc_fia, ppet_unc_fia, soil_unc_fia, ncol = 1)
dev.off()

ggplot()+geom_histogram(data = dens, aes(mean_dens_fia), fill = "red")+geom_histogram(data= dens, aes(ci.high_dens_fia), aes = 2, color = "grey")+geom_histogram(data= dens, aes(ci.low_dens_fia), alpha = 0.2, fill = "blue")



# <<<<<<<<<<<<<<< map bimodality evaluated on samples from  esimates: >>>>>>>>>>>>>>>>>>>>>>>>>>>>

# map bimodal based on PC1:
out.df <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PC1fia_stat.rds")
pvalues <- out.df  %>% group_by(PC1fia_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                               median.p = median(pvalue, na.rm = TRUE),
                                                               ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                               ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                               mean.d = mean(dip, na.rm = TRUE),
                                                               median.d = median(dip, na.rm = TRUE),
                                                               ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                               ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


dens$PC1fia_bins <- cut(dens$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))

# also make density bins:
FIA.df <- dens

ordered.cuts <- data.frame(PC1fia_bins = unique(cut(FIA.df[order(FIA.df$PC1fia),]$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))),
                           mids = seq(-5.375, 4.5, by = 0.25))
pvalues <- left_join(ordered.cuts, pvalues, by = "PC1fia_bins")
bimod.pc.FIA <- left_join(dens, pvalues, by = c("PC1fia_bins"))


# merge with the envt + pc data

bimod.pc.FIA$bimclass <- ifelse(bimod.pc.FIA$mean.p <= 0.05, "bimodal", "unimodal")

bimod.pc.FIA.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.pc.FIA, aes(x=x, y=y, fill = bimclass))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027',"unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")



pc1.dip.FIA <- ggplot(pvalues, aes(mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("FIA PC1")+ylab("DIP value")+xlim(-6.4, 4.5)
pc1.pval.FIA <- ggplot(pvalues, aes(mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("FIA PC1")+ylab("P value")+xlim(-6.4, 4.5)


png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/FIA_dip_pvalues_unc_pc1.png")
plot_grid(pc1.dip.FIA, pc1.pval.FIA)
dev.off()

# pased on P-PET:

out.df <- readRDS("outputs/bimodal_bins_p_value_dipP_FIA_PPET_stat.rds")
pvalues <- out.df  %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                median.p = median(pvalue, na.rm = TRUE),
                                                                ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                mean.d = mean(dip, na.rm = TRUE),
                                                                median.d = median(dip, na.rm = TRUE),
                                                                ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


dens$ppet_binsfia <- cut(dens$GS_ppet_mod, breaks=seq(-170, 205, by = 15))

ordered.cuts <- data.frame(ppet_bins = unique(cut(FIA.df[order(FIA.df$GS_ppet_mod),]$GS_ppet, breaks=seq(-170, 205, by = 15))),
                           mids = seq(-167.5, 205, by = 15))
pvalues <- left_join(ordered.cuts, pvalues, by = "ppet_bins")

bimod.ppet.FIA <- merge(dens, pvalues, by.x = c("ppet_binsfia"), by.y = c("ppet_bins"))


# merge with the envt + pc data

bimod.ppet.FIA$bimclass_ppet <- ifelse(bimod.ppet.FIA$mean.p <= 0.05, "bimodal", "unimodal")
bimod.ppet.FIA.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.ppet.FIA, aes(x=x, y=y, fill = bimclass_ppet))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal" = '#d73027', "unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")



ppet.dip.FIA <- ggplot(pvalues, aes(mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("FIA P-PET")+ylab("DIP value")
ppet.pval.FIA <- ggplot(pvalues, aes(mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("FIA P-PET")+ylab("P value")

png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/FIA_dip_pvalues_unc_ppet.png")
plot_grid(ppet.dip.FIA, ppet.pval.FIA)
dev.off()

# based on soil moisture estimates:
out.df <- readRDS("outputs/bimodal_bins_p_value_dipP_FIA_soil_15bins_kde_stat.rds")

pvalues <- out.df  %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                median.p = median(pvalue, na.rm = TRUE),
                                                                ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                mean.d = mean(dip, na.rm = TRUE),
                                                                median.d = median(dip, na.rm = TRUE),
                                                                ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


dens$soil_binsfia <- cut(dens$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05))

ordered.cuts <- data.frame(soil_bins = levels(unique(cut(FIA.df[order(FIA.df$mean_GS_soil_m),]$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05)))),
                           mids = seq(0.025, 1.8, by = 0.05))
pvalues <- left_join(ordered.cuts, pvalues, by = "soil_bins")

bimod.sm.FIA <- merge(dens, pvalues, by.x = "soil_binsfia", by.y = c("soil_bins"))


# merge with the envt + pc data

bimod.sm.FIA$bimclass_soil <- ifelse(bimod.sm.FIA$mean.p <= 0.05, "bimodal", "unimodal")



bimod.sm.FIA.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bimod.sm.FIA, aes(x=x, y=y, fill = bimclass_soil))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("bimodal"='#d73027', "unimodal" = '#4575b4', "low sample" = "tan"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")




soil.dip.FIA <- ggplot(pvalues, aes(mids, median.d))+geom_point()+geom_errorbar(aes(ymin=ci.low.d, ymax=ci.high.d), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("FIA Soil moisture")+ylab("DIP value")
soil.pval.FIA <- ggplot(pvalues, aes(mids, median.p))+geom_point()+geom_errorbar(aes(ymin=ci.low.p, ymax=ci.high.p), color = "purple", alpha = 0.5, width = 0)+theme_bw()+geom_hline(yintercept = 0.02, linetype = "dashed")+xlab("FIA Soil moisture")+ylab("P value")


png(height = 6, width = 6, units = "in",res = 300,"outputs/paper_figs_unc/FIA_dip_pvalues_unc_ppet.png")
plot_grid(soil.dip.FIA, soil.pval.FIA)
dev.off()


png(height = 10, width = 10, units = "in", res = 300, "outputs/paper_figs_unc/bimodal_maps_with_pvalues_FIA.png")
plot_grid(bimod.pc.FIA.map, bimod.ppet.FIA.map, bimod.sm.FIA.map,
          pc1_unc_fia, ppet_unc_fia, soil_unc_fia,
          pc1.dip.FIA + ylim(0,0.1), ppet.dip.FIA+ylim(0,0.1), soil.dip.FIA+ylim(0,0.1),
          pc1.pval.FIA, ppet.pval.FIA, soil.pval.FIA, 
          ncol = 3, rel_heights = c(1,1,0.5, 0.5))
dev.off()




# -------------------------Make FIA & PLS histogram plots + uncertainty-------------------
png("outputs/paper_figs_unc/mean_density_histogram_pls_fia.png")
ggplot()+geom_histogram(data = dens, aes(mean_dens_fia), bw = 25, fill = "red", alpha = 0.75)+geom_histogram(data = dens, aes(mean_dens), bw = 25,fill = "blue", alpha = 0.75)+xlim(0, 600)+xlab("Density (trees/ha)")
dev.off()


# want to plot the total number of grid cells in each density class
# then want to plot the minimum number and maximum number of grid cells in each density class
# need to define the breaks for mean_density, ci.low, and ci.high



# cut den into pls density bins:
# also make density bins:
dens$dens.bins <- cut(dens$mean_dens, breaks=seq(0, 775, by = 25))
dens$dens.bins.low <- cut(dens$ci.low_dens, breaks=seq(0, 775, by = 25))
dens$dens.bins.high <- cut(dens$ci.high_dens, breaks=seq(0, 775, by = 25))

pls <- dens %>% count(dens.bins)
colnames(pls) <- c("bins", "pls")

pls.low <- dens %>% count(dens.bins.low)
colnames(pls.low) <- c("bins", "pls.low")

pls.high <- dens %>% count(dens.bins.high)
colnames(pls.high) <- c("bins", "pls.high")
count.ci <- merge(pls, pls.low, by = "bins")
count.ci <- merge(count.ci, pls.high, by = "bins")

ordered.bins <- data.frame(bins = levels(cut(dens[order(dens$mean_dens),]$mean_dens, breaks=seq(0, 775, by = 25))),
                           mids = seq(13.5, 775, by = 25))

count.ci <- merge(count.ci, ordered.bins, by = "bins")
ggplot(count.ci, aes(bins, pls))+geom_bar(stat = "identity")+geom_errorbar(data = count.ci, aes(ymin=pls.low, ymax=pls.high),width=1)

ggplot()+geom_ribbon(data = count.ci, aes(x=mids, ymin=pls.low, ymax=pls.high),fill="darkgrey", alpha=0.9)


# >>>>>>>>>>>>>>>>>>>>>>>.new figures with 2d density plots: <<<<<<<<<<<<<<<<<<<<<<<
pls.df <- dens
library(ks)
library(ggplotify)
# for PC1:
H <- Hpi.diag(x=na.omit(cbind(pls.df$PC1, pls.df$mean_dens)) )
fhat <- kde(x=na.omit(cbind(pls.df$PC1, pls.df$mean_dens)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550))
#points(na.omit(cbind(pls.df$PC1, pls.df$mean_dens)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)


#bimodal.pls.pc1 <- read.csv("outputs/new_bim_surface_PC1_pls_0.1_mode_crit_1000.csv")
bimodal.pls.pc1 <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PC1_stat.rds")
#bimodal.pls.pc1$dip <- ifelse(bimodal.pls.pc1$pvalue == 1, NA, bimodal.pls.pc1$dip)
#bimodal.pls.pc1$pvalue <- ifelse(bimodal.pls.pc1$pvalue == 1, NA, bimodal.pls.pc1$pvalue)


pvalues <- bimodal.pls.pc1 %>% group_by(pc1_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                              median.p = median(pvalue, na.rm = TRUE),
                                                              ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                              ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                              mean.d = mean(dip, na.rm = TRUE),
                                                              median.d = median(dip, na.rm = TRUE),
                                                              ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                              ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$pc1_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(pc1_bins = unique(unique(pvalues$pc1_bins)),
                           pc1_mids = as.numeric(mids)+0.125)





bimodal.pls.pc1 <- merge(pvalues, ordered.cuts, by = "pc1_bins")
bimodal.pls.pc1$bimclass <- ifelse(bimodal.pls.pc1$median.p <= 0.05, "bimodal", "unimodal")
pc1.bim.line <- data.frame(PC1 = unique(bimodal.pls.pc1[bimodal.pls.pc1$bimclass %in% "bimodal",]$pc1_mids), y = -37, bimodal = "bimodal")
pls.kde.plot.pc1 <- recordPlot()
ggplot(bimodal.pls.pc1, aes(PC1, fill = bimclass))+geom_histogram()

library(base2grob)
pls.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550)))


# make the plot with GGPLOT:
pls.kde.plot.pc1.gg <- ggplot(pls.df, aes(x=PC1, y=PLSdensity) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")
  
  
pls.kde.plot.pc1.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7 ) + points(pc1.bim.line[pc1.bim.line$bimodal %in% "bimodal",]$PC1 , pc1.bim.line[pc1.bim.line$bimodal %in% "bimodal",]$y, col = "darkblue", pch = 15, cex = 1)+ text(-5.5, 500, "A"))
pls.kde.plot.pc1.gg

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7 ) + points(data = pc1.bim.line[pc1.bim.line$bimodal %in% "bimodal",], y~PC1, cex = 0.9,  pch = 15,col = "darkblue") + text(-6,500, "A")

ylow2redscale <- c('#ffffcc',
    '#ffeda0',
    '#fed976',
    '#feb24c',
    '#fd8d3c',
    '#fc4e2a',
    '#e31a1c',
    '#bd0026',
    '#800026')



# for P-PET:
H <- Hpi.diag(x=na.omit(cbind(pls.df$GS_ppet, pls.df$mean_dens)) )
fhat <- kde(x=na.omit(cbind(pls.df$GS_ppet, pls.df$mean_dens)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(0,550))
#points(na.omit(cbind(pls.df$PC1, pls.df$mean_dens)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
pls.kde.plot.ppet <- recordPlot()
pls.kde.plot.ppet.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(0,550), xlim = c(-200, 300)))


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)



#bimodal.pls.ppet <- read.csv("outputs/new_bim_surface_PPET_pls_4_mode_crit_1000.csv")
bimodal.pls.ppet <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PPET_stat.rds")
#bimodal.pls.ppet$dip <- ifelse(bimodal.pls.ppet$pvalue == 1, NA, bimodal.pls.ppet$dip)
#bimodal.pls.ppet$pvalue <- ifelse(bimodal.pls.ppet$pvalue == 1, NA, bimodal.pls.ppet$pvalue)

pvalues <- bimodal.pls.ppet %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                               median.p = median(pvalue, na.rm = TRUE),
                                                               ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                               ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                               mean.d = mean(dip, na.rm = TRUE),
                                                               median.d = median(dip, na.rm = TRUE),
                                                               ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                               ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

first <- strsplit(as.character(unique(pvalues$ppet_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(ppet_bins = pvalues$ppet_bins,
                           ppet_mids = as.numeric(mids)+7.5)


bimodal.pls.ppet <- merge(pvalues, ordered.cuts, by = "ppet_bins")
bimodal.pls.ppet$bimclass_ppet <- ifelse(bimodal.pls.ppet$median.p <= 0.05, "bimodal", "unimodal")


ppet.bim.line <- data.frame(PPET = unique(bimodal.pls.ppet[bimodal.pls.ppet$bimclass_ppet %in% "bimodal",]$ppet_mids), y = -37, bimodal = "bimodal")


# ggplotify the kde plots here:
pls.kde.plot.ppet.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + points(data = ppet.bim.line[ppet.bim.line$bimodal %in% "bimodal",], y~PPET, cex = 1,  pch = 15,col = "darkblue") + text(-170,500, "B"))
pls.kde.plot.ppet.gg


  # for soil moisture/bucket model

H <- Hpi.diag(x=na.omit(cbind(pls.df$mean_GS_soil, pls.df$mean_dens)) )
fhat <- kde(x=na.omit(cbind(pls.df$mean_GS_soil, pls.df$mean_dens)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density", ylim = c(-40,550))
#points(na.omit(cbind(pls.df$PC1, pls.df$mean_dens)), cex=0.3, pch=16)
abline(h = -40, col = "purple", lwd = 10)
plot(fhat, display="slice", cont=c(85), add = TRUE)
pls.kde.plot.sm <- recordPlot()
pls.kde.plot.sm.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "soil moisture", ylab = "Tree density", ylim = c(0,550), xlim=c(0, 1.5)))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)



bimodal.pls.soil<- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_soil_15bins_kde_stat.rds")
# if pvalues == 1, then we dont evaluate over it

pvalues <- bimodal.pls.soil %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                               median.p = median(pvalue, na.rm = TRUE),
                                                               ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                               ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                               mean.d = mean(dip, na.rm = TRUE),
                                                               median.d = median(dip, na.rm = TRUE),
                                                               ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                               ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

first <- strsplit(as.character(unique(pvalues$soil_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(soil_bins = unique(pvalues$soil_bins),
                           soil_mids = as.numeric(mids)+0.025)

bimodal.pls.soil <- merge(pvalues, ordered.cuts, by.x = "soil_bins")
bimodal.pls.soil$bimclass_soil <- ifelse(bimodal.pls.soil$median.p <= 0.05, "bimodal", "unimodal")

sm.bim.line <- data.frame(SM = unique(bimodal.pls.soil[bimodal.pls.soil$bimclass_soil %in% "bimodal",]$soil_mids), y = -37, bimodal = "bimodal")

# ggplotify the kde plots here:
pls.kde.plot.sm.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7) + points(data = sm.bim.line[sm.bim.line$bimodal %in% "bimodal",], y~SM, cex = 1,  pch = 15,col = "darkblue") + text(-0.05,500, "C"))
pls.kde.plot.sm.gg





# >>>>>>>>>>>>>>>>>>>>>>> FIA figures with 2d density plots: <<<<<<<<<<<<<<<<<<<<<<<
pls.df <- dens
library(ks)
# for PC1:
H <- Hpi.diag(x=na.omit(cbind(pls.df$PC1fia, pls.df$mean_dens_fia)) )
fhat <- kde(x=na.omit(cbind(pls.df$PC1fia, pls.df$mean_dens_fia)), H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density")
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["16%"])[[1]])
contour_95 <- data.frame(contour_95)




#bimodal.fia.pc1 <- read.csv("outputs/new_bim_surface_PC1_fia_0.1_mode_crit_1000.csv")
bimodal.fia.pc1 <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PC1fia_stat.rds")


pvalues <- bimodal.fia.pc1 %>% group_by(PC1fia_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                       median.p = median(pvalue, na.rm = TRUE),
                                                                       ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                       ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                       mean.d = mean(dip, na.rm = TRUE),
                                                                       median.d = median(dip, na.rm = TRUE),
                                                                       ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                       ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)


# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$PC1fia_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(pc1_bins_f = unique(unique(pvalues$PC1fia_bins)),
                           pc1_mids_f = as.numeric(mids)+0.125)



colnames(pvalues)[1] <- "pc1_bins_f"

bimodal.fia.pc1 <- merge(pvalues, ordered.cuts, by = "pc1_bins_f")
bimodal.fia.pc1$bimclass_f <- ifelse(bimodal.fia.pc1$median.p <= 0.05, "bimodal", "unimodal")

pc1.f.bim.line <- data.frame(PC1 = ifelse(is.null(nrow(unique(bimodal.fia.pc1[bimodal.fia.pc1$bimclass_f %in% "bimodal",]$mids))),NA, 
                                          unique(bimodal.fia.pc1[bimodal.fia.pc1$bimclass_f %in% "bimodal",]$mids)), y = -37, bimodal = "bimodal")


#pc1.f.bim.line <- data.frame(PC1 = seq(from = -6, to =6, by = 0.05), pval = NA, bimodal = NA)
#pc1.f.bim.line$pval <- apply(data.frame(pc1.f.bim.line$PC1), MARGIN= 1, FUN=interp.densp)
#pc1.f.bim.line$bimodal <- ifelse(pc1.f.bim.line$pval <= 0.05, "bimodal", "unimodal")
#pc1.f.bim.line$y <- -37
#fia.kde.plot.pc1 <- recordPlot()
library(base2grob)
fia.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), cex.axis=1.5
                                         )+ text(-6,500, "B"))

#plot_grid(pls.kde.plot.pc1.grob, three.color.bimodal.plots, ncol = 2)

# make the plot with GGPLOT:
fia.kde.plot.pc1.gg <- ggplot(pls.df, aes(x=PC1, y=mean_dens_fia) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")


fia.kde.plot.pc1.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  yaxt="n", cex.axis=0.7) + points(data = pc1.f.bim.line[pc1.f.bim.line$bimodal %in% "bimodal",], y~PC1, cex = 0.9,  pch = 15,col = "red")+ text(-5.5,500, "E"))+ xlab("P-PET")
fia.kde.plot.pc1.gg + xlab("PC1")


# --------------------for P-PET:
H <- Hpi.diag(x=na.omit(cbind(pls.df$GS_ppet_mod, pls.df$mean_dens_fia)) )
fhat <- kde(x=na.omit(cbind(pls.df$GS_ppet_mod, pls.df$mean_dens_fia)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density")
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.ppet <- recordPlot()
fia.kde.plot.ppet.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)



bimodal.fia.ppet <- readRDS("outputs/bimodal_bins_p_value_dipP_fia_PPET_stat.rds")


pvalues <- bimodal.fia.ppet %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                       median.p = median(pvalue, na.rm = TRUE),
                                                                       ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                       ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                       mean.d = mean(dip, na.rm = TRUE),
                                                                       median.d = median(dip, na.rm = TRUE),
                                                                       ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                       ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$ppet_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(ppet_bins_f = unique(unique(pvalues$ppet_bins)),
                           ppet_mids_f = as.numeric(mids)+7.5)


colnames(pvalues)[1] <- "ppet_bins_f"



bimodal.fia.ppet <- merge(pvalues, ordered.cuts, by = "ppet_bins_f")
bimodal.fia.ppet$bimclass_ppet_f <- ifelse(bimodal.fia.ppet$median.p <= 0.05, "bimodal", "unimodal")

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

ppet.f.bim.line <- data.frame(PPET = ifelse(is.null(nrow(unique(bimodal.fia.ppet[bimodal.fia.ppet$bimclass_ppet_f %in% "bimodal",]$GS_ppet_mod))),NA, 
                                            unique(bimodal.fia.ppet[bimodal.fia.ppet$bimclass_ppet_f %in% "bimodal",]$GS_ppet_mod)), y = -37, bimodal = "bimodal")


fia.kde.plot.pc1 <- recordPlot()
library(base2grob)
fia.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PPET", ylab = NA, ylim = c(-40,550)))


# make the plot with GGPLOT:
fia.kde.plot.ppet.gg <- ggplot(pls.df, aes(x=PC1, y=mean_dens_fia) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")


fia.kde.plot.ppet.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab="P-PET",ylab=NA,  ylim = c(-40,550), xlim = c(-200, 300),  yaxt="n" , cex.axis=0.7) + points(data = ppet.f.bim.line[ppet.f.bim.line$bimodal %in% "bimodal",], y~PPET, cex = 0.9,  pch = 15,col = "red")+ text(-170,500, "F"))+xlab("P-PET")
fia.kde.plot.ppet.gg +xlab("P-PET")


# for soil moisture/bucket model

H <- Hpi.diag(x=na.omit(cbind(pls.df$mean_GS_soil_m, pls.df$mean_dens_fia)) )
fhat <- kde(x=na.omit(cbind(pls.df$mean_GS_soil_m, pls.df$mean_dens_fia)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density")
#points(na.omit(cbind(pls.df$PC1, pls.df$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.sm <- recordPlot()
fia.kde.plot.sm.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "soil moisture", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)



#sm.f.bim.line <- data.frame(SM = seq(from = 0, to = 1.5, by = 0.05), pval = NA, bimodal = NA)
#sm.f.bim.line$pval <- apply(data.frame(sm.f.bim.line$SM), MARGIN= 1, FUN=interp.densp)
#sm.f.bim.line$bimodal <- ifelse(sm.f.bim.line$pval <= 0.05, "bimodal", "unimodal")
#sm.f.bim.line$y <- -37

#bimodal.fia.sm <- read.csv("outputs/new_bim_surface_soil_moist_fia_0.01_mode_crit.csv")
bimodal.fia.sm <- readRDS("outputs/bimodal_bins_p_value_dipP_fia_soil_15bins_kde_stat.rds")


pvalues <- bimodal.fia.sm %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                         median.p = median(pvalue, na.rm = TRUE),
                                                                         ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                         ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                         mean.d = mean(dip, na.rm = TRUE),
                                                                         median.d = median(dip, na.rm = TRUE),
                                                                         ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                         ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$soil_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(soil_bins_f = unique(unique(pvalues$soil_bins)),
                           soil_mids_f = as.numeric(mids)+0.025)

colnames(pvalues)[1] <- "soil_bins_f"



bimodal.fia.sm <- merge(pvalues, ordered.cuts, by = "soil_bins_f")
bimodal.fia.sm$bimclass_soil_f <- ifelse(bimodal.fia.sm$median.p <= 0.05, "bimodal", "unimodal")

sm.f.bim.line <- data.frame(SM = ifelse(is.null(nrow(unique(bimodal.fia.sm[bimodal.fia.sm$bimclass_soil_f %in% "bimodal",]$mean_GS_soil_m))),NA, 
                                        unique(bimodal.fia.sm[bimodal.fia.sm$bimclass_soil_f %in% "bimodal",]$mean_GS_soil_m)), y = -37, bimodal = "bimodal")



fia.kde.plot.pc1 <- recordPlot()
library(base2grob)
fia.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550)))

#plot_grid(pls.kde.plot.pc1.grob, three.color.bimodal.plots, ncol = 2)

# make the plot with GGPLOT:
fia.kde.plot.sm.gg <- ggplot(pls.df, aes(x=mean_GS_soil_m, y=mean_dens_fia) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")


fia.kde.plot.sm.gg <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = NA, ylim = c(-40,550), yaxt="n",  cex.axis=0.9) + points(data = sm.f.bim.line[sm.f.bim.line$bimodal %in% "bimodal",], y~SM, cex = 0.8,  pch = 15,col = "red")+ text(-0.05,500, "G"))
fia.kde.plot.sm.gg 


# make ggplot figures of cluster density

# need to merge together all of the bimodal/unimodal tags
library(ggplotify)

pls.df$pc1_bins <- cut(pls.df$PC1, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.pls.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass"], bimodal.pls.pc1, by = "pc1_bins")

pls.df$ppet_bins <- cut(pls.df$GS_ppet, breaks=seq(-170, 205, by = 15))
kde.surf.ppet.pls.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_ppet"], bimodal.pls.ppet, by = "ppet_bins")

pls.df$soil_bins <- cut(pls.df$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.pls.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_soil"], bimodal.pls.soil, by = "soil_bins")


# now merge all of these together to make a map of 1, 2, 3, bimodal metrics:
bim.class.m <- merge(kde.surf.pc1.pls.df[,c("x", "y", "bimclass")],kde.surf.soil.pls.df [,c("x", "y", "bimclass_soil")], by = c("x", "y"))
bim.class.m <- merge(bim.class.m, kde.surf.ppet.pls.df[,c("x", "y", "bimclass_ppet", "PLSdensity")])

bim.class.m$nbimod <- as.character(rowSums(bim.class.m[,3:5] == "bimodal", na.rm = TRUE))
# define nbimod as a category:
bim.class.m$nbimod <- factor(bim.class.m$nbimod, levels = c("0","1", "2", "3", "No data"))

three.color.bimodal.plots <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.m, aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
  ), labels = c("0","1", "2", "3", "No data"), drop = F) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "D", size = 4)


one.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "1",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
two.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "2",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
three.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "3",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)

one.bimpct <- one.bimpct + two.bimpct + three.bimpct
two.bimpct <-  two.bimpct + three.bimpct
three.bimpct <-  three.bimpct



# merge for fia
pls.df$pc1_bins_f <- cut(pls.df$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.fia.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_f"], bimodal.fia.pc1, by = "pc1_bins_f")

pls.df$ppet_bins_f <- cut(pls.df$GS_ppet_mod, breaks=seq(-125, 310, by = 15))
kde.surf.ppet.fia.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_ppet_f"], bimodal.fia.ppet, by = "ppet_bins_f")

pls.df$soil_bins_f <- cut(pls.df$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.fia.df <- merge(pls.df[,!colnames(pls.df) %in% "bimclass_soil_f"], bimodal.fia.sm, by = "soil_bins_f")


# FIA three color maps
# now merge all of these together to make a map of 1, 2, 3, bimodal metrics:
bim.class.m.f <- merge(kde.surf.pc1.fia.df[,c("x", "y", "bimclass_f")],kde.surf.soil.fia.df [,c("x", "y", "bimclass_soil_f")], by = c("x", "y"))
bim.class.m.f <- merge(bim.class.m.f, kde.surf.ppet.fia.df[,c("x", "y", "bimclass_ppet_f", "mean_dens_fia")])

bim.class.m.f$nbimod <- as.character(rowSums(bim.class.m.f[,3:5] == "bimodal", na.rm = TRUE))
# define nbimod as a category:
bim.class.m.f$nbimod <- factor(bim.class.m.f$nbimod, levels = c("0","1", "2", "3", "No data"))

three.color.bimodal.plots.fia <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=bim.class.m.f[!is.na(bim.class.m.f$mean_dens),], aes(x=x, y=y, fill = nbimod))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
  ), labels = c("0","1", "2", "3", "No data"), drop = F) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "D", size = 4)


one.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "1",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
two.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "2",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
three.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "3",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)

one.bimpct <- one.bimpct + two.bimpct + three.bimpct
two.bimpct <-  two.bimpct + three.bimpct
three.bimpct <-  three.bimpct


#kh left off here


# merge fia and pls bimodal dfs together:
kde.surf.pc1.df <- merge(kde.surf.pc1.pls.df[,c("x", "y",  "mean_dens",  "bimclass","pc1_bins", "pc1_mids")], kde.surf.pc1.fia.df[,c("x", "y",  "mean_dens_fia","bimclass_f","pc1_bins_f", "pc1_mids_f")], by = c("x", "y"), all = TRUE)
kde.surf.ppet.df <- merge(kde.surf.ppet.pls.df[,c("x", "y",  "mean_dens",  "bimclass_ppet","ppet_bins", "ppet_mids")], kde.surf.ppet.fia.df[,c("x", "y",  "mean_dens_fia","bimclass_ppet_f", "ppet_bins_f","ppet_mids_f")], by = c("x", "y"), all = TRUE)
kde.surf.soilm.df <- merge(kde.surf.soil.pls.df[,c("x", "y",  "mean_dens",  "bimclass_soil", "soil_bins", "soil_mids")], kde.surf.soil.fia.df[,c("x", "y",  "mean_dens_fia","bimclass_soil_f", "soil_bins_f","soil_mids_f")], by = c("x", "y"), all = TRUE)




flipped.pc1.hist <- ggplot(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",], aes(mean_dens))+geom_density(color = "blue")+
  geom_density(data = kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",], aes(mean_dens_fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

flipped.ppet.hist <- ggplot(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",], aes(mean_dens))+geom_density(color = "blue")+
  geom_density(data = kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",], aes(mean_dens_fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

flipped.soilm.hist <- ggplot(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",], aes(mean_dens))+geom_density(color = "blue")+
  geom_density(data = kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",], aes(mean_dens_fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)


# alternative: get density lines then ggplotify them to align:
pls.soilm.density.df <- data.frame(y = density(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$mean_dens)$y, 
           x = density(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$mean_dens)$x)

fia.soilm.density.df <- data.frame(y = density(na.omit(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$mean_dens_fia))$y, 
                                 x = density(na.omit(kde.surf.soilm.df[kde.surf.soilm.df$bimclass_soil %in% "bimodal",]$mean_dens_fia))$x)
flipped.soilm.hist.gg <- as.ggplot(~plot(fia.soilm.density.df[fia.soilm.density.df$x < 550,] , type = "l", col = "red", ylim = c(-40, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.soilm.density.df[pls.soilm.density.df$x < 550 & pls.soilm.density.df$x > -41,] , type = "l", col = "blue"))



pls.ppet.density.df <- data.frame(y = density(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$mean_dens)$y, 
                                 x = density(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$mean_dens)$x)

fia.ppet.density.df <- data.frame(y = density(na.omit(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$mean_dens_fia))$y, 
                                 x = density(na.omit(kde.surf.ppet.df[kde.surf.ppet.df$bimclass_ppet %in% "bimodal",]$mean_dens_fia))$x)
#flipped.ppet.hist.gg <- as.ggplot(~plot( fia.ppet.density.df[fia.ppet.density.df$x < 550,] , type = "l", col = "red", ylim = c(-40, 550),yaxt="n", ylab = NA, xlab = NA, xaxt = "n",xlim = c(0, 0.005)) + lines(pls.ppet.density.df[pls.ppet.density.df$x < 550 & pls.ppet.density.df$x > -41,] , type = "l", col = "blue"))

flipped.ppet.hist.gg <- as.ggplot(~plot(fia.ppet.density.df[fia.ppet.density.df$x < 550,],type = "l", col = "red", ylim = c(-40, 550),yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.ppet.density.df[pls.ppet.density.df$x < 550 & pls.ppet.density.df$x > -41,] , type = "l", col = "blue"))

pls.pc1.density.df <- data.frame(y = density(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$mean_dens)$y, 
                                  x = density(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$mean_dens)$x)

fia.pc1.density.df <- data.frame(y = density(na.omit(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$mean_dens_fia))$y, 
                                  x = density(na.omit(kde.surf.pc1.df[kde.surf.pc1.df$bimclass %in% "bimodal",]$mean_dens_fia))$x)
flipped.pc1.hist.gg <- as.ggplot(~plot(fia.pc1.density.df[fia.pc1.density.df$x < 550 ,], type = "l", col = "red", ylim = c(-41, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.pc1.density.df[pls.pc1.density.df$x < 550,], type = "l", col = "blue"))

library(gtable)
g1 <- ggplotGrob(pls.kde.plot.pc1.gg+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-0.1), "cm")))
g2 <- ggplotGrob(fia.kde.plot.pc1.gg+theme(plot.margin=unit(c(-0.7,-1,-0.5,-1), "cm")))
g3 <- ggplotGrob(flipped.pc1.hist.gg+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g4 <- ggplotGrob(pls.kde.plot.ppet.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g5 <- ggplotGrob(fia.kde.plot.ppet.gg+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g6 <- ggplotGrob(flipped.ppet.hist.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g7 <- ggplotGrob(pls.kde.plot.sm.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g8 <- ggplotGrob(fia.kde.plot.sm.gg+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g9 <- ggplotGrob(flipped.soilm.hist.gg+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))
g10 <- ggplotGrob(three.color.bimodal.plots)
g11 <- ggplotGrob(three.color.bimodal.plots.fia)

g <- cbind(g1 ,g2, g3, size = "first")
g$heights <-unit.pmax(g1$heights, g2$heights, g3$heights)#, 

grow2 <- cbind(g4 ,g5, g6, size = "first")
grow2$heights <-unit.pmax(g4$heights, g5$heights, g6$heights)#, 

grow3 <- cbind(g7 ,g8, g9, size = "first")
grow3$heights <-unit.pmax(g7$heights, g8$heights, g9$heights)#, 
grow4 <- cbind(g10, g11, size = "first")
grow4$heights <-unit.pmax(g10$heights, g11$heights)
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
#dev.off()


png(height = 10, width = 6, units = "in", res = 500, "outputs/paper_figs_unc/new_figure_3_kde_plot_with_hist_uncertainty.png")
fig3 <- grid.arrange(g, grow2, grow3,grow4, ncol = 1)
fig3
dev.off()




# >>>>>>>>>>>>>>>>>>>>>>> ALL STAT DRAWS 2d density plots: <<<<<<<<<<<<<<<<<<<<<<<
total.pls <- read.csv("data/extracted_total_PLS_density_draws.csv")
total.fia <- read.csv("data/extracted_total_FIA_density_draws.csv")

colnames(total.pls) <- c("x",  "y","sample_pls" ,"pls" )
colnames(total.fia) <-c("x",  "y","sample_fia" ,"fia" )

total.pls <- total.pls[!is.na(total.pls$pls),]
total.fia <- total.fia[!is.na(total.fia$fia),]

pls.dens <- left_join(total.pls, dens.pr, by=c("x", "y"))
fia.dens <- left_join(total.fia, dens.pr,  by=c("x", "y"))



# for PC1:
H <- Hpi.diag(x=na.omit(cbind(pls.dens$PC1, pls.dens$pls)) )
fhat <- kde(x=na.omit(cbind(pls.dens$PC1, pls.dens$pls)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550))
#points(na.omit(cbind(pls.dens$PC1, pls.dens$pls)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)


#bimodal.pls.pc1 <- read.csv("outputs/new_bim_surface_PC1_pls_0.1_mode_crit_1000.csv")
bimodal.pls.pc1 <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PC1_stat.rds")
#bimodal.pls.pc1$dip <- ifelse(bimodal.pls.pc1$pvalue == 1, NA, bimodal.pls.pc1$dip)
#bimodal.pls.pc1$pvalue <- ifelse(bimodal.pls.pc1$pvalue == 1, NA, bimodal.pls.pc1$pvalue)


pvalues <- bimodal.pls.pc1 %>% group_by(pc1_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                       median.p = median(pvalue, na.rm = TRUE),
                                                                       ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                       ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                       mean.d = mean(dip, na.rm = TRUE),
                                                                       median.d = median(dip, na.rm = TRUE),
                                                                       ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                       ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$pc1_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(pc1_bins = unique(unique(pvalues$pc1_bins)),
                           pc1_mids = as.numeric(mids)+0.125)





bimodal.pls.pc1 <- merge(pvalues, ordered.cuts, by = "pc1_bins")
bimodal.pls.pc1$bimclass <- ifelse(bimodal.pls.pc1$median.p <= 0.05, "bimodal", "unimodal")
pc1.bim.line <- data.frame(PC1 = unique(bimodal.pls.pc1[bimodal.pls.pc1$bimclass %in% "bimodal",]$pc1_mids), y = -37, bimodal = "bimodal")

library(base2grob)
library(ggplotify)
pls.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,45,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550)))


# make the plot with GGPLOT:
pls.kde.plot.pc1.gg.full <- ggplot(pls.dens, aes(x=PC1, y=PLSdensity) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")


pls.kde.plot.pc1.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7 ) + points(pc1.bim.line[pc1.bim.line$bimodal %in% "bimodal",]$PC1 , pc1.bim.line[pc1.bim.line$bimodal %in% "bimodal",]$y, col = "darkblue", pch = 15, cex = 1)+ text(-5.5, 500, "A"))
pls.kde.plot.pc1.gg.full

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7 ) + points(data = pc1.bim.line[pc1.bim.line$bimodal %in% "bimodal",], y~PC1, cex = 0.9,  pch = 15,col = "darkblue") + text(-6,500, "A")

ylow2redscale <- c('#ffffcc',
                   '#ffeda0',
                   '#fed976',
                   '#feb24c',
                   '#fd8d3c',
                   '#fc4e2a',
                   '#e31a1c',
                   '#bd0026',
                   '#800026')



# for P-PET:
H <- Hpi.diag(x=na.omit(cbind(pls.dens$GS_ppet, pls.dens$pls)) )
fhat <- kde(x=na.omit(cbind(pls.dens$GS_ppet, pls.dens$pls)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(0,550))
#points(na.omit(cbind(pls.dens$PC1, pls.dens$pls)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
pls.kde.plot.ppet <- recordPlot()
pls.kde.plot.ppet.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(0,550), xlim = c(-200, 300)))


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)



#bimodal.pls.ppet <- read.csv("outputs/new_bim_surface_PPET_pls_4_mode_crit_1000.csv")
bimodal.pls.ppet <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PPET_stat.rds")
#bimodal.pls.ppet$dip <- ifelse(bimodal.pls.ppet$pvalue == 1, NA, bimodal.pls.ppet$dip)
#bimodal.pls.ppet$pvalue <- ifelse(bimodal.pls.ppet$pvalue == 1, NA, bimodal.pls.ppet$pvalue)

pvalues <- bimodal.pls.ppet %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                         median.p = median(pvalue, na.rm = TRUE),
                                                                         ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                         ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                         mean.d = mean(dip, na.rm = TRUE),
                                                                         median.d = median(dip, na.rm = TRUE),
                                                                         ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                         ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

first <- strsplit(as.character(unique(pvalues$ppet_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(ppet_bins = pvalues$ppet_bins,
                           ppet_mids = as.numeric(mids)+7.5)


bimodal.pls.ppet <- merge(pvalues, ordered.cuts, by = "ppet_bins")
bimodal.pls.ppet$bimclass_ppet <- ifelse(bimodal.pls.ppet$median.p <= 0.05, "bimodal", "unimodal")


ppet.bim.line <- data.frame(PPET = unique(bimodal.pls.ppet[bimodal.pls.ppet$bimclass_ppet %in% "bimodal",]$ppet_mids), y = -37, bimodal = "bimodal")


# ggplotify the kde plots here:
pls.kde.plot.ppet.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density", ylim = c(-40,550), xlim = c(-200, 300), cex.axis = 0.7) + points(data = ppet.bim.line[ppet.bim.line$bimodal %in% "bimodal",], y~PPET, cex = 1,  pch = 15,col = "darkblue") + text(-170,500, "B"))
pls.kde.plot.ppet.gg.full


# for soil moisture/bucket model

H <- Hpi.diag(x=na.omit(cbind(pls.dens$mean_GS_soil, pls.dens$pls)) )
fhat <- kde(x=na.omit(cbind(pls.dens$mean_GS_soil, pls.dens$pls)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density", ylim = c(-40,550))

plot(fhat, display="slice", cont=c(85), add = TRUE)
pls.kde.plot.sm <- recordPlot()
pls.kde.plot.sm.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "soil moisture", ylab = "Tree density", ylim = c(0,550), xlim=c(0, 1.5)))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["10%"])[[1]])
contour_95 <- data.frame(contour_95)



bimodal.pls.soil<- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_soil_15bins_kde_stat.rds")
# if pvalues == 1, then we dont evaluate over it

pvalues <- bimodal.pls.soil %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                         median.p = median(pvalue, na.rm = TRUE),
                                                                         ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                         ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                         mean.d = mean(dip, na.rm = TRUE),
                                                                         median.d = median(dip, na.rm = TRUE),
                                                                         ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                         ci.high.d = quantile(dip, 0.975, na.rm = TRUE))


# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

first <- strsplit(as.character(unique(pvalues$soil_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(soil_bins = unique(pvalues$soil_bins),
                           soil_mids = as.numeric(mids)+0.025)

bimodal.pls.soil <- merge(pvalues, ordered.cuts, by.x = "soil_bins")
bimodal.pls.soil$bimclass_soil <- ifelse(bimodal.pls.soil$median.p <= 0.05, "bimodal", "unimodal")

sm.bim.line <- data.frame(SM = unique(bimodal.pls.soil[bimodal.pls.soil$bimclass_soil %in% "bimodal",]$soil_mids), y = -37, bimodal = "bimodal")

# ggplotify the kde plots here:
pls.kde.plot.sm.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = "Tree density", ylim = c(-40,550), cex.axis = 0.7) + points(data = sm.bim.line[sm.bim.line$bimodal %in% "bimodal",], y~SM, cex = 1,  pch = 15,col = "darkblue") + text(-0.05,500, "C"))
pls.kde.plot.sm.gg.full





# >>>>>>>>>>>>>>>>>>>>>>> FIA full sample distn with 2d density plots: <<<<<<<<<<<<<<<<<<<<<<<

# for PC1:
H <- Hpi.diag(x=na.omit(cbind(fia.dens$PC1fia, fia.dens$fia)) )
fhat <- kde(x=na.omit(cbind(fia.dens$PC1fia, fia.dens$fia)), H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95,99), xlab = "PC1", ylab = "Tree density")
#points(na.omit(cbind(fia.dens$PC1, fia.dens$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["16%"])[[1]])
contour_95 <- data.frame(contour_95)




#bimodal.fia.pc1 <- read.csv("outputs/new_bim_surface_PC1_fia_0.1_mode_crit_1000.csv")
bimodal.fia.pc1 <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PC1fia_stat.rds")


pvalues <- bimodal.fia.pc1 %>% group_by(PC1fia_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                          median.p = median(pvalue, na.rm = TRUE),
                                                                          ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                          ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                          mean.d = mean(dip, na.rm = TRUE),
                                                                          median.d = median(dip, na.rm = TRUE),
                                                                          ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                          ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)


# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$PC1fia_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(pc1_bins_f = unique(unique(pvalues$PC1fia_bins)),
                           pc1_mids_f = as.numeric(mids)+0.125)



colnames(pvalues)[1] <- "pc1_bins_f"

bimodal.fia.pc1 <- merge(pvalues, ordered.cuts, by = "pc1_bins_f")
bimodal.fia.pc1$bimclass_f <- ifelse(bimodal.fia.pc1$median.p <= 0.05, "bimodal", "unimodal")

pc1.f.bim.line <- data.frame(PC1 = ifelse(is.null(nrow(unique(bimodal.fia.pc1[bimodal.fia.pc1$bimclass_f %in% "bimodal",]$mids))),NA, 
                                          unique(bimodal.fia.pc1[bimodal.fia.pc1$bimclass_f %in% "bimodal",]$mids)), y = -37, bimodal = "bimodal")


#pc1.f.bim.line <- data.frame(PC1 = seq(from = -6, to =6, by = 0.05), pval = NA, bimodal = NA)
#pc1.f.bim.line$pval <- apply(data.frame(pc1.f.bim.line$PC1), MARGIN= 1, FUN=interp.densp)
#pc1.f.bim.line$bimodal <- ifelse(pc1.f.bim.line$pval <= 0.05, "bimodal", "unimodal")
#pc1.f.bim.line$y <- -37
#fia.kde.plot.pc1 <- recordPlot()
library(base2grob)
fia.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), cex.axis=1.5
)+ text(-6,500, "B"))

#plot_grid(pls.kde.plot.pc1.grob, three.color.bimodal.plots, ncol = 2)

# make the plot with GGPLOT:
fia.kde.plot.pc1.gg <- ggplot(fia.dens, aes(x=PC1, y=fia) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")


fia.kde.plot.pc1.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,99), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  yaxt="n", cex.axis=0.7) + points(data = pc1.f.bim.line[pc1.f.bim.line$bimodal %in% "bimodal",], y~PC1, cex = 0.9,  pch = 15,col = "red")+ text(-5.5,500, "E"))+ xlab("P-PET")
fia.kde.plot.pc1.gg.full + xlab("PC1")


# --------------------for P-PET:
H <- Hpi.diag(x=na.omit(cbind(fia.dens$GS_ppet_mod, fia.dens$fia)) )
fhat <- kde(x=na.omit(cbind(fia.dens$GS_ppet_mod, fia.dens$fia)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density")
#points(na.omit(cbind(fia.dens$PC1, fia.dens$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.ppet <- recordPlot()
fia.kde.plot.ppet.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)



bimodal.fia.ppet <- readRDS("outputs/bimodal_bins_p_value_dipP_fia_PPET_stat.rds")


pvalues <- bimodal.fia.ppet %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                         median.p = median(pvalue, na.rm = TRUE),
                                                                         ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                         ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                         mean.d = mean(dip, na.rm = TRUE),
                                                                         median.d = median(dip, na.rm = TRUE),
                                                                         ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                         ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$ppet_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(ppet_bins_f = unique(unique(pvalues$ppet_bins)),
                           ppet_mids_f = as.numeric(mids)+7.5)


colnames(pvalues)[1] <- "ppet_bins_f"



bimodal.fia.ppet <- merge(pvalues, ordered.cuts, by = "ppet_bins_f")
bimodal.fia.ppet$bimclass_ppet_f <- ifelse(bimodal.fia.ppet$median.p <= 0.05, "bimodal", "unimodal")

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

ppet.f.bim.line <- data.frame(PPET = ifelse(is.null(nrow(unique(bimodal.fia.ppet[bimodal.fia.ppet$bimclass_ppet_f %in% "bimodal",]$GS_ppet_mod))),NA, 
                                            unique(bimodal.fia.ppet[bimodal.fia.ppet$bimclass_ppet_f %in% "bimodal",]$GS_ppet_mod)), y = -37, bimodal = "bimodal")


fia.kde.plot.pc1 <- recordPlot()
library(base2grob)
fia.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PPET", ylab = NA, ylim = c(-40,550)))


# make the plot with GGPLOT:
fia.kde.plot.ppet.gg.full <- ggplot(fia.dens, aes(x=PC1, y=fia) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")


fia.kde.plot.ppet.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab="P-PET",ylab=NA,  ylim = c(-40,550), xlim = c(-200, 300),  yaxt="n" , cex.axis=0.7) + points(data = ppet.f.bim.line[ppet.f.bim.line$bimodal %in% "bimodal",], y~PPET, cex = 0.9,  pch = 15,col = "red")+ text(-170,500, "F"))+xlab("P-PET")
fia.kde.plot.ppet.gg.full +xlab("P-PET")


# for soil moisture/bucket model

H <- Hpi.diag(x=na.omit(cbind(fia.dens$mean_GS_soil_m, fia.dens$fia)) )
fhat <- kde(x=na.omit(cbind(fia.dens$mean_GS_soil_m, fia.dens$fia)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density")
#points(na.omit(cbind(fia.dens$PC1, fia.dens$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.sm <- recordPlot()
fia.kde.plot.sm.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "soil moisture", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)




bimodal.fia.sm <- readRDS("outputs/bimodal_bins_p_value_dipP_fia_soil_15bins_kde_stat.rds")


pvalues <- bimodal.fia.sm %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                       median.p = median(pvalue, na.rm = TRUE),
                                                                       ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                       ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                       mean.d = mean(dip, na.rm = TRUE),
                                                                       median.d = median(dip, na.rm = TRUE),
                                                                       ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                       ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$soil_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(soil_bins_f = unique(unique(pvalues$soil_bins)),
                           soil_mids_f = as.numeric(mids)+0.025)

colnames(pvalues)[1] <- "soil_bins_f"



bimodal.fia.sm <- merge(pvalues, ordered.cuts, by = "soil_bins_f")
bimodal.fia.sm$bimclass_soil_f <- ifelse(bimodal.fia.sm$median.p <= 0.05, "bimodal", "unimodal")

sm.f.bim.line <- data.frame(SM = ifelse(is.null(nrow(unique(bimodal.fia.sm[bimodal.fia.sm$bimclass_soil_f %in% "bimodal",]$mean_GS_soil_m))),NA, 
                                        unique(bimodal.fia.sm[bimodal.fia.sm$bimclass_soil_f %in% "bimodal",]$mean_GS_soil_m)), y = -37, bimodal = "bimodal")



fia.kde.plot.pc1 <- recordPlot()
library(base2grob)
fia.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550)))

#plot_grid(pls.kde.plot.pc1.grob, three.color.bimodal.plots, ncol = 2)



fia.kde.plot.sm.gg.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = NA, ylim = c(-40,550), yaxt="n",  cex.axis=0.9) + points(data = sm.f.bim.line[sm.f.bim.line$bimodal %in% "bimodal",], y~SM, cex = 0.8,  pch = 15,col = "red")+ text(0.2,500, "G"))
fia.kde.plot.sm.gg.full 


# make ggplot figures of cluster density

# need to merge together all of the bimodal/unimodal tags
library(ggplotify)

pls.dens$pc1_bins <- cut(pls.dens$PC1, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.pls.dens <- left_join(pls.dens[,!colnames(pls.dens) %in% "bimclass"], bimodal.pls.pc1, by = "pc1_bins")


pls.dens$ppet_bins <- cut(pls.dens$GS_ppet, breaks=seq(-170, 205, by = 15))
kde.surf.ppet.pls.dens <- left_join(pls.dens[,!colnames(pls.dens) %in% "bimclass_ppet"], bimodal.pls.ppet, by = "ppet_bins")

pls.dens$soil_bins <- cut(pls.dens$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.pls.dens <- left_join(pls.dens[,!colnames(pls.dens) %in% "bimclass_soil"], bimodal.pls.soil, by = "soil_bins")


# now merge all of these together to make a map of 1, 2, 3, bimodal metrics:
#bim.class.m.dens <- left_join(kde.surf.pc1.pls.dens[,c("x", "y", "bimclass")],kde.surf.soil.pls.dens [,c("x", "y", "bimclass_soil")], by = c("x", "y"))
#bim.class.m.dens <- left_join(bim.class.m.dens, kde.surf.ppet.pls.dens[,c("x", "y", "bimclass_ppet", "PLSdensity")])

#bim.class.m$nbimod <- as.character(rowSums(bim.class.m[,3:5] == "bimodal", na.rm = TRUE))
# define nbimod as a category:
#bim.class.m$nbimod <- factor(bim.class.m$nbimod, levels = c("0","1", "2", "3", "No data"))

#three.color.bimodal.plots <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
 # geom_raster(data=bim.class.m, aes(x=x, y=y, fill = nbimod))+
  #geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  #labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
  #), labels = c("0","1", "2", "3", "No data"), drop = F) +
  #coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
        #                                      panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "D", size = 4)


#one.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "1",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
#two.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "2",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
#three.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "3",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)

#one.bimpct <- one.bimpct + two.bimpct + three.bimpct
#two.bimpct <-  two.bimpct + three.bimpct
#three.bimpct <-  three.bimpct



# left_join for fia
fia.dens$pc1_bins_f <- cut(fia.dens$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.fia.dens <- left_join(fia.dens[,!colnames(fia.dens) %in% "bimclass_f"], bimodal.fia.pc1, by = "pc1_bins_f")

fia.dens$ppet_bins_f <- cut(fia.dens$GS_ppet_mod, breaks=seq(-125, 310, by = 15))
kde.surf.ppet.fia.dens <- left_join(fia.dens[,!colnames(fia.dens) %in% "bimclass_ppet_f"], bimodal.fia.ppet, by = "ppet_bins_f")

fia.dens$soil_bins_f <- cut(fia.dens$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.fia.dens <- left_join(fia.dens[,!colnames(fia.dens) %in% "bimclass_soil_f"], bimodal.fia.sm, by = "soil_bins_f")


# FIA three color maps
# now left_join all of these together to make a map of 1, 2, 3, bimodal metrics:
#bim.class.m.f <- left_join(kde.surf.pc1.fia.df[,c("x", "y", "bimclass_f")],kde.surf.soil.fia.df [,c("x", "y", "bimclass_soil_f")], by = c("x", "y"))
#bim.class.m.f <- left_join(bim.class.m.f, kde.surf.ppet.fia.df[,c("x", "y", "bimclass_ppet_f", "mean_dens_fia")])

#bim.class.m.f$nbimod <- as.character(rowSums(bim.class.m.f[,3:5] == "bimodal", na.rm = TRUE))
# define nbimod as a category:
#bim.class.m.f$nbimod <- factor(bim.class.m.f$nbimod, levels = c("0","1", "2", "3", "No data"))

#three.color.bimodal.plots.fia <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
 # geom_raster(data=bim.class.m.f[!is.na(bim.class.m.f$mean_dens),], aes(x=x, y=y, fill = nbimod))+
#  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
 # labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
  #), labels = c("0","1", "2", "3", "No data"), drop = F) +
  #coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
   #                                           panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "D", size = 4)


#one.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "1",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
#two.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "2",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
#three.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "3",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)

#one.bimpct <- one.bimpct + two.bimpct + three.bimpct
#two.bimpct <-  two.bimpct + three.bimpct
#three.bimpct <-  three.bimpct


#kh left off here


# left_join fia and pls bimodal dfs together:
#kde.surf.pc1.df <- left_join(kde.surf.pc1.pls.dens[,c("x", "y",  "pls",  "bimclass","pc1_bins", "pc1_mids")], kde.surf.pc1.fia.dens[,c("x", "y",  "fia","bimclass_f","pc1_bins_f", "pc1_mids_f")], by = c("x", "y"))
#kde.surf.ppet.df <- left_join(kde.surf.ppet.pls.df[,c("x", "y",  "mean_dens",  "bimclass_ppet","ppet_bins", "ppet_mids")], kde.surf.ppet.fia.df[,c("x", "y",  "mean_dens_fia","bimclass_ppet_f", "ppet_bins_f","ppet_mids_f")], by = c("x", "y"), all = TRUE)
#kde.surf.soilm.df <- left_join(kde.surf.soil.pls.df[,c("x", "y",  "mean_dens",  "bimclass_soil", "soil_bins", "soil_mids")], kde.surf.soil.fia.df[,c("x", "y",  "mean_dens_fia","bimclass_soil_f", "soil_bins_f","soil_mids_f")], by = c("x", "y"), all = TRUE)

pc1.bimodal.bins <- unique(kde.surf.pc1.pls.dens[kde.surf.pc1.pls.dens$bimclass %in% "bimodal",]$pc1_bins)
ppet.bimodal.bins <- unique(kde.surf.ppet.pls.dens[kde.surf.ppet.pls.dens$bimclass_ppet %in% "bimodal",]$ppet_bins)
soil.bimodal.bins <- unique(kde.surf.soil.pls.dens[kde.surf.soil.pls.dens$bimclass_soil %in% "bimodal",]$soil_bins)



flipped.pc1.hist.full <- ggplot(kde.surf.pc1.pls.dens[kde.surf.pc1.pls.dens$pc1_bins %in% pc1.bimodal.bins,], aes(pls))+geom_density(color = "blue")+
  geom_density(data = kde.surf.pc1.fia.dens[kde.surf.pc1.fia.dens$pc1_bins_f %in% pc1.bimodal.bins,], aes(fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

flipped.ppet.hist.full <- ggplot(kde.surf.ppet.pls.dens[kde.surf.ppet.pls.dens$ppet_bins %in% ppet.bimodal.bins,], aes(pls))+geom_density(color = "blue")+
  geom_density(data = kde.surf.ppet.fia.dens[kde.surf.ppet.fia.dens$ppet_bins %in% ppet.bimodal.bins,], aes(fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

flipped.soilm.hist.full <- ggplot(kde.surf.soil.pls.dens[kde.surf.soil.pls.dens$soil_bins %in% soil.bimodal.bins,], aes(pls))+geom_density(color = "blue")+
  geom_density(data = kde.surf.soil.fia.dens[kde.surf.soil.fia.dens$soil_bins %in% soil.bimodal.bins,], aes(fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)


# alternative: get density lines then ggplotify them to align:
pls.soilm.density.df <- data.frame(y = density(kde.surf.soil.pls.dens[kde.surf.soil.pls.dens$soil_bins %in% soil.bimodal.bins,]$pls)$y, 
                                   x = density(kde.surf.soil.pls.dens[kde.surf.soil.pls.dens$soil_bins %in% soil.bimodal.bins,]$pls)$x)

fia.soilm.density.df <- data.frame(y = density(na.omit(kde.surf.pc1.fia.dens[kde.surf.pc1.fia.dens$pc1_bins_f %in% pc1.bimodal.bins,]$fia))$y, 
                                   x = density(na.omit(kde.surf.pc1.fia.dens[kde.surf.pc1.fia.dens$pc1_bins_f %in% pc1.bimodal.bins,]$fia))$x)
flipped.soilm.hist.gg.full <- as.ggplot(~plot(fia.soilm.density.df , type = "l", col = "red", ylim = c(-40, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.soilm.density.df[pls.soilm.density.df$x < 550 & pls.soilm.density.df$x > -41,] , type = "l", col = "blue"))



pls.ppet.density.df <- data.frame(y = density(kde.surf.ppet.pls.dens[kde.surf.ppet.pls.dens$ppet_bins %in% ppet.bimodal.bins,]$pls)$y, 
                                  x = density(kde.surf.ppet.pls.dens[kde.surf.ppet.pls.dens$ppet_bins %in% ppet.bimodal.bins,]$pls)$x)

fia.ppet.density.df <- data.frame(y = density(na.omit(kde.surf.ppet.fia.dens[kde.surf.ppet.fia.dens$ppet_bins %in% ppet.bimodal.bins,]$fia))$y, 
                                  x = density(na.omit(kde.surf.ppet.fia.dens[kde.surf.ppet.fia.dens$ppet_bins %in% ppet.bimodal.bins,]$fia))$x)

flipped.ppet.hist.gg.full <- as.ggplot(~plot(fia.ppet.density.df,type = "l", col = "red", ylim = c(-40, 550),yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.ppet.density.df[pls.ppet.density.df$x < 550 & pls.ppet.density.df$x > -41,] , type = "l", col = "blue"))


pls.pc1.density.df <- data.frame(y = density(kde.surf.pc1.pls.dens[kde.surf.pc1.pls.dens$pc1_bins %in% pc1.bimodal.bins,]$pls)$y, 
                                 x = density(kde.surf.pc1.pls.dens[kde.surf.pc1.pls.dens$pc1_bins %in% pc1.bimodal.bins,]$pls)$x)

fia.pc1.density.df <- data.frame(y = density(na.omit(kde.surf.pc1.fia.dens[kde.surf.pc1.fia.dens$pc1_bins_f %in% pc1.bimodal.bins,]$fia))$y, 
                                 x = density(na.omit(kde.surf.pc1.fia.dens[kde.surf.pc1.fia.dens$pc1_bins_f %in% pc1.bimodal.bins,]$fia))$x)
flipped.pc1.hist.gg <- as.ggplot(~plot(fia.pc1.density.df[fia.pc1.density.df$x < 550 ,], type = "l", col = "red", ylim = c(-41, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.pc1.density.df[pls.pc1.density.df$x < 550,], type = "l", col = "blue"))

library(gtable)
g1 <- ggplotGrob(pls.kde.plot.pc1.gg.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-0.1), "cm")))
g2 <- ggplotGrob(fia.kde.plot.pc1.gg.full+theme(plot.margin=unit(c(-0.7,-1,-0.5,-1), "cm")))
g3 <- ggplotGrob(flipped.pc1.hist.gg.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g4 <- ggplotGrob(pls.kde.plot.ppet.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g5 <- ggplotGrob(fia.kde.plot.ppet.gg.full+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g6 <- ggplotGrob(flipped.ppet.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g7 <- ggplotGrob(pls.kde.plot.sm.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g8 <- ggplotGrob(fia.kde.plot.sm.gg.full+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g9 <- ggplotGrob(flipped.soilm.hist.gg.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))
g10 <- ggplotGrob(three.color.bimodal.plots)
g11 <- ggplotGrob(three.color.bimodal.plots.fia)

g <- cbind(g1 ,g2, g3, size = "first")
g$heights <-unit.pmax(g1$heights, g2$heights, g3$heights)#, 

grow2 <- cbind(g4 ,g5, g6, size = "first")
grow2$heights <-unit.pmax(g4$heights, g5$heights, g6$heights)#, 

grow3 <- cbind(g7 ,g8, g9, size = "first")
grow3$heights <-unit.pmax(g7$heights, g8$heights, g9$heights)#, 
grow4 <- cbind(g10, g11, size = "first")
grow4$heights <-unit.pmax(g10$heights, g11$heights)
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
#dev.off()


png(height = 10, width = 6, units = "in", res = 500, "outputs/paper_figs_unc/new_figure_3_kde_plot_with_hist_uncertainty_full_stats.png")
fig3 <- grid.arrange(g, grow2, grow3,grow4, ncol = 1)
fig3
dev.off()





# >>>>>>>>>>>>>>>>>>>>> Plot figure 3 with grid cells without FIA plots masked out of FIA <<<<<<<<<<<<<<<<<<<<<<<<<
# for PC1:
total.fia <- read.csv("data/extracted_total_FIA_density_draws.csv")
colnames(total.fia) <-c("x",  "y","sample_fia" ,"fia" )
total.fia <- total.fia[!is.na(total.fia$fia),]
fia.dens.omit <- left_join(total.fia, dens.pr,  by=c("x", "y"))

fia.dens.omit <- fia.dens.omit[!is.na(fia.dens.omit$FIAdensity),]

fia.check.full <- fia.dens %>% group_by(x,y) %>% summarize(mean=mean(fia, na.rm=TRUE), 
                                                           PC1fia = mean(PC1fia, na.rm=TRUE))
ggplot(fia.check.full, aes(x, y, fill = PC1fia))+geom_raster()

fia.check <- fia.dens.omit %>% group_by(x,y) %>% summarize(mean=mean(fia, na.rm=TRUE))
ggplot(fia.check, aes(x, y, fill = mean))+geom_raster()


# 
H <- Hpi.diag(x=na.omit(cbind(fia.dens.omit$PC1fia, fia.dens.omit$fia)) )
fhat <- kde(x=na.omit(cbind(fia.dens.omit$PC1fia, fia.dens.omit$fia)), H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95,99), xlab = "PC1", ylab = "Tree density")
#points(na.omit(cbind(fia.dens.omit$PC1, fia.dens.omit$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)

contour.95 <- with(fhat, contourLines(x=eval.points[[1]],y=eval.points[[2]],
                                      z=estimate,levels=cont["95%"])[[1]])


contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["16%"])[[1]])
contour_95 <- data.frame(contour_95)




#bimodal.fia.pc1 <- read.csv("outputs/new_bim_surface_PC1_fia_0.1_mode_crit_1000.csv")
bimodal.fia.pc1 <- readRDS("outputs/bimodal_bins_p_value_dipP_PLS_PC1fia_stat.rds")


pvalues <- bimodal.fia.pc1 %>% group_by(PC1fia_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                          median.p = median(pvalue, na.rm = TRUE),
                                                                          ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                          ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                          mean.d = mean(dip, na.rm = TRUE),
                                                                          median.d = median(dip, na.rm = TRUE),
                                                                          ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                          ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)


# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$PC1fia_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(pc1_bins_f = unique(unique(pvalues$PC1fia_bins)),
                           pc1_mids_f = as.numeric(mids)+0.125)



colnames(pvalues)[1] <- "pc1_bins_f"

bimodal.fia.pc1 <- merge(pvalues, ordered.cuts, by = "pc1_bins_f")
bimodal.fia.pc1$bimclass_f <- ifelse(bimodal.fia.pc1$median.p <= 0.05, "bimodal", "unimodal")

pc1.f.bim.line <- data.frame(PC1 = ifelse(is.null(nrow(unique(bimodal.fia.pc1[bimodal.fia.pc1$bimclass_f %in% "bimodal",]$mids))),NA, 
                                          unique(bimodal.fia.pc1[bimodal.fia.pc1$bimclass_f %in% "bimodal",]$mids)), y = -37, bimodal = "bimodal")


#pc1.f.bim.line <- data.frame(PC1 = seq(from = -6, to =6, by = 0.05), pval = NA, bimodal = NA)
#pc1.f.bim.line$pval <- apply(data.frame(pc1.f.bim.line$PC1), MARGIN= 1, FUN=interp.densp)
#pc1.f.bim.line$bimodal <- ifelse(pc1.f.bim.line$pval <= 0.05, "bimodal", "unimodal")
#pc1.f.bim.line$y <- -37
#fia.kde.plot.pc1 <- recordPlot()
library(base2grob)
fia.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550), cex.axis=1.5
)+ text(-6,500, "B"))

#plot_grid(pls.kde.plot.pc1.grob, three.color.bimodal.plots, ncol = 2)

# make the plot with GGPLOT:
fia.kde.plot.pc1.gg.clipped <- ggplot(fia.dens.omit, aes(x=PC1, y=fia) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")


fia.kde.plot.pc1.gg.clipped.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,99), ylab = " ",xlab = "PC1",  ylim = c(-40,550),  yaxt="n", cex.axis=0.7) + points(data = pc1.f.bim.line[pc1.f.bim.line$bimodal %in% "bimodal",], y~PC1, cex = 0.9,  pch = 15,col = "red")+ text(-5.5,500, "E"))+ xlab("P-PET")
fia.kde.plot.pc1.gg.clipped.full + xlab("PC1")


# --------------------for P-PET:
H <- Hpi.diag(x=na.omit(cbind(fia.dens.omit$GS_ppet_mod, fia.dens.omit$fia)) )
fhat <- kde(x=na.omit(cbind(fia.dens.omit$GS_ppet_mod, fia.dens.omit$fia)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,75,80,85,95), xlab = "P-PET", ylab = "Tree density")
#points(na.omit(cbind(fia.dens.omit$PC1, fia.dens.omit$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.ppet <- recordPlot()
fia.kde.plot.ppet.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "P-PET", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)



bimodal.fia.ppet <- readRDS("outputs/bimodal_bins_p_value_dipP_fia_PPET_stat.rds")


pvalues <- bimodal.fia.ppet %>% group_by(ppet_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                         median.p = median(pvalue, na.rm = TRUE),
                                                                         ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                         ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                         mean.d = mean(dip, na.rm = TRUE),
                                                                         median.d = median(dip, na.rm = TRUE),
                                                                         ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                         ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$ppet_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(ppet_bins_f = unique(unique(pvalues$ppet_bins)),
                           ppet_mids_f = as.numeric(mids)+7.5)


colnames(pvalues)[1] <- "ppet_bins_f"



bimodal.fia.ppet <- merge(pvalues, ordered.cuts, by = "ppet_bins_f")
bimodal.fia.ppet$bimclass_ppet_f <- ifelse(bimodal.fia.ppet$median.p <= 0.05, "bimodal", "unimodal")

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

ppet.f.bim.line <- data.frame(PPET = ifelse(is.null(nrow(unique(bimodal.fia.ppet[bimodal.fia.ppet$bimclass_ppet_f %in% "bimodal",]$GS_ppet_mod))),NA, 
                                            unique(bimodal.fia.ppet[bimodal.fia.ppet$bimclass_ppet_f %in% "bimodal",]$GS_ppet_mod)), y = -37, bimodal = "bimodal")


fia.kde.plot.pc1 <- recordPlot()
library(base2grob)
fia.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PPET", ylab = NA, ylim = c(-40,550)))


# make the plot with GGPLOT:
fia.kde.plot.ppet.gg.clipped.full <- ggplot(fia.dens.omit, aes(x=PC1, y=fia) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")+ scale_fill_distiller(palette= c("YlOrRd"), direction=1 )+ylab("Tree Density")+theme(legend.position = "none")


fia.kde.plot.ppet.gg.clipped.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab="P-PET",ylab=NA,  ylim = c(-40,550), xlim = c(-200, 300),  yaxt="n" , cex.axis=0.7) + points(data = ppet.f.bim.line[ppet.f.bim.line$bimodal %in% "bimodal",], y~PPET, cex = 0.9,  pch = 15,col = "red")+ text(-170,500, "F"))+xlab("P-PET")
fia.kde.plot.ppet.gg.clipped.full +xlab("P-PET")


# for soil moisture/bucket model

H <- Hpi.diag(x=na.omit(cbind(fia.dens.omit$mean_GS_soil_m, fia.dens.omit$fia)) )
fhat <- kde(x=na.omit(cbind(fia.dens.omit$mean_GS_soil_m, fia.dens.omit$fia)), #H=H, 
            compute.cont = TRUE )

plot(fhat, display="filled.contour2", cont=c(1,5,15,25,30,35,40,35,45,50,60,65,75,80,85,95), xlab = "Growing season soil", ylab = "Tree density")
#points(na.omit(cbind(fia.dens.omit$PC1, fia.dens.omit$PLSdensity)), cex=0.3, pch=16)
plot(fhat, display="slice", cont=c(85), add = TRUE)
fia.kde.plot.sm <- recordPlot()
fia.kde.plot.sm.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "soil moisture", ylab = "Tree density"))

contour_95 <- with(fhat, contourLines(x=eval.points[[1]], y=eval.points[[2]],
                                      z=estimate, levels=cont["15%"])[[1]])
contour_95 <- data.frame(contour_95)




bimodal.fia.sm <- readRDS("outputs/bimodal_bins_p_value_dipP_fia_soil_15bins_kde_stat.rds")


pvalues <- bimodal.fia.sm %>% group_by(soil_bins) %>% dplyr::summarise(mean.p = mean(pvalue, na.rm = TRUE),
                                                                       median.p = median(pvalue, na.rm = TRUE),
                                                                       ci.low.p = quantile(pvalue, 0.025, na.rm = TRUE),
                                                                       ci.high.p = quantile(pvalue, 0.975, na.rm = TRUE),
                                                                       mean.d = mean(dip, na.rm = TRUE),
                                                                       median.d = median(dip, na.rm = TRUE),
                                                                       ci.low.d = quantile(dip, 0.025, na.rm = TRUE),
                                                                       ci.high.d = quantile(dip, 0.975, na.rm = TRUE))

# for pvalues assigned to 1 b/c modes were not above and below 100, assign NA
pvalues$median.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.d)
pvalues$ci.low.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.low.d)
pvalues$ci.high.d <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$ci.high.d)

pvalues$median.p <- ifelse(pvalues$median.p == 1 & pvalues$ci.high.p == 1 & pvalues$ci.low.p == 1, NA, pvalues$median.p)

# get matching bins and mid point values for plotting
first <- strsplit(as.character(unique(pvalues$soil_bins)), c(","))
firsts <- unlist(first) 
x <- 1:length(firsts)
firsts <- firsts[x[!1:length(firsts) %% 2 == 0]]# get only odds
mids <- substring(firsts, first = 2,last = 6)

ordered.cuts <- data.frame(soil_bins_f = unique(unique(pvalues$soil_bins)),
                           soil_mids_f = as.numeric(mids)+0.025)

colnames(pvalues)[1] <- "soil_bins_f"



bimodal.fia.sm <- merge(pvalues, ordered.cuts, by = "soil_bins_f")
bimodal.fia.sm$bimclass_soil_f <- ifelse(bimodal.fia.sm$median.p <= 0.05, "bimodal", "unimodal")

sm.f.bim.line <- data.frame(SM = ifelse(is.null(nrow(unique(bimodal.fia.sm[bimodal.fia.sm$bimclass_soil_f %in% "bimodal",]$mean_GS_soil_m))),NA, 
                                        unique(bimodal.fia.sm[bimodal.fia.sm$bimclass_soil_f %in% "bimodal",]$mean_GS_soil_m)), y = -37, bimodal = "bimodal")



fia.kde.plot.pc1 <- recordPlot()
library(base2grob)
fia.kde.plot.pc1.grob <- base2grob(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "PC1", ylab = "Tree density", ylim = c(-40,550)))

#plot_grid(pls.kde.plot.pc1.grob, three.color.bimodal.plots, ncol = 2)



fia.kde.plot.sm.gg.clipped.full <- as.ggplot(~plot(fhat, display="filled.contour2", cont=c(1,5,10,15,25,30,50,60,75,85,95), xlab = "Soil Moisture", ylab = NA, ylim = c(-40,550), yaxt="n",  cex.axis=0.9) + points(data = sm.f.bim.line[sm.f.bim.line$bimodal %in% "bimodal",], y~SM, cex = 0.8,  pch = 15,col = "red")+ text(0.2,500, "G"))
fia.kde.plot.sm.gg.clipped.full 


# make ggplot figures of cluster density

# need to merge together all of the bimodal/unimodal tags
library(ggplotify)

pls.dens$pc1_bins <- cut(pls.dens$PC1, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.pls.dens <- left_join(pls.dens[,!colnames(pls.dens) %in% "bimclass"], bimodal.pls.pc1, by = "pc1_bins")


pls.dens$ppet_bins <- cut(pls.dens$GS_ppet, breaks=seq(-170, 205, by = 15))
kde.surf.ppet.pls.dens <- left_join(pls.dens[,!colnames(pls.dens) %in% "bimclass_ppet"], bimodal.pls.ppet, by = "ppet_bins")

pls.dens$soil_bins <- cut(pls.dens$mean_GS_soil, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.pls.dens <- left_join(pls.dens[,!colnames(pls.dens) %in% "bimclass_soil"], bimodal.pls.soil, by = "soil_bins")


# now merge all of these together to make a map of 1, 2, 3, bimodal metrics:
#bim.class.m.dens <- left_join(kde.surf.pc1.pls.dens[,c("x", "y", "bimclass")],kde.surf.soil.pls.dens [,c("x", "y", "bimclass_soil")], by = c("x", "y"))
#bim.class.m.dens <- left_join(bim.class.m.dens, kde.surf.ppet.pls.dens[,c("x", "y", "bimclass_ppet", "PLSdensity")])

#bim.class.m$nbimod <- as.character(rowSums(bim.class.m[,3:5] == "bimodal", na.rm = TRUE))
# define nbimod as a category:
#bim.class.m$nbimod <- factor(bim.class.m$nbimod, levels = c("0","1", "2", "3", "No data"))

#three.color.bimodal.plots <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
# geom_raster(data=bim.class.m, aes(x=x, y=y, fill = nbimod))+
#geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
#labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
#), labels = c("0","1", "2", "3", "No data"), drop = F) +
#coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
#                                      panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "D", size = 4)


#one.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "1",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
#two.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "2",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
#three.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "3",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)

#one.bimpct <- one.bimpct + two.bimpct + three.bimpct
#two.bimpct <-  two.bimpct + three.bimpct
#three.bimpct <-  three.bimpct



# left_join for fia
fia.dens.omit$pc1_bins_f <- cut(fia.dens.omit$PC1fia, breaks=seq(-5.5, 4.5, by = 0.25))
kde.surf.pc1.fia.dens.omit <- left_join(fia.dens.omit[,!colnames(fia.dens.omit) %in% "bimclass_f"], bimodal.fia.pc1, by = "pc1_bins_f")

fia.dens.omit$ppet_bins_f <- cut(fia.dens.omit$GS_ppet_mod, breaks=seq(-125, 310, by = 15))
kde.surf.ppet.fia.dens.omit <- left_join(fia.dens.omit[,!colnames(fia.dens.omit) %in% "bimclass_ppet_f"], bimodal.fia.ppet, by = "ppet_bins_f")

fia.dens.omit$soil_bins_f <- cut(fia.dens.omit$mean_GS_soil_m, breaks=seq(0, 1.8, by = 0.05))
kde.surf.soil.fia.dens.omit <- left_join(fia.dens.omit[,!colnames(fia.dens.omit) %in% "bimclass_soil_f"], bimodal.fia.sm, by = "soil_bins_f")


# FIA three color maps
# now left_join all of these together to make a map of 1, 2, 3, bimodal metrics:
#bim.class.m.f <- left_join(kde.surf.pc1.fia.df[,c("x", "y", "bimclass_f")],kde.surf.soil.fia.df [,c("x", "y", "bimclass_soil_f")], by = c("x", "y"))
#bim.class.m.f <- left_join(bim.class.m.f, kde.surf.ppet.fia.df[,c("x", "y", "bimclass_ppet_f", "mean_dens_fia")])

#bim.class.m.f$nbimod <- as.character(rowSums(bim.class.m.f[,3:5] == "bimodal", na.rm = TRUE))
# define nbimod as a category:
#bim.class.m.f$nbimod <- factor(bim.class.m.f$nbimod, levels = c("0","1", "2", "3", "No data"))

#three.color.bimodal.plots.fia <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
# geom_raster(data=bim.class.m.f[!is.na(bim.class.m.f$mean_dens),], aes(x=x, y=y, fill = nbimod))+
#  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
# labs(x="easting", y="northing", title="Prob(forest)")+ scale_fill_manual(values= c("white",'yellow', 'blue',"red","darkgrey"
#), labels = c("0","1", "2", "3", "No data"), drop = F) +
#coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.23),legend.background = element_rect(fill=alpha('transparent', 0 )),legend.key = element_rect(colour = 'black',  size = 0.5, linetype='solid'),
#                                           panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")+ggtitle("")+ annotate("text", x=-90000, y=1486000,label= "D", size = 4)


#one.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "1",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
#two.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "2",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)
#three.bimpct <- round(length(bim.class.m[bim.class.m$nbimod %in% "3",]$nbimod)/length(bim.class.m$nbimod)*100, digits = 2)

#one.bimpct <- one.bimpct + two.bimpct + three.bimpct
#two.bimpct <-  two.bimpct + three.bimpct
#three.bimpct <-  three.bimpct


#kh left off here


# left_join fia and pls bimodal dfs together:
#kde.surf.pc1.df <- left_join(kde.surf.pc1.pls.dens[,c("x", "y",  "pls",  "bimclass","pc1_bins", "pc1_mids")], kde.surf.pc1.fia.dens.omit[,c("x", "y",  "fia","bimclass_f","pc1_bins_f", "pc1_mids_f")], by = c("x", "y"))
#kde.surf.ppet.df <- left_join(kde.surf.ppet.pls.df[,c("x", "y",  "mean_dens",  "bimclass_ppet","ppet_bins", "ppet_mids")], kde.surf.ppet.fia.df[,c("x", "y",  "mean_dens_fia","bimclass_ppet_f", "ppet_bins_f","ppet_mids_f")], by = c("x", "y"), all = TRUE)
#kde.surf.soilm.df <- left_join(kde.surf.soil.pls.df[,c("x", "y",  "mean_dens",  "bimclass_soil", "soil_bins", "soil_mids")], kde.surf.soil.fia.df[,c("x", "y",  "mean_dens_fia","bimclass_soil_f", "soil_bins_f","soil_mids_f")], by = c("x", "y"), all = TRUE)

pc1.bimodal.bins <- unique(kde.surf.pc1.pls.dens[kde.surf.pc1.pls.dens$bimclass %in% "bimodal",]$pc1_bins)
ppet.bimodal.bins <- unique(kde.surf.ppet.pls.dens[kde.surf.ppet.pls.dens$bimclass_ppet %in% "bimodal",]$ppet_bins)
soil.bimodal.bins <- unique(kde.surf.soil.pls.dens[kde.surf.soil.pls.dens$bimclass_soil %in% "bimodal",]$soil_bins)



flipped.pc1.hist.full <- ggplot(kde.surf.pc1.pls.dens[kde.surf.pc1.pls.dens$pc1_bins %in% pc1.bimodal.bins,], aes(pls))+geom_density(color = "blue")+
  geom_density(data = kde.surf.pc1.fia.dens.omit[kde.surf.pc1.fia.dens.omit$pc1_bins_f %in% pc1.bimodal.bins,], aes(fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

flipped.ppet.hist.full <- ggplot(kde.surf.ppet.pls.dens[kde.surf.ppet.pls.dens$ppet_bins %in% ppet.bimodal.bins,], aes(pls))+geom_density(color = "blue")+
  geom_density(data = kde.surf.ppet.fia.dens.omit[kde.surf.ppet.fia.dens.omit$ppet_bins %in% ppet.bimodal.bins,], aes(fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)

flipped.soilm.hist.full <- ggplot(kde.surf.soil.pls.dens[kde.surf.soil.pls.dens$soil_bins %in% soil.bimodal.bins,], aes(pls))+geom_density(color = "blue")+
  geom_density(data = kde.surf.soil.fia.dens.omit[kde.surf.soil.fia.dens.omit$soil_bins %in% soil.bimodal.bins,], aes(fia), color = "red")+coord_flip()+xlab("Tree density")+ylab("Frequency")+theme_bw(base_size = 8)+xlim(0,550)


# alternative: get density lines then ggplotify them to align:
pls.soilm.density.df <- data.frame(y = density(kde.surf.soil.pls.dens[kde.surf.soil.pls.dens$soil_bins %in% soil.bimodal.bins,]$pls)$y, 
                                   x = density(kde.surf.soil.pls.dens[kde.surf.soil.pls.dens$soil_bins %in% soil.bimodal.bins,]$pls)$x)

fia.soilm.density.df <- data.frame(y = density(na.omit(kde.surf.pc1.fia.dens.omit[kde.surf.pc1.fia.dens.omit$pc1_bins_f %in% pc1.bimodal.bins,]$fia))$y, 
                                   x = density(na.omit(kde.surf.pc1.fia.dens.omit[kde.surf.pc1.fia.dens.omit$pc1_bins_f %in% pc1.bimodal.bins,]$fia))$x)
flipped.soilm.hist.gg.clipped.full <- as.ggplot(~plot(fia.soilm.density.df , type = "l", col = "red", ylim = c(-40, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.soilm.density.df[pls.soilm.density.df$x < 550 & pls.soilm.density.df$x > -41,] , type = "l", col = "blue"))



pls.ppet.density.df <- data.frame(y = density(kde.surf.ppet.pls.dens[kde.surf.ppet.pls.dens$ppet_bins %in% ppet.bimodal.bins,]$pls)$y, 
                                  x = density(kde.surf.ppet.pls.dens[kde.surf.ppet.pls.dens$ppet_bins %in% ppet.bimodal.bins,]$pls)$x)

fia.ppet.density.df <- data.frame(y = density(na.omit(kde.surf.ppet.fia.dens.omit[kde.surf.ppet.fia.dens.omit$ppet_bins %in% ppet.bimodal.bins,]$fia))$y, 
                                  x = density(na.omit(kde.surf.ppet.fia.dens.omit[kde.surf.ppet.fia.dens.omit$ppet_bins %in% ppet.bimodal.bins,]$fia))$x)

flipped.ppet.hist.gg.clipped.full <- as.ggplot(~plot(fia.ppet.density.df,type = "l", col = "red", ylim = c(-40, 550),yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.ppet.density.df[pls.ppet.density.df$x < 550 & pls.ppet.density.df$x > -41,] , type = "l", col = "blue"))


pls.pc1.density.df <- data.frame(y = density(kde.surf.pc1.pls.dens[kde.surf.pc1.pls.dens$pc1_bins %in% pc1.bimodal.bins,]$pls)$y, 
                                 x = density(kde.surf.pc1.pls.dens[kde.surf.pc1.pls.dens$pc1_bins %in% pc1.bimodal.bins,]$pls)$x)

fia.pc1.density.df <- data.frame(y = density(na.omit(kde.surf.pc1.fia.dens.omit[kde.surf.pc1.fia.dens.omit$pc1_bins_f %in% pc1.bimodal.bins,]$fia))$y, 
                                 x = density(na.omit(kde.surf.pc1.fia.dens.omit[kde.surf.pc1.fia.dens.omit$pc1_bins_f %in% pc1.bimodal.bins,]$fia))$x)
flipped.pc1.hist.gg.clipped <- as.ggplot(~plot(fia.pc1.density.df[fia.pc1.density.df$x < 550 ,], type = "l", col = "red", ylim = c(-41, 550), yaxt="n", ylab = NA, xlab = NA, xaxt = "n") + lines(pls.pc1.density.df[pls.pc1.density.df$x < 550,], type = "l", col = "blue"))

library(gtable)
g1 <- ggplotGrob(pls.kde.plot.pc1.gg.clipped.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-0.1), "cm")))
g2 <- ggplotGrob(fia.kde.plot.pc1.gg.clipped.full+theme(plot.margin=unit(c(-0.7,-1,-0.5,-1), "cm")))
g3 <- ggplotGrob(flipped.pc1.hist.gg.clipped.full+theme(plot.margin=unit(c(-0.7,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g4 <- ggplotGrob(pls.kde.plot.ppet.gg.clipped.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g5 <- ggplotGrob(fia.kde.plot.ppet.gg.clipped.full+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g6 <- ggplotGrob(flipped.ppet.hist.gg.clipped.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))

g7 <- ggplotGrob(pls.kde.plot.sm.gg.clipped.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-0.1), "cm")))
g8 <- ggplotGrob(fia.kde.plot.sm.gg.clipped.full+theme(plot.margin=unit(c(-0.9,-1,-0.5,-1), "cm")))
g9 <- ggplotGrob(flipped.soilm.hist.gg.clipped.full+theme(plot.margin=unit(c(-0.9,-0.1,-0.5,-1), "cm"))) #+xlim(-40, 550) + theme(axis.ticks.y = element_blank(), axis.title.y = element_blank(), axis.text.y =element_blank(), axis.text.x = element_text(size = 8, angle = 45, vjust = 0.5)))
g10 <- ggplotGrob(three.color.bimodal.plots)
g11 <- ggplotGrob(three.color.bimodal.plots.fia)

g <- cbind(g1 ,g2, g3, size = "first")
g$heights <-unit.pmax(g1$heights, g2$heights, g3$heights)#, 

grow2 <- cbind(g4 ,g5, g6, size = "first")
grow2$heights <-unit.pmax(g4$heights, g5$heights, g6$heights)#, 

grow3 <- cbind(g7 ,g8, g9, size = "first")
grow3$heights <-unit.pmax(g7$heights, g8$heights, g9$heights)#, 
grow4 <- cbind(g10, g11, size = "first")
grow4$heights <-unit.pmax(g10$heights, g11$heights)
#png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/new_figure_3.png")
grid.arrange(arrangeGrob(g1,g2,g3, ncol=3, nrow=1, widths = c(1,1,0.2)), 
             arrangeGrob(g4, g5, g6, ncol = 3, nrow = 1, widths = c(1,1,0.2)) ,
             arrangeGrob(g7,g8, g9, ncol = 3, nrow = 1, widths = c(1,1,0.2)), 
             arrangeGrob(g10, g11, ncol = 3, nrow = 1, widths = c(1,1, 0)))
#dev.off()


png(height = 10, width = 6, units = "in", res = 500, "outputs/paper_figs_unc/new_figure_3_kde_plot_with_hist_uncertainty_full_stats_clipped_FIA.png")
fig3 <- grid.arrange(g, grow2, grow3,grow4, ncol = 1)
fig3
dev.off()

