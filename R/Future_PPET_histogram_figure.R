library(ggplot2)
library(cowplot)
library(tidyr)
library(dplyr)
library(maps)
library(sp)

dens.pr <- read.csv("outputs/Future_PCA.csv")
fia.ppet <-ggplot(dens.pr, aes(GS_ppet_mod))+geom_histogram()+xlim(-175, 300)+xlab("FIA growing season P-PET")
pls.ppet <-ggplot(dens.pr, aes(GS_ppet))+geom_histogram()+xlim(-175, 300)+xlab("PLS growing season P-PET")
fut.ppet <- ggplot(dens.pr, aes(mean_ppet_GS))+geom_histogram()+xlim(-175, 300)+xlab("RCP 8.5 growing season P-PET")



state.summaries <- read.csv( "outputs/state_mean_ppet.csv" )
basic.fut.ppet <- ggplot(dens.pr, aes(mean_ppet_GS))+geom_histogram()+xlim(-175, 300)+xlab("RCP 8.5 growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.605, 0.43))

png(height = 10, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/future_ppet_with_extra_states.png")
plot_grid(pls.ppet, fia.ppet, basic.fut.ppet, ncol = 1, align = "hv")
dev.off()

# lets make the plot but with past bimodal shaded:
pls.nona <- read.csv( "outputs/mixture_model/pls_ppet_mixture_mode_estimates.csv")

mid.summary.ppet <- pls.nona %>% group_by(mode, GS_ppet, mids_ppet) %>% summarise(mean = mean(mean_dens),
                                                                                         ci.low = quantile(mean_dens,0.025),
                                                                                         ci.high = quantile(mean_dens, 0.975))

#ggplot(mid.summary.ppet, aes(mids_ppet, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.ppet, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)


mid.summary.lowprob <- pls.nona %>% group_by(prob_ppet >= 0.4999 & prob_ppet <= 0.5099 ,  GS_ppet_bins, mids_ppet) %>% summarise(mean = mean(mean_dens),
                                                                                                                                  ci.low = quantile(mean_dens, 0.025),
                                                                                                                                  ci.high = quantile(mean_dens, 0.975))


# find the range where there are two modes:
# for each mids_ppet, if there is a "Forest" and a "Savanna" column, then we call it bimodal:
bins.summary.ppet <- pls.nona %>% group_by(mode, GS_ppet_bins, mids_ppet) %>% dplyr::summarise(mean = mean(mean_dens),
                                                                                  ci.low = quantile(mean_dens,0.025),
                                                                                  ci.high = quantile(mean_dens, 0.975))


test <- bins.summary.ppet %>% group_by(GS_ppet_bins) %>% spread(key = mode, value = 1)
test$Savanna <- ifelse(!is.na(test$Savanna), 1, 0)
test$Forest <- ifelse(!is.na(test$Forest), 1, 0)
nstates <- test %>% group_by(GS_ppet_bins, mids_ppet) %>% dplyr::summarise(nstates = sum(Forest, Savanna))

ggplot(nstates, aes(mids_ppet, nstates))+geom_point()
bimodal.region <- nstates[nstates$nstates > 1,] %>% group_by(nstates) %>% dplyr::summarise(min = min(mids_ppet), 
                                                                                    xmax = max(mids_ppet), 
                                                                                    ymin = 0, ymax = 5000)


unimodal.open.region <- nstates[nstates$nstates == 1 & nstates$mids_ppet < 0,] %>% group_by(nstates) %>% dplyr::summarise(min = min(mids_ppet), 
                                                                                    xmax = max(mids_ppet)+15, 
                                                                                    ymin = 0, ymax = 5000)


pls.ppet.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(GS_ppet), binwidth = 10)+xlim(-175, 300)+xlab("PLS growing season P-PET")+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")
  
fia.ppet.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(GS_ppet_mod), binwidth = 10)+xlim(-175, 300)+xlab("FIA growing season P-PET")+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")


fut.ppet.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS), binwidth = 10)+xlim(-175, 300)+xlab("RCP 8.5 future growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")


fut.ppet2.6.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_2.6), binwidth = 10)+xlim(-175, 300)+xlab("RCP 2.6 future growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")

fut.ppet4.5.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_4.5), binwidth = 10)+xlim(-175, 300)+xlab("RCP 4.5 future growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")

fut.ppet6.0.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_6.0), binwidth = 10)+xlim(-175, 300)+xlab("RCP 6.0 future growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")



# plot the 2028-2050 ppet:

fut.ppet.rect.2028 <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_8.5_2028_2058), binwidth = 10)+xlim(-175, 300)+xlab("RCP 8.5 future growing season P-PET (2028 - 2058)")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")


fut.ppet2.6.rect.2028 <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_2.6_2028_2058), binwidth = 10)+xlim(-175, 300)+xlab("RCP 2.6 future growing season P-PET (2028 - 2058)")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")

fut.ppet4.5.rect.2028 <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_4.5_2028_2058), binwidth = 10)+xlim(-175, 300)+xlab("RCP 4.5 future growing season P-PET (2028 - 2059)")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")

fut.ppet6.0.rect.2028 <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_6.0_2028_2058), binwidth = 10)+xlim(-175, 300)+xlab("RCP 6.0 future growing season P-PET (2028 - 2059)")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")



png(height = 14, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/future_ppet_with_extra_states_bimodal_shading.png")
plot_grid(pls.ppet.rect, fia.ppet.rect,fut.ppet2.6.rect, fut.ppet4.5.rect, fut.ppet6.0.rect,  fut.ppet.rect, ncol = 1, align = "hv")
dev.off()    

# combine the two time periods on one graph:
rcp26 <- dens.pr %>% select(x,y, mean_ppet_GS_2.6, mean_ppet_GS_2.6_2028_2058) %>% rename( `2059-2099` = mean_ppet_GS_2.6, 
                                                                                           `2028-2058` = mean_ppet_GS_2.6_2028_2058) %>% gather(key = period, value = GS_ppet_fut, -c(x,y))
rcp45 <- dens.pr %>% select(x,y, mean_ppet_GS_4.5, mean_ppet_GS_4.5_2028_2058)%>% rename( `2059-2099` = mean_ppet_GS_4.5, 
                                                                                          `2028-2058` = mean_ppet_GS_4.5_2028_2058) %>% gather(key = period, value = GS_ppet_fut, -c(x,y))
rcp60 <- dens.pr %>% select(x,y, mean_ppet_GS_6.0, mean_ppet_GS_6.0_2028_2058)%>% rename( `2059-2099` = mean_ppet_GS_6.0, 
                                                                                          `2028-2058` = mean_ppet_GS_6.0_2028_2058) %>% gather(key = period, value = GS_ppet_fut, -c(x,y))
rcp85 <- dens.pr %>% select(x,y, mean_ppet_GS, mean_ppet_GS_8.5_2028_2058)%>% rename( `2059-2099` = mean_ppet_GS, 
                                                                                      `2028-2058` = mean_ppet_GS_8.5_2028_2058) %>% gather(key = period, value = GS_ppet_fut, -c(x,y))


# now plot out with different fill colors for the two time periods:
fut.ppet.rect.both <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = rcp85, aes(GS_ppet_fut, fill = period), binwidth = 10)+scale_fill_manual(values = c("#cccccc", "#525252"))+xlim(-175, 300)+xlab("RCP 8.5 future growing season P-PET (2028 - 2058)")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")


fut.ppet2.6.rect.both <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = rcp26, aes(GS_ppet_fut, fill = period), binwidth = 10)+scale_fill_manual(values = c("#cccccc", "#525252"))+xlim(-175, 300)+xlab("RCP 2.6 future growing season P-PET ")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")

fut.ppet4.5.rect.both <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = rcp45, aes(GS_ppet_fut, fill = period), binwidth = 10)+scale_fill_manual(values = c("#cccccc", "#525252"))+xlim(-175, 300)+xlab("RCP 4.5 future growing season P-PET ")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")

fut.ppet6.0.rect.both <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = rcp60, aes(GS_ppet_fut, fill = period), binwidth = 10)+scale_fill_manual(values = c("#cccccc", "#525252"))+xlim(-175, 300)+xlab("RCP 6.0 future growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")


png(height = 16, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/future_ppet_with_extra_states_bimodal_shading_both_time_periods.png")
plot_grid(pls.ppet.rect, fia.ppet.rect,fut.ppet2.6.rect.both, fut.ppet4.5.rect.both, fut.ppet6.0.rect.both,  fut.ppet.rect.both, ncol = 1, align = "hv", labels = "AUTO")
dev.off()   

png(height = 10, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/future_ppet_with_extra_states_bimodal_shading_both_time_periods_8.5.png")
plot_grid(pls.ppet.rect, fia.ppet.rect, fut.ppet.rect.both, ncol = 1, align = "hv", labels = "AUTO")
dev.off()  

# now map out the places that are bimodal vs. 
# kh note: need to decide if we should call < 50 grid cells represented to be "out-of-sample"

#dens.pr[duplicated(dens.pr[,c("x", "y")]),]

ncell_grid_cell <- pls.nona %>% group_by(GS_ppet_bins, mids_ppet) %>% summarise(ncell = length(GS_ppet))
dens.pr$future_class <- ifelse(dens.pr$mean_ppet_GS >= unimodal.open.region$min & dens.pr$mean_ppet_GS < unimodal.open.region$xmax, "Open", 
                               ifelse( dens.pr$mean_ppet_GS >= bimodal.region$min & dens.pr$mean_ppet_GS < bimodal.region$xmax, "Bimodal", "no sample"))
dens.pr$future_class <- ifelse(is.na(dens.pr$future_class), "no data", dens.pr$future_class)

ggplot(dens.pr, aes(x,y, fill = future_class))+geom_raster()


all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota', 'wisconsin', 'michigan', "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)



bimod.future.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.pr, aes(x=x, y=y, fill = future_class))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("Bimodal"='#d73027',"Open" = 'tan', "no data" = "grey"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")


dens.pr$future_class_fia <- ifelse(dens.pr$mean_ppet_GS >= min(dens.pr$GS_ppet_mod, na.rm = TRUE), "Forest", 
                                   ifelse(dens.pr$mean_ppet_GS <  min(dens.pr$GS_ppet_mod, na.rm = TRUE), "out-of-sample","no sample"))
dens.pr$future_class_fia <- ifelse(is.na(dens.pr$future_class_fia), "no data", dens.pr$future_class_fia)


bimod.future.fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.pr, aes(x=x, y=y, fill = future_class_fia))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("Forest"='forestgreen',"Open" = 'tan', "no data" = "grey"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")




png(height = 12, width = 10, units = "in", res = 300, "outputs/paper_figs_unc/future_ppet_with_extra_states_bimodal_shading_maps.png")
plot_grid(plot_grid(pls.ppet.rect, fia.ppet.rect, fut.ppet.rect, ncol = 1, align = "hv"), bimod.future.map,ncol = 2)
          
dev.off()  


#-----------------------redo, with climate classes with < 50 grid cells as low sample----------------

mid.summary.ppet <- pls.nona %>% group_by(mode, GS_ppet, mids_ppet) %>% summarise(mean = mean(mean_dens),
                                                                                  ci.low = quantile(mean_dens,0.025),
                                                                                  ci.high = quantile(mean_dens, 0.975), 
                                                                                  ncell = length(mean_dens))

#ggplot(mid.summary.ppet, aes(mids_ppet, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.ppet, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)


mid.summary.lowprob <- pls.nona %>% group_by(prob_ppet >= 0.4999 & prob_ppet <= 0.5099 ,  GS_ppet_bins, mids_ppet) %>% summarise(mean = mean(mean_dens),
                                                                                                                                 ci.low = quantile(mean_dens, 0.025),
                                                                                                                                 ci.high = quantile(mean_dens, 0.975))


# find the range where there are two modes:
# for each mids_ppet, if there is a "Forest" and a "Savanna" column, then we call it bimodal:
bins.summary.ppet <- pls.nona %>% group_by(mode, GS_ppet_bins, mids_ppet) %>% summarise(mean = mean(mean_dens),
                                                                                        ci.low = quantile(mean_dens,0.025),
                                                                                        ci.high = quantile(mean_dens, 0.975),
                                                                                        ncell = length(mean_dens))


test <- bins.summary.ppet %>% group_by(GS_ppet_bins) %>% spread(key = mode, value = 1)
test$Savanna <- ifelse(!is.na(test$Savanna), 1, 0)
test$Forest <- ifelse(!is.na(test$Forest), 1, 0)
nstates <- test %>% group_by(GS_ppet_bins, mids_ppet) %>% summarise(nstates = sum(Forest, Savanna),
                                                                    ncell = sum(ncell))

ggplot(nstates, aes(mids_ppet, ncell,color = nstates))+geom_point()+geom_hline(yintercept = 50)
bimodal.region <- nstates[nstates$nstates > 1,] %>% group_by(nstates) %>% summarise(min = min(mids_ppet), 
                                                                                    xmax = max(mids_ppet), 
                                                                                    ymin = 0, ymax = 5000)


unimodal.open.region <- nstates[nstates$nstates == 1 & nstates$mids_ppet < 0 & nstates$ncell <= 50,] %>% group_by(nstates) %>% summarise(min = min(mids_ppet), 
                                                                                                                   xmax = max(mids_ppet)+15, 
                                                                                                                   ymin = 0, ymax = 5000)
# get the unimodal and bimodal regions for FIA:
fia.nona <- read.csv( "outputs/mixture_model/fia_ppet_mixture_mode_estimates.csv")

mid.summary.fia <- fia.nona %>% group_by(mode, GS_ppet_mod_bins, mids) %>% summarise(mean = mean(mean_dens_fia),
                                                                                        ci.low = quantile(mean_dens_fia,0.025),
                                                                                        ci.high = quantile(mean_dens_fia, 0.975))

#ggplot(mid.summary.soil, aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.soil, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)


#find the range where there are two modes:
# for each mids, if there is a "Forest" and a "Savanna" column, then we call it bimodal:
bins.summary.ppet.fia <- fia.nona %>% group_by(mode, GS_ppet_mod_bins, mids) %>% summarise(mean = mean(mean_dens_fia),
                                                                                              ci.low = quantile(mean_dens_fia,0.025),
                                                                                              ci.high = quantile(mean_dens_fia, 0.975),
                                                                                              ncell = length(mean_dens_fia))


test <- bins.summary.ppet.fia %>% group_by(GS_ppet_mod_bins) %>% spread(key = mode, value = 1)
test$Savanna <- ifelse(!is.na(test$`Low Density Forest`), 1, 0)
test$Forest <- ifelse(!is.na(test$Forest), 1, 0)
nstates.fia <- test %>% group_by(GS_ppet_mod_bins, mids) %>% summarise(nstates = sum(Forest, Savanna))
ncell.fia <- bins.summary.ppet.fia  %>% group_by(GS_ppet_mod_bins, mids) %>% summarise(ncell = sum(ncell))
nstates.fia <- merge(ncell.fia, nstates.fia, by = c("GS_ppet_mod_bins", "mids"))
ggplot(nstates.fia, aes(mids, nstates))+geom_point()

unimodal.forest.fia <- nstates.fia[nstates.fia$nstates == 1.00 & nstates.fia$ncell > 50 ,] %>% group_by(nstates) %>% summarise(min = min(mids), 
                                                                                                                               xmax = max(mids), ymin = 0, ymax = 5000)
                                                                                                                               

pls.ppet.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(GS_ppet))+xlim(-175, 300)+xlab("PLS growing season P-PET")+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal Past", color = "black")+
  annotate(geom = "text", x = -134, y = 4500, label = "Savanna \n low sample", color = "black")

fia.ppet.rect <- ggplot()+#geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.forest.fia) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(GS_ppet_mod))+xlim(-175, 300)+xlab("FIA growing season P-PET")+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal Past \n Unimodal Forest Modern", color = "black")#+
  #annotate(geom = "text", x = -134, y = 4500, label = "Savanna \n low sample", color = "black")


fut.ppet.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.forest.fia) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS))+xlim(-175, 300)+xlab("RCP 8.5 future growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.805, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal Past \n Unimodal Modern", color = "black")+
  annotate(geom = "text", x = -134, y = 4500, label = "Savanna \n low sample", color = "black")

dev.off()
png(height = 10, width = 7, units = "in", res = 300, "outputs/paper_figs_unc/future_ppet_with_extra_states_bimodal_shading_low_sample.png")
plot_grid(pls.ppet.rect, fia.ppet.rect, fut.ppet.rect, ncol = 1, align = "hv", labels = "AUTO")
dev.off()    


# now map out the places that are bimodal vs. 
# kh note: need to decide if we should call < 50 grid cells represented to be "out-of-sample"

#dens.pr[duplicated(dens.pr[,c("x", "y")]),]

ncell_grid_cell <- pls.nona %>% group_by(GS_ppet_bins, mids_ppet) %>% summarise(ncell = length(GS_ppet))
dens.pr$future_class <- ifelse(dens.pr$mean_ppet_GS >= unimodal.open.region$min & dens.pr$mean_ppet_GS < unimodal.open.region$xmax, "Open/Low Sample", 
                               ifelse( dens.pr$mean_ppet_GS >= bimodal.region$min & dens.pr$mean_ppet_GS < bimodal.region$xmax, "Bimodal", "no sample"))
dens.pr$future_class <- ifelse(is.na(dens.pr$future_class), "no data", dens.pr$future_class)

ggplot(dens.pr, aes(x,y, fill = future_class))+geom_raster()


all_states <- map_data("state")
states <- subset(all_states, region %in% c(  'minnesota', 'wisconsin', 'michigan', "illinois",  'indiana') )
coordinates(states)<-~long+lat
class(states)
proj4string(states) <-CRS("+proj=longlat +datum=NAD83")
mapdata<-spTransform(states, CRS('+init=epsg:3175'))
mapdata <- data.frame(mapdata)



bimod.future.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.pr, aes(x=x, y=y, fill = future_class))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("Bimodal"='#d73027',"Open" = 'tan', "no data" = "grey"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")


dens.pr$future_class_fia <- ifelse(dens.pr$mean_ppet_GS >= min(dens.pr$GS_ppet_mod, na.rm = TRUE), "Forest", 
                                   ifelse(dens.pr$mean_ppet_GS <  min(dens.pr$GS_ppet_mod, na.rm = TRUE), "out-of-sample","no sample"))
dens.pr$future_class_fia <- ifelse(is.na(dens.pr$future_class_fia), "no data", dens.pr$future_class_fia)


bimod.future.fia.map <- ggplot()+ geom_polygon(data = mapdata, aes(group = group,x=long, y =lat), fill = 'darkgrey')+
  geom_raster(data=dens.pr, aes(x=x, y=y, fill = future_class_fia))+
  geom_polygon(data = mapdata, aes(group = group,x=long, y =lat),colour="black", fill = NA)+
  labs(x="easting", y="northing")+ scale_fill_manual(values= c("Forest"='forestgreen',"Open" = 'tan', "no data" = "grey"
  )) +
  coord_equal()+theme_bw(base_size = 8)+theme(axis.text = element_blank(),axis.title = element_blank(), axis.ticks=element_blank(),legend.key.size = unit(0.25,'lines'), legend.position = c(0.205, 0.13),legend.background = element_rect(fill=alpha('transparent', 0)),
                                              panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = " ")




png(height = 12, width = 10, units = "in", res = 300, "outputs/paper_figs_unc/future_ppet_with_extra_states_bimodal_shading_maps_low_sample.png")
plot_grid(plot_grid(pls.ppet.rect, fia.ppet.rect, fut.ppet.rect, ncol = 1, align = "hv"), bimod.future.map,ncol = 2)

dev.off()  

