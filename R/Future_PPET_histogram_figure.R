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

############get FIA unimodal
# lets make the plot but with past bimodal shaded:

# get 
fia.nona <- read.csv( "outputs/mixture_model/fia_ppet_mixture_mode_estimates.csv")
bins.summary.ppet.fia <- fia.nona %>% group_by(mode, GS_ppet_mod_bins, mids) %>% dplyr::summarise(mean = mean(mean_dens_fia),
                                                                                                ci.low = quantile(mean_dens_fia,0.025),
                                                                                                ci.high = quantile(mean_dens_fia, 0.975), 
                                                                                                ncell = n())

test <- bins.summary.ppet.fia  %>% group_by(GS_ppet_mod_bins) %>% spread(key = mode, value = 1)
test$Savanna <- test$`Low Density Forest`
test$Savanna <- ifelse(!is.na(test$Savanna), 1, 0)
test$Forest <- ifelse(!is.na(test$Forest), 1, 0)
nstates <- test %>% group_by(GS_ppet_mod_bins, mids) %>% dplyr::summarise(nstates = sum(Forest, Savanna), 
                                                                          ncell_tot = sum(ncell ))

# 
# ggplot(nstates, aes(mids, nstates))+geom_point()
# bimodal.region <- nstates[nstates$nstates > 1,] %>% group_by(nstates) %>% dplyr::summarise(min = min(mids), 
#                                                                                            xmax = max(mids), 
#                                                                                            ymin = 0, ymax = 5000)
# 
# 
unimodal.fia.region <- nstates[nstates$ncell_tot >= 50, ] %>% group_by(nstates) %>% dplyr::summarise(min = min(mids), 
                                                                                                                         xmax = max(mids)+15, 
                                                                                                                           ymin = 0, ymax = 5000) %>% filter(nstates == 1)


pls.ppet.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(GS_ppet), binwidth = 10)+xlim(-175, 300)+xlab("PLS growing season P-PET")+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")
  
fia.ppet.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(GS_ppet_mod), binwidth = 10)+xlim(-175, 300)+xlab("FIA growing season P-PET")+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")

# plot p-pet range + bars below the figures:

full.GS <- dens.pr[,c("x", "y","GS_ppet", "GS_ppet_mod")]
colnames(full.GS) <- c("x", "y", "Past", "Modern")
full.GS.m <- reshape2::melt(full.GS, id.vars= c("x", "y"))

pls.fia.ppet.bars <- ggplot() + geom_histogram(data = full.GS.m, aes(value, fill = variable), position = "identity", binwidth = 10, alpha = 0.45)+scale_fill_manual(values = c("Past" = "#003c30", "Modern" = "#80cdc1"))+theme_bw(base_size = 12)+
  theme(panel.grid = element_blank(),legend.title = element_blank(), legend.position = c(0.1, 0.9))+xlab("P - PET")



pls.ppet.bars.below <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax =xmax,ymin = ymin, ymax = 400), fill = "#003c30", alpha = 0.85, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax, ymin = ymin, ymax = 400), fill = "#8c510a", alpha = 0.95, color = "black")+
  annotate(geom = "text", x = 40, y = 200, label = "Bimodal Past", color = "black")+
  annotate(geom = "text", x = -130, y = 200, label = "Open", color = "black")+theme_nothing()+xlim(-175, 300)

fia.ppet.bars <- ggplot()+#geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  #geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(GS_ppet_mod), binwidth = 10)+xlim(-175, 300)+xlab("FIA growing season P-PET")#+
  #annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  #annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")


fia.ppet.bars.below <- ggplot()+#geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax =xmax,ymin = ymin, ymax = 400), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.fia.region) , aes(xmin = min + 5, xmax = xmax, ymin = ymin, ymax = 400), fill = "#80cdc1", alpha = 0.95, color = "black")+
  annotate(geom = "text", x = 65, y = 200, label = "Unimodal Forest Modern", color = "black")+
  #annotate(geom = "text", x = -130, y = 200, label = "Savanna", color = "black")+
  theme_nothing()+xlim(-175, 300) +
  geom_segment(data=state.summaries[state.summaries$mean < 0 & state.summaries$mean > -115,], aes(x = mean, xend=mean,y = 0, yend=400, color = state), size = 1.5)+scale_color_manual(values = c( "black","blue","yellow"))

png(height = 8, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/test_bar_fut.png")
plot_grid(pls.ppet.bars, pls.ppet.bars.below, fia.ppet.bars, fia.ppet.bars.below,align = "v", ncol = 1, rel_heights = c(1,0.15, 1, 0.15))
dev.off()


fut.ppet.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS), binwidth = 10)+xlim(-175, 300)+xlab("RCP 8.5 future growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+scale_color_manual(values = c("yellow", "black", "blue"))+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")


fut.ppet2.6.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_2.6), binwidth = 10)+xlim(-175, 300)+xlab("RCP 2.6 future growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+scale_color_manual(values = c("yellow", "black", "blue"))+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")

fut.ppet4.5.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_4.5), binwidth = 10)+xlim(-175, 300)+xlab("RCP 4.5 future growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+scale_color_manual(values = c("yellow", "black", "blue"))+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")

fut.ppet6.0.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_6.0), binwidth = 10)+xlim(-175, 300)+xlab("RCP 6.0 future growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+scale_color_manual(values = c("yellow", "black", "blue"))+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")



# plot the 2028-2050 ppet:

fut.ppet.rect.2028 <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_8.5_2028_2058), binwidth = 10)+xlim(-175, 300)+xlab("RCP 8.5 future growing season P-PET (2028 - 2058)")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+scale_color_manual(values = c("yellow", "black", "blue"))+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")


fut.ppet2.6.rect.2028 <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_2.6_2028_2058), binwidth = 10)+xlim(-175, 300)+xlab("RCP 2.6 future growing season P-PET (2028 - 2058)")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+scale_color_manual(values = c("yellow", "black", "blue"))+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")

fut.ppet4.5.rect.2028 <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_4.5_2028_2058), binwidth = 10)+xlim(-175, 300)+xlab("RCP 4.5 future growing season P-PET (2028 - 2059)")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+scale_color_manual(values = c("yellow", "black", "blue"))+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")

fut.ppet6.0.rect.2028 <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS_6.0_2028_2058), binwidth = 10)+xlim(-175, 300)+xlab("RCP 6.0 future growing season P-PET (2028 - 2059)")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+scale_color_manual(values = c("yellow", "black", "blue"))+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")



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
  geom_histogram(data = rcp85, aes(GS_ppet_fut, fill = period), binwidth = 10)+scale_fill_manual(values = c("#cccccc", "#525252"))+xlim(-175, 300)+xlab("RCP 8.5 future growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+scale_color_manual(values = c("yellow", "black", "blue"))+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")


fut.ppet2.6.rect.both <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = rcp26, aes(GS_ppet_fut, fill = period), binwidth = 10)+scale_fill_manual(values = c("#cccccc", "#525252"))+xlim(-175, 300)+xlab("RCP 2.6 future growing season P-PET ")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+scale_color_manual(values = c("yellow", "black", "blue"))+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")

fut.ppet4.5.rect.both <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = rcp45, aes(GS_ppet_fut, fill = period), binwidth = 10)+scale_fill_manual(values = c("#cccccc", "#525252"))+xlim(-175, 300)+xlab("RCP 4.5 future growing season P-PET ")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+scale_color_manual(values = c("yellow", "black", "blue"))+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")

fut.ppet6.0.rect.both <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = rcp60, aes(GS_ppet_fut, fill = period), binwidth = 10)+scale_fill_manual(values = c("#cccccc", "#525252"))+xlim(-175, 300)+xlab("RCP 6.0 future growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+scale_color_manual(values = c("yellow", "black", "blue"))+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")


png(height = 16, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/future_ppet_with_extra_states_bimodal_shading_both_time_periods.png")
plot_grid(pls.ppet.rect, fia.ppet.rect,fut.ppet2.6.rect.both, fut.ppet4.5.rect.both, fut.ppet6.0.rect.both,  fut.ppet.rect.both, ncol = 1, align = "hv", labels = "AUTO")
dev.off()   

png(height = 10, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/future_ppet_with_extra_states_bimodal_shading_both_time_periods_8.5.png")
plot_grid(pls.ppet.rect, fia.ppet.rect, fut.ppet.rect.both, ncol = 1, align = "hv", labels = "AUTO")
dev.off()  

state.summaries.three <- state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,]
state.summaries.three$state <- factor(state.summaries.three$state, levels = c("oklahoma", "kansas", "nebraska"))

# future plot without the boxes behind
fut.ppet.8.5.both <- ggplot()+#geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  #geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = rcp85, aes(GS_ppet_fut, fill = period), binwidth = 10)+scale_fill_manual(values = c("#bdbdbd", "#525252"), name = "future period")+xlim(-175, 300)+xlab("RCP 8.5 future growing season P-PET")+
  geom_vline(data = state.summaries.three, aes(xintercept = mean, color = state), linetype = "dashed")+scale_color_manual(values = c("yellow", "black", "blue"), name = "modern state")+theme_bw(base_size = 12)+
  theme(legend.position  = c(0.805, 0.53), panel.grid = element_blank())#+
  #annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  #annotate(geom = "text", x = -130, y = 4500, label = "Savanna", color = "black")



png(height = 9, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/Future_rcp_8.5_bar_bimodal_combined.png")
plot_grid(pls.fia.ppet.bars + xlab("Growing Season P - PET")+ylab("# of grid cells"), pls.ppet.bars.below, fia.ppet.bars.below, fut.ppet.8.5.both+ylab("# of grid cells"), align = "v", ncol = 1, rel_heights = c(1, 0.15, 0.15, 1), 
          labels = c("A", "B", " ", "C"))
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

