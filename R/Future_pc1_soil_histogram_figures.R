library(ggplot2)
library(cowplot)
library(tidyr)
library(dplyr)
library(maps)
library(sp)

dens.pr <- read.csv("outputs/Future_PCA.csv")
fia.ppet <-ggplot(dens.pr, aes(PC1fia))+geom_histogram()+xlim(-5,5)+xlab("FIA PC1")
pls.ppet <-ggplot(dens.pr, aes(PC1))+geom_histogram()+xlim(-5,5)+xlab("PLS PC1")
fut.ppet <- ggplot(dens.pr, aes(PC1_cc85))+geom_histogram()+xlim(-5,5)+xlab("RCP 8.5 PC1")



state.summaries <- read.csv( "outputs/state_mean_ppet.csv" )
basic.fut.ppet <- ggplot(dens.pr, aes(PC1))+geom_histogram()+xlim(-175, 300)+xlab("RCP 8.5 growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.605, 0.43))


# lets make the plot but with past bimodal shaded:
pls.nona <- read.csv( "outputs/mixture_model/pls_pc1_mixture_mode_estimates.csv")

mid.summary.pc1 <- pls.nona %>% group_by(mode, pc1_bins, mids) %>% summarise(mean = mean(mean_dens),
                                                                                  ci.low = quantile(mean_dens,0.025),
                                                                                  ci.high = quantile(mean_dens, 0.975))

#ggplot(mid.summary.pc1, aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.pc1, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)


mid.summary.lowprob <- pls.nona %>% group_by(prob >= 0.4999 & prob <= 0.5099 ,  pc1_bins, mids) %>% summarise(mean = mean(mean_dens),
                                                                                                                                 ci.low = quantile(mean_dens, 0.025),
                                                                                                                                 ci.high = quantile(mean_dens, 0.975))


# find the range where there are two modes:
# for each mids, if there is a "Forest" and a "Savanna" column, then we call it bimodal:
bins.summary.pc1 <- pls.nona %>% group_by(mode, pc1_bins, mids) %>% summarise(mean = mean(mean_dens),
                                                                                        ci.low = quantile(mean_dens,0.025),
                                                                                        ci.high = quantile(mean_dens, 0.975),
                                                                               ncell = length(mean_dens))


test <- bins.summary.pc1 %>% group_by(pc1_bins) %>% spread(key = mode, value = 1)
test$Savanna <- ifelse(!is.na(test$Savanna), 1, 0)
test$Forest <- ifelse(!is.na(test$Forest), 1, 0)
nstates <- test %>% group_by(pc1_bins, mids) %>% summarise(nstates = sum(Forest, Savanna))
ncell <- bins.summary.pc1 %>% group_by(pc1_bins, mids) %>% summarise(ncell = sum(ncell))
nstates <- merge(ncell, nstates, by = c("pc1_bins", "mids"))
ggplot(nstates, aes(mids, nstates))+geom_point()
bimodal.region <- nstates[nstates$nstates > 1 & nstates$ncell > 50,] %>% group_by(nstates) %>% summarise(min = min(mids), 
                                                                                    xmax = max(mids), 
                                                                                    ymin = 0, ymax = 5000)


unimodal.open.region <- nstates[nstates$nstates == 1 & nstates$ncell <= 50,] %>% group_by(nstates) %>% summarise(min = min(mids), 
                                                                                                                   xmax = max(mids)+0.25, 
                                                                                                                   ymin = 0, ymax = 5000)
# get the unimodal and bimodal regions for FIA:
fia.nona <- read.csv( "outputs/mixture_model/fia_pc1_mixture_mode_estimates.csv")

mid.summary.fia <- fia.nona %>% group_by(mode, pc1_bins_fia, mids) %>% summarise(mean = mean(mean_dens_fia),
                                                                              ci.low = quantile(mean_dens_fia,0.025),
                                                                              ci.high = quantile(mean_dens_fia, 0.975))

#ggplot(mid.summary.pc1, aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.pc1, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)


mid.summary.lowprob.fia <- fia.nona %>% group_by(prob >= 0.4999 & prob <= 0.5099 ,  pc1_bins_fia, mids) %>% summarise(mean = mean(mean_dens_fia),
                                                                                                              ci.low = quantile(mean_dens_fia, 0.025),
                                                                                                              ci.high = quantile(mean_dens_fia, 0.975))


# find the range where there are two modes:
# for each mids, if there is a "Forest" and a "Savanna" column, then we call it bimodal:
bins.summary.pc1.fia <- fia.nona %>% group_by(mode, pc1_bins_fia, mids) %>% summarise(mean = mean(mean_dens_fia),
                                                                               ci.low = quantile(mean_dens_fia,0.025),
                                                                               ci.high = quantile(mean_dens_fia, 0.975),
                                                                               ncell = length(mean_dens_fia))


test <- bins.summary.pc1.fia %>% group_by(pc1_bins_fia) %>% spread(key = mode, value = 1)
test$Savanna <- ifelse(!is.na(test$Savanna), 1, 0)
test$Forest <- ifelse(!is.na(test$Forest), 1, 0)
nstates.fia <- test %>% group_by(pc1_bins_fia, mids) %>% summarise(nstates = sum(Forest))
ncell.fia <- bins.summary.pc1.fia  %>% group_by(pc1_bins_fia, mids) %>% summarise(ncell = sum(ncell))
nstates.fia <- merge(ncell.fia, nstates.fia, by = c("pc1_bins_fia", "mids"))
ggplot(nstates.fia, aes(mids, nstates))+geom_point()

unimodal.forest.fia <- nstates.fia[nstates.fia$nstates == 1.00 & nstates.fia$ncell > 50 ,] %>% group_by(nstates) %>% summarise(min = min(mids), 
                                                                                                              xmax = max(mids), 
                                                                                                              ymin = 0, ymax = 5000)



pls.pc1.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  #geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(PC1))+xlim(-5.5, 5)+xlab("PLS PC1")+
  annotate(geom = "text", x = 0, y = 4500, label = "Bimodal Past", color = "black")
  annotate(geom = "text", x = -125, y = 4500, label = "Low Sample", color = "black")

fia.pc1.rect <- ggplot()+#geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.forest.fia) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(PC1fia))+xlim(-5.5, 5)+xlab("FIA PC1")+
  annotate(geom = "text", x = 0, y = 4500, label = "Unimodal Modern", color = "black")#+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")


fut.pc1.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.forest.fia) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.5, color = "black")+
  #geom_rect(data = data.frame(unimodal.forest.fia) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(PC1_cc85))+xlim(-5.5, 5)+xlab("RCP 8.5 PC1")+
   theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 0, y = 4500, label = "Bimodal Past \n Unimodal Modern", color = "black")#+
  #annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")

dev.off()
png(height = 10, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/future_pc1_with_extra_states_bimodal_shading.png")
plot_grid(pls.pc1.rect, fia.pc1.rect, fut.pc1.rect, ncol = 1, align = "hv")
dev.off()    


#----------------------- now do the same for soil moisture ---------------------------


# lets make the plot but with past bimodal shaded:
pls.nona <- read.csv( "outputs/mixture_model/pls_soil_mixture_mode_estimates.csv")

mid.summary.soil <- pls.nona %>% group_by(mode, mean_GS_soil_bins, mids) %>% summarise(mean = mean(mean_dens),
                                                                             ci.low = quantile(mean_dens,0.025),
                                                                             ci.high = quantile(mean_dens, 0.975))

#ggplot(mid.summary.soil, aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.soil, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)


mid.summary.lowprob <- pls.nona %>% group_by(prob_soil >= 0.4999 & prob_soil <= 0.5099 ,  mean_GS_soil_bins, mids) %>% summarise(mean = mean(mean_dens),
                                                                                                              ci.low = quantile(mean_dens, 0.025),
                                                                                                              ci.high = quantile(mean_dens, 0.975))


# find the range where there are two modes:
# for each mids, if there is a "Forest" and a "Savanna" column, then we call it bimodal:
bins.summary.soil <- pls.nona %>% group_by(mode, mean_GS_soil_bins, mids) %>% summarise(mean = mean(mean_dens),
                                                                              ci.low = quantile(mean_dens,0.025),
                                                                              ci.high = quantile(mean_dens, 0.975),
                                                                              ncell = length(mean_dens))


test <- bins.summary.soil %>% group_by(mean_GS_soil_bins) %>% spread(key = mode, value = 1)
test$Savanna <- ifelse(!is.na(test$Savanna), 1, 0)

test$Forest <- ifelse(!is.na(test$Forest), 1, 0)
nstates <- test %>% group_by(mean_GS_soil_bins, mids) %>% summarise(nstates = sum(Forest, Savanna))
ncell <- bins.summary.soil %>% group_by(mean_GS_soil_bins, mids) %>% summarise(ncell = sum(ncell))
nstates <- merge(ncell, nstates, by = c("mean_GS_soil_bins", "mids"))
ggplot(nstates, aes(mids, nstates))+geom_point()
bimodal.region <- nstates[nstates$nstates > 1 & nstates$ncell > 50,] %>% group_by(nstates) %>% summarise(min = min(mids), 
                                                                                                         xmax = max(mids), 
                                                                                                         ymin = 0, ymax = 5000)


unimodal.open.region <- nstates[nstates$nstates == 1 & nstates$ncell <= 50,] %>% group_by(nstates) %>% summarise(min = min(mids), 
                                                                                                                 xmax = max(mids)+0.25, 
                                                                                                                 ymin = 0, ymax = 5000)
# get the unimodal and bimodal regions for FIA:
fia.nona <- read.csv( "outputs/mixture_model/fia_soil_mixture_mode_estimates.csv")

mid.summary.fia <- fia.nona %>% group_by(mode, mean_GS_soil_m_bins, mids) %>% summarise(mean = mean(mean_dens_fia),
                                                                                 ci.low = quantile(mean_dens_fia,0.025),
                                                                                 ci.high = quantile(mean_dens_fia, 0.975))

#ggplot(mid.summary.soil, aes(mids, mean, color = mode))+stat_smooth(se = FALSE)+geom_ribbon(data = mid.summary.soil, aes(ymin = ci.low, ymax = ci.high, fill = mode), alpha = 0.25, linetype = "dashed", colour = NA)


mid.summary.lowprob.fia <- fia.nona %>% group_by(prob_soil >= 0.4999 & prob_soil <= 0.5099 ,  mean_GS_soil_m_bins, mids) %>% summarise(mean = mean(mean_dens_fia),
                                                                                                                      ci.low = quantile(mean_dens_fia, 0.025),
                                                                                                                      ci.high = quantile(mean_dens_fia, 0.975))


# find the range where there are two modes:
# for each mids, if there is a "Forest" and a "Savanna" column, then we call it bimodal:
bins.summary.soil.fia <- fia.nona %>% group_by(mode, mean_GS_soil_m_bins, mids) %>% summarise(mean = mean(mean_dens_fia),
                                                                                      ci.low = quantile(mean_dens_fia,0.025),
                                                                                      ci.high = quantile(mean_dens_fia, 0.975),
                                                                                      ncell = length(mean_dens_fia))


test <- bins.summary.soil.fia %>% group_by(mean_GS_soil_m_bins) %>% spread(key = mode, value = 1)
test$Savanna <- ifelse(!is.na(test$`Low Density Mode`), 0, 1)
test$Forest <- ifelse(!is.na(test$`High Density Mode`), 0, 1)
nstates.fia <- test %>% group_by(mean_GS_soil_m_bins, mids) %>% summarise(nstates = sum(Forest))
ncell.fia <- bins.summary.soil.fia  %>% group_by(mean_GS_soil_m_bins, mids) %>% summarise(ncell = sum(ncell))
nstates.fia <- merge(ncell.fia, nstates.fia, by = c("mean_GS_soil_m_bins", "mids"))
ggplot(nstates.fia, aes(mids, nstates))+geom_point()

unimodal.forest.fia <- nstates.fia[nstates.fia$nstates == 1.00 & nstates.fia$ncell > 50 ,] %>% group_by(nstates) %>% summarise(min = min(mids), 
                                                                                                                               xmax = max(mids), 
                                                                                                                               ymin = 0, ymax = 5000)



pls.soil.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  #geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_GS_soil))+xlim(0, 2)+xlab("PLS soil moisture")+
  annotate(geom = "text", x = 0.6, y = 4500, label = "Bimodal Past", color = "black")
#annotate(geom = "text", x = -125, y = 4500, label = "Low Sample", color = "black")

fia.soil.rect <- ggplot()+#geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.forest.fia) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_GS_soil_m))+xlab("FIA soil moisture")+xlim(0, 2)+
  annotate(geom = "text", x = 0.7, y = 4500, label = "Unimodal Modern", color = "black")#+
#annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")


fut.soil.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.forest.fia) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.5, color = "black")+
  #geom_rect(data = data.frame(unimodal.forest.fia) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "blue", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_GS_soil_8.5))+xlim(0, 2)+xlab("RCP 8.5 soil moisture")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 0.65, y = 4500, label = "Bimodal Past \n Unimodal Modern", color = "black")#+
#annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")

dev.off()

png(height = 10, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/future_soil_with_extra_states_bimodal_shading.png")
plot_grid(pls.soil.rect, fia.soil.rect, fut.soil.rect, ncol = 1, align = "hv")
dev.off() 


png(height = 10, width = 8, units = "in", res = 300, "outputs/paper_figs_unc/future_soil_pc1_with_extra_states_bimodal_shading.png")
plot_grid(
  pls.pc1.rect,  pls.soil.rect,
  fia.pc1.rect, fia.soil.rect,
  fut.pc1.rect, fut.soil.rect, 
  ncol = 2, align = "hv", labels = "AUTO")
dev.off() 

