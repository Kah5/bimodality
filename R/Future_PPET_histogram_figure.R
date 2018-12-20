library(ggplot2)
library(cowplot)
library(tidyr)
library(dplyr)

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


mid.summary.lowprob <- pls.nona %>% group_by(prob_ppet >= 0.4999 & prob_ppet <= 0.5099 , ppet_bins, mids_ppet) %>% summarise(mean = mean(mean_dens),
                                                                                                                                  ci.low = quantile(mean_dens, 0.025),
                                                                                                                                  ci.high = quantile(mean_dens, 0.975))


# find the range where there are two modes:
# for each mids_ppet, if there is a "Forest" and a "Savanna" column, then we call it bimodal:
bins.summary.ppet <- pls.nona %>% group_by(mode, GS_ppet_bins, mids_ppet) %>% summarise(mean = mean(mean_dens),
                                                                                  ci.low = quantile(mean_dens,0.025),
                                                                                  ci.high = quantile(mean_dens, 0.975))


test <- bins.summary.ppet %>% group_by(GS_ppet_bins) %>% spread(key = mode, value = 1)
test$Savanna <- ifelse(!is.na(test$Savanna), 1, 0)
test$Forest <- ifelse(!is.na(test$Forest), 1, 0)
nstates <- test %>% group_by(GS_ppet_bins, mids_ppet) %>% summarise(nstates = sum(Forest, Savanna))

ggplot(nstates, aes(mids_ppet, nstates))+geom_point()
bimodal.region <- nstates[nstates$nstates > 1,] %>% group_by(nstates) %>% summarise(min = min(mids_ppet)+ 7.5, 
                                                                                    xmax = max(mids_ppet)+7.5, 
                                                                                    ymin = 0, ymax = 5000)


unimodal.open.region <- nstates[nstates$nstates == 1 & nstates$mids_ppet < 0,] %>% group_by(nstates) %>% summarise(min = min(mids_ppet) + 7.5, 
                                                                                    xmax = max(mids_ppet)+15+7.5, 
                                                                                    ymin = 0, ymax = 5000)


pls.ppet.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(GS_ppet))+xlim(-175, 300)+xlab("PLS growing season P-PET")+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")
  
fia.ppet.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(GS_ppet_mod))+xlim(-175, 300)+xlab("FIA growing season P-PET")+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")


fut.ppet.rect <- ggplot()+geom_rect(data = data.frame(bimodal.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "red", alpha = 0.5, color = "black")+
  geom_rect(data = data.frame(unimodal.open.region) , aes(xmin = min, xmax = xmax,ymin = ymin, ymax = ymax), fill = "tan", alpha = 0.5, color = "black")+
  geom_histogram(data = dens.pr, aes(mean_ppet_GS))+xlim(-175, 300)+xlab("RCP 8.5 future growing season P-PET")+
  geom_vline(data = state.summaries[state.summaries$mean < 0 & state.summaries$mean > -110,], aes(xintercept = mean, color = state), linetype = "dashed")+
  theme(legend.position  = c(0.705, 0.53))+
  annotate(geom = "text", x = 50, y = 4500, label = "Bimodal", color = "black")+
  annotate(geom = "text", x = -125, y = 4500, label = "Savanna", color = "black")

       
png(height = 10, width = 6, units = "in", res = 300, "outputs/paper_figs_unc/future_ppet_with_extra_states_bimodal_shading.png")
plot_grid(pls.ppet.rect, fia.ppet.rect, fut.ppet.rect, ncol = 1, align = "hv")
dev.off()                                       
