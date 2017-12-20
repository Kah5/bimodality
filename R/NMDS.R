#!/usr/bin/env R

# NMDS script for the CRC:
library(vegan)
library(MASS)
#fullcomps <- read.csv("outputs/cluster/fullcomps.csv")

fullcomps <- read.csv("data/fullcomps.csv")
dens.pr <- read.csv("data/PLS_full_dens_pr_with_bins.csv")

fia.bim <- read.csv("data/FIA_Dens_Bimodal_width_0.25.csv")
#fia.bim <- read.csv("outputs/cluster/bimodal_widths/FIA_Dens_Bimodal_width_0.25.csv")

dens.pr.bim<- dens.pr[dens.pr$PC1 >= -2.5 & dens.pr$PC1 <= 1.25, c("x", "y", "cell", "PC1")]

fia.pr.bim <- fia.bim[fia.bim$PC1 >= -2.5 & fia.bim$PC1 <= 1.25, c("x", "y", "cell", "PC1")]

dens.pr.bim$period <- "PLS"
fia.pr.bim$period <- "FIA"

bim.cells <- rbind(dens.pr.bim, fia.pr.bim)
fullcomps <- merge(fullcomps, bim.cells, by = c("x", "y", "cell", "period"))

#fullcomps <- read.csv("data/fullcomps.csv")
fullcomps <- fullcomps[!names(fullcomps) %in% c("No.tree", "Other.softwood", "period", "FIAdensity")]
fullcomps <- fullcomps[!duplicated(fullcomps[,5:39]),]

#df <- fullcomps[sample(nrow(fullcomps), 1000), ]
df <- fullcomps
system.time(NMDS <- metaMDS(as.matrix(df[,5:39]), trymax = 50))

NMDS 
saveRDS(NMDS, "outputs/cluster/NMDS_bimodal_only_test.rds")

png('data/stressplot_rand.png')
stressplot(NMDS)
dev.off()

png('data/plot_NMDS_rand.png')
plot(NMDS)
dev.off()


saveRDS(NMDS, "NMDS.obj.rds")

variableScores <- NMDS$species
sampleScores <- NMDS$points
saveRDS(variableScores, "NMDS_var_scores_rand.rds")
saveRDS(sampleScores, "NMDS_sample_scores_rand.rds")
