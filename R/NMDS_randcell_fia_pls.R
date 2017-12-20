#!/usr/bin/env R

# NMDS script for the CRC:
library(vegan)
library(MASS)
library(ggplot2)
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
fullcomps <- fullcomps[!names(fullcomps) %in% c("No.tree", "Other.softwood", "FIAdensity", "Bald.cypress", "Hackberry", "Sweet.gum","Unknown.tree")]
fullcomps <- fullcomps[!duplicated(fullcomps[,6:36]),]

#df <- fullcomps[sample(nrow(fullcomps), 1000), ]
df <- fullcomps

randcellpls <- df[sample(x = nrow(df[df$period %in% "PLS",]) , size = 1500, replace = FALSE),]
randcellfia <- df[sample(x = nrow(df[df$period %in% "FIA",]) , size = 1500, replace = FALSE),]
randcell <- rbind(randcellpls, randcellfia)
system.time(NMDS <- metaMDS(as.matrix(randcell[,6:36]), trymax = 100))

NMDS 
saveRDS(NMDS, "data/NMDS_bimodal_only_fia_pls2.rds")

png('data/stressplot_rand_fia_pls2.png')
stressplot(NMDS)
dev.off()

png('data/plot_NMDS_rand_fia_pls2.png')
plot(NMDS)
dev.off()

png('data/plot_NMDS_rand_spec_fia_pls2.png')
MDSplot<- plot(NMDS, type = "t")
MDSplot
dev.off()

saveRDS(NMDS, "NMDS_1000_rand_fia_pls_2.obj.rds")

variableScores <- NMDS$species
sampleScores <- NMDS$points
saveRDS(variableScores, "NMDS_var_scores_rand_fia_pls2.rds")
saveRDS(sampleScores, "NMDS_sample_scores_rand_fia_pls2.rds")

randcell[,38:39] <- sampleScores

png("data/NMDS1_histogram_fia_pls2.png")
ggplot(randcell, aes(V38, fill = period))+geom_histogram(position = "identity", alpha = 0.5)+xlab("NMDS 1")#+facet_wrap(~period)

dev.off()

png("data/NMDS2_histogram_fia_pls2.png")
ggplot(randcell, aes(V39, fill = period))+geom_histogram(position = "identity", alpha = 0.5)+xlab("NMDS 1")#+facet_wrap(~period)

dev.off()

saveRDS(randcell, file = "data/NMDS_1000_fia_pls_randcell2.rds")
#MDS.new<- readRDS("outputs/NMDS_bimodal_only_test.rds")

#plot(MDS.new, type = "t")



