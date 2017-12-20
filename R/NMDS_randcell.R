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

dens.pr.bim$period <- "Past"
fia.pr.bim$period <- "Modern"

bim.cells <- rbind(dens.pr.bim, fia.pr.bim)
fullcomps <- merge(fullcomps, bim.cells, by = c("x", "y", "cell", "period"))

#fullcomps <- read.csv("data/fullcomps.csv")
fullcomps <- fullcomps[!names(fullcomps) %in% c("No.tree", "Other.softwood", "FIAdensity")]
fullcomps <- fullcomps[!duplicated(fullcomps[,5:40]),]

#df <- fullcomps[sample(nrow(fullcomps), 1000), ]
df <- fullcomps

randcellpls <- df[sample(x = nrow(df[df$period %in% "Past",]) , size = 1000, replace = TRUE),]
randcellfia <- df[sample(x = nrow(df[df$period %in% "Modern",]) , size = 1000, replace = TRUE),]
randcell <- rbind(randcellpls, randcellfia)
system.time(NMDS <- metaMDS(as.matrix(randcell[,6:40]), trymax = 50))

NMDS 
saveRDS(NMDS, "data/NMDS_bimodal_only_test.rds")

png('data/stressplot_rand.png')
stressplot(NMDS)
dev.off()

png('data/plot_NMDS_rand.png')
plot(NMDS)
dev.off()

png('data/plot_NMDS_rand_spec.png')
MDSplot<- plot(NMDS, type = "t")
MDSplot
dev.off()

saveRDS(NMDS, "NMDS_1000_rand.obj.rds")

variableScores <- NMDS$species
sampleScores <- NMDS$points
saveRDS(variableScores, "NMDS_var_scores_rand.rds")
saveRDS(sampleScores, "NMDS_sample_scores_rand.rds")

randcell[,42:43] <- sampleScores

png("data/NMDS1_histogram_fia_pls.png")
MDS1hist <- ggplot(randcell, aes(V42, fill = period))+geom_histogram(position = "identity", alpha = 0.5)+xlab("NMDS 1")#+facet_wrap(~period)
MDS1hist
dev.off()

png("data/NMDS2_histogram_fia_pls.png")
MDS2hist <- ggplot(randcell, aes(V43, fill = period))+geom_histogram(position = "identity", alpha = 0.5)+xlab("NMDS 1")#+facet_wrap(~period)
MDS2hist
dev.off()

saveRDS(randcell, file = "data/NMDS_1000_fia_pls_randcell.rds")
#MDS.new<- readRDS("outputs/NMDS_bimodal_only_test.rds")

#plot(MDS.new, type = "t")



