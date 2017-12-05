#------------What is the support for/strength of the bimodality?--------------

full<- read.csv("outputs/full_bimodality_data_for_strength_analysis.csv")



# define ecotype for both fia and pls
ecotype  <- ifelse(full$Density == 0,  "prairie", 
                   ifelse(full$Density <= 47, "Savanna",
                          ifelse(full$Density > 47, "Forest", "Check")))
full$ecotype <- factor(ecotype)

# for each environmental bin, calculate the ratio of forest to savanna:
bins <- as.character(unique(full[full$period %in% "Past",]$PC1_bins_f))
ratio <- data.frame(bins = bins, 
                    ratio = NA, 
                    count = NA,
                    savanna = NA,
                    forest = NA)

coeffs <- matrix(NA, length(bins), 2)

library(modes)
for(i in 1:length(bins)){
  df <- summary(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i],]$ecotype)
  ratio[i,]$ratio <- df[3]/df[1]
  ratio[i,]$count<- sum(df, na.rm=TRUE)
  ratio[i,]$savanna <- df[3]
  ratio[i,]$forest <- df[1]
  coeffs[i,1]<- bimodality_coefficient(na.omit(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i], 'pc2']))
  coeffs[i,2] <- diptest::dip.test(na.omit(density(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i], 'pc2'])$y))$p
  
}

coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
coef.bins<- data.frame(cbind(coeffs, bins))
coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
coef.new <- strsplit(as.character(coef.bins$bins), " - ")
library(plyr)
coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
colnames(coef.new) <- c("low", "high")
coef.bins <- cbind(coef.bins, coef.new)

#merge bins with the "bins" column
merged <- merge(coef.bins, ratio, by = "bins")
#criteria for bimodality
bi <- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05, "Bimodal", "Unimodal")
merged$bimodal <- bi



saveRDS(merged, "outputs/cluster/PLS_ratio_sav_forest.rds")

merged$bins_factor = factor(merged$bins, levels=c('-5 - -4','-4 - -3',
                                                  '-3 - -2','-2 - -1',
                                                  '-1 - 0', '0 - 1',
                                                  '1 - 2', '2 - 3', 
                                                  '3 - 4', '4 - 5', 
                                                  '5 - 6'))

# get the values for whether the bins are bimodal from above to color by significance:
merg.m <- melt(merged[,c("bins_factor", "bimodal", "savanna", "forest")], id.vars = c("bins_factor", "bimodal"))

png("outputs/cluster/ratio_sav_for_comp_bimodality_PLS.png")
ggplot(merg.m, aes(bins_factor,value, fill = variable))+geom_bar(stat='identity', position = position_dodge())+scale_fill_manual(values = c("forestgreen", "tan"), 
                                                                                                                                 limits = c("forest", "savanna"))+geom_text(aes(label=bimodal), vjust=0.5)+ylab('ratio of savanna to forest in PLS')+xlab("Environmental PC1 bins")
dev.off()

png("outputs/cluster/counts_sav_for_comp_bimodality_PLS.png")
ggplot(merged, aes(bins_factor,count, fill = bimodal))+geom_bar(stat='identity')+ylab('number of grid cells in PLS')+xlab("Environmental PC1 bins")
dev.off()



# do the same for FIA:
bins <- as.character(unique(full[full$period %in% "Modern",]$PC1_bins_f))
ratiofia <- data.frame(bins = bins, 
                       ratio = NA, 
                       count=NA,
                       savanna= NA, 
                       forest = NA)

coeffs <- matrix(NA, length(bins), 2)

for(i in 1:length(bins)){
  df <- summary(full[full$period %in% "Modern" & full$PC1_bins_f %in% bins[i],]$ecotype)
  ratiofia[i,]$ratio <- df[3]/df[1]
  ratiofia[i,]$count <- sum(df, na.rm=TRUE)
  ratiofia[i,]$savanna <- df[3]
  ratiofia[i,]$forest <- df[1]
  coeffs[i,1]<- bimodality_coefficient(na.omit(full[full$period %in% "Modern" & full$PC1_bins_f %in% bins[i], 'pc2']))
  coeffs[i,2] <- diptest::dip.test(na.omit(density(full[full$period %in% "Modern" & full$PC1_bins_f %in% bins[i], 'pc2'])$y))$p
  #coeffs[i,3]<- bimodality_coefficient(na.omit(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i], 'Density']))
  #coeffs[i,4] <- diptest::dip.test(na.omit(density(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i], 'Density'])$y))$p
  
}

coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
coef.bins<- data.frame(cbind(coeffs, bins))
coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
coef.new <- strsplit(as.character(coef.bins$bins), " - ")
library(plyr)
coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
colnames(coef.new) <- c("low", "high")
coef.bins <- cbind(coef.bins, coef.new)

#merge bins with the "bins" column
merged <- merge(coef.bins, ratiofia, by = "bins")
#criteria for bimodality
bi <- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05, "Bimodal", "Unimodal")
merged$bimodal <- bi

saveRDS(merged, "outputs/cluster/FIA_ratio_sav_forest.rds")

merged$bins_factor = factor(merged$bins, levels=c('-5 - -4','-4 - -3',
                                                  '-3 - -2','-2 - -1',
                                                  '-1 - 0', '0 - 1',
                                                  '1 - 2', '2 - 3', 
                                                  '3 - 4', '4 - 5', 
                                                  '5 - 6'))
merg.m <- melt(merged[,c("bins_factor", "bimodal", "savanna", "forest")], id.vars = c("bins_factor", "bimodal"))

png("outputs/cluster/ratio_sav_for_comp_bimodality_FIA.png")
ggplot(merg.m, aes(bins_factor,value, fill = variable))+geom_bar(stat='identity', position = position_dodge())+scale_fill_manual(values = c("forestgreen", "tan"), 
                                                                                                                                 limits = c("forest", "savanna"))+geom_text(aes(label=bimodal), vjust=0.5)+ylab('ratio of savanna to forest in PLS')+xlab("Environmental PC1 bins")
dev.off()

png("outputs/cluster/count_sav_for_comp_bimodality_FIA.png")
ggplot(merged, aes(bins_factor,count, fill=bimodal))+geom_bar(stat='identity')+ylab('count of points in FIA')+xlab("Environmental PC1 bins")
dev.off()




# now lets look at bimodality of composition within environmental bins:
bins <- as.character(unique(full$PC1_bins_f))
ratiofia <- data.frame(bins = bins, 
                       ratio = NA, 
                       count=NA)
coeffs <- matrix(NA, length(bins), 2)

for(i in 1:length(bins)){
  df <- summary(full[ full$PC1_bins_f %in% bins[i],]$ecotype)
  ratiofia[i,]$ratio <- df[3]/df[1]
  ratiofia[i,]$count <- sum(df, na.rm=TRUE)
  coeffs[i,1]<- bimodality_coefficient(na.omit(full[ full$PC1_bins_f %in% bins[i], 'pc2']))
  coeffs[i,2] <- diptest::dip.test(na.omit(density(full[ full$PC1_bins_f %in% bins[i], 'pc2'])$y))$p
  #coeffs[i,3]<- bimodality_coefficient(na.omit(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i], 'Density']))
  #coeffs[i,4] <- diptest::dip.test(na.omit(density(full[full$period %in% "Past" & full$PC1_bins_f %in% bins[i], 'Density'])$y))$p
  
}

coeffs[is.na(coeffs)]<- 0 # replace NANs with 0 values here
coef.bins<- data.frame(cbind(coeffs, bins))
coef.bins$BC <- as.numeric(as.character(coef.bins$V1))
coef.bins$dipP <- as.numeric(as.character(coef.bins$V2))
coef.new <- strsplit(as.character(coef.bins$bins), " - ")
library(plyr)
coef.new<- rbind.fill(lapply(coef.new, function(X) data.frame(t(X))))
colnames(coef.new) <- c("low", "high")
coef.bins <- cbind(coef.bins, coef.new)

#merge bins with the "bins" column
merged <- merge(coef.bins, ratiofia, by = "bins")
#criteria for bimodality
bi <- ifelse(merged$BC >= 0.55 & merged$dipP <= 0.05, "Bimodal", "Unimodal")
merged$bimodal <- bi

saveRDS(merged, "outputs/cluster/full_ratio_sav_forest.rds")

merged$bins_factor = factor(merged$bins, levels=c('-5 - -4','-4 - -3',
                                                  '-3 - -2','-2 - -1',
                                                  '-1 - 0', '0 - 1',
                                                  '1 - 2', '2 - 3', 
                                                  '3 - 4', '4 - 5', 
                                                  '5 - 6'))


ggplot(merged, aes(bins_factor,ratio, fill=bimodal))+geom_bar(stat='identity')+ylab('ratio of savanna to forest in FIA')+xlab("Environmental PC1 bins")
