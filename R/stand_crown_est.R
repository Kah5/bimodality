#Estimated Stand Crown cover from diameter data
# using the equation from USFS

#diameter data
diams <- read.csv("data/plss_diam_alb_v0.9-10.csv")
diams[,4:ncol(diams)]<- diams[,4:ncol(diams)]*2.54
SCC = 0.0175 + 0.205*rowSums(diams[,4:ncol(diams)]) + 0.0060*rowSums(diams[,4:ncol(diams)]^2)
