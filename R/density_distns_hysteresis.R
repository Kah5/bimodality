# make density histrograms for conceptual figure
library(ggplot2)
library(reshape2)

# strong density feedbacks
mu1 <- 75   
mu2 <- 200
sig1 <- 25
sig2 <- 25
cpct <- 0.5   

bimodalDistFunc <- function (n,cpct, mu1, mu2, sig1, sig2) {
  y0 <- rnorm(n,mean=mu1, sd = sig1)
  y1 <- rnorm(n,mean=mu2, sd = sig2)
  
  flag <- rbinom(n,size=1,prob=cpct)
  y <- y0*(1 - flag) + y1*flag 
}

bimodalData <- bimodalDistFunc(n=10000,cpct,mu1,mu2, sig1,sig2)
hist(bimodalData, breaks = 25)

mu1 <- 75   
mu2 <- 200
sig1 <- 40
sig2 <- 40
cpct <- 0.5  

bimodalData2 <- bimodalDistFunc(n=10000,cpct,mu1,mu2, sig1,sig2)
hist(bimodalData2, breaks = 25)

# normal data:
mu <- 150
sig <- 50
n <- 10000
normalData <- rnorm(n, mean= mu, sd = sig)
hist(normalData, breaks = 25)

densitydistns <- data.frame(linear = normalData, 
           weakhysteresis = bimodalData2, 
           stronghysteresis = bimodalData, 
           id = 1:n)

distn <- melt(densitydistns, id.vars = "id")

png(height = 4, width = 6, units = "in",res= 300, "outputs/conceptual_figure_1.png")
ggplot(distn, aes(value, fill = variable))+geom_histogram()+xlim(300,0)+
  facet_grid(variable~., switch = "both") + theme_bw()+ xlab("Tree Density (stems/ha)")+ guides(fill=guide_legend(title="Type of Relationship"))
dev.off()

