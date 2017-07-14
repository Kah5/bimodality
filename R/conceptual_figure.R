library(ggplot2) 

# make dummy datasets of Modern and PLS:
one <- data.frame(time = "FIA", value = rnorm(n = 4000, mean = -2, sd = 3))
two <- data.frame(time = "PLS", value = rnorm(n = 4000, mean = 2, sd = 3))
full <- rbind(one, two)

label.breaks <- function(beg, end, splitby){
  labels.test <- data.frame(first = seq(beg, end, by = splitby), second = seq((beg + splitby), (end + splitby), by = splitby))
  labels.test <- paste (labels.test$first, '-' , labels.test$second)
  labels.test
}

# use the label.breaks function and cut to cut environmental data up into different bins

full$bins <-  cut(full$value, breaks = seq(-5, 6, by = 1), labels = label.breaks(-5, 5, 1))



png("outputs/conceptual_fig_mesophication.png")
ggplot(full, aes(x = value, fill = time))+geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("FIA", "PLS"))#+theme(axis.text.x = element_blank(), axis.title.x = element_blank())

ggplot(full, aes(x = value, fill = time))+geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("FIA", "PLS"))+facet_wrap(~bins, scales = "free_x")#theme(axis.text.x = element_blank(), axis.title.x = element_blank())

dev.off()

# conceptual figure for tree density:

#one <- data.frame(time = "PLS", value = rnorm(n = 1000, mean = 40, sd=20))
#three <- data.frame(time = "PLS", value = rnorm(n = 1000, mean = 20, sd = 20))
two <- data.frame(time = "FIA", value = rnorm(n = 4000, mean = 200, sd = 50))

n = 2000
y1 = rnorm(n, 20, 40)  
y2 = rnorm(n, 150, 50)
w = rbinom(n, 1, .5)                      # 50:50 random choice
x2 = w*y1 + (1-w)*y2      
x3 = rnorm(2000, 0, 4)
one <- data.frame( time = "PLS", value = x2, climate = x3)
two <- data.frame(time = "FIA", value = rnorm(n = 4000, mean = 200, sd = 50), climate = x3)


full <- rbind(one, two)

#gam()
# need to somehow get climate data that allows for intermediate bimodality

# use the label.breaks function and cut to cut environmental data up into different bins

full$bins <-  cut(full$climate, breaks = seq(-16, 16, by = 2))

ggplot(full, aes(x = value, fill = time)) + geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("FIA", "PLS"))+facet_wrap(~bins, scales ="free_x")#theme(axis.text.x = element_blank(), axis.title.x = element_blank())




png("outputs/conceptual_fig_density.png")
ggplot(full, aes(x = value, fill = time)) + geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("FIA", "PLS"))+theme(axis.text.x = element_blank(), axis.title.x = element_blank())

ggplot(full, aes(x = value, fill = time)) + geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("FIA", "PLS"))+facet_wrap(~bins, scales ="free_x")#theme(axis.text.x = element_blank(), axis.title.x = element_blank())

dev.off()

mu1 <- log(1)   
mu2 <- log(50)
sig1 <- log(3)
sig2 <- log(3)
cpct <- 0.4   

bimodalDistFunc <- function (n,cpct, mu1, mu2, sig1, sig2) {
  y0 <- rnorm(n,mean=mu1, sd = sig1)
  y1 <- rnorm(n,mean=mu2, sd = sig2)
  
  flag <- rbinom(n,size=1,prob=cpct)
  y <- y0*(1 - flag) + y1*flag 
}

bimodalData <- bimodalDistFunc(n=1000,cpct,mu1,mu2, sig1,sig2)
hist(log(bimodalData))


n = 500;  
#x1 = rbeta(n, .5, .4)
y1 = rnorm(n, -4, 3)  
y2 = rnorm(n, 4, 2)
w = rbinom(n, 1, .5)                      # 50:50 random choice
x2 = w*y1 + (1-w)*y2                      # normal mixture model of bimodality



par(mfrow=c(1,2))                         # two panels per plot
hist(x1, prob=T, col="skyblue2", main="BETA(.5, .4)");  rug(x1)
curve(dbeta(x, .5, .4), lwd=2, col="blue", add=T)


hist(x2, prob=T, col="skyblue2", main="Normal Mixture"); 
rug(x2)
curve(.5*dnorm(x,5,2)+.5*dnorm(x,10,1), lwd=2, col="blue", add=T)
