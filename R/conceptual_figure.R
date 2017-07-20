library(ggplot2) 

# make dummy datasets of Modern and PLS:

x3 = rnorm(2000, -3, 2)
x4 = rnorm(2000,3,2)
x3 <- c(x3,x4)
one <- data.frame(time = "Modern", value = rnorm(n = 4000, mean = -2, sd = 2), climate = x3)
two <- data.frame(time = "Past", value = rnorm(n = 4000, mean = 2, sd = 2), climate=x3)
full <- rbind(one, two)


# use the label.breaks function and cut to cut environmental data up into different bins

full$bins <-  cut(full$climate, breaks = seq(-16, 16, by = 2))



png("outputs/conceptual_fig_mesophication.png")
ggplot(full, aes(x = value, fill = time))+geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))#+theme(axis.text.x = element_blank(), axis.title.x = element_blank())
dev.off()

png(height = 3, width = 8, units = 'in', res = 300, "outputs/conceptual_fig_mesophication_panel.png")
ggplot(full[!full$bins %in% c("(-10,-8]", '(-12,-10]','(8,10]','(10,12]'),], aes(x = value, fill = time))+geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+xlab("Species composition")+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+facet_wrap(~bins, scales = "free_x", ncol = 4)#theme(axis.text.x = element_blank(), axis.title.x = element_blank())

dev.off()

# conceptual figure for tree density:

#one <- data.frame(time = "PLS", value = rnorm(n = 1000, mean = 40, sd=20))
#three <- data.frame(time = "PLS", value = rnorm(n = 1000, mean = 20, sd = 20))
#two <- data.frame(time = "FIA", value = rnorm(n = 4000, mean = 200, sd = 50))


n = 2000
y1 = rnorm(n, 25, 15)  
y2 = rnorm(n, 100, 25)
w = rbinom(n, 1, .5)                      # 50:50 random choice
x2 = w*y1 + (1-w)*y2      
x3 = rnorm(2000, -3, 2)
x4 = rnorm(2000,3,2)
x3 <- c(x3,x4)
x2 <- x2[order(x2)]
#x3 <- x3[order(x3)]
one <- data.frame( time = "Past", value = x2, climate = x3)
one[one$value <= 0,]$value <- 0
one$climate<- ifelse(one$value <= 55, one$climate - 1, one$climate + 1)
two <- data.frame( time = "Modern", value = rnorm(n = 4000, mean = 100, sd = 40 ), climate = x3)
two[two$value <0,]$value <- 0

full <- rbind(one, two)

# use the label.breaks function and cut to cut environmental data up into different bins

full$bins <-  cut(full$climate, breaks = seq(-16, 16, by = 2))

ggplot(full, aes(x = climate, y = value))+geom_point()+facet_wrap(~time)



ggplot(full[!full$bins %in% c("(-10,-8]", '(-12,-10]','(8,10]','(10,12]'),], aes(x = value, fill = time)) + geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+xlab("Tree Density")



# write outputs
png("outputs/conceptual_fig_density.png")
ggplot(full[!full$bins %in% c("(-10,-8]", '(-12,-10]','(8,10]','(10,12]'),], aes(x = value, fill = time)) + geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+xlab("Tree Density")
dev.off()

png(height = 3, width = 8, units = 'in',res = 200,'outputs/conceptual_fig_density_by_bins.png')
ggplot(full[!full$bins %in% c("(-10,-8]", '(-12,-10]','(8,10]','(10,12]'),], aes(x = value, fill = time)) + geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+facet_wrap(~bins, scales ="free_x", ncol=4)+xlab("Tree Density")
dev.off()


ggplot(full, aes(x = climate, y = value))+geom_point()+facet_wrap(~time)

library(plotly)

kd <- MASS::kde2d(one$value, one$climate, n = 25)
p <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()
p

dens.pr <- read.csv("data/PLS_full_dens_pr_with_bins.csv")

a <- dens.pr[complete.cases(dens.pr),]
#a <- a[a$PC1 <= 1 & a$PC1 >= 0,]
kd <- MASS::kde2d(a$PLSdensity, a$PC1, n =  100)
p <- plot_ly(x = kd$x, y = kd$y, z = t(kd$z)) %>% add_surface() %>%
layout(
  title = "Surface plot of Tree density in the Environment",
  scene = list(
    xaxis = list(title = "Tree density"),
    yaxis = list(title = "PC1 environment"),
    zaxis = list(title = "frequency")
  ))
p

library(sm)
sm.density(cbind(a$PLSdensity, a$PC1), display="image",
           props=seq(from=5, to=95, by=10), ylab="Tree Density", xlab="Environmental PC1")

test <- sm.density(cbind(a$PLSdensity, a$PC1), display="none",
           props=seq(from=5, to=95, by=10))

new.df <- data.frame(x = testdf$x[,1],
           y= testdf$x[,2],
            freq = testdf$freq)
psm <- plot_ly(x = new.df$x, y = new.df$y, z = new.df$freq) %>% add_surface()
