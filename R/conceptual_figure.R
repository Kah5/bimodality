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
ggplot(full[!full$bins %in% c("(-10,-8]", '(-12,-10]','(8,10]','(10,12]'),], aes(x = value, fill = time))+geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+facet_wrap(~bins, scales = "free_x", ncol = 5)#theme(axis.text.x = element_blank(), axis.title.x = element_blank())

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

png(height = 6, width = 8, units = 'in',res = 200,'outputs/conceptual_fig_density_by_bins.png')
ggplot(full[!full$bins %in% c("(-10,-8]", '(-12,-10]','(8,10]','(10,12]'),], aes(x = value, fill = time)) + geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+facet_wrap(~bins, scales ="free_x", ncol=4)+xlab("Tree Density")
dev.off()


ggplot(full, aes(x = climate, y = value))+geom_point()+facet_wrap(~time)

library(plotly)

kd <- MASS::kde2d(one$value, one$climate, n = 25)
p <- plot_ly(x = kd$x, y = kd$y, z = kd$z) %>% add_surface()

