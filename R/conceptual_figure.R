library(ggplot2) 

# make dummy datasets of Modern and PLS:

x3 = rnorm(2000, -3, 2)
x4 = rnorm(2000,3,2)
x3 <- c(x3, x4)
x3<- x3[order(x3)]
one <- data.frame(time = "Past", value = rnorm(n = 1100, mean = -2, sd = 2), climate = x3[c(1501:2600)])
two <- data.frame(time = "Modern", value = rnorm(n = 1100, mean = 2, sd = 2), climate=x3[c(1501:2600)])
test <- data.frame(time = "Modern", value = rnorm(n = 100, mean = 2, sd = 2), climate=x3[c(2801:2900)])
test2 <- data.frame(time = "Past", value = rnorm(n = 1000, mean = -2, sd = 2), climate=x3[c(2601:3100)])
full <- rbind(one, two, test, test2)

library(ggplot2) 

# make dummy datasets of Modern and PLS:

x3 = rnorm(2000, -3, 2)
x4 = rnorm(2000,3,2)
x3 <- c(x3,x4)
one <- data.frame(time = "Past", value = rnorm(n = 500, mean = -2, sd = 2), climate = x3[101:600])
two <- data.frame(time = "Modern", value = rnorm(n = 4000, mean = 2, sd = 2), climate=x3)
one1 <- data.frame(time = "Past", value = rnorm(n = 100, mean = 2, sd = 2), climate = x3[1:100])
two2 <- data.frame(time = "Modern", value = rnorm(n = 400, mean = 2, sd = 2), climate=x3[1:400])

# high envt
high.p <- data.frame(time = "Past", value = rnorm(n = 200, mean = 1.5, sd = 2), climate = x3[701:900], envt = "High")
high.m <- data.frame(time = "Modern", value = rnorm(n = 200, mean = 2, sd = 2), climate=x3[701:900], envt = "High")


low.p <- data.frame(time = "Past", value = rnorm(n = 200, mean = -3, sd = 2), climate = x3[301:500], envt = "Low")
low.m <- data.frame(time = "Modern", value = rnorm(n = 200, mean = -3, sd = 2), climate=x3[301:500], envt = "Low")

int.p <- data.frame(time = "Past", value = rnorm(n = 200, mean = -3, sd = 2), climate = x3[501:700], envt = "Intermediate")
int.m <- data.frame(time = "Modern", value = rnorm(n = 200, mean = 2, sd = 2), climate=x3[501:700], envt = "Intermediate")


full <- rbind(low.p, low.m, int.p, int.m, high.p, high.m)


# use the label.breaks function and cut to cut environmental data up into different bins

#full$bins <-  cut(full$climate, breaks = seq(-16, 16, by = 2))

#levels(full$bins) <- c("(-16,-14]", "(-14,-12]", "(-12,-10]" ,"High",  "(-8,-6]",   "test2",   "(-4,-2]",   "Intermediate",    "(0,2]",    
                     #  "Low",     "(4,6]",     "test"  ,   "(8,10]"  ,  "(10,12]" ,  "(12,14]",   "(14,16]"  )


png("outputs/conceptual_fig_mesophication.png")
ggplot(full, aes(x = value, fill = time))+geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))#+theme(axis.text.x = element_blank(), axis.title.x = element_blank())
dev.off()

#png(height = 3, width = 8, units = 'in', res = 300, "outputs/conceptual_fig_mesophication_panel.png")
ggplot(full, aes(x = value, fill = time))+geom_density(alpha = 0.5, position = "identity", adjust = 1.5)+theme_bw()+xlab("Species composition")+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+facet_wrap(~envt, scales = "free_x", ncol = 4)+coord_flip()#theme(axis.text.x = element_blank(), axis.title.x = element_blank())

#dev.off()


png(height = 3, width = 5, units = 'in', res = 300, "outputs/conceptual_fig_mesophication_panel.png")
ggplot(full[full$envt %in% c("Low", 'Intermediate','High'),], aes(x = value, fill = time))+geom_density(alpha = 0.5, position = "identity", adjust = 1.2)+theme_bw(base_size = 12)+xlab(expression(atop("Species Composition","Oak         " %<->% "        Mesic Species")))+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+facet_wrap(~envt, scales = "free_x", ncol = 3)+ylab("Frequency")+coord_flip()+theme(axis.title.x=element_blank(),
                                                                                                                                                                 axis.text=element_blank(),
                                                                                                                                                                 axis.ticks=element_blank())
  
dev.off()

#------------- conceptual figure for tree density:

#one <- data.frame(time = "PLS", value = rnorm(n = 1000, mean = 40, sd=20))
#three <- data.frame(time = "PLS", value = rnorm(n = 1000, mean = 20, sd = 20))
#two <- data.frame(time = "FIA", value = rnorm(n = 4000, mean = 200, sd = 50))


n = 2000
y1 = rnorm(n, 25, 20)  
y2 = rnorm(n, 150, 20)
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
high.p <- data.frame(time = "Past", value = rnorm(n = 200, mean = 1.5, sd = 2), climate = x3[701:900], envt = "High")
high.m <- data.frame(time = "Modern", value = rnorm(n = 200, mean = 2, sd = 2), climate=x3[701:900], envt = "High")


low.p <- data.frame(time = "Past", value = rnorm(n = 500, mean = 30, sd = 10), climate = x3[301:800], bins = "Low")
low.m <- data.frame(time = "Modern", value = rnorm(n = 500, mean = 25, sd = 10), climate=x3[301:800], bins = "Low")

int.p <- data.frame(time = "Past", value = rnorm(n = 200, mean = -3, sd = 2), climate = x3[501:700], envt = "Intermediate")
int.m <- data.frame(time = "Modern", value = rnorm(n = 200, mean = 2, sd = 2), climate=x3[501:700], envt = "Intermediate")


#full <- rbind(low.p, low.m, full)


ggplot(full, aes(x = climate, y = value))+geom_point()+facet_wrap(~time)


# use the label.breaks function and cut to cut environmental data up into different bins

full$bins <-  cut(full$climate, breaks = seq(-16, 16, by = 2))
full <- rbind(full, low.p, low.m)
ggplot(full, aes(x = climate, y = value))+geom_point()+facet_wrap(~time)



ggplot(full[!full$bins %in% c("(-10,-8]", '(-12,-10]','(8,10]','(10,12]'),], aes(x = value, fill = time)) + geom_histogram(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+xlab("Tree Density")+facet_wrap(~bins)

png(height = 4, width = 2, units = "in", res = 300, "outputs/paper_figs/conceptual_hist_1.png")
ggplot(full[full$bins %in% c("(-2,0]") & full$time %in% "Past",], aes(x = value*2, fill = time)) + geom_density(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red"), limits = c("Past"))+xlab("Tree Density")+ylab("frequency")+coord_flip()+theme(axis.text = element_blank(), axis.ticks=element_blank(),
                                                                                                                     legend.position = 'none',
                                                                                                                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")


dev.off()

png(height = 4, width = 2, units = "in", res = 300, "outputs/paper_figs/conceptual_hist_2.png")

ggplot(full[full$bins %in% c("(-2,0]") & full$time %in% "Modern",], aes(x = value*2, fill = time)) + geom_density(alpha = 0.5, position = "identity", bw = 30)+theme_bw()+
  scale_fill_manual(values = c("blue"), limits = c("Modern"))+xlab("Tree Density")+ylab("frequency")+coord_flip()+theme(axis.text = element_blank(), axis.ticks=element_blank(),
                                                                                                                     legend.position = 'none',
                                                                                                                     panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_rect(colour = "black", fill=NA, size=1)) + labs(fill = "p (forest)")+ggtitle("")


dev.off()
# write outputs
png("outputs/conceptual_fig_density.png")
ggplot(full[!full$bins %in% c("(-10,-8]", '(-12,-10]','(8,10]','(10,12]'),], aes(x = value, fill = time)) + geom_density(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+xlab("Tree Density")+coord_flip()
dev.off()

png(height = 3, width = 8, units = 'in',res = 200,'outputs/conceptual_fig_density_by_bins.png')
ggplot(full[!full$bins %in% c("(-10,-8]", '(-12,-10]','(8,10]','(10,12]'),], aes(x = value, fill = time)) + geom_density(alpha = 0.5, position = "identity")+theme_bw()+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+facet_wrap(~bins, scales ="free_x", ncol=4)+xlab("Tree Density")+coord_flip()
dev.off()

levels(full$bins) <- c("(-16,-14]", "(-14,-12]", "(-12,-10]" ,"(-10,-8]",  "(-8,-6]",   "test",   "(-4,-2]",   "Intermediate",    "(0,2]",    
                       "(2,4]",     "(4,6]",     "High"  ,   "(8,10]"  ,  "(10,12]" ,  "(12,14]",   "(14,16]", "Low")

full$bins_f <-  factor(full$bins, levels = c("Low","(-16,-14]", "(-14,-12]", "(-12,-10]" ,"(-10,-8]",  "(-8,-6]",   "test",   "(-4,-2]",   "Intermediate",    "(0,2]",    
                                             "(2,4]",     "(4,6]",     "High"  ,   "(8,10]"  ,  "(10,12]" ,  "(12,14]",   "(14,16]"))

png(height = 3, width = 5, units = 'in',res = 200,'outputs/conceptual_fig_density_by_bins.png')
ggplot(full[full$bins %in% c("Low", 'Intermediate','High'),], aes(x = value, fill = time)) + geom_density(alpha = 0.5, position = "identity", adjust = 1.3)+theme_bw(base_size = 12)+
  scale_fill_manual(values = c("red", "blue"), limits = c("Modern", "Past"))+facet_wrap(~bins_f, scales ="free_x", ncol=3)+xlab("Tree Density \n")+ylab('frequency')+coord_flip()+theme(axis.title.x=element_blank(),
                                                                                                                                                                                   axis.text.x=element_blank(),
                                                                          
                                                                                                                                                                                                                                                                                            axis.ticks.x=element_blank(), 
                                                                                                                                                                                    )

dev.off()
# high envt


# Plotin curves
df <- data.frame(x1 = 0, x2 = 5,x3= 2, x4=6, y1 = 0, y2 = 1, y3 = 4, y4 = 5)
ggplot()+geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "curve"), data = df, curvature = 0.2) +
  geom_curve(aes(x = x2, y = y2, xend = x3, yend = y3, colour = "segment"), data = df, curvature = 0.2)






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
