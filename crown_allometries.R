#this script investigates different Crown area DBH relationships
#utlizing the Forest Health Monitoring 1991-1999 datasets
#this is different than Bechtold allometries because wer
#are using open and stand grown trees.
library(plyr)

IL1999<- read.csv("./data/IL/IL_TREE_1999.CSV")
MN1999<- read.csv("./data/MN/MN_TREE_1999.CSV")
IN1999 <- read.csv("./data/IN/IN_TREE_1999.CSV")
WI1999 <- read.csv("./data/WI/WI_TREE_1999.CSV")

keeps <- c("FHM_SPECIES", "SPECIES_COMMON_NAME", "DBH", "DBH_CHECK", "CROWN_DIAMETER_90", 
           "CROWN_DIAMETER_WIDE", "FOREST_TYPE")


IL1999 <- IL1999[,keeps]
IN1999 <- IN1999[,keeps]
MN1999 <- MN1999[,keeps]
WI1999 <- WI1999[,keeps]

mw1999 <- rbind(IL1999, 
                IN1999, 
                MN1999, 
                WI1999)

summary(mw1999$SPECIES_COMMON_NAME)
mw1999<- mw1999[!summary(mw1999$SPECIES_COMMON_NAME)<2,]

convert <- read.csv("./data/fullpaleon_conversion_v0.3.csv")
library(Hmisc)
mw1999$SPECIES_COMMON_NAME<- capitalize(as.character(mw1999$SPECIES_COMMON_NAME))
index <- match(mw1999$SPECIES_COMMON_NAME, convert$Level.0)
index
 

plot(mw1999$DBH, mw1999$CROWN_DIAMETER_90)
plot(mw1999$DBH, mw1999$CROWN_DIAMETER_WIDE)
summary(mw1999$SPECIES_COMMON_NAME)




#want a simple crown model 
#LCW = b0 + b1* DBH
full.lm <- lm( mw1999$CROWN_DIAMETER_90 ~ mw1999$DBH)

w.oak <- lm(CROWN_DIAMETER_90 ~ DBH, SPECIES_COMMON_NAME=="white oak", data=mw1999)

#remove na for DBH and CROWn diameters

mw1999 <- mw1999[complete.cases(mw1999),]
library("data.table")
set.seed(1)
df <- data.frame(id = letters[1:3], 
                 cyl = sample(c("a","b","c"), 30, replace = TRUE),
                 factor = sample(c(TRUE, FALSE), 30, replace = TRUE),   
                 hp = sample(c(20:50), 30, replace = TRUE))
mw1999 <- data.table(mw1999, key = "SPECIES_COMMON_NAME")

fits <- lapply(unique(mw1999$SPECIES_COMMON_NAME),
               function(z)lm(CROWN_DIAMETER_90~DBH, data=mw1999[J(z),], y=T))

sapply(fits,coef)
sapply(fits,predict)
sapply(fits,residuals)
sapply(fits, function(x)c(se=summary(x)$sigma, rsq=summary(x)$r.squared))

par(mfrow=c(3,3))
lapply(fits,plot,2)

#or could set it up like this
coef_tbl = mw1999[, as.list(coef(lm(CROWN_DIAMETER_90 ~ DBH))), by=SPECIES_COMMON_NAME]



#################
#create generic model function to creat allometries
model <- function(x){
  lm(CROWN_DIAMETER_90 ~ DBH, data=x)
}

#example of allometry for single species type
fit <- model(mw1999[mw1999$SPECIES_COMMON_NAME=="White oak" ,])

#loop over all subsets of species
fitted.crown.allom <- dlply(mw1999, .(SPECIES_COMMON_NAME), model)

#gives the coefficients of all the lm, note NA values in DBH coef for some species
ldply(fitted.crown.allom, coef)
#r-squared value for the model
ldply(fitted.crown.allom, function(x) summary(x)$r.squared)


add.trend.line <- function(x, y, d, ...) {
  fit <- lm(d[[y]] ~ (d[[x]]))
  abline(fit, ...)
}
col.table <- rainbow(length(unique(mw1999$SPECIES_COMMON_NAME)))
plot(CROWN_DIAMETER_90 ~ DBH, mw1999, cex=1.5, pch=21)
d_ply(mw1999, .(SPECIES_COMMON_NAME), function(x) add.trend.line("DBH", "CROWN_DIAMETER_90", x, col=col.table[x$SPECIES_COMMON_NAME]))

##plot the crown diameters using ggplot


#ggplot(mw1999, aes(DBH, CROWN_DIAMETER_90, colour = "SPECIES_COMMON_NAME")


#should we generate paleon taxa level relationships?



#or a less simple crown model 
#LCW = b0 + b1* DBH +b2*DBH^2