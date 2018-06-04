#--------------------------calculate soil moisture for FIA:
calc.soilmoist <- function(p, pe, awcs, awcu) {
  # ------------------------------------------
  # 1. Check Inputs, Create placeholders for outputs
  # ------------------------------------------
  ntime = length(p)
  
  
  # Insert formatting checks when I'm not being lazy!
  
  # Creating place holders for variables
  # Soil Moisture
  ss1  <- rep(NaN, ntime)
  ss2  <- rep(NaN, ntime)
  su1  <- rep(NaN, ntime)
  su2  <- rep(NaN, ntime)
  
  # Recharge
  rs   <- rep(NaN, ntime)
  ru   <- rep(NaN, ntime)
  
  # Runoff
  ro   <- rep(NaN, ntime)
  
  # Net loss to Evapotranspiration
  es   <- rep(NaN, ntime)
  eu   <- rep(NaN, ntime)
  
  # Change in soil moisture
  dels <- rep(NaN, ntime)
  delu <- rep(NaN, ntime)	
  
  # Adding additional variables to help track a bug
  sempty <- rep(NaN, ntime)	
  uempty <- rep(NaN, ntime)	
  excess <- rep(NaN, ntime)	
  # ------------------------------------------
  
  # ------------------------------------------
  # 2. Caluclate water balance
  # ------------------------------------------
  d   <- pe - p # deficit = excess of potential evapotraspiration over precipitation
  awc <- awcu + awcs # Combined water capacity
  # ------------------------------------------
  
  # # Calculating a running deficit for diagnostics
  # d.run <- rep(NaN, ntime)	
  # d.run[1] <- d[1]
  # for(i in 2:ntime){
  #   d.run[i] <- d[i] + d.run[i-1]
  # }
  
  # set inital condictions: kh added these two lines so the inital conditions are at max water compacity
  ssgo <- 1
  sugo <- awcu
  # ------------------------------------------
  # 3. Initialize soil moisture
  # ------------------------------------------
  # Start things with the values provided
  ss1this = ssgo
  su1this = sugo
  # ------------------------------------------
  
  # ------------------------------------------
  # 4. Loop through time to calculate soil moisture
  #    4.1 Surface Layer Dynamics
  #    4.2 Underlying Layer Dynamics
  #    4.3 Calculate some additional variables for total soil
  #    4.4 Calculate recharge, loss, and runoff
  # ------------------------------------------
  for(i in 1:ntime){
    dthis = d[i] # pe-p for right now
    
    # -------------------------
    # 4.1 Surface Layer Dynamics
    # -------------------------
    sempty[i] = awcs - ss1this # how much the sfc layer could take in
    
    if(dthis >= 0){ # if pe exceeds precip, we're going to lose soil moisture
      dels[i] <- -dthis # tentatively set the soil moisture to pe-pe
      if(dthis > ss1this) { # if pe - p exceeds what we have in the sfc layer, get rid of what we have
        dels[i] <- -ss1this
      }
      
      rs[i] = 0 # No excess, so no recharge
      ro[i] = 0 # no excess, so no runoff
      
      # Net Loss from sfc layer
      if(dels[i] < 0){
        es[i] <- -as.numeric(dels[i])
      } else {
        es[i] <- 0
      }
      
      excess[i] = 0
    } else { # ppt exceeds pe, so our soils will get wetter (or stay at capacity)
      dels[i]   <- min(sempty[i], as.numeric(-dthis)) # either all the precip, or as much as the soils can take in
      rs[i]     <- as.numeric(dels[i]) # surface recharge
      excess[i] <- -dthis - dels[i] #
      es[i]     <- 0			
    } # End surface balance ifelse
    
    ss1[i]  <- ss1this # save our starting point
    ss2[i]  <- ss1this + as.numeric(dels[i]) #
    ss1this <- ss2[i] # Next starting point will be our current end
    # -------------------------
    
    # -------------------------
    # 4.2 Underlying Layer Dynamics
    # -------------------------
    uempty[i] <- awcu - su1this # how much the under layer could take in
    
    if(excess[i]<=0){ # no moisture input from above
      eu[i] <- (dthis - es[i]) * (su1this/awc) # "loss" from the under layer
      eu[i] <- min(as.numeric(eu[i]), su1this)
      
      if(eu[i] < 0) eu[i] = 0 # no negative values allowed
      ru[i] = 0 # no recharge
      ro[i] = 0 # no runoff
      delu[i] = -as.numeric(eu[i]) # change in under soil moisture			
    } else { # There is some moisture input from above
      eu[i] = 0 # no loss from underlying layer
      delu[i] = min(uempty[i], as.numeric(excess[i])) # change is how much it could take or how much there is
      ru[i] = delu[i] # setting the recharge
      if(excess[i] > uempty[i]) { # We have more than the soil can take --> runoff!
        ro[i] <- as.numeric(excess[i]) - uempty[i]
      } else { # no runoff because we can take it all
        ro[i] <- 0
      }
    }
    
    su1[i] <- su1this # Save our starting point
    su2[i] <- su1this + delu[i] # save our ending point
    su1this <- su2[i] # This ending point is the next time step's starting point
    # -------------------------
    if(is.na(ss1[i]) | is.na(su1[i])) stop("su1 is na")
  }
  
  # ------------------------------------------
  
  # -------------------------
  # 5. Calculate some additional variables for the soil
  # -------------------------
  del   <- delu + as.numeric(dels) # total change in soil moisture, combining layers
  et    <- p - ro - del # evapotranspiration
  r     <- as.numeric(rs) + ru # total recharge, combined layers
  loss  <- es + as.numeric(eu) # total losses
  s1    <- ss1 + su1 # total starting soil moisture
  s2    <- ss2 + su2 # total ending soil moisture
  smean <- (s1 + s2)/2 # mean starting and ending soil moisture
  # -------------------------
  
  # -------------------------
  # 6. Calculate recharge, loss, and runoff
  # -------------------------
  pr <- awc - s1 # Potential recharge
  
  # Potential losses
  dope <- c(as.numeric(pe), ss1)
  plosss <- min(dope)
  plossu <- (pe - plosss) * (su1/awc)
  ploss  <- plosss + plossu
  
  pro <- awc - pr # potential runoff 
  # -------------------------
  
  # ------------------------------------------
  # 7. Format & return output
  # ------------------------------------------
  meat <- list()
  meat[["dels" ]] <- dels
  meat[["delu" ]] <- delu
  meat[["del"  ]] <- del
  
  meat[["ss1"  ]] <- ss1
  meat[["su1"  ]] <- su1
  meat[["s1"   ]] <- s1
  
  meat[["ss2"  ]] <- ss2
  meat[["su2"  ]] <- su2
  meat[["s2"   ]] <- s2
  meat[["smean"]] <- smean
  
  meat[["r"    ]] <- r
  meat[["pr"   ]] <- pr
  
  meat[["ro"   ]] <- ro
  meat[["pro"  ]] <- pro
  
  meat[["loss" ]] <- loss
  meat[["ploss"]] <- ploss
  
  meat[["et"   ]] <- et
  
  return(meat$s2)
  # ------------------------------------------
  
}

pe <-  read.csv("data/PET_fia_extracted_full1999-2015.csv")
mod.precip.mo <- readRDS(paste0("outputs/PR_pls_extracted", "1999-2015",".RDS"))
mod.precip.mo <- mod.precip.mo[complete.cases(mod.precip.mo),]


pet <- pe[,2:length(pe)]


pet$meanJJA_soil <- NA

dens.pr <- read.csv("data/PLS_FIA_density_climate_full.csv")

soil.moist <- list()

for(k in 1:length(pet$y)){
  # get p for the site
  p <- mod.precip.mo 
  p <- mod.precip.mo[mod.precip.mo$x == as.numeric(pet[k,]$x) & mod.precip.mo$y == as.numeric(pet[k,]$y) , 1:122]
  
  awcs <- (dens.pr[dens.pr$x == pet[k,]$x & dens.pr$y == pet[k,]$y ,]$awc*0.393701)*1 # kh added: since PDSI assumes always 1 for uppers surface layer
  awcu <- (dens.pr[dens.pr$x == pet[k,]$x & dens.pr$y == pet[k,]$y ,]$awc*0.393701)*(30) # kh added: AWC from gridded 8km gssurgo estimates-- need to
  # convert from cm/cm AWC to to in in the top 30 cm of soil by multiplying by 30
  
  
  
  p <- p*0.0393701
  pe <- pet[k,1:122]*0.0393701
  if(length(awcu) == 0 | is.na(awcu) | awcu == 0 ){soil.moist[[k]]  <- NA
  }else{
    if(is.na(p[,1]) | is.na(awcu) | is.na(pet[k,1:204])) {soil.moist[[k]] <- NA
    }else{
      #pet[k,]$meanJJA_soil <- mean(calc.soilmoist(p, pe, awcs, awcu)[6:8], na.rm=TRUE)
      soil.moist[[k]] <- calc.soilmoist(p, pe,awcs, awcu)
      
    }
  }
  
  
}

soil.moist.df <- do.call(rbind, soil.moist)
colnames(soil.moist.df) <- colnames(pet[,1:122])
soil.moist.df <- data.frame(soil.moist.df)

soil.moist.df$x <- pet$x
soil.moist.df$y <- pet$y

write.csv(soil.moist.df, "outputs/soil.moisture_end_of_mo_1895_1905.csv")
soil.moist.df <- read.csv("outputs/soil.moisture_1895_1905.csv")
splits <- strsplit(colnames(soil.moist.df), split = "_")
splits2 <- do.call(rbind, splits)
#write.csv(pet, "outputs/pet_with_JJAsoil_moist_1895_1905.csv")

# select only growing season and get average JJAS moisture:

GS_index <- colnames(soil.moist.df) %like% "_06" | colnames(soil.moist.df) %like% "_07"| colnames(soil.moist.df) %like% "_08" | colnames(soil.moist.df) %like% "_09"
soil.moist.df$Mean_GS <- rowMeans(soil.moist.df[,GS_index], na.rm = TRUE)
jun_index <- colnames(soil.moist.df) %like% "_06" 
jul_index <- colnames(soil.moist.df) %like% "_07" 
aug_index <- colnames(soil.moist.df) %like% "_08" 
sep_index <- colnames(soil.moist.df) %like% "_09" 

soil.moist.df$Mean_06 <- rowMeans(soil.moist.df[,jun_index], na.rm = TRUE)
soil.moist.df$Mean_07 <- rowMeans(soil.moist.df[,jul_index], na.rm = TRUE)
soil.moist.df$Mean_08 <- rowMeans(soil.moist.df[,aug_index], na.rm = TRUE)
soil.moist.df$Mean_09 <- rowMeans(soil.moist.df[,sep_index], na.rm = TRUE)

ggplot(soil.moist.df, aes(x,y, fill = Mean_09))+geom_raster()
write.csv(soil.moist.df, "outputs/soil.moisture_1999_2015_with_mean.csv")