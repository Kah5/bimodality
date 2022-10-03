cut.2000 <- read.csv("outputs/logged_prev_years.csv")
merged.tree <- read.csv("outputs/merged.tree.new.csv")

get_prev_survey <- function(df.prev){
  plt<- df.prev['PLOT']
  st <- df.prev['STATECD']
  invyr <- df.prev['prev_year']
  
  prev_survey <- merged.tree[merged.tree$PLOT %in% plt & merged.tree$STATECD %in% st &  merged.tree$INVYR == invyr,]
  
  
  return(prev_survey)
}

prev_yr_survey <- list()
prev_yr_survey <- apply(X = as.matrix(cut.2000[1:20,]), MARGIN=1, FUN = get_prev_survey)

testcall <- do.call(rbind, prev_yr_survey)
# write the data to csv
write.csv(testcall, "outputs/logged_prev_years_data.csv",row.names = FALSE)
