#--------------------------------------
# Reconcile ages 0-1, 1-4 and 0-4

# ---------
# Function
# ---------
dd_firstages_compute <- function(data){
  
  require(tidyverse)
  
  df <- dd_age_standard(data, abridged = TRUE)
  
  # if 0-4 is missing and 0-1 and 1-4 are present, then sum to 0-4
  dv01 <- df$DataValue[df$AgeLabel == "0"]
  dv14 <- df$DataValue[df$AgeLabel == "1-4"]
  dv04 <- df$DataValue[df$AgeLabel == "0-4"]
  
  if(!is.na(dv14) & !is.na(dv01) & is.na(dv04)) {
    df$DataValue[df$AgeLabel == "0-4"] <- dv01 + dv14
  }
  
  if(is.na(dv14) & !is.na(dv01) & !is.na(dv04)) {
    df$DataValue[df$AgeLabel == "1-4"] <- dv04 - dv01
  }
  
  data.out <- df %>% 
    filter(!is.na(DataValue) & !is.na(AgeSort))
  
 return(data.out)
}


