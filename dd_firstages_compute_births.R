#--------------------------------------
# Reconcile early age groups for births

# ---------
# Function
# ---------
dd_firstages_compute_births <- function(data){
  
  require(tidyverse)
  
  df <- dd_age_standard(data, abridged = TRUE)
  
  # if 0-4 is missing and 0-1 and 1-4 are present, then sum to 0-4
  dv01 <- df$DataValue[df$AgeLabel == "0"]
  dv14 <- df$DataValue[df$AgeLabel == "1-4"]
  dv04 <- df$DataValue[df$AgeLabel == "0-4"]
  dv10_14 <- df$DataValue[df$AgeLabel == "10-14"]
  dv15_19 <- df$DataValue[df$AgeLabel == "15-19"]
  
  dv0_14 <- data$DataValue[data$AgeLabel == "0-14"]
  dv0_19 <- data$DataValue[data$AgeLabel == "0-19"]
  dv10_19 <- data$DataValue[data$AgeLabel == "10-19"]
  
  
  if(!is.na(dv14) & !is.na(dv01) & is.na(dv04)) {
    df$DataValue[df$AgeLabel == "0-4"] <- dv01 + dv14
  }
  
  if(is.na(dv14) & !is.na(dv01) & !is.na(dv04)) {
    df$DataValue[df$AgeLabel == "1-4"] <- dv04 - dv01
  }
  
  if (!is_empty(dv0_14)) {
    if(is.na(dv10_14 & !is.na(dv0_14))) {
      df$DataValue[df$AgeLabel == "10-14"] <- dv0_14
    }
  }
  if (!is_empty(dv0_14) & !is_empty(dv0_19)) {
    if(is.na(dv15_19) & !is.na(dv0_14) & !is.na(dv0_19)) {
      df$DataValue[df$AgeLabel == "15-19"] <- dv0_19 - dv0_14
    }
  }
  if (!is_empty(dv0_19)) {
    if(is.na(dv10_14) & !is.na(dv15_19) & !is.na(dv0_19)) {
      df$DataValue[df$AgeLabel == "10-14"] <- dv0_19 - dv15_19
    }
    if(is.na(dv10_14) & is.na(dv15_19) & !is.na(dv0_19)) {
      df$DataValue[df$AgeLabel == "10-14"] <- 0
      df$DataValue[df$AgeLabel == "15-19"] <- dv0_19
    }
  }
  
  data.out <- df %>% 
    dplyr::filter(!is.na(DataValue) & !is.na(AgeSort))
  
  return(data.out)
}


