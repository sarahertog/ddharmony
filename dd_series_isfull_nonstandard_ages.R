#--------------------------------------
# Check whether the age vector of a non-standard age series meets conditions for a full series
# for one ddharmony id at a time
# returns TRUE/FALSE

# ---------
# Function
# ---------
dd_series_isfull_nonstandard_age <- function(data){
  
  require(tidyverse)
  
  df <- unique(data) %>% 
    mutate(AgeEnd = replace(AgeEnd, AgeLabel == "55-74", 75)) %>% # change AgeEnd to 75 for the 55-74 age group, consistent with other age groups
    arrange(SexID, AgeStart, AgeEnd) 
  
  age_start_m <- c(df$AgeStart[df$SexID == 1 & !(df$AgeLabel %in% c("Total", "Unknown"))], NA)
  age_end_m <- c(NA,df$AgeEnd[df$SexID == 1 & !(df$AgeLabel %in% c("Total", "Unknown"))])
  
  full_m <- age_start_m[1] == 0 & all(age_start_m[2:(length(age_start_m)-1)] == age_end_m[2:(length(age_end_m)-1)]) & age_end_m[length(age_end_m)] == 0
  
  age_start_f <- c(df$AgeStart[df$SexID == 2 & !(df$AgeLabel %in% c("Total", "Unknown"))], NA)
  age_end_f <- c(NA,df$AgeEnd[df$SexID == 2 & !(df$AgeLabel %in% c("Total", "Unknown"))])
  
  full_f <- age_start_f[1] == 0 & all(age_start_f[2:(length(age_start_f)-1)] == age_end_f[2:(length(age_end_f)-1)]) & age_end_f[length(age_end_f)] == 0
  
  is_full <- all(full_m, full_f)
  
  return(is_full)
  
}
