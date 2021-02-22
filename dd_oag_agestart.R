#--------------------------------------
# Identify the starting age needed for the open age group that closes the series
# Must be a multiple of 5


# ---------
# Function
# ---------
dd_oag_agestart <- function(data, multiple5 = TRUE){
  require(tidyverse)
  
  maxage <- data %>% 
    dplyr::filter(AgeSpan > 0) %>% 
    dplyr::filter(AgeStart == max(AgeStart))
    
  oag_start <- maxage$AgeEnd
  
  if (multiple5 == TRUE) {
    # ensure it is a multiple of 5
    oag_start <- floor(oag_start/5) * 5
  }
  
  return(oag_start)
}
