#--------------------------------------
# Check whether the series has an open age group that closes it


# ---------
# Function
# ---------
dd_oag_exists <- function(data){
  require(tidyverse)
  oag_begin <- data$AgeEnd[data$AgeStart == max(data$AgeStart[data$AgeSpan > 0])] 
  
  check <- paste0(oag_begin,"+") %in% data$AgeLabel
  
  return(check)
}
