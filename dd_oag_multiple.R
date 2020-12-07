#--------------------------------------
# Check whether the series has more than one open age group


# ---------
# Function
# ---------
dd_oag_multiple <- function(data){
  require(tidyverse)
  oags <- data$AgeLabel[data$AgeSpan == -1 & data$AgeStart != 0] 
  
  check <- length(oags) > 1
  
  return(check)
}
