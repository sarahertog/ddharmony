#--------------------------------------
# Check whether the age vector meets conditions for a full series

# ---------
# Function
# ---------
dd_series_isfull <- function(data, abridged = TRUE){
  require(tidyverse)
  
  oag <- data %>% dd_oag_agestart
  
  if (abridged) {
    ages_closed <- c(0,seq(5,max(5,oag-5),5)) 
  } else {
    ages_closed <- seq(0, max(1,oag-1), 1)
  }
  
  # a full series requires all open age groups and either an open age group or a total
  all_closed <- all(ages_closed %in% data$AgeStart[data$AgeSpan > 0])
  all_closed_and_oag <- all_closed & paste0(oag,"+") %in% data$AgeLabel
  all_closed_and_total <- all_closed & "Total" %in% data$AgeLabel

  is_full <- any(all_closed_and_oag, all_closed_and_total)

  return(is_full)
}
