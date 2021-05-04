#--------------------------------------
# Check whether the age vector meets conditions for a full series

# ---------
# Function
# ---------
dd_series_isfull <- function(data, abridged = TRUE){
  require(tidyverse)
  
  if (abridged) {
    oag <- dd_oag_agestart(data = data, multiple5 = TRUE)
    ages_closed <- c(0,seq(5,max(5,oag-5),5)) 
    
    ages_child_abr <- all(c("0","1-4") %in% data$AgeLabel)
    ages_child_five <- all(c("0-4","5-9") %in% data$AgeLabel)
    
  } else {
    oag <- dd_oag_agestart(data = data, multiple5 = FALSE)
    ages_closed <- seq(0, max(1,oag-1), 1)
  }
  
  # a full series requires all closed age groups and either an open age group or a total
  all_closed <- all(ages_closed %in% data$AgeStart[data$AgeSpan > 0])
  all_closed_and_oag <- all_closed & paste0(oag,"+") %in% data$AgeLabel
  all_closed_and_total <- all_closed & "Total" %in% data$AgeLabel

  is_full <- any(all_closed_and_oag, all_closed_and_total)
  
  # if abridged, then a full series also requires either both abridged child ages or both five-year child ages to be present
  if (abridged) {
    is_full <- is_full & (ages_child_abr | ages_child_five)
  }

  return(is_full)
}
