#--------------------------------------
# Extract records that refer to single-year of age

# Sometimes a series identified as abridged in DemoData includes records by single year of age
# We extract these for use on a single year series

# ---------
# Function
# ---------
dd_extract_single <- function(data){
  require(tidyverse)
  data.out <- data %>% 
    dplyr::filter(AgeSpan == 1 )
  return(data.out)
}
