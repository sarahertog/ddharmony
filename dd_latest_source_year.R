#--------------------------------------
# Select the latest data source year

# For some periods of reference, there are two **DataSourceYear**. 
# For these cases, the criteria is to choose the latest **DataSourceYear**

# ---------
# Function
# ---------
dd_latest_source_year <- function(data){
  require(tidyverse)
  data.out <- data %>% 
    group_by(AgeLabel) %>% 
    slice(which.max(DataSourceYear)) %>% 
    ungroup() 
  return(data.out)
}
