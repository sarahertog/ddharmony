#--------------------------------------------------------------------------------
# Create abridged age groups based on population counts by single years of age

# In case there are missing age groups in abridged series, one can use population 
# counts by single year of age to fill missing information. 
# The function dd_single2abridged creates 5-year age groups based on data by single years
# of age.

# SH modified to require fewer by group variables as input and use standard abridged age groups 
# as defined in std_ages file
# ---------
# Function
# ---------
# The argument of the function is:
# data = dataset with age series by single years of age
dd_single2abridged  <- function(data){ #input should be a dataset by single year of age
  require(tidyverse)
  
  # define standard abridged age groups
  load("data/std_ages.RData")
  age_std <- std_ages %>% 
    filter(abridged == TRUE) %>% 
    select(-abridged, -complete)
  
  # a) Closed age groups
  age_std_closed <- age_std %>% 
    filter(AgeSpan >0)
  
  data.out1 <- NULL
  for (i in 1:nrow(age_std_closed)) {
    
    df <- data %>% 
      filter(AgeSpan==1 & 
               AgeStart>= age_std_closed$AgeStart[i] & 
               AgeStart < age_std_closed$AgeEnd[i] & 
               !is.na(DataValue)) 
    
    if (nrow(df) == age_std_closed$AgeSpan[i]) {
    
    df <- df %>% 
      summarise(DataValue = sum(DataValue)) %>% 
      mutate(AgeStart = age_std_closed$AgeStart[i],
             AgeEnd   = age_std_closed$AgeEnd[i],
             AgeSpan  = age_std_closed$AgeSpan[i],
             AgeLabel = age_std_closed$AgeLabel[i],
             AgeSort  = age_std_closed$AgeSort[i])
    } else {
      df <- data.frame(AgeStart = age_std_closed$AgeStart[i],
                       AgeEnd = age_std_closed$AgeEnd[i],
                       AgeSpan = age_std_closed$AgeSpan[i],
                       AgeLabel = age_std_closed$AgeLabel[i],
                       AgeSort = age_std_closed$AgeSort[i],
                       DataValue = NA)
    }
    data.out1 <- rbind(data.out1,df)
  }
    
  # append open age group, unknown an total
  
  data.out <- data %>% 
    bind_rows(data.out1 %>% 
                filter(AgeLabel !="0")) %>% # remove first age group to avoid dups
    filter(AgeSort %in% age_std$AgeSort & !is.na(DataValue)) %>% 
    arrange(AgeSort)
  
  return(data.out)
}