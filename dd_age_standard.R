#------------------------------------------
# Add standard age groups in the dataset

# The function StdAge.fun adds in the dataset a variable with standard age groups.
# This variable is important to check missing age groups.

# The output of the function StdAge.fun is the dataset with standard age groups.

# Note that the standard series of age group has different open age interval, 
# which goes from 60+ to 100+. The function covers all possible open age intervals across countries. 
# Later, I will select one open age group for each country/area. 
# SH modified this from MN's original function to accomodate any open age groups from
# 40+ to 130+ and also to store this standard age info in a data frame that can be
# modified, as needed, for future use.

# ---------
# Function
# ---------
dd_age_standard<- function(data, abridged = TRUE){ 
  require(tidyverse)

  # get stanadard age groups
    std_ages <- std_age_function()
    
    if (abridged) {
      std_ages <- std_ages %>% 
        dplyr::filter(abridged == TRUE)
    } else { std_ages <- std_ages %>% 
      dplyr::filter(complete==TRUE)
    }
    std_ages <- std_ages %>% 
      select(-abridged, -complete) 
  
   
  #c)Join standard groups with the data from DemoData
  data.out <- data %>% 
   full_join(x=std_ages, 
             by=c("AgeStart","AgeEnd","AgeLabel","AgeSpan")) %>% 
    arrange(AgeSort) 
    
  return(data.out)
}
