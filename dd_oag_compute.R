#--------------------------------------------------------------------------------
# compute data value for the open age group needed to close the series

# ---------
# Function
# ---------

# MAKE THIS COMPUTE FOR ALL POSSIBLE OPEN AGE GROUPS

dd_oag_compute  <- function(data, age_span = c(1, 5)){ 
  require(tidyverse)
  
  # define standard abridged age groups
  load("data/std_ages.RData")
  age_std <- std_ages %>% 
    filter(abridged == TRUE) %>% 
    select(-abridged, -complete)
  
  
  df <- data %>% 
    filter(!is.na(DataValue) & AgeSpan %in% c(age_span, -1, -2))
  
  oag_start <- data %>% dd_oag_agestart
  
  # list standard open age groups that can be computed from the complete series
  if (!is_empty(oag_start)) {
  age_std_open <- age_std %>% 
    filter(AgeSpan < 0 & AgeStart <= oag_start & !(AgeStart %in% c(-2,0)))
  } else {
    age_std_open <- NULL
  }
  
  nrows <- ifelse(is.null(age_std_open), 0, nrow(age_std_open))
  
 if (nrows > 0) {
  
  # look for an open age group record on input file that closes out the series
  oag_indata <- df %>% 
    filter(AgeLabel == paste0(oag_start,"+")) %>% 
    select(AgeStart, AgeEnd, AgeSpan, AgeLabel, DataValue)

  oag_check <- nrow(oag_indata) > 0
  
  total_check <- "Total" %in% data$AgeLabel
  if (total_check) {
    total_value <- data$DataValue[data$AgeLabel=="Total"]
    total_value_valid <- total_value >= sum(df$DataValue[df$AgeSpan == age_span])
  } else { total_value_valid <- FALSE }

  unknown_check <- "Unknown" %in% data$AgeLabel
  unknown_value <- ifelse(unknown_check, data$DataValue[data$AgeLabel=="Unknown"], 0)
  
  # if there is an open age group record on complete, then use this to compute other open
  # age groups
  if (oag_check) {
    
    data.out <- NULL
    for (i in 1:nrow(age_std_open)) {
      df.out <- df %>% 
        filter((AgeStart >= age_std_open$AgeStart[i] & AgeSpan > 0) | AgeLabel==paste0(oag_start,"+")) %>% 
        select(AgeStart,AgeEnd,AgeSpan,AgeLabel,DataValue) %>% 
        summarise(DataValue = sum(DataValue)) %>% 
        mutate(AgeStart = age_std_open$AgeStart[i],
               AgeEnd   = age_std_open$AgeEnd[i],
               AgeSpan  = age_std_open$AgeSpan[i],
               AgeLabel = age_std_open$AgeLabel[i],
               AgeSort  = age_std_open$AgeSort[i])
      data.out <- rbind(data.out,df.out)
    }
    
  } 
  
  # if the needed open age group is not on the series, but there is a total, then use this to compute oag values
  if (!(oag_check) & total_check & total_value_valid) {
    
    data.out <- NULL
    for (i in 1:nrow(age_std_open)) {
      df.out <- df %>% 
        filter(AgeStart < age_std_open$AgeStart[i] & AgeSpan > 0) %>% 
        select(AgeStart,AgeEnd,AgeSpan,AgeLabel,DataValue) %>% 
        summarise(DataValue = total_value - unknown_value - sum(DataValue)) %>% 
        mutate(AgeStart = age_std_open$AgeStart[i],
               AgeEnd   = age_std_open$AgeEnd[i],
               AgeSpan  = age_std_open$AgeSpan[i],
               AgeLabel = age_std_open$AgeLabel[i],
               AgeSort  = age_std_open$AgeSort[i])
      data.out <- rbind(data.out,df.out)
    }
  }
  
  # if there is neither an open age group nor a Total on abridged, then we don't
  # compute open age group values
  if ((!(oag_check)) & (!(total_check) | !(total_value_valid))) {
    data.out <- NULL
  }
   
 } else { data.out <- NULL}
  
  return(data.out)
}