#------------------------
# Distribute Unknowns over age

# ---------
# Function
# ---------
dd_distribute_unknowns <- function(data){
  require(tidyverse)
  
  out.data <- NULL
  
  for (sex in unique(data$SexID)) {
    
    df_abr <- data %>% 
      filter(SexID == sex & abridged == TRUE)
    
    total_value   <- df_abr$DataValue[df_abr$AgeLabel == "Total"]
    unknown_value <- df_abr$DataValue[df_abr$AgeLabel == "Unknown"]
    
    if (!is_empty(total_value)) {
    df_abr <- df_abr %>% 
      mutate(pct_dist = DataValue/total_value,
             add_unk  = unknown_value * pct_dist,
             add_unk  = ifelse(AgeLabel == "Total", 0, add_unk),
             DataValue = DataValue + add_unk) %>% 
      filter(AgeLabel != "Unknown") %>% 
      select(-pct_dist, -add_unk)
    }
    rm(total_value, unknown_value)
    
    df_cpl <- data %>% 
      filter(SexID == sex & complete == TRUE)
    
    total_value   <- df_cpl$DataValue[df_cpl$AgeLabel == "Total"]
    unknown_value <- df_cpl$DataValue[df_cpl$AgeLabel == "Unknown"]
    
    if (!is_empty(total_value)) {
      df_cpl <- df_cpl %>% 
        mutate(pct_dist = DataValue/total_value,
               add_unk  = unknown_value * pct_dist,
               add_unk  = ifelse(AgeLabel == "Total", 0, add_unk),
               DataValue = DataValue + add_unk) %>% 
        filter(AgeLabel != "Unknown") %>% 
        select(-pct_dist, -add_unk)
    }
    
  out.data <- rbind(out.data, df_abr, df_cpl)
  }
  
  return(out.data)
  
}
