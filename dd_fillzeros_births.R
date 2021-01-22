#--------------------------------------
# Fill in zeros for mother ages younger than 15 if there are no data reported

# ---------
# Function
# ---------
dd_fillzeros_births <- function(data, abridged = TRUE){
  
  require(tidyverse)
  
  sexes <- unique(data$SexID)
  
  df_sex <- NULL
  for (sex in sexes) {
    
    df <- dd_age_standard(data %>% filter(SexID == sex), abridged = abridged) %>% 
    mutate(DataValue = replace(DataValue, is.na(DataValue & AgeStart < 15 & AgeSpan > 0), 0),
           SexID = sex) %>% 
      filter(!is.na(DataValue))
    
    df_sex <- rbind(df_sex, df)
  }
  
  return(df_sex)

}


