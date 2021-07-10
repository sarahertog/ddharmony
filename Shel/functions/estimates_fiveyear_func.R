## --------------------------------------------------------------------------------------------
## Merge the population data and births data, and generate age specific birth rate estimates.
## --------------------------------------------------------------------------------------------

estimates_fiveyear_func <- function(data){
  
  if(nrow(data) > 0){
    
  births_df <- data

  ### Merge the births data with the Location data and rename the agelabel variable to AgeLabel_num
  births_df2 <- Locations %>% 
    right_join(.,births_df , by = c("LocID", "LocName")) %>% 
    rename(AgeLabel_num = AgeLabel)
  
  ### Extract the 5-year counts..
  births_df2_5 <- births_df2 %>% 
    filter(five_year == TRUE)
  
  ### Generate a variable that shows the maximum age label
  
  if(nrow(births_df2_5) >0){
    
    births_df2_5 <- births_df2_5 %>% 
      separate(AgeLabel_num, into = c("ll", "ul"), sep = "-", remove = FALSE) %>% 
      mutate(ul = ifelse(is.na(ul) & 
                           AgeLabel_num %in% grep("\\d+", AgeLabel_num, value = TRUE), AgeLabel_num, ul)) %>% 
      mutate(ul = as.numeric(trimws(gsub("\\+", "", ul)))) %>% 
      group_by(id) %>% 
      mutate(max_age = ifelse(any(ul == max(ul, na.rm = TRUE)), 
                              AgeLabel_num[ul == max(ul, na.rm = TRUE)], NA)) %>% 
      ungroup() 
  }
  
  births_df2_5 <- births_df2_5 %>% 
    mutate(TimeLabel = as.character(TimeLabel))

  ### Merge the population data with the births data.
  
  merged_df <- pop_fiveyear_df %>% 
    #mutate(LocID = as.numeric(trimws(LocID))) %>% 
    #        TimeLabel = as.numeric(trimws(TimeLabel))) %>% 
    full_join(.,births_df2_5 , by = c("LocID", "TimeLabel",  "AgeLabel_denom" = "AgeLabel_num")) %>% 
    rename(births.count = DataValue, 
           AgeLabel = AgeLabel_denom) %>% 
    relocate(c(TimeLabel, AgeLabel, pop.count, births.count), .after = last_col()) %>% 
    arrange(TimeLabel)
  
  if(!"max_age" %in% names(merged_df)){
    merged_df$max_age <- NA
  }
  
  ### Fill in the missing values
  merged_df <- merged_df %>% 
    group_by(TimeLabel) %>% 
    mutate(id = na.locf0(id),
           max_age = na.locf0(max_age, fromLast = TRUE)) %>% 
    ungroup() %>% 
    group_by(id, TimeLabel) %>% 
    mutate(max_age = ifelse(all(is.na(births.count)), NA, max_age)) %>% 
    ungroup()
  
  ### Split age label into ll and ul and collapse the higher ages in the pop data to match those in the births data. 
  merged_df <- merged_df %>% 
    separate(AgeLabel, into = c("ll", "ul"), sep = "-", remove = FALSE) %>% 
    mutate(ul = ifelse(is.na(ul) & 
                         AgeLabel %in% grep("\\d+", AgeLabel, value = TRUE), AgeLabel, ul)) %>% 
    mutate(max_age2 = max_age) %>% 
    mutate(across(c(max_age2, ll, ul), ~as.numeric(trimws(gsub("\\+", "", .))))) %>% 
    mutate(juncture = ifelse(ul >= max_age2, 1, 0)) %>% 
    mutate(AgeLabel = ifelse(is.na(juncture), AgeLabel,
                             ifelse(juncture == 1 & AgeLabel!= "Total"  , max_age, 
                                    AgeLabel ))) %>% 
    group_by(id) %>% 
    mutate(pop.count = ifelse(is.na(juncture), pop.count,
                              ifelse(juncture == 1, sum(pop.count[juncture == 1], na.rm = TRUE),
                                     pop.count))) %>% 
    ungroup()
  
  ### Drop the ages that have been collapsed into a smaller bracket
  merged_df <- merged_df %>% 
    mutate(todrop = ifelse(AgeLabel == max_age & is.na(births.count) & !is.na(pop.count),
                           "drop", "keep")) %>% 
    filter(todrop == "keep"|is.na(todrop)) %>% 
    group_by(id) %>% 
    mutate(pop.count = ifelse(is.na(pop.count)  & AgeLabel == "Total", sum(pop.count, na.rm = TRUE), pop.count)) %>% 
    select(-ll, -ul, -max_age, -juncture, -todrop, -max_age2) %>% 
    ungroup()
  
  ### Generate a variable that shows whether births are missing, and another that shows whether population counts are missing
  merged_df <- merged_df %>% 
    group_by(id, TimeLabel) %>% 
    mutate(missing_births = ifelse(all(is.na(births.count)|all(births.count == 0)), TRUE, FALSE),
           missing_popcounts = ifelse(all(is.na(pop.count)|all(pop.count == 0)), TRUE, FALSE)) %>% 
    ungroup()
  
  ### Calculate the age specific birth rates for each of the years and age labels.
  merged_df <- merged_df %>% 
    mutate(asbr = births.count / pop.count)
  
  ### Reshape the data so that we have category on one variable (pop.count, births.count, asbr).
  
  merged_df2 <- merged_df %>% 
    select(-Index, -AgeStart, -AgeEnd, -AgeSpan, -AgeSort, -abridged, -complete, 
           -non_standard) %>% 
    mutate(pop.count = ifelse(is.na(pop.count), 0, pop.count),
           births.count = ifelse(is.na(births.count), 0, births.count),
           asbr = ifelse(is.na(asbr), 0, asbr)) %>% 
    group_by(id, TimeLabel) %>% 
    mutate_all(~na.locf0(., fromLast = FALSE)) %>% 
    ungroup() %>% 
    gather("category", "value", pop.count, births.count, asbr) %>%
    mutate(AgeLabel = trimws(str_squish(AgeLabel))) %>% 
    group_by(id, TimeLabel) %>% 
    spread(AgeLabel, value) %>% 
    arrange(TimeLabel) %>% 
    ungroup() 
  
  if(!"Total" %in% names(merged_df2)){
    merged_df2$Total <- NA
  }
  
  ### Re-order the variables
  vars <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30+" , "30-34","35-39","40+", "40-44", "45+", "45-49", "50+", "50-54", "55+", "55-59","60+",  "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")
  
  merged_df2 <- merged_df2 %>% 
    select(Variant: category, any_of(vars) , everything()) 
  
  agelabel_vars <- merged_df2 %>% 
    select(-Variant: -category, -Total) %>% 
    names()
  
  ### Calculate population count totals and birth count totals in cases where they are missing
  merged_df2$Total <- ifelse(is.na(merged_df2$Total) & merged_df2$category != "asbr", 
                             apply(merged_df2[,agelabel_vars], 1, function(x) 
                               sum(x, na.rm = TRUE)),merged_df2$Total)
  merged_df2$dummy_total <- apply(merged_df2[,agelabel_vars], 1, function(x) 
    sum(x, na.rm = TRUE))
  
  merged_df2$Total <- ifelse(is.na(merged_df2$Total) & merged_df2$category == "asbr" &
                               merged_df2$dummy_total==0, 0,merged_df2$Total) 

  merged_df2 <- merged_df2 %>% 
    select(-dummy_total)
  
  # vars <- grep("\\d", names(merged_df2), value = TRUE)
  # vars <- vars[!vars %in% "ISO3Alpha"]
  # 
  # merged_df2 <- merged_df2 %>% 
  #   select(id, TimeLabel:category, any_of(vars) ,  Total)
  
  }else
    merged_df2 <- data.frame()
  
  return(merged_df2)
}