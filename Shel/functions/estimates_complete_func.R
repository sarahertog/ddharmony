## --------------------------------------------------------------------------------------------
## Merge the population data and births data, and generate age specific birth rate estimates.
## --------------------------------------------------------------------------------------------

estimates_complete_func <- function(data){
  
  if(nrow(data) > 0){
    
    births_df <- data
    
    ### Merge the births data with the Location data and rename the agelabel variable to AgeLabel_num.
    births_df2 <- Locations %>% 
      right_join(.,births_df , by = c("LocID", "LocName")) %>% 
      rename(AgeLabel_num = AgeLabel)
    
    ### Extract the complete counts
    births_df2_1 <- births_df2 %>% 
      filter(complete == TRUE)
    
    ### Generate a variable that shows the maximum age label
    if(nrow(births_df2_1) >0){
      
      births_df2_1 <- births_df2_1 %>% 
        mutate(AgeLabel_num2 = as.numeric(trimws(gsub("\\+", "", AgeLabel_num)))) %>% 
        group_by(id) %>% 
        mutate(max_age = ifelse(any(AgeLabel_num2 == max(AgeLabel_num2, na.rm = TRUE)), 
                                AgeLabel_num[AgeLabel_num2 == max(AgeLabel_num2, 
                                                                  na.rm = TRUE)], NA)) %>% 
        ungroup() %>% 
        select(-AgeLabel_num2)
    }
    
    births_df2_1 <- births_df2_1 %>% 
      mutate(TimeLabel = as.character(TimeLabel))
    
       
    ### Merge the population data with the births data.
    merged_df <- pop_complete_df %>% 
      full_join(.,births_df2_1 , by = c("LocID", "TimeLabel",  "AgeLabel_denom" = "AgeLabel_num")) %>% 
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
    
    ### Collapse the higher ages in the pop data to match those in the births data.
    
    merged_df <- merged_df %>% 
      mutate(max_age2 = max_age,
             AgeLabel2 = AgeLabel) %>% 
      mutate(max_age2 = as.numeric(trimws(gsub("\\+", "", max_age2))),
             AgeLabel2 = as.numeric(trimws(gsub("\\+", "", AgeLabel2)))) %>% 
      mutate(juncture = ifelse(AgeLabel2 >= max_age2, 1, 0)) %>% 
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
      mutate(todrop = ifelse(AgeLabel == max_age & is.na(births.count) & !is.na(pop.count),"drop",
                             "keep")) %>% 
      filter(todrop == "keep"|is.na(todrop)) %>% 
      group_by(id) %>% 
      mutate(pop.count = ifelse(is.na(pop.count) & AgeLabel == "Total", sum(pop.count, na.rm = TRUE), pop.count)) %>% 
      select(-AgeLabel2, -juncture, -todrop, -max_age, -max_age2) %>% 
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
    vars <- c(paste(0:49), "50", "50+", paste(51:99), "100", "100+")
    
    merged_df2 <- merged_df2 %>% 
      select(Variant: category, any_of(vars) , 
             everything()) 
    
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

  }else
    merged_df2 <- data.frame()
  
  return(merged_df2)
}