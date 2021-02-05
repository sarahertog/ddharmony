
# Ranking id 
# For each country- year, we rank the available ids for counts of vital events

dd_rank_id_vitals <- function(indata){
  
  # Check id duplication for each country-datatype-year
  out <-  indata %>% 
    group_by(TimeLabel, DataProcessType) %>% 
    mutate(num.id = length(unique(id))) %>% 
    ungroup()
  
  # If there is more than 1 id for each country-datatype-census year:
  if (max(out$num.id)>1) {
    out1 <-  out %>% 
      filter(num.id>=2) %>% 
      group_by(id) %>% 
      mutate(num.serie = length(unique(abridged[AgeLabel != "0-4"])),
             maxage = max(AgeStart)) %>% 
      ungroup %>% 
      
      # First: Prefer ids that have both abridged and complete available
      group_by(TimeLabel, DataProcessType) %>% 
      filter(num.serie == max(num.serie)) %>% 
      
      # Second:Prefer better DataReliability (the minimum of the DataReliabilitySort field)
      filter(DataReliabilitySort == min(DataReliabilitySort)) %>% 
      
      # Third: Prefer StatisticalConceptName == “Year of occurrence” over “Year of registration”
      mutate(has_occur = ifelse('Year of occurrence' %in% StatisticalConceptName, TRUE, FALSE),
             keep_occur = ifelse(has_occur == TRUE, 'Year of occurrence', StatisticalConceptName)) %>% 
      filter(StatisticalConceptName == keep_occur) %>% 
      
      # Third: Prefer StatisticalConceptName == “De facto” over “De jure”
      mutate(has_occur = ifelse('Year of occurrence' %in% StatisticalConceptName, TRUE, FALSE),
             keep_occur = ifelse(has_occur == TRUE, 'Year of occurrence', StatisticalConceptName)) %>% 
      filter(StatisticalConceptName == keep_occur) %>% 
      
      # Third: Prefer StatisticalConceptName == “De-facto” over “De-jure”
      mutate(has_de_facto = ifelse('De-facto' %in% StatisticalConceptName, TRUE, FALSE),
             keep_de_facto = ifelse(has_de_facto == TRUE, 'De-facto', StatisticalConceptName)) %>% 
      filter(StatisticalConceptName == keep_de_facto) %>% 
      
      # Fourth: Prefer the series with the highest open age group
      filter(maxage == max(maxage)) %>% 
      
      # Fifth: Prefer DataSourceName = "Demographic Yearbook"
      mutate(has_dyb = ifelse('Demographic Yearbook' %in% DataSourceName, TRUE, FALSE),
             keep_dyb = ifelse(has_dyb == TRUE, 'Demographic Yearbook', DataSourceName)) %>% 
      filter(DataSourceName == keep_dyb) %>% 
      
      # Sixth, keep most recent data source year
      filter(DataSourceYear == max(DataSourceYear)) %>% 
      ungroup() %>%
      
      # Finally discard a couple of duplicate series (if in sample) that have been hardcoded here bc they are not eliminated by above criteria
      filter(!(id %in% discard_these_dups)) %>% 

      select(-num.serie, -maxage, -has_occur, -keep_occur, -has_dyb, -keep_dyb, -has_de_facto, -keep_de_facto) 
    
  }  else { out1 <- NULL }
  
  # Only 1 id for each country-census year:
  out2 <- out %>% 
    filter(num.id == 1)
  
  outdata <- rbind(out1, out2) 
  
  outdata <- outdata %>% 
    arrange(TimeLabel, DataProcessType) %>% 
    select(!c(num.id))
  
} # end function

