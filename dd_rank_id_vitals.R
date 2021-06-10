
# Ranking id 
# For each country- year, we rank the available ids for counts of vital events

dd_rank_id_vitals <- function(indata){
  
  # Check id duplication for each country-year
  out <-  indata %>% 
    group_by(TimeLabel) %>% 
    mutate(num.id = length(unique(id))) %>% 
    ungroup()
  
  # If there is more than 1 id for each country-datatype-census year:
  if (max(out$num.id)>1) {
    out1 <-  out %>% 
      dplyr::filter(num.id>=2) %>% 
      group_by(id) %>% 
      mutate(num.serie = length(unique(abridged[AgeLabel != "0-4"])),
             maxage = max(AgeStart)) %>% 
      ungroup %>% 
      
      # First: Rank by Statistical Concept, preferring “Year of occurrence” over “Year of registration”, for example
      group_by(TimeLabel) %>% 
      dplyr::filter(StatisticalConceptSort == min(StatisticalConceptSort)) %>% 
      
      # Second: Rank by DataStatus, preferring Final over Provisional, for example
      dplyr::filter(DataStatusSort == min(DataStatusSort)) %>% 
      
      # Third: Rank by DataProcess
      dplyr::filter(DataProcessSort == min(DataProcessSort)) %>% 
      
      # Fourth: Rank by DataProcessType
      dplyr::filter(DataProcessTypeSort == min(DataProcessTypeSort)) %>% 
      
      # Fifth: Prefer better DataReliability 
      dplyr::filter(DataReliabilitySort == min(DataReliabilitySort)) %>% 
      
      # Sixth, keep most recent data source year
      dplyr::filter(DataSourceYear == max(DataSourceYear)) %>% 
      
      # Seventh: Prefer ids that have both abridged and complete available
      dplyr::filter(num.serie == max(num.serie)) %>% 
      
      # Eighth: Prefer the series with the highest open age group
      dplyr::filter(maxage == max(maxage)) %>% 
      
      # Ninth: Prefer DataSourceName = "Demographic Yearbook"
      mutate(has_dyb = ifelse('Demographic Yearbook' %in% DataSourceName, TRUE, FALSE),
             keep_dyb = ifelse(has_dyb == TRUE, 'Demographic Yearbook', DataSourceName)) %>% 
      dplyr::filter(DataSourceName == keep_dyb) %>% 
      
      ungroup() %>%
      
      # Finally discard a couple of duplicate series (if in sample) that have been hardcoded here bc they are not eliminated by above criteria
      dplyr::filter(!(id %in% discard_these_dups)) %>% 

      select(-num.serie, -maxage, -has_dyb, -keep_dyb) 
    
  }  else { out1 <- NULL }
  
  # Only 1 id for each country-census year:
  out2 <- out %>% 
    dplyr::filter(num.id == 1)
  
  outdata <- rbind(out1, out2) 
  
  outdata <- outdata %>% 
    arrange(TimeLabel, DataProcessType) %>% 
    select(!c(num.id))
  
} # end function

