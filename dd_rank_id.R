
# Ranking id 
# For each country-census year, we rank the available ids

dd_rank_id <- function(indata){
  
  # Discard DataTypeName==“Direct (standard abridged age groups computed)” 
  # or “Direct (standard abridged age groups computed - Unknown redistributed)”
  out <- indata %>% filter(DataTypeName!= 'Direct (standard abridged age groups computed)',
                           DataTypeName!= 'Direct (standard abridged age groups computed - Unknown redistributed)')
  # Check id duplication for each country-census year
  out <-  out %>% 
    group_by(ReferencePeriod) %>% 
    mutate(num.id = length(unique(id))) %>% 
    ungroup()
  
  # If there is more than 1 id for each country-census year:
  if (length(unique(out$num.id))>1) {
    out1 <-  out %>% 
      filter(num.id>=2) %>% 
      group_by(id) %>% 
      mutate(num.serie = length(unique(abridged)),
             maxage = max(AgeStart)) %>% 
      ungroup %>% 
      
      # First: Prefer ids that have both abridged and complete available
      group_by(ReferencePeriod) %>% 
      filter(num.serie == max(num.serie)) %>% 
      
      # Second:Prefer better DataReliability (the minimum of the DataReliabilitySort field)
      filter(DataReliabilitySort == min(DataReliabilitySort)) %>% 
      
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
      ungroup() %>% 
      select(-num.serie, -maxage, -has_de_facto, -keep_de_facto, -has_dyb, -keep_dyb) 
    
  }  else { out1 <- NULL }
  
  # Only 1 id for each country-census year:
  out2 <- out %>% 
    filter(num.id == 1)
  
  outdata <- rbind(out1, out2) 
  
  outdata <- outdata %>% 
    arrange(ReferencePeriod) %>% 
    select(!c(num.id))
  
} # end function

