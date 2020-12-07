
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
    mutate(num.id = length(unique(id)), 
           minDRsort = min(unique(out$DataReliabilitySort)),
           num.serie = length(unique(abridged)))
  
  # If there is more than 1 id for each country-census year:
  if (length(unique(out$num.id))>1) {
    out1 <-  out %>% 
      filter(num.id>=2)%>% 
      # First: Prefer ids that have both abridged and complete available
      filter(num.serie>1) %>% 
      # Second:Prefer better DataReliability (the minimum of the DataReliabilitySort field)
      filter(DataReliabilitySort==minDRsort) %>% 
      # Third: Prefer StatisticalConceptName == “De-facto” over “De-jure”
      filter(StatisticalConceptName=='De-facto') %>% 
      ungroup() %>%
      # Fourth: Prefer the series with the highest open age group
      group_by(ReferencePeriod, id) %>% # one max age for each ref period and id
      mutate(maxage = max(AgeStart)) %>% 
      filter(maxage == max(maxage)) %>% 
      ungroup() %>% 
      # Fifth: Prefer DataSourceName = "Demographic Yearbook"
      group_by(ReferencePeriod) %>% 
      filter(DataSourceName=="Demographic Yearbook") %>% 
      select(-maxage) %>% 
      ungroup()
    
  } else { out1 <- NULL }
  
  # Only 1 id for each country-census year:
  out2 <- out %>% 
    filter(num.id == 1)
  
  outdata <- rbind(out1, out2) 
  
  outdata <- outdata %>% 
    arrange(ReferencePeriod) %>% 
    select(!c(num.id,minDRsort,num.serie))
  
} # end function

