
# Extract all census population counts for a given country and time period


DDextract_CensusPopCounts <- function(locid, start_year, end_year, DataSourceShortName = NULL, DataSourceYear = NULL) {
  
  # Abridged or five year age groups
  tryCatch({
    pop_abridged <- get_recorddata(locIds = locid,
                                   dataProcessIds = 2, # Census
                                   indicatorIds = 58,# Population by age and sex (abridged)
                                   startYear = start_year,
                                   endYear = end_year,
                                   locAreaTypeIds = 2, # whole area (as opposed to urban/rural or some other sub-national unit)
                                   subGroupIds = 2, # Total or all groups (as opposed to some population subgroup)
                                   dataSourceShortNames = DataSourceShortName,
                                   dataSourceYears = DataSourceYear) 
  }, error=function(e){cat("Error in file", conditionMessage(e), "\n")})
  
  if (exists('pop_abridged')) {
    
    pop_abridged <- pop_abridged 
  } else { pop_abridged <- NULL }
  
  # By single year of age
  tryCatch({
    pop_single <- get_recorddata(locIds = locid,
                                   dataProcessIds = 2, # Census
                                   indicatorIds = 60,# Population by age and sex (Complete by single years of age)
                                   startYear = start_year,
                                   endYear = end_year,
                                   locAreaTypeIds = 2, # whole area (as opposed to urban/rural or some other sub-national unit)
                                   subGroupIds = 2, # Total or all groups (as opposed to some population subgroup)
                                   dataSourceShortNames = DataSourceShortName,
                                   dataSourceYears = DataSourceYear) 
  }, error=function(e){cat("Error in file", conditionMessage(e), "\n")})

  if (exists('pop_single')) {
    pop_single <- pop_single 
      

  } else { pop_single <- NULL }

  outdata <- rbind(pop_abridged, pop_single)

  return(outdata)
  
}