
# Extract all census population counts for a given country and time period
# and create unique id according to Census year, Data Source, Statistical Concept, Data Type
# other fields needed?

DDextract_VitalCounts <- function(locid, 
                                  type = c("births","deaths"), 
                                  process = c("census","vr"),
                                  start_year, end_year, 
                                  DataSourceShortName = NULL, 
                                  DataSourceYear = NULL) {
  
  if (type == "births") {
    indicator_ids <- c(159,170) # total births and births by age of mother
  } else if (type == "deaths") {
    indicator_ids <- c(194, 195, 188) 
  }
  
  dpi <- ifelse(process == "census", 2, 36)
  
  # Abridged or five year age groups
  tryCatch({
    vital_counts <- get_recorddata(locIds = locid,
                                   dataProcessIds = dpi, # Census or register
                                   indicatorIds = indicator_ids,
                                   startYear = start_year,
                                   endYear = end_year,
                                   locAreaTypeIds = 2, # whole area (as opposed to urban/rural or some other sub-national unit)
                                   subGroupIds = 2, # Total or all groups (as opposed to some population subgroup)
                                   dataSourceShortNames = DataSourceShortName,
                                   dataSourceYears = DataSourceYear) 
  }, error=function(e){cat("Error in file", conditionMessage(e), "\n")})
  
  if (exists('vital_counts')) {
    
    vital_counts <- vital_counts 
  } else { vital_counts <- NULL }
  
  outdata <- vital_counts

  return(outdata)
  
}