
# Extract population counts by age and sex for a given country and time period
# can choose process = "census", "estimate", or "register"


DDextract_PopCounts <- function(locid, 
                                      process = c("census","estimate","register"), 
                                      start_year, 
                                      end_year, 
                                      DataSourceShortName = NULL, 
                                      DataSourceYear = NULL) {
  
  # get data process id
  dpi <- NA
  dpi <- ifelse(process == "census", 2, dpi)
  dpi <- ifelse(process == "estimate", 6, dpi)
  dpi <- ifelse(process == "register", 9, dpi)
  
  # Abridged or five year age groups
  tryCatch({
    pop_abridged <- get_recorddata(locIds = locid,
                                   dataProcessIds = dpi, 
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
                                   dataProcessIds = dpi, 
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