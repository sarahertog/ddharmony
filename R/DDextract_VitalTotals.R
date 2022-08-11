#' @title
#' DDextract_VitalTotals
#'
#' @description
#' Extracts total births and/or total deaths data for a given country and time period from the UNDP portal.
#'
#' @details
#' This function extracts data using \href{https://timriffe.github.io/DDSQLtools/reference/get_recorddata.html}{get_recorddata()}.
#' The function returns `NULL` in cases where the data is not available and a dataframe of 93 variables where it exists.
#' In cases where the user inserts an invalid location id, a message is returned prompting them to run `View(get_locations())` to get a list of plausible location ids. See
#' \link{check_locid} for more details on this. You can run `getAnywhere(DDextract_VitalCounts)` to see how the function is defined.
#'
#' @param locid The Location Id for each country. Run `DemoTools::get_locations()` to get information about available locations. The ids are indicated in the `PK_LocID` variable.
#' @param type The type of data to be pulled i.e. births/deaths.
#' @param process The data collection process i.e. Census (census) or Vital Registrations (vr).
#' @param start_year The minimum year for the data to be extracted.
#' @param end_year The maximum year for the data to be extracted.
#' @param DataSourceShortName NULL.
#' @param DataSourceYear NULL.
#'
#' @return A dataset showing counts for each location, type of data (births or deaths), process, year, sex and age label.
#'
#' @export
#'}
DDextract_VitalTotals <- function(locid,
                                  type = c("births","deaths"),
                                  process = c("census","vr"),
                                  start_year,
                                  end_year,
                                  DataSourceShortName = NULL,
                                  DataSourceYear = NULL,
                                  server = "https://popdiv.dfs.un.org/DemoData/api/") {
  
  ## UNPD server housing DemoData
  options(unpd_server = server)

## List the indicator ids for each type of data
  if (type == "births") {
    indicator_ids <- 159 # total births
  } else if (type == "deaths") {
    indicator_ids <- 188
  }

  ## Indicate the data process id. dpi == 2 if process is census and 36 if process is vr
  dpi <- ifelse(process == "census", 2, 36)

  ## Extract the data from the UNDP portal and return NULL if the data does not exist
  tryCatch({
    vital_counts <- DDSQLtools::get_recorddata(locIds = locid,
                                   dataProcessIds = dpi, # Census or register
                                   indicatorIds = indicator_ids,
                                   startYear = start_year,
                                   endYear = end_year,
                                   locAreaTypeIds = c(2,3,4), 
                                   subGroupIds = 2, # Total or all groups (as opposed to some population subgroup)
                                   dataSourceShortNames = DataSourceShortName,
                                   dataSourceYears = DataSourceYear)
  }, error=function(e){check_locid(locid)})


  ## If the data exists, it will appear on the environment as a dataframe, else it will appear as NULL
  if (exists('vital_counts')) {

    vital_counts <- vital_counts

  } else {

    vital_counts <- NULL }

  outdata <- vital_counts

  return(outdata)

}
