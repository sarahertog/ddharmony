#' DDextract_PopTotals
#'
#' Extract population totals, by sex and location type, for a given country and time period can choose process = "census", "estimate", or "register"
#'
#' @param locid The Location Id for each country. Run `DemoTools::get_locations()` to get information about available locations. The ids are indicated in the `PK_LocID` variable.
#' @param process The data collection process i.e. Census (census), Estimate (estimate) or Register (register).
#' @param start_year The minimum year for the data available for each country
#' @param end_year The maximum year for the data available for each country
#' @param DataSourceShortName NULL.
#' @param DataSourceYear NULL.
#' @return A data.frame showing population counts for each location, process, year, location type and sex.
#' @export
#'
#' @examples
#' \dontrun{
#' dd_extract <- DDextract_PopTotals(404, #Kenya
#'                                    process = c("census","estimate","register"),
#'                                    1950,
#'                                    2050,
#'                                    DataSourceShortName = NULL,
#'                                    DataSourceYear = NULL)
#'}


DDextract_PopTotals <- function(locid,
                                      process = c("census","estimate","register"),
                                      start_year,
                                      end_year,
                                      DataSourceShortName = NULL,
                                      DataSourceYear = NULL,
                                      server = "https://popdiv.dfs.un.org/DemoData/api/") {
  
  ## UNPD server housing DemoData
  options(unpd_server = server)

  ## Indicate the data process id. dpi == 2 if process is census, 6 if process is estimate and 9 if process is register
  dpi <- NA
  dpi <- ifelse(process == "census", 2, dpi)
  dpi <- ifelse(process == "estimate", 6, dpi)
  dpi <- ifelse(process == "register", 9, dpi)

  tryCatch({
    pop_total <- get_recorddata(locIds = locid,
                                   dataProcessIds = dpi,
                                   indicatorIds = 52,# Total population by sex
                                   startYear = start_year,
                                   endYear = end_year,
                                   locAreaTypeIds = c(2,3,4),
                                   subGroupIds = 2, # Total or all groups (as opposed to some population subgroup)
                                   dataSourceShortNames = DataSourceShortName,
                                   dataSourceYears = DataSourceYear)
  }, error=function(e){check_locid(locid)})

  if (exists('pop_total')) {
    pop_total <- pop_total
  } else { pop_total <- NULL }

 return(pop_total)

}
