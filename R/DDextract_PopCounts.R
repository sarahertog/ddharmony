#' DDextract_PopCounts
#'
#' Extract population counts by age and sex for a given country and time period can choose process = "census", "estimate", or "register"
#'
#' @param locid The Location Id for each country. Run `DemoTools::get_locations()` to get information about available locations. The ids are indicated in the `PK_LocID` variable.
#' @param process The data collection process i.e. Census (census), Estimate (estimate) or Register (register).
#' @param start_year The minimum year for the data available for each country
#' @param end_year The maximum year for the data available for each country
#' @param locType The location type for which data are extracted i.e. Whole, Urban, Rural
#' @param DataSourceShortName NULL.
#' @param DataSourceYear NULL.
#' @return A dataset showing population counts for each location, process, year, sex and age label.
#' @export
#'
#' @examples
#' \dontrun{
#' dd_extract <- DDextract_PopCounts(404, #Kenya
#'                                    process = c("census","estimate","register"),
#'                                    1950,
#'                                    2050,
#'                                    DataSourceShortName = NULL,
#'                                    DataSourceYear = NULL)
#'}


DDextract_PopCounts <- function(locid,
                                      process = c("census","estimate","register"),
                                      start_year,
                                      end_year,
                                      locType = "whole", # default is whole area
                                      DataSourceShortName = NULL,
                                      DataSourceYear = NULL) {

  ## Indicate the data process id. dpi == 2 if process is census, 6 if process is estimate and 9 if process is register
  dpi <- NA
  dpi <- ifelse(process == "census", 2, dpi)
  dpi <- ifelse(process == "estimate", 6, dpi)
  dpi <- ifelse(process == "register", 9, dpi)

  ## Indicate location area type. lt ==2 if whole, 3 if urban, 4 if rural
  lt <- NA
  if (is.null(locType)) { locType <- "whole" } # default is whole area
  lt <- ifelse(tolower(locType) == "whole", 2, lt)
  lt <- ifelse(tolower(locType) == "urban", 3, lt)
  lt <- ifelse(tolower(locType) == "rural", 4, lt)

  # Abridged or five year age groups
  tryCatch({
    pop_abridged <- get_recorddata(locIds = locid,
                                   dataProcessIds = dpi,
                                   indicatorIds = 58,# Population by age and sex (abridged)
                                   startYear = start_year,
                                   endYear = end_year,
                                   locAreaTypeIds = lt,
                                   subGroupIds = 2, # Total or all groups (as opposed to some population subgroup)
                                   dataSourceShortNames = DataSourceShortName,
                                   dataSourceYears = DataSourceYear)
  }, error=function(e){check_locid(locid)})

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
                                   locAreaTypeIds = lt,
                                   subGroupIds = 2, # Total or all groups (as opposed to some population subgroup)
                                   dataSourceShortNames = DataSourceShortName,
                                   dataSourceYears = DataSourceYear)
  }, error=function(e){check_locid(locid)})

  if (exists('pop_single')) {
    pop_single <- pop_single
  } else { pop_single <- NULL }
  outdata <- rbind(pop_abridged, pop_single)

  return(outdata)

}
