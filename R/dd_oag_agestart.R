#' @title
#' dd_oag_agestart
#'
#' @description
#' Identifies the starting age of the open age group needed to close the series.
#'
#' @details
#' The function returns the starting age needed for the open age group that closes the series. This corresponds to the
#' `AgeEnd` of the maximum `AgeStart` in the series, for records with an `AgeSpan > 0` per id, sex id and time label.
#' You can run `getAnywhere(dd_oag_agestart)` to see the function definition.
#'
#' @param data The dataset to be harmonized
#' @param multiple5 Whether the series contains 5 year age labels or not
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return The starting age of the open age group needed to close the series.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- series_isfull_df
#' oag_start <- dd_oag_agestart(df, multiple5 = TRUE)
#' }
dd_oag_agestart <- function(data, multiple5 = TRUE){

  ## Shel added suppressWarnings() to remove the following warning:
  ## In max(AgeStart) : no non-missing arguments to max; returning -Inf
  ## "748 - Eswatini - VR - Deaths - 2016 - Register - Demographic Yearbook - Year of registration - Direct - Low"

  maxage <- suppressWarnings(data %>%
    dplyr::filter(AgeSpan > 0) %>%
    dplyr::filter(AgeStart == max(AgeStart)))

  oag_start <- maxage$AgeEnd

  if (multiple5 == TRUE) {
    # ensure it is a multiple of 5
    oag_start <- floor(oag_start/5) * 5
  }

  return(oag_start)
}
