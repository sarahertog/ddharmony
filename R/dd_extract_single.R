#' dd_extract_single
#'
#' Extract records that refer to single-year of age. Sometimes a series identified as abridged in DemoData includes records by single year of age We extract these for use on a single year series
#'
#' @param data The data to be harmonized
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset with single-year of age records only
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame()
#' }
dd_extract_single <- function(data){
  data.out <- data %>%
    dplyr::filter(AgeSpan == 1 )
  return(data.out)
}
