#' @title
#' dd_oag_multiple
#'
#' @description
#' Checks whether the series has more than one open age group.
#'
#' @details
#' An open age group is identified as one where the `AgeSpan` == -1 and `AgeStart` != 0.
#' You can run `getAnywhere(dd_oag_multiple)` to view the definition of this function.
#'
#' @param data The data to be harmonized
#'
#' @return `TRUE/FALSE`
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dd_oag_multiple(oag_multiple_df)
#' }

dd_oag_multiple <- function(data){

  oags <- data$AgeLabel[data$AgeSpan == -1 & data$AgeStart != 0]

  check <- length(oags) > 1

  return(check)
}
