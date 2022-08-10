#' @title
#' dd_firstages_compute
#'
#' @description
#' Reconcile ages 0-1, 1-4 and 0-4
#'
#' @details
#' If `0-4` is missing and `0-1` and `1-4` are present, then sum both to get `0-4`.
#' You can run `getAnywhere(dd_firstages_compute)` to view the definition of this function.
#'
#' @param data The data to be harmonized
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset with ages `0-1`, `1-4` and `0-4` reconciled.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- firstages_compute_df
#' df <- dd_firstages_compute(df)
#' }

dd_firstages_compute <- function(data){

  df <- dd_age_standard(data, abridged = TRUE)

  dv01 <- df$DataValue[df$AgeLabel == "0"]
  dv14 <- df$DataValue[df$AgeLabel == "1-4"]
  dv04 <- df$DataValue[df$AgeLabel == "0-4"]

  # if 0-4 is missing and 0-1 and 1-4 are present, then sum to 0-4
  if(!is.na(dv14) & !is.na(dv01) & is.na(dv04)) {
    df$DataValue[df$AgeLabel == "0-4"] <- dv01 + dv14
  }

  # if 1-4 is missing and 0-1 and 0-4 are present, then compute 1-4
  if(is.na(dv14) & !is.na(dv01) & !is.na(dv04)) {
    df$DataValue[df$AgeLabel == "1-4"] <- dv04 - dv01
  }

  # if 0-1 is missing and 0-4 and 1-4 are present, then compute 0-1
  if(is.na(dv01) & !is.na(dv04) & !is.na(dv14)) {
    df$DataValue[df$AgeLabel == "0"] <- dv04 - dv14
  }

  data.out <- df %>%
    dplyr::filter(!is.na(DataValue) & !is.na(AgeSort))

  return(data.out)
}


