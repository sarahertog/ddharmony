#' @title
#' dd_firstages_compute_births
#'
#' @description
#' Reconcile early age groups for births.
#'
#' @details
#' In cases where for example 10_14 is missing and 0_14 is present, convert the latter to the former.
#' Another case is if 0-4 is missing and 0-1 and 1-4 are present, then sum both to get 0-4.
#' You can run `getAnywhere(dd_firstages_compute_births)` to view the definition of this function.
#'
#' @param data The data to be harmonized
#'
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom purrr is_empty
#'
#' @return A dataset with early age groups reconciled e.g 0-14 becomes 10-14
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- firstages_compute_births_df
#' df <- dd_firstages_compute_births(df)
#' }

dd_firstages_compute_births <- function(data){

df <- dd_age_standard(data, abridged = TRUE)

  # if 0-4 is missing and 0-1 and 1-4 are present, then sum to 0-4
  dv01 <- df$DataValue[df$AgeLabel == "0"]
  dv14 <- df$DataValue[df$AgeLabel == "1-4"]
  dv04 <- df$DataValue[df$AgeLabel == "0-4"]
  dv10_14 <- df$DataValue[df$AgeLabel == "10-14"]
  dv15_19 <- df$DataValue[df$AgeLabel == "15-19"]

  dv0_14 <- data$DataValue[data$AgeLabel == "0-14"]
  dv0_19 <- data$DataValue[data$AgeLabel == "0-19"]
  dv10_19 <- data$DataValue[data$AgeLabel == "10-19"]


  if(!is.na(dv14) & !is.na(dv01) & is.na(dv04)) {
    df$DataValue[df$AgeLabel == "0-4"] <- dv01 + dv14
  }

  if(is.na(dv14) & !is.na(dv01) & !is.na(dv04)) {
    df$DataValue[df$AgeLabel == "1-4"] <- dv04 - dv01
  }

  if (!is_empty(dv0_14)) {
    if(is.na(dv10_14) & !is.na(dv0_14)) {
      df$DataValue[df$AgeLabel == "10-14"] <- dv0_14
    }
  }
  if (!is_empty(dv0_14) & !is_empty(dv0_19)) {
    if(is.na(dv15_19) & !is.na(dv0_14) & !is.na(dv0_19)) {
      df$DataValue[df$AgeLabel == "15-19"] <- dv0_19 - dv0_14
    }
  }
  if (!is_empty(dv0_19)) {
    if(is.na(dv10_14) & !is.na(dv15_19) & !is.na(dv0_19)) {
      df$DataValue[df$AgeLabel == "10-14"] <- dv0_19 - dv15_19
    }
    if(is.na(dv10_14) & is.na(dv15_19) & !is.na(dv0_19)) {
      df$DataValue[df$AgeLabel == "10-14"] <- 0
      df$DataValue[df$AgeLabel == "15-19"] <- dv0_19
    }
  }

  data.out <- df %>%
    dplyr::filter(!is.na(DataValue) & !is.na(AgeSort))

  return(data.out)
}


