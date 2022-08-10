#' @title
#' dd_drop_unknowns
#'
#' @description
#' If reported total - calculated total == "Unknown" , then set Unknown to 0.
#'
#' @param data The data to be harmonised
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return  A data frame without `Unknowns` in cases where the value of `Unknown` is equal to `reported total - calculated total`.
#' @export
dd_drop_unknowns <- function(data){

  ## Added by Shel (26th Oct 15:16)
  ## Case (births): "52 - Barbados - VR - Births - 2006 - Register - Demographic Yearbook - Year of registration - Direct - Fair"
  ## if reported total - calculated total == "unknown" drop unknown

  data <- data %>%
    mutate(calc_total = sum(round(DataValue[AgeLabel != "Total"]), na.rm = TRUE),
           reported_total = ifelse(any(AgeLabel == "Total"),DataValue[AgeLabel == "Total"], 0 )) %>%
    mutate(DataValue = ifelse(any(AgeLabel == "Unknown") &
                                     abs(calc_total - reported_total) == DataValue[AgeLabel == "Unknown"] &
                                     AgeLabel == "Unknown", 0, DataValue)) %>%
    select(-calc_total, -reported_total)
}
