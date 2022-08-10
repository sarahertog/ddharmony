#' @title
#' dd_validate_totals_over_age
#'
#' @description
#' Reconcile `Totals`, in cases where reported values are different from calculated values, after having standardized age series.
#' If the computed total is greater than reported total, then replace reported with computed. If computed is less than reported, then add difference to "Unknown" age
#'
#' @details
#'See the \href{https://shelmith-kariuki.github.io/rddharmony/articles/dd_validate_totals_over_age.html}{"Validating totals over age" vignette} for more details about this function.
#'
#' @param data The data to be harmonized
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset with reconciled `Totals` i.e. reported totals == computed totals
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- vitals_std_full_sample
#' df <- dd_validate_totals_over_age(df)
#' }

dd_validate_totals_over_age <- function(data){

  options(scipen = 999)

  out.data <- NULL

  for (sex in unique(data$SexID)) {

    ## subset the data to have abridged records for each particular sex id
    df_abr <- data %>%
      dplyr::filter(SexID == sex & abridged == TRUE)

    ## extract the reported totals and the actual totals
    total_reported_abr <- df_abr$DataValue[df_abr$AgeLabel == "Total"]
    total_computed_abr <- sum(df_abr$DataValue[df_abr$AgeSpan %in% c(-1,-2, 5) & df_abr$AgeLabel != "Total"])


    ## if the reported and actual totals exist...
    if (!is_empty(total_reported_abr) & !is_empty(total_computed_abr)) {

      # if computed is greater than reported, then replace reported with computed
      diff <- total_computed_abr - total_reported_abr
      if (diff > 0 ) {
        df_abr$DataValue[df_abr$AgeLabel == "Total"] <- total_computed_abr
      }
      # if computed is less than reported, then add difference to "Unknown" age
      if (diff < 0 ) {
        df_abr$DataValue[df_abr$AgeLabel == "Unknown"] <- df_abr$DataValue[df_abr$AgeLabel == "Unknown"] +
          (total_reported_abr-total_computed_abr)
      }

    }

    ## subset the data to have complete records for each particular sex id

    df_cpl <- data %>%
      dplyr::filter(SexID == sex & complete == TRUE)

    total_reported_cpl <- df_cpl$DataValue[df_cpl$AgeLabel == "Total"]
    total_computed_cpl <- sum(df_cpl$DataValue[df_cpl$AgeLabel != "Total"])

    if (!is_empty(total_reported_cpl) & !is_empty(total_computed_cpl)) {

      # if computed is greater than reported, then replace reported with computed
      diff <- total_computed_cpl - total_reported_cpl
      if (diff > 0 ) {
        df_cpl$DataValue[df_cpl$AgeLabel == "Total"] <- total_computed_cpl
      }
      # if computed is less than reported, then add difference to "Unknown" age
      if (diff < 0 ) {
        df_cpl$DataValue[df_cpl$AgeLabel == "Unknown"] <- df_cpl$DataValue[df_cpl$AgeLabel == "Unknown"] +
          (total_reported_cpl-total_computed_cpl)
      }

    }

    out.data <- rbind(out.data, df_abr, df_cpl)

  }

  return(out.data)
}


