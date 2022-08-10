#' @title
#' dd_distribute_unknowns
#'
#' @description
#' Distribute Unknowns over age
#'
#' @details
#' You can run `getAnywhere(dd_distribute_unknowns)` to see how the function is defined.
#'
#' @param data The data to be harmonised
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset without unknowns
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- vitals_std_valid_sample
#' df <- dd_distribute_unknowns(df)
#'}
dd_distribute_unknowns <- function(data){

  out.data <- NULL

  for (sex in unique(data$SexID)) {

    ## abridged
    df_abr <- data %>%
      dplyr::filter(SexID == sex & abridged == TRUE)

    total_value   <- df_abr$DataValue[df_abr$AgeLabel == "Total"]
    unknown_value <- df_abr$DataValue[df_abr$AgeLabel == "Unknown"]
    total_less_unknown <- total_value - unknown_value

    if (!is_empty(total_value)) {
      df_abr <- df_abr %>%
        mutate(pct_dist = DataValue/total_less_unknown,
               add_unk  = unknown_value * pct_dist,
               add_unk  = ifelse(AgeLabel == "Total", 0, add_unk),
               DataValue = DataValue + add_unk) %>%
        dplyr::filter(AgeLabel != "Unknown") %>%
        select(-pct_dist, -add_unk)
    }
    rm(total_value, unknown_value)

    ## complete
    df_cpl <- data %>%
      dplyr::filter(SexID == sex & complete == TRUE)

    total_value   <- df_cpl$DataValue[df_cpl$AgeLabel == "Total"]
    unknown_value <- df_cpl$DataValue[df_cpl$AgeLabel == "Unknown"]
    total_less_unknown <- total_value - unknown_value

    if (!is_empty(total_value)) {
      df_cpl <- df_cpl %>%
        mutate(pct_dist = DataValue/total_less_unknown,
               add_unk  = unknown_value * pct_dist,
               add_unk  = ifelse(AgeLabel == "Total", 0, add_unk),
               DataValue = DataValue + add_unk) %>%
        dplyr::filter(AgeLabel != "Unknown") %>%
        select(-pct_dist, -add_unk)
    }

    out.data <- rbind(out.data, df_abr, df_cpl)

    ## Added by Shel. Drop Unknowns that are equal to 0, in cases where Totals are not reported
    ## "60 - Bermuda - VR - Births - 2006 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair"
    out.data <- out.data %>%
                 mutate(todrop = ifelse(AgeLabel == "Unknown" & DataValue == 0, "drop", "")) %>%
                  filter(todrop != "drop") %>%
                  select(-todrop)
  }

  return(out.data)

}

