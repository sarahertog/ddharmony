#' @title
#' dd_oag2closed
#'
#' @description
#' Compute associated closed age group values from multiple open-age groups
#'
#' @details
#' You can run `getAnywhere(dd_oag2closed)` to view the definition of this function.
#'
#' @param data The data to be harmonized
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset with closed age groups derived from multiple open age groups.
#'
#' @export
#'
dd_oag2closed <- function(data){
  df_oag <- data %>%
    dplyr::filter(AgeSpan == -1 & AgeStart > 0) %>%
    arrange(AgeStart)

  if (nrow(df_oag) > 1) {
    df_oag_add <- df_oag %>%
      mutate(AgeEnd = c(AgeStart[2:nrow(df_oag)],NA),
             AgeSpan = AgeEnd - AgeStart,
             AgeLabel = paste(AgeStart, AgeEnd-1, sep="-" ),
             DataSourceYear = NA,
             DataValue = DataValue - c(DataValue[2:nrow(df_oag)],NA)) %>%
      dplyr::filter(AgeSpan %in% c(1,5) )
  } else { df_oag_add <- NULL }

  return(df_oag_add)
}


