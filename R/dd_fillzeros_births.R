#' @title
#' dd_fillzeros_births
#'
#' @description
#' Fill in zeros for mother ages younger than 15 if there is no data reported.
#'
#' @details
#' You can run `getAnywhere(dd_fillzeros_births)` to see the function definition.
#'
#' @param data The data to be harmonized
#' @param abridged `TRUE/FALSE`
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset with zeros filled in for ages younger than 15 if there is no data reported.

#' @export
#'
#' @examples
#' \dontrun{
#' df_abr <- dd_fillzeros_births(data = vitals_abr_cpl1_f0, abridged = TRUE)
#' df_cpl <- dd_fillzeros_births(data = vitals_abr_cpl2_f0, abridged = FALSE)
#' }

dd_fillzeros_births <- function(data, abridged = TRUE){

  sexes <- unique(data$SexID)

  df_sex <- NULL
  for (sex in sexes) {

    df <- dd_age_standard(data %>% dplyr::filter(SexID == sex), abridged = abridged) %>%
      mutate(DataValue = replace(DataValue, is.na(DataValue & AgeStart < 15 & AgeSpan > 0), 0),
             SexID = sex) %>%
      dplyr::filter(!is.na(DataValue))

    df_sex <- rbind(df_sex, df)
  }

  return(df_sex)

}


