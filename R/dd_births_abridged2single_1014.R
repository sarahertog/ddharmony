#' @title
#' dd_births_abridged2single_1014
#'
#' @description
#' Distribute births to ages 10-14 across single ages. Arbitrary distribution of births across ages 10-14 for now. Need to update with Kirill's regression
#'
#' @param indata The data to be harmonized
#' @param births10_14 The data value of age label "10-14"
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset with ages 10-14 distributed across single ages.
#'
#' @export
#' @examples
#' \dontrun{
#' df <- abridged2single_1014_df
#' df <- dd_births_abridged2single_1014(df, 4)
#' }
dd_births_abridged2single_1014 <- function(indata, births10_14) {

  df <- indata

  births_single_1014 <- births10_14 * c(0,0,0.05,0.25,0.70)
  for (j in 5:1) {
    df_add <- df[df$AgeLabel == 15,] %>%
      mutate(AgeStart = 9+j,
             AgeEnd = 10+j,
             AgeLabel = AgeStart,
             DataSourceYear = NA,
             DataValue = births_single_1014[j])

    df <- rbind(df_add, df)

  }

  return(df)

}
