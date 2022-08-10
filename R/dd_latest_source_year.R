#' @title
#' dd_latest_source_year
#'
#' @description
#'For some periods of reference, there are multiple values in the **DataSourceYear** variable. For these cases, the criteria is to choose the latest **DataSourceYear**.
#'
#' @details
#' You can run `getAnywhere(dd_latest_source_year)` to view the definition of this function.
#'
#' @param data The data to be harmonized
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset with a unique **DataSourceYear** per id and age label.
#'
#' @export
#' @examples
#' \dontrun{
#' df <- latest_source_year_df
#' df <- dd_latest_source_year(df)
#' }

dd_latest_source_year <- function(data){
  data.out <- data %>%
    group_by(AgeLabel) %>%
    slice(which.max(DataSourceYear)) %>%
    ungroup()
  return(data.out)
}
