#' @title
#' dd_age_standard
#'
#' @description
#' Adds standard age groups (the result of `std_age_function()`) in the dataset
#'
#' @details
#' Performs a `full_join()` operation between the standard age groups dataset (the result of `std_age_function()`) and the data to be harmonized,
#' using `AgeStart`,`AgeEnd`,`AgeLabel`,`AgeSpan` as keys. You can run `getAnywhere(dd_age_standard)` to view the definition of this function.
#'
#' @param data The data to be harmonized
#' @param abridged `TRUE/FALSE`
#'
#' @return The DemoData dataset with standard age groups appended to it.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- firstages_compute_births_df
#' df <- dd_age_standard(df)
#' }

dd_age_standard<- function(data, abridged = TRUE){

  # get stanadard age groups
  std_ages <- std_age_function()

  if (abridged) {
    std_ages <- std_ages %>%
      dplyr::filter(abridged == TRUE)
  } else { std_ages <- std_ages %>%
    dplyr::filter(complete==TRUE)
  }
  std_ages <- std_ages %>%
    select(-abridged, -complete)


  # join standard groups with the data from DemoData
  data.out <- data %>%
    full_join(x=std_ages,
              by=c("AgeStart","AgeEnd","AgeLabel","AgeSpan")) %>%
    arrange(AgeSort)

  return(data.out)
}
