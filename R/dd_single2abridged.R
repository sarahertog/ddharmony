#' @title
#' dd_single2abridged
#'
#' @description
#' Creates 5-year age groups based on single years of age data.
#'
#' @param data A dataset with age series by single years of age
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset containing 5-year age groups generated from single years of age.

#' @export
#'
#' @examples
#' \dontrun{
#' df <- dd_single2abridged(vitals_abr_cpl2)
#' }

dd_single2abridged  <- function(data){ #input should be a dataset with single year age labels

  # define standard abridged age groups
  std_ages <- std_age_function()

  age_std <- std_ages %>%
    dplyr::filter(abridged == TRUE) %>%
    select(-abridged, -complete)

  # a) Closed age groups
  age_std_closed <- age_std %>%
    dplyr::filter(AgeSpan >0)

  data.out1 <- NULL
  for (i in 1:nrow(age_std_closed)) {

    df <- data %>%
      dplyr::filter(AgeSpan==1 &
                      AgeStart>= age_std_closed$AgeStart[i] &
                      AgeStart < age_std_closed$AgeEnd[i] &
                      !is.na(DataValue))

    if (nrow(df) == age_std_closed$AgeSpan[i]) {

      df <- df %>%
        summarise(DataValue = sum(DataValue)) %>%
        mutate(AgeStart = age_std_closed$AgeStart[i],
               AgeEnd   = age_std_closed$AgeEnd[i],
               AgeSpan  = age_std_closed$AgeSpan[i],
               AgeLabel = age_std_closed$AgeLabel[i],
               AgeSort  = age_std_closed$AgeSort[i])
    } else {
      df <- data.frame(AgeStart = age_std_closed$AgeStart[i],
                       AgeEnd = age_std_closed$AgeEnd[i],
                       AgeSpan = age_std_closed$AgeSpan[i],
                       AgeLabel = age_std_closed$AgeLabel[i],
                       AgeSort = age_std_closed$AgeSort[i],
                       DataValue = NA)
    }
    data.out1 <- rbind(data.out1,df)
  }

  # append open age group, unknown an total

  data.out <- data %>%
    bind_rows(data.out1 %>%
                dplyr::filter(AgeLabel !="0")) %>% # remove first age group to avoid dups
    dplyr::filter(AgeSort %in% age_std$AgeSort & !is.na(DataValue)) %>%
    arrange(AgeSort)

  return(data.out)
}
