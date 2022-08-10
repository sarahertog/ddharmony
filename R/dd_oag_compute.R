#' @title
#' dd_oag_compute
#'
#' @description
#' Compute data values for the open age group needed to close the series
#'
#' @details
#' You can read more about this function on \href{https://shelmith-kariuki.github.io/rddharmony/articles/dd_oag_compute.html}{this vignette}.
#'
#' @param data The data to be harmonized
#' @param age_span The range of the age label e.g the age span of the age label 10-14 is 5
#'
#' @import dplyr
#' @import purrr
#' @importFrom magrittr %>%
#'
#' @return A dataset with plausible closing age groups and their data values
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- oag_compute_df
#' df <- dd_oag_compute(df)
#' }

dd_oag_compute  <- function(data, age_span = c(1, 5)){

  # define standard abridged age groups
  std_ages <- std_age_function()

  # subset the standard age groups data to only have abridged age groups (abridged == TRUE)
  age_std <- std_ages %>%
    dplyr::filter(abridged == TRUE) %>%
    select(-abridged, -complete)

  # subset the data to remove any missing data values
  df <- data %>%
    dplyr::filter(!is.na(DataValue) & AgeSpan %in% c(age_span, -1, -2))

  # identify the start age of the open age group needed to close the series
  oag_start <- data %>% dd_oag_agestart

  # list standard open age groups that can be computed from the complete series
  if (!is_empty(oag_start)) {
    age_std_open <- age_std %>%
      dplyr::filter(AgeSpan < 0 & AgeStart <= oag_start & !(AgeStart %in% c(-2,0)))
  } else {
    age_std_open <- NULL
  }

  nrows <- ifelse(is.null(age_std_open), 0, nrow(age_std_open))

  if (nrows > 0) {

    # look for an open age group record on input file that closes out the series
    oag_indata <- df %>%
      dplyr::filter(AgeLabel == paste0(oag_start,"+")) %>%
      select(AgeStart, AgeEnd, AgeSpan, AgeLabel, DataValue)

    # if nrow(oag_indata) >0, it means the open age group exists
    oag_check <- nrow(oag_indata) > 0

    # check if there is a "Total" age label in the data
    total_check <- "Total" %in% data$AgeLabel

    # if the total age label exists, check if it is valid (is it an actual total or is it different from the summation of the
    # rest of the data?)
    if (total_check) {
      total_value <- data$DataValue[data$AgeLabel=="Total"]
      total_value_valid <- total_value >= suppressWarnings(sum(df$DataValue[df$AgeSpan == age_span]))
    } else { total_value_valid <- FALSE }

    # check if there is an unknown that exists in the data, if it exists, extract its value, if it does not, the value is 0
    unknown_check <- "Unknown" %in% data$AgeLabel
    unknown_value <- ifelse(unknown_check, data$DataValue[data$AgeLabel=="Unknown"], 0)

    # if there is an open age group record on complete, then use this to compute other open age groups
    if (oag_check) {

      data.out <- NULL
      for (i in 1:nrow(age_std_open)) {
        df.out <- df %>%
          dplyr::filter((AgeStart >= age_std_open$AgeStart[i] & AgeSpan > 0) | AgeLabel==paste0(oag_start,"+")) %>%
          select(AgeStart,AgeEnd,AgeSpan,AgeLabel,DataValue) %>%
          summarise(DataValue = sum(DataValue)) %>%
          mutate(AgeStart = age_std_open$AgeStart[i],
                 AgeEnd   = age_std_open$AgeEnd[i],
                 AgeSpan  = age_std_open$AgeSpan[i],
                 AgeLabel = age_std_open$AgeLabel[i],
                 AgeSort  = age_std_open$AgeSort[i])
        data.out <- rbind(data.out,df.out)
      }

    }

    # if the needed open age group is not on the series, but there is a total, then use this to compute oag values
    if (!(oag_check) & total_check & total_value_valid) {

      data.out <- NULL
      for (i in 1:nrow(age_std_open)) {
        df.out <- df %>%
          dplyr::filter(AgeStart < age_std_open$AgeStart[i] & AgeSpan > 0) %>%
          select(AgeStart,AgeEnd,AgeSpan,AgeLabel,DataValue) %>%
          summarise(DataValue = total_value - unknown_value - sum(DataValue)) %>%
          mutate(AgeStart = age_std_open$AgeStart[i],
                 AgeEnd   = age_std_open$AgeEnd[i],
                 AgeSpan  = age_std_open$AgeSpan[i],
                 AgeLabel = age_std_open$AgeLabel[i],
                 AgeSort  = age_std_open$AgeSort[i])
        data.out <- rbind(data.out,df.out)
      }
    }

    # if there is neither an open age group nor a Total on abridged, then we don't
    # compute open age group values
    if ((!(oag_check)) & (!(total_check) | !(total_value_valid))) {
      data.out <- NULL
    }

  } else { data.out <- NULL}

  return(data.out)
}
