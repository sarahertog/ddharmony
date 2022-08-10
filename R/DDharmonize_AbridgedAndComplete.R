#' @title
#' DDharmonize_AbridgedAndComplete
#'
#' @description
#' This wrapper takes the output of harmonization and reconciles abridged and complete series.
#'
#' @details
#' See the \href{https://shelmith-kariuki.github.io/rddharmony/articles/DDharmonize_AbridgedAndComplete.html}{Reconciling abridged and complete series vignette} for more details about this function.
#'
#' @param data_abr data containing abridged series age labels
#' @param data_cpl_from_abr data containing complete series derived from abridged series
#' @param data_cpl data containing complete series age labels
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset with reconciled abridged and complete series i.e. abridged reconciled with complete and complete reconciled with abridged.
#'
#' @export
#' @examples
#' \dontrun{
#' df <- DDharmonize_AbridgedAndComplete(data_abr,
#'                                      data_cpl_from_abr = NULL,
#'                                      data_cpl)
#' }

DDharmonize_AbridgedAndComplete <- function (data_abr,data_cpl_from_abr, data_cpl) {

  abr_sex <- NULL
  cpl_sex <- NULL

  for (sex in unique(c(data_abr$SexID, data_cpl$SexID))) {

    # create some flags for data availability
    has_abr <- nrow(data_abr[!is.na(data_abr$DataValue) & data_abr$SexID == sex,]) >0 # checks whether abridged series exists
    has_abr <- ifelse(is_empty(has_abr), FALSE, has_abr)
    has_cpl_from_abr <- nrow(data_cpl_from_abr[!is.na(data_cpl_from_abr$DataValue) & data_cpl_from_abr$SexID == sex,]) >0 #?
    has_cpl_from_abr <- ifelse(is_empty(has_cpl_from_abr), FALSE, has_cpl_from_abr)
    has_cpl <- nrow(data_cpl[!is.na(data_cpl$DataValue) & data_cpl$SexID == sex,]) >0 # checks whether complete series exists
    has_cpl <- ifelse(is_empty(has_cpl), FALSE, has_cpl)


  ## if the data has abridged series, subset the data to only have records for the specific sex being looped over and
  ##  extract the data value of AgeLabel == "Total"
    if (has_abr) {

      df_abr <- data_abr %>% dplyr::filter(SexID == sex) %>%
        select(-note, -SexID)
      total_abr <- df_abr$DataValue[df_abr$AgeLabel == "Total"]

    } else { df_abr <- NULL }

 ## ?
    if (has_cpl_from_abr) {
      df_cpl_from_abr <- data_cpl_from_abr %>% dplyr::filter(SexID == sex) %>%
        select(-note, -SexID)
    } else { df_cpl_from_abr <- NULL }

    ## if the data has complete series, subset the data to only have records for the specific sex being looped over and
    ##  extract the data value of AgeLabel == "Total"
    ## Also generate abridged records from complete series using the dd_single2abridged function
    if (has_cpl) {
      df_cpl <- data_cpl %>% dplyr::filter(SexID == sex ) %>%
        select(-note, -SexID)
      total_cpl <- df_cpl$DataValue[df_cpl$AgeLabel == "Total"]
      df_abr_from_cpl <- df_cpl %>% dd_single2abridged()

    } else { # if no data_cpl, df_cpl will be equal to df_cpl_from_abr (complete records obtained from abridged)
      if (has_cpl_from_abr) {
        # use data_cpl_from_abr
        if (all(data_cpl_from_abr$AgeSpan < 0)) { data_cpl_from_abr <- NULL }
        df_cpl <- df_cpl_from_abr
        if (!is.null(df_cpl)) {
          total_cpl <- df_cpl$DataValue[df_cpl$AgeLabel == "Total"]
          df_abr_from_cpl <- df_cpl %>% dd_single2abridged
          has_cpl <- TRUE
        }

      } else {
        df_cpl <- NULL
        df_abr_from_cpl <- NULL
      }
    }

## if both abridged and complete series exist and if they are not empty, check if the totals match
    if (has_abr & has_cpl) {

      total_diff <- total_abr - total_cpl
      # total_match <- total_diff == 0
      total_match <- total_diff <= 0.5
      if (is_empty(total_match)) { total_match = TRUE }

    } else { total_match <- FALSE }

    if (exists('total_abr') & exists('total_cpl')) {
      total_match <- ifelse(is_empty(total_abr) & is_empty(total_cpl), FALSE, total_match)
    }

## if the totals agree or if total for one series is missing then fill it in
    if (total_match) {

      ######################################################
      # reconcile abridged series with records from complete
      ######################################################

      # append the abridged series with the abridged records derived from complete series, but only for the
      # age labels that do not exist in the former, to avoid duplication
      df_abr <- df_abr %>%
        bind_rows(df_abr_from_cpl %>% dplyr::filter(!(AgeLabel %in% df_abr$AgeLabel)))

      # drop records for open age groups that do not close the series
      oag_start_abr <- dd_oag_agestart(df_abr, multiple5 = TRUE)

      if (!is_empty(oag_start_abr)) {
        df_abr <- df_abr %>%
          dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start_abr))
      }

      # compute all possible open age groups
      oag_abr <- dd_oag_compute(df_abr, age_span = 5)
      if(!is.null(oag_abr)) {
        df_abr <- df_abr %>%
          bind_rows(oag_abr %>% dplyr::filter(!(AgeLabel %in% df_abr$AgeLabel)))
      }

      # drop records for open age groups that do not close the series
      oag_start_abr <- dd_oag_agestart(df_abr, multiple5 = TRUE)
      if (!is_empty(oag_start_abr)) {
        df_abr <- df_abr %>%
          dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start_abr)) %>%
          mutate(series = "abridged reconciled with complete") %>%
          arrange(AgeSort)
      }

      # check to see whether the series is full
      isfull_abr <- dd_series_isfull(df_abr, abridged = TRUE)

      df_abr$note <- ifelse(isfull_abr, NA, "The abridged series is missing data for one or more age groups.")
      df_abr$SexID <- sex

      ######################################################
      # reconcile complete series with records from abridged
      ######################################################

      # append the complete series with the complete records derived from abridged series, but only for the
      # age labels that do not exist in the former, to avoid duplication

      if (!is.null(df_cpl_from_abr)) {
        df_cpl <- df_cpl %>%
          bind_rows(df_cpl_from_abr %>% dplyr::filter(!(AgeLabel %in% df_cpl$AgeLabel)))
      }

      # only process if there are multiple closed age groups in the series
      if (nrow(df_cpl[df_cpl$AgeSpan == 1,]) > 1) {

        # drop records for open age groups that do not close the series
        oag_start_cpl <- dd_oag_agestart(df_cpl, multiple5 = FALSE)
        df_cpl <- df_cpl %>%
          dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start_cpl))

        # compute all possible open age groups
        oag_cpl <- dd_oag_compute(df_cpl, age_span = 1)
        if (!is.null(oag_cpl)) {
          df_cpl <- df_cpl %>%
            bind_rows(oag_cpl %>% dplyr::filter(!(AgeLabel %in% df_cpl$AgeLabel)))
        }

        # identify the open age group that is a multiple of five
        oag_start_cpl <- dd_oag_agestart(df_cpl, multiple5 = TRUE)

        df_cpl <- df_cpl %>%
          dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start_cpl)) %>%
          dplyr::filter(!(AgeSpan ==1 & AgeStart >= oag_start_cpl)) %>%
          mutate(series = "complete reconciled with abridged") %>%
          arrange(AgeSort)

        isfull_cpl <- dd_series_isfull(df_cpl, abridged = FALSE)

        df_cpl$note <- ifelse(isfull_cpl, NA, "The complete series is missing data for one or more age groups.")
        df_cpl$SexID = sex
      } else { df_cpl <- NULL }

      # if the only remaining record on complete is "Unknown" or "Total" then discard the whole series

      if (all(unique(df_cpl$AgeLabel) %in% c("Unknown","Total"))) {
        df_cpl <- NULL
      }

    } else {

  ## if totals don't match
      if (!is.null(df_abr)) {
        df_abr$note <- "Different totals on abridged and complete preclude reconciliation"
        df_abr$SexID <- sex
      }
      if (!is.null(df_cpl)) {
        df_cpl$note <- "Different totals on abridged and complete preclude reconciliation"
        df_cpl$SexID <- sex

      }

    }

    ## update abr_sex and cpl_sex with the new data after running the loop for each SexID
    abr_sex <- rbind(abr_sex, df_abr)
    cpl_sex <- rbind(cpl_sex, df_cpl)

    ## generate abridged and complete variables which are TRUE/FALSE depending on the series
    if (!is.null(abr_sex)) {
      abr_sex <- abr_sex %>%
        mutate(abridged = TRUE,
               complete = FALSE)
    }
    if (!is.null(cpl_sex)) {
      cpl_sex <- cpl_sex %>%
        mutate(abridged = FALSE,
               complete = TRUE)
    }
  } # close loop for sex

  outdata <- rbind(abr_sex ,
                   cpl_sex )

  return(outdata)

}
