#' @title
#' DDharmonize_Vitals1
#'
#' @description
#' This wrapper takes vital counts from census and registers from DemoData and Standardizes/harmonizes them.
#'
#' @details
#'
#' See the \href{https://shelmith-kariuki.github.io/rddharmony/articles/DDharmonize_1YearCounts.html}{"Harmonizing one-year counts" vignette} for more details about this function.
#'
#' @param indata The data to be harmonised
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A data frame that contains harmonized single year of age counts and closed by an open age group.
#'
#' @export
#'
DDharmonize_Vitals1 <- function (indata) {

  # Initialize sex specific outputs
  cpl_sex <- NULL
  abr_from_cpl_sex <-NULL

  sexes <- unique(indata$SexID)

  for (sex in sexes) { # loop through sex ids, 1=males, 2=females, 3= both

    ##0. Print the SexID whose data we are dealing with
    # print(paste("SexID = ", sex))

    ##1. Filter the data to only be left with data for this specific SexID
    df <- indata %>%
      dplyr::filter(SexID == sex & !is.na(DataValue)) %>%
      mutate(AgeLabel = as.character(AgeLabel)) %>%
      distinct()

    if (nrow(df) > 0) {

      ##2. if "Final" data status is available, keep only the final series

      if ("Final" %in% unique(df$DataStatusName)) {
        df <- df %>%
          dplyr::filter(DataStatusName == "Final")
      }

      ##3. check for multiple series ids

      ids_series <- unique(df$SeriesID)
      n_series <- length(ids_series)

      # for each unique series,
      df_out <- NULL
      for (i in 1:n_series) {

        ##4. Filter the data for a specific SeriesID

        df_one <- df %>% dplyr::filter(SeriesID == ids_series[i])

        ##5. check whether it is a full series with all age groups represented
        df_one_std    <- df_one[df_one$AgeSpan %in% c(-1, -2, 1),]
        if (nrow(df_one_std) > 60) {
          df_one$check_full <- dd_series_isfull(df_one_std, abridged = FALSE)
        } else {
          df_one$check_full <- FALSE
          }
        df_out <- rbind(df_out, df_one)
      }
      df <- df_out
      rm(df_out)

      ## Added by Shel (26th October at 14:26)
      ## Case study (births): id == "578 - Norway - VR - Births - 2002 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair"
      ## If there exists more than one total and there is a one that is equal to the computed total,
      ## drop the others to be left with this one.
      df <- df %>% dd_multiple_totals

      ##6.  Keep the latest datasource year

      # if there is more than one series ...
      if (n_series > 1) {
        latest_source_year <- max(df$DataSourceYear)
        check_latest_full  <- unique(df$check_full[df$DataSourceYear == latest_source_year])
        # ... and latest series is full then keep only that one
        if (any(check_latest_full)) {# Edited this. Case Study: 104 - Myanmar - VR - Deaths - 2012 - Register - Demographic Yearbook - Year of registration - Direct - Low
          df <- df[df$DataSourceYear == latest_source_year,]
        } else {
          # ... and latest series is not full, then keep the latest data source record for each age
          df <- df  %>%  dd_latest_source_year
        }
      }

      ##7. Tidy up the data frame
      df <- df %>%
        select(DataSourceYear, AgeStart, AgeEnd, AgeLabel, AgeSpan, DataValue) %>%
        distinct()

      ##8. if no record for unknown age, set data value to zero
      if (!("Unknown" %in% df$AgeLabel)) {
        df <- df %>%
          bind_rows(data.frame(AgeStart = -2,
                               AgeEnd = -2,
                               AgeSpan = -2,
                               AgeLabel = "Unknown",
                               DataSourceYear = NA,
                               DataValue = 0))
      }

      ## Added by Shel (26th Oct 15:16)
      ## Case (births): "52 - Barbados - VR - Births - 2006 - Register - Demographic Yearbook - Year of registration - Direct - Fair"
      ## if reported total - calculated total == "unknown" , then unknown == 0
      df <- df %>% dd_drop_unknowns()

      ##9. if the "Total" value is less than the sum over age, discard it
      total_reported <- df$DataValue[df$AgeLabel == "Total"]
      total_computed <- sum(df$DataValue[df$AgeLabel != "Total"])

      if (!is_empty(total_reported) & !is_empty(total_computed)) {

        if(total_reported < total_computed) {

          df <- df %>%
            dplyr::filter(AgeLabel != "Total")

        }
      }

    }

    if (nrow(df[df$AgeSpan == 1,]) > 0) {

      ##10. identify the start age of the open age group needed to close the series
      oag_start <- dd_oag_agestart(df, multiple5 = FALSE)

      ##11. flag whether this open age group exists in the series
      oag_check <- paste0(oag_start,"+") %in% df$AgeLabel

      ##12. drop records for open age groups that do not close the series
      if (!is_empty(oag_start)) {
        df <- df %>%
          dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start)) %>%
          arrange(AgeStart, AgeSpan)
      }

      ##13. add AgeSort field that identify standard age groups
      df <- dd_age_standard(df, abridged = FALSE) %>%
        dplyr::filter(!is.na(DataValue))

      ##14. check that df is in fact a complete series starting at age zero and without gaps
      check_cpl <- df %>%
        dplyr::filter(AgeSpan ==1) %>%
        summarise(minAge = min(AgeStart),
                  maxAge = max(AgeStart),
                  nAge   = length(unique(AgeStart)))
      check_cpl <- check_cpl$minAge == 0 & check_cpl$nAge == check_cpl$maxAge+1

      if (check_cpl==TRUE) {

        ##15. compute all possible open age groups given available input
        df_oag <- dd_oag_compute(df, age_span = 1)

        df <- df %>%
          bind_rows(df_oag[!(df_oag$AgeLabel %in% df$AgeLabel) &
                             df_oag$AgeStart == oag_start,]) %>%
          arrange(AgeSort)
      }

    }

    ## 16. Check that there are no missing age groups on the complete series
    if (!("AgeSort" %in% names(df))) {
      df <- dd_age_standard(df, abridged = FALSE) %>%
        dplyr::filter(!is.na(DataValue))
      check_cpl <- FALSE
    }

    ##17. check again whether any open age group exists
    oag_start <- df %>% dd_oag_agestart
    oag_check <- paste0(oag_start,"+") %in% df$AgeLabel


    ##18. if total is missing and series is otherwise complete, compute total
    if (!("Total" %in% df$AgeLabel) & oag_check == TRUE) {
      df <- df %>%
        bind_rows(data.frame(AgeStart = 0,
                             AgeEnd = -1,
                             AgeLabel = "Total",
                             AgeSpan = -1,
                             AgeSort = 184,
                             DataSourceYear = NA,
                             DataValue = sum(df$DataValue[df$AgeSpan == 1]) +
                               df$DataValue[df$AgeSpan == -1 & df$AgeStart == oag_start] +
                               df$DataValue[df$AgeLabel == "Unknown"]))
    }

    ##19. write a note to alert about missing data
    df$note <- NA
    if (check_cpl == FALSE | oag_check == FALSE) {
      df$note <- "The complete series is missing data for one or more age groups."
    }

    df$SexID <- sex

    ##20. now compile these for each sex
    cpl_sex <- rbind(cpl_sex, df)

    # clean up the environment before beginning next loop
    rm(df, check_cpl)

  } # close loop for sex

  ##20. add series field to data
  if (!is.null(cpl_sex)) {
    cpl_sex <- cpl_sex %>%
      mutate(abridged = FALSE,
             complete = TRUE,
             series = "complete")
  }

  outdata <- cpl_sex

  return(outdata)

}
