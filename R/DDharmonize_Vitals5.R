#' @title
#' DDharmonize_Vitals5
#'
#' @description
#' This wrapper takes five-year vital counts from census and registers from the UNDP database and standardizes/harmonizes them.
#'
#' @details
#' See the \href{https://shelmith-kariuki.github.io/rddharmony/articles/DDharmonize_5YearCounts.html}{"Harmonizing five-year counts" vignette} for more details about this function.
#'
#' @param indata The data to be harmonised
#' @param type births / deaths
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return  A data frame that contains harmonized counts for five-year age groups and closed by an open age group.

#' @export
#'
#' @examples
#' \dontrun{
#' df <- vitals5_df
#' df <- DDharmonize_Vitals5(df, type = "births")
#'}
DDharmonize_Vitals5 <- function (indata, type = c("births","deaths")) {

  # Initialize sex specific outputs
  abr_sex <- NULL

  sexes <- unique(indata$SexID)

  for (sex in sexes) { # loop through sex ids, 1=males, 2=females, 3= both

    ##0. Print the SexID whose data we are dealing with
    # print(paste("SexID = ", sex))

    ##1. Filter the data to only be left with data for this specific SexID
    abr <- indata %>%
      dplyr::filter(SexID == sex & !is.na(DataValue)) %>%
      select(-SexID) %>%
      distinct()

    if (nrow(abr[abr$AgeSpan == 5,]) > 0) { # only process those that have at least one abridged age group

      ##2. if "Final" data status is available, keep only the final series
      if ("Final" %in% unique(abr$DataStatusName)) {
        abr <- abr %>%
          dplyr::filter(DataStatusName == "Final")
      }

      ##3. check for multiple series ids
      ids_series <- unique(abr$SeriesID)
      n_series <- length(ids_series)

      # for each unique series,
      abr_out <- NULL

      for (i in 1:n_series) {

        ##4. Filter the data for a specific SeriesID
        df <- abr %>% dplyr::filter(SeriesID == ids_series[i])

        ##5. Check whether it is a full series with all age groups represented and an open age greater than 60
        ## AgeSpan == -1 (Total), AgeSpan == -2 (Unknown), AgeSpan == 5(five-year)

        df_abr_std    <- df[(df$AgeStart == 0 & df$AgeSpan == 1 ) |
                              df$AgeSpan %in% c(-1, -2, 5),]

        if (nrow(df_abr_std) > 13) {

          df$check_full <- dd_series_isfull(df_abr_std, abridged = TRUE)

        } else { df$check_full <- FALSE }
        abr_out <- rbind(abr_out, df)
      }
      abr <- abr_out
      rm(abr_out)

      ## Added by Shel (26th October at 14:26)
      ## Case study (births): id == "578 - Norway - VR - Births - 2002 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair"
      ## If there exists more than one total and there is a one that is equal to the computed total,
      ## drop the others to be left with this one.
      abr <- abr %>% dd_multiple_totals

      ##6. Keep the latest datasource year
      ## Should this be per series? because we are ending up dropping 159 at this point

      # if there is more than one series ...
      if (n_series > 1) {
        latest_source_year <- max(abr$DataSourceYear)
        check_latest_full  <- unique(abr$check_full[abr$DataSourceYear == latest_source_year])
        # ... and latest series is full then keep only that one
        if (any(check_latest_full)) {# Edited this. Case Study: 104 - Myanmar - VR - Deaths - 2012 - Register - Demographic Yearbook - Year of registration - Direct - Low
          abr <- abr[abr$DataSourceYear == latest_source_year,]
        } else {
          # ... and latest series is not full, then keep the latest data source record for each age
          abr <- abr  %>%  dd_latest_source_year
        }
      }

      ##7. Tidy up the data frame

      abr <- abr %>%
        select(DataSourceYear, AgeStart, AgeEnd, AgeLabel, AgeSpan, DataValue) %>%
        distinct()

      ##8. If there is no record for unknown age, set data value to zero
      if (!("Unknown" %in% abr$AgeLabel)) {
        abr <- abr %>%
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

      abr <- abr %>% dd_drop_unknowns()

      ##9. For births, reconcile wide early age groups to abridged (e.g., 0-19, 0-14, 10-19 etc)
      if (type == "births") {

        abr <- abr %>% dd_firstages_compute_births %>%
          select(-AgeSort)

      }
      # reconcile first age groups 0-1, 0-4, 1-4
      abr <- abr %>% dd_firstages_compute

      ##10.Check whether there are multiple open age groups (TRUE/FALSE Output)
      oag_multi <- abr %>% dd_oag_multiple

      ## 11. Compute closed age groups from multiple open age groups and add to data if missing
      if (oag_multi) {
        add <- abr %>%
          dd_oag2closed %>%
          dplyr::filter(!(AgeLabel %in% abr$AgeLabel[!is.na(abr$DataValue)]))

        if (nrow(add > 0)) {
          abr <- abr %>%
            bind_rows(add) %>%
            arrange(AgeStart)
        }
      }

      ## 12. Identify the start age of the open age group needed to close the series
      oag_start <- abr %>% dd_oag_agestart

      # flag whether this open age group exists in the series
      oag_check <- paste0(oag_start,"+") %in% abr$AgeLabel

      ## 13. Drop records for open age groups that do not close the series
      if (!is_empty(oag_start)){
        abr <- abr %>%
          dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start))
      }

      ## 14. Check that there are no missing age groups on the abridged series
      if (nrow(abr[abr$AgeStart >= 5,]) > 0) {
        check_abr <- DemoTools::is_abridged(abr$AgeStart[abr$AgeStart >=5])
      } else {
        check_abr <- FALSE
      }

      if (check_abr==TRUE) {

        ## 15. Compute all possible open age groups given available input
        abr_oag <- dd_oag_compute(abr, age_span = 5)

        ## 16. Append the oag that completes the abridged series
        abr <- abr %>%
          bind_rows(abr_oag[!(abr_oag$AgeLabel %in% abr$AgeLabel) &
                              abr_oag$AgeStart == oag_start,]) %>%
          arrange(AgeSort)

      }

      ## 17. Check again whether any open age group exists
      oag_check <- paste0(oag_start,"+") %in% abr$AgeLabel


      ## 18. If total is missing and series is otherwise complete, compute total
      if (!("Total" %in% abr$AgeLabel) & "0-4" %in% abr$AgeLabel & oag_check == TRUE) {
        abr <- abr %>%
          bind_rows(data.frame(AgeStart = 0,
                               AgeEnd = -1,
                               AgeLabel = "Total",
                               AgeSpan = -1,
                               AgeSort = 184,
                               DataSourceYear = NA,
                               DataValue = sum(abr$DataValue[abr$AgeSpan == 5]) +
                                 abr$DataValue[abr$AgeSpan == -1 & abr$AgeStart == oag_start] +
                                 abr$DataValue[abr$AgeLabel == "Unknown"]))
      }

      ## 19. Write a note to alert about missing data
      abr$note <- NA
      if (check_abr == FALSE | oag_check == FALSE) {
        abr$note <- "The abridged series is missing data for one or more age groups."
      }
      if (!("0" %in% abr$AgeLabel & "1-4" %in% abr$AgeLabel & "0-4" %in% abr$AgeLabel)) {
        abr$note <- "The abridged series is missing data for one or more age groups."
      }

      abr$SexID <- sex

      # now compile these for each sex
      abr_sex <- rbind(abr_sex, abr)

    } else {

      ## 20. close for if nrow(abr) >0
      abr <- abr  %>%  dd_latest_source_year  %>%
        select(DataSourceYear, AgeStart, AgeEnd, AgeLabel, AgeSpan, DataValue)
      abr <- dd_age_standard(abr, abridged = TRUE) %>%
        dplyr::filter(!is.na(DataValue)) %>%
        select(DataSourceYear, AgeStart, AgeEnd, AgeLabel, AgeSpan, AgeSort,DataValue) %>%
        mutate(note = "The abridged series is missing data for one or more age groups.",
               SexID = sex)

      ## 21 Now compile these for each sex
      abr_sex <- rbind(abr_sex, abr)

    }

    # Clean up the environment before beginning next loop
    # rm(abr, check_abr, abr_oag)

  } # close loop for sex

  ##22. Add series field to data
  if (!is.null(abr_sex)) {
    abr_sex <- abr_sex %>%
      mutate(abridged = TRUE,
             complete = FALSE,
             series = "abridged") %>%
      dplyr::filter(AgeSpan %in% c(-2, -1, 1, 4, 5)) # this is the point where the wide age groups are being dropped.
  }

  outdata <- rbind(abr_sex)

  return(outdata)

}

