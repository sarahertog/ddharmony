#' @title
#' DDharmonize_Pop5
#'
#' @description
#' This wrapper takes Population5 indicators from DemoData and Standardizes/harmonizes them.
#'
#' @details
#' See the \href{https://shelmith-kariuki.github.io/rddharmony/articles/DDharmonize_5YearCounts.html}{"Harmonizing five-year counts" vignette} for more details about this function.
#'
#' @param indata The data to be harmonised
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return  A data frame that contains standard abridged age groups 0, 1-4, 0-4, 5-9, 10-14 ..... up to the open age group

#' @export


DDharmonize_Pop5 <- function (indata) {

    ##1. Create an object containing the data
      pop_abridged <- indata

    ##2. Initialize sex specific outputs
    abr_sex <- NULL
    cpl_from_abr_sex <-NULL

    sexes <- unique(pop_abridged$SexID)

    for (sex in sexes) { # loop through sex ids, 1=males, 2=females, 3= both

    ##3. Print the SexID whose data we are dealing with
      print(paste("SexID = ", sex))

    ##4. Filter the data to only be left with data for this specific SexID
      abr <- pop_abridged %>%
          dplyr::filter(SexID == sex & !is.na(DataValue)) %>%
          select(-SexID) %>%
          distinct()

    if (nrow(abr[abr$AgeSpan == 5,]) > 0) { # only process those that have at least one abridged age group

    ##5. if "Final" data status is available, keep only the final series

       if ("Final" %in% unique(abr$DataStatusName)) {
         abr <- abr %>%
           dplyr::filter(DataStatusName == "Final")
       }

    ##6. check for multiple series ids
      ids_series <- unique(abr$SeriesID)
      n_series <- length(ids_series)

      # for each unique series,
      abr_out <- NULL
      for (i in 1:n_series) {

        ##7. Filter the data for a specific SeriesID
        df <- abr %>% dplyr::filter(SeriesID == ids_series[i])

        ##8. populate any missing abridged records based on any data by single year of age
        sngl <- df %>% dplyr::filter(AgeSpan == 1)

        if (nrow(sngl) > 1) {
          sngl2abr <- sngl %>% dd_single2abridged %>%
            select(-AgeSort) %>%
            mutate(DataSourceYear = sngl$DataSourceYear[1])
          df <- df %>%
            bind_rows(sngl2abr %>% dplyr::filter(!(AgeLabel %in% df$AgeLabel)))
        }

        ##9. check whether it is a full series with all age groups represented and an open age greater than 60
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

        ##10. Keep the latest datasource year

        ## if there is more than one series ...
      if (n_series > 1) {
        latest_source_year <- max(abr$DataSourceYear)
        check_latest_full  <- unique(abr$check_full[abr$DataSourceYear == latest_source_year])
        ## ... and latest series is full then keep only that one
        if (any(check_latest_full)) {
          abr <- abr[abr$DataSourceYear == latest_source_year,]
        } else {
        ## ... and latest series is not full, then keep the latest data source record for each age
          abr <- abr  %>%  dd_latest_source_year
        }
      }

      ##11. tidy up the data frame
      abr <- abr %>%
        select(DataSourceYear, AgeStart, AgeEnd, AgeLabel, AgeSpan, DataValue) %>%
        distinct()

      ##12. if there are still duplicate age groups (e.g., Eswatini 2017 DYB),keep the last one in current sort order
      abr <- abr %>%
        mutate(sorting = 1:nrow(abr)) %>%
        group_by(AgeLabel) %>%
        mutate(keeping = max(sorting)) %>%
        ungroup() %>%
        dplyr::filter(sorting == keeping) %>%
        select(-sorting, -keeping)

       ##13. if there is no record for unknown age, set data value to zero
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

       ##14. sometimes there are single year ages (not 0) on the abridged series (often for children), extract these for use on single series
        cpl_from_abr <- abr %>% dd_extract_single %>%
          bind_rows(abr[abr$AgeSpan < 0,])

        cpl_from_abr <- dd_age_standard(cpl_from_abr, abridged = FALSE) %>%
          dplyr::filter(!is.na(DataValue)) %>%
          mutate(note = NA)

       ##15. remove single year records for age> 0 from abridged
        abr <- abr %>%
          dplyr::filter(!(AgeSpan == 1 & AgeStart != 0)) %>%
          arrange(AgeStart)

      ##16. reconcile first age groups
        abr <- abr %>% dd_firstages_compute

      ##17. check whether there are multiple open age groups
        oag_multi <- abr %>% dd_oag_multiple

      ##18. compute closed age groups from multiple open age groups and add to data if missing
        if (oag_multi) {
          add <- abr %>% dd_oag2closed %>%
            dplyr::filter(!(AgeLabel %in% abr$AgeLabel[!is.na(abr$DataValue)]))
          if (nrow(add > 0)) {
            abr <- abr %>%
              bind_rows(add) %>%
              arrange(AgeStart)
          }
        }

      ##19 identify the start age of the open age group needed to close the series
        oag_start <- abr %>% dd_oag_agestart

      ##20. flag whether this open age group exists in the series
        oag_check <- paste0(oag_start,"+") %in% abr$AgeLabel

      ##21. drop records for open age groups that do not close the series
      ## Edited the part below because of the error below, in cases where we only have the Total and Unknown:
      ## Error : Problem with `filter()` input `..1`.
      ## ℹ Input `..1` is `!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start)`.
      ## x Input `..1` must be of size 2 or 1, not size 0.
      ## Case: "470 - Malta - Estimate - 1953 - Demographic Yearbook - De-facto - Population by age and sex - Fair"

        # abr <- abr %>%
        #   dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start))

      if(!all(abr$AgeLabel %in% c("Total", "Unknown"))){

       abr <- abr %>%
         dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start))

      }

      ##22. check that there series is actually abridged
       if (nrow(abr[abr$AgeStart >= 5,]) > 0) {
        check_abr <- is_abridged(abr$AgeStart[abr$AgeStart >=5])
       } else {
         check_abr <- FALSE
       }

        if (check_abr==TRUE) {

          ##23. compute all possible open age groups given available input
          abr_oag <- dd_oag_compute(abr, age_span = 5)

          ##24. append the oag that completes the abridged series
          abr <- abr %>%
            bind_rows(abr_oag[!(abr_oag$AgeLabel %in% abr$AgeLabel) &
                                abr_oag$AgeStart == oag_start,]) %>%
            arrange(AgeSort)

        }

      ##24. check again whether any open age group exists
        oag_check <- paste0(oag_start,"+") %in% abr$AgeLabel

      ##25. if total is missing and series is otherwise complete, compute total
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

      ##26. write a note to alert about missing data
         abr$note <- NA
         if (check_abr == FALSE | oag_check == FALSE) {
           abr$note <- "The abridged series is missing data for one or more age groups."
         }
         if (!("0" %in% abr$AgeLabel & "1-4" %in% abr$AgeLabel & "0-4" %in% abr$AgeLabel)) {
           abr$note <- "The abridged series is missing data for one or more age groups."
         }

         abr$SexID <- sex
         cpl_from_abr$SexID <- sex

         ##27. now compile these for each sex
         abr_sex <- rbind(abr_sex, abr)
         cpl_from_abr_sex <- rbind(cpl_from_abr_sex, cpl_from_abr)

      } else { # close for if nrow(abr) >0

      ##28. sometimes there are no 5-year age groups but there are 1-year.  We reserve those for complete
        if (nrow(abr[abr$AgeSpan == 1,]) > 0) {
          abr <- abr  %>%  dd_latest_source_year

          cpl_from_abr <- abr %>% dd_extract_single %>%
            bind_rows(abr[abr$AgeSpan < 0,]) %>%
            select(DataSourceYear, AgeStart, AgeEnd, AgeLabel, AgeSpan, DataValue)

          cpl_from_abr <- dd_age_standard(cpl_from_abr, abridged = FALSE) %>%
            dplyr::filter(!is.na(DataValue)) %>%
            select(DataSourceYear, AgeStart, AgeEnd, AgeLabel, AgeSpan, AgeSort, DataValue) %>%
            mutate(note = NA,
                   note = as.character(note),
                   SexID = sex)

          abr <- NULL

          ##29. now compile these for each sex
          abr_sex <- rbind(abr_sex, abr)
          cpl_from_abr_sex <- rbind(cpl_from_abr_sex, cpl_from_abr)

        } else { # if no 5 or 1 year age groups, then just keep total, open and unknown

          abr <- abr  %>%  dd_latest_source_year  %>%
            select(DataSourceYear, AgeStart, AgeEnd, AgeLabel, AgeSpan, DataValue)
          abr <- dd_age_standard(abr, abridged = TRUE) %>%
            dplyr::filter(!is.na(DataValue)) %>%
            select(DataSourceYear, AgeStart, AgeEnd, AgeLabel, AgeSpan, AgeSort, DataValue) %>%
            mutate(note = "The abridged series is missing data for one or more age groups.",
                   SexID = sex)

                    cpl_from_abr <- NULL

          ##30. now compile these for each sex
          abr_sex <- rbind(abr_sex, abr)
          cpl_from_abr_sex <- rbind(cpl_from_abr_sex, cpl_from_abr)

          }
      }

      ##31. clean up the environment before beginning next loop
      ## Added the line below for cases where check_abr and abr_oag do not exist
      ## Case id: 752 - Sweden
      check_abr <- NULL
      abr_oag <- NULL
    rm(abr, abr_oag, cpl_from_abr, check_abr)

    } # close loop for sex

    ##32. add series field to data
    if (!is.null(abr_sex)) {
      abr_sex <- abr_sex %>%
        mutate(abridged = TRUE,
               complete = FALSE,
               series = "abridged") %>%
        dplyr::filter(AgeSpan %in% c(-2, -1, 1, 4, 5))
    }
    if (!is.null(cpl_from_abr_sex)) {
      cpl_from_abr_sex <- cpl_from_abr_sex %>%
        mutate(abridged = FALSE,
               complete = TRUE,
               series = "complete from abridged") %>%
        dplyr::filter(AgeSpan %in% c(-2, -1, 1))
    }

    outdata <- rbind(abr_sex, cpl_from_abr_sex)

  return(outdata)

}
