#' @title
#' DDharmonize_Pop1
#'
#' @description
#' This wrapper takes population1 indicators from DemoData and Standardizes/harmonizes them.
#'
#' @details
#' See the \href{https://shelmith-kariuki.github.io/rddharmony/articles/DDharmonize_1YearCounts.html}{"Harmonizing one-year counts" vignette} for more details about this function.
#'
#' @param indata The data to be harmonised.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A data frame that contains harmonized single year of age counts and closed by an open age group.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- pop1_df
#' df <- DDharmonize_Pop1(df)
#'}
DDharmonize_Pop1 <- function (indata) {

    ##1. Create an object containing population counts by single year of age
    pop_complete <- indata

    ##2. Initialize sex specific outputs
    cpl_sex <- NULL
    abr_from_cpl_sex <-NULL

    sexes <- unique(pop_complete$SexID)

    for (sex in sexes) { # loop through sex ids, 1=males, 2=females, 3= both

    ##3. Print the SexID whose data we are dealing with
      print(paste("SexID = ", sex))

    ##4. Filter the data to only be left with data for this specific SexID
      df <- pop_complete %>%
          dplyr::filter(SexID == sex & !is.na(DataValue)) %>%
          mutate(AgeLabel = as.character(AgeLabel)) %>%
          distinct()

      if (nrow(df) > 0) {

        ##5. if "Final" data status is available, keep only the final series
        if ("Final" %in% unique(df$DataStatusName)) {
         df <- df %>%
           dplyr::filter(DataStatusName == "Final")
       }

        ##6. check for multiple series ids
        ids_series <- unique(df$SeriesID)
        n_series <- length(ids_series)

        ##7. Filter the data for a specific SeriesID
        df_out <- NULL
        for (i in 1:n_series) {
          df_one <- df %>% dplyr::filter(SeriesID == ids_series[i])

          ##9. check whether it is a full series with all age groups represented and an open age greater than 60
          df_one_std    <- df_one[df_one$AgeSpan %in% c(-1, -2, 1),]
          if (nrow(df_one_std) > 60) {
            df_one$check_full <- dd_series_isfull(df_one_std, abridged = FALSE)
          } else { df_one$check_full <- FALSE }
          df_out <- rbind(df_out, df_one)
        }
        df <- df_out
        rm(df_out)

        ## Added by Shel (26th October at 14:26)
        ## Case study (births): id == "578 - Norway - VR - Births - 2002 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair"
        ## If there exists more than one total and there is a one that is equal to the computed total,
        ## drop the others to be left with this one.
        df <- df %>% dd_multiple_totals

        ##10. Keep the latest datasource year
        # if there is more than one series ...
        if (n_series > 1) {
          latest_source_year <- max(df$DataSourceYear)
          check_latest_full  <- unique(df$check_full[df$DataSourceYear == latest_source_year])
          # ... and latest series is full then keep only that one
          if (any(check_latest_full)) {
            df <- df[df$DataSourceYear == latest_source_year,]
          } else {
            # ... and latest series is not full, then keep the latest data source record for each age
            df <- df  %>%  dd_latest_source_year
          }
        }

        ##11. tidy up the data frame
        df <- df %>%
          select(DataSourceYear, AgeStart, AgeEnd, AgeLabel, AgeSpan, DataValue) %>%
          distinct()

        ##12. if there are still duplicate age groups (e.g., Eswatini 2017 DYB),keep the last one in current sort order
        df <- df %>%
          mutate(sorting = 1:nrow(df)) %>%
          group_by(AgeLabel) %>%
          mutate(keeping = max(sorting)) %>%
          ungroup() %>%
          dplyr::filter(sorting == keeping) %>%
          select(-sorting, -keeping)

        ##13. if there is no record for unknown age, set data value to zero
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
        ## if reported total - calculated total == "unknown", then unknown == 0

        df <- df %>% dd_drop_unknowns()

      ##14. if the "Total" value is less than the sum over age, discard it
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

        ##15 identify the start age of the open age group needed to close the series
        oag_start <- dd_oag_agestart(df, multiple5 = FALSE)

        ##16. flag whether this open age group exists in the series
        oag_check <- paste0(oag_start,"+") %in% df$AgeLabel

        ##17. drop records for open age groups that do not close the series
        ## Edited the part below because of the error below, in cases where we only have the Total and Unknown:
        ## Error : Problem with `filter()` input `..1`.
        ## ℹ Input `..1` is `!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start)`.
        ## x Input `..1` must be of size 2 or 1, not size 0.
        ## Case: "470 - Malta - Estimate - 1953 - Demographic Yearbook - De-facto - Population by age and sex - Fair"

        # df <- df %>%
        #   dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start)) %>%
        #   arrange(AgeStart, AgeSpan)

        if(!all(df$AgeLabel %in% c("Total", "Unknown"))){

          df <- df %>%
            dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start)) %>%
            arrange(AgeStart, AgeSpan)

        }


        ##18. add AgeSort field that identify standard age groups
        df <- dd_age_standard(df, abridged = FALSE) %>%
          dplyr::filter(!is.na(DataValue))

        ##19. check that df is in fact a complete series starting at age zero and without gaps
        check_cpl <- df %>%
          dplyr::filter(AgeSpan ==1) %>%
          summarise(minAge = min(AgeStart),
                    maxAge = max(AgeStart),
                    nAge   = length(unique(AgeStart)))
        check_cpl <- check_cpl$minAge == 0 & check_cpl$nAge == check_cpl$maxAge+1

        if (check_cpl==TRUE) {

          ##20. compute all possible open age groups given available input
          df_oag <- dd_oag_compute(df, age_span = 1)

          df <- df %>%
            bind_rows(df_oag[!(df_oag$AgeLabel %in% df$AgeLabel) &
                                df_oag$AgeStart == oag_start,]) %>%
            arrange(AgeSort)
        }

      }

      if (!("AgeSort" %in% names(df))) {
        df <- dd_age_standard(df, abridged = FALSE) %>%
          dplyr::filter(!is.na(DataValue))
        check_cpl <- FALSE
      }

        ##21. check again whether any open age group exists
        oag_start <- df %>% dd_oag_agestart
        oag_check <- paste0(oag_start,"+") %in% df$AgeLabel


      ##22. if total is missing and series is otherwise complete, compute total
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

        ##23. write a note to alert about missing data
        df$note <- NA
        if (check_cpl == FALSE | oag_check == FALSE) {
          df$note <- "The complete series is missing data for one or more age groups."
        }

         df$SexID <- sex

         ##24. now compile these for each sex
         cpl_sex <- rbind(cpl_sex, df)

  ##25. clean up the environment before beginning next loop
    rm(df,check_cpl)

    } # close loop for sex

    ##25. add series field to data
    if (!is.null(cpl_sex)) {
      cpl_sex <- cpl_sex %>%
        mutate(abridged = FALSE,
               complete = TRUE,
               series = "complete")
    }

    outdata <- cpl_sex

  return(outdata)

}
