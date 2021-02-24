# This wrapper takes Population5 indicators from DemoData and Standardizes/harmonizes
# by age group
# output data frame includes two series: 
#   "abridged"  contains standard abridged age groups 0, 1-4, 0-4, 5-9, 10-14 ..... up to the open age group

DDharmonize_Pop5 <- function (indata) {
  
  # split input data by indicator (abridged or single)
  pop_abridged <- indata 
  
    # Initialize sex specific outputs
    abr_sex <- NULL
    cpl_from_abr_sex <-NULL
   
    sexes <- unique(pop_abridged$SexID)
    
    for (sex in sexes) { # loop through sex ids, 1=males, 2=females, 3= both
      
      print(paste("SexID = ", sex))

      abr <- pop_abridged %>% 
          dplyr::filter(SexID == sex & !is.na(DataValue)) %>% 
          select(-SexID) %>% 
          distinct()
    
    if (nrow(abr[abr$AgeSpan == 5,]) > 0) { # only process those that have at least one abridged age group
      
      # if "Final" data status is available, keep only the final series
       if ("Final" %in% unique(abr$DataStatusName)) {
         abr <- abr %>% 
           dplyr::filter(DataStatusName == "Final") 
       }
      
      # check for multiple series ids
      ids_series <- unique(abr$SeriesID)
      n_series <- length(ids_series)    
      
      # for each unique series, 
      abr_out <- NULL
      for (i in 1:n_series) {
        df <- abr %>% dplyr::filter(SeriesID == ids_series[i])
        
        # populate any missing abridged records based on any data by single year of age
        sngl <- df %>% dplyr::filter(AgeSpan == 1)
        
        if (nrow(sngl) > 1) {
          sngl2abr <- sngl %>% dd_single2abridged %>% 
            select(-AgeSort) %>% 
            mutate(DataSourceYear = sngl$DataSourceYear[1])
          df <- df %>% 
            bind_rows(sngl2abr %>% dplyr::filter(!(AgeLabel %in% df$AgeLabel)))
        }
        
        # check whether it is a full series with all age groups represented and an open age greater than 60
        df_abr_std    <- df[(df$AgeStart == 0 & df$AgeSpan == 1 ) | 
                              df$AgeSpan %in% c(-1, -2, 5),]
        if (nrow(df_abr_std) > 13) {
          df$check_full <- dd_series_isfull(df_abr_std, abridged = TRUE)
        } else { df$check_full <- FALSE }
        abr_out <- rbind(abr_out, df)
      }
      abr <- abr_out
      rm(abr_out)
      
      # if there is more than one series ...
      if (n_series > 1) {
        latest_source_year <- max(abr$DataSourceYear)
        check_latest_full  <- unique(abr$check_full[abr$DataSourceYear == latest_source_year])
        # ... and latest series is full then keep only that one
        if (check_latest_full) {
          abr <- abr[abr$DataSourceYear == latest_source_year,]
        } else {
        # ... and latest series is not full, then keep the latest data source record for each age
          abr <- abr  %>%  dd_latest_source_year 
        }
      }
      
      # tidy up the data frame
      abr <- abr %>% 
        select(DataSourceYear, AgeStart, AgeEnd, AgeLabel, AgeSpan, DataValue) %>% 
        distinct()
      
      # if there are still duplicate age groups (e.g., Eswatini 2017 DYB)
      # keep the last one in current sort order
      abr <- abr %>% 
        mutate(sorting = 1:nrow(abr)) %>% 
        group_by(AgeLabel) %>% 
        mutate(keeping = max(sorting)) %>% 
        ungroup() %>% 
        dplyr::filter(sorting == keeping) %>% 
        select(-sorting, -keeping)

       # if no record for unknown age, set data value to zero
        if (!("Unknown" %in% abr$AgeLabel)) {
          abr <- abr %>% 
            bind_rows(data.frame(AgeStart = -2,
                                 AgeEnd = -2,
                                 AgeSpan = -2,
                                 AgeLabel = "Unknown",
                                 DataSourceYear = NA,
                                 DataValue = 0))
        }
        
       # sometimes there are single year ages (not 0) on the abridged series (often for children)
       # extract these for use on single series
        cpl_from_abr <- abr %>% dd_extract_single %>% 
          bind_rows(abr[abr$AgeSpan < 0,])
        
        cpl_from_abr <- dd_age_standard(cpl_from_abr, abridged = FALSE) %>% 
          dplyr::filter(!is.na(DataValue)) %>% 
          mutate(note = NA)
        
       # remove single year records for age> 0 from abridged
        abr <- abr %>% 
          dplyr::filter(!(AgeSpan == 1 & AgeStart != 0)) %>% 
          arrange(AgeStart)
        
      # reconcile first age groups
        abr <- abr %>% dd_firstages_compute 
        
      # check whether there are multiple open age groups
        oag_multi <- abr %>% dd_oag_multiple
        
       # compute closed age groups from multiple open age groups and add to data if missing
        if (oag_multi) {
          add <- abr %>% dd_oag2closed %>% 
            dplyr::filter(!(AgeLabel %in% abr$AgeLabel[!is.na(abr$DataValue)]))
          if (nrow(add > 0)) {
            abr <- abr %>% 
              bind_rows(add) %>% 
              arrange(AgeStart)
          }
        }

      # identify the start age of the open age group needed to close the series
        oag_start <- abr %>% dd_oag_agestart
      # flag whether this open age group exists in the series
        oag_check <- paste0(oag_start,"+") %in% abr$AgeLabel

      # drop records for open age groups that do not close the series
       abr <- abr %>% 
         dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start))

      # check that there are no missing age groups on the abridged series
       if (nrow(abr[abr$AgeStart >= 5,]) > 0) {
        check_abr <- is_abridged(abr$AgeStart[abr$AgeStart >=5])
       } else {
         check_abr <- FALSE
       }
        
        if (check_abr==TRUE) {
          
          # compute all possible open age groups given available input 
          abr_oag <- dd_oag_compute(abr, age_span = 5) 
          
          # append the oag that completes the abridged series
          abr <- abr %>% 
            bind_rows(abr_oag[!(abr_oag$AgeLabel %in% abr$AgeLabel) &
                                abr_oag$AgeStart == oag_start,]) %>% 
            arrange(AgeSort)

        }
        
      # check again whether any open age group exists
        oag_check <- paste0(oag_start,"+") %in% abr$AgeLabel
        
      # if total is missing and series is otherwise complete, compute total
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

      # write a note to alert about missing data 
         abr$note <- NA
         if (check_abr == FALSE | oag_check == FALSE) {
           abr$note <- "The abridged series is missing data for one or more age groups."
         }
         if (!("0" %in% abr$AgeLabel & "1-4" %in% abr$AgeLabel & "0-4" %in% abr$AgeLabel)) {
           abr$note <- "The abridged series is missing data for one or more age groups."
         }
         
         abr$SexID <- sex
         cpl_from_abr$SexID <- sex
         
         # now compile these for each sex
         abr_sex <- rbind(abr_sex, abr) 
         cpl_from_abr_sex <- rbind(cpl_from_abr_sex, cpl_from_abr) 
         
      } else { # close for if nrow(abr) >0 
        # sometimes there are no 5-year age groups but there are 1-year.  We reserve those for complete
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
          
          # now compile these for each sex
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
          
          # now compile these for each sex
          abr_sex <- rbind(abr_sex, abr) 
          cpl_from_abr_sex <- rbind(cpl_from_abr_sex, cpl_from_abr) 
          
          }
      }
 
  # clean up the environment before beginning next loop
    rm(abr, abr_std, abr_cpl_recon, abr_from_cpl, abr_oag, cpl, cpl_std, cpl_abr_recon,
       cpl_from_abr, check_cpl, cpl_oag, check_abr, check_cpl, open.age)

    } # close loop for sex
    
    # add series field to data
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
