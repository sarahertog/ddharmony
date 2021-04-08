# This wrapper takes vital counts from census and registers from DemoData and Standardizes/harmonizes
# by age group
# output data frame includes one series: 
#   "complete"  contains data by single year of age up to the open age group

DDharmonize_Vitals1 <- function (indata) {

    # Initialize sex specific outputs
    cpl_sex <- NULL
    abr_from_cpl_sex <-NULL
   
    sexes <- unique(indata$SexID)
    
    for (sex in sexes) { # loop through sex ids, 1=males, 2=females, 3= both
      
      print(paste("SexID = ", sex))

      df <- indata %>% 
          dplyr::filter(SexID == sex & !is.na(DataValue)) %>% 
          mutate(AgeLabel = as.character(AgeLabel)) %>% 
          distinct()
    
      if (nrow(df) > 0) {
      
       # if "Final" data status is available, keep only the final series
       if ("Final" %in% unique(df$DataStatusName)) {
         df <- df %>% 
           dplyr::filter(DataStatusName == "Final")
       }
        
        # check for multiple series ids
        ids_series <- unique(df$SeriesID)
        n_series <- length(ids_series)    
        
        # for each unique series, 
        df_out <- NULL
        for (i in 1:n_series) {
          df_one <- df %>% dplyr::filter(SeriesID == ids_series[i])
          
          # check whether it is a full series with all age groups represented
          df_one_std    <- df_one[df_one$AgeSpan %in% c(-1, -2, 1),]
          if (nrow(df_one_std) > 60) {
            df_one$check_full <- dd_series_isfull(df_one_std, abridged = FALSE)
          } else { df_one$check_full <- FALSE }
          df_out <- rbind(df_out, df_one)
        }
        df <- df_out
        rm(df_out)
        
        # if there is more than one series ...
        if (n_series > 1) {
          latest_source_year <- max(df$DataSourceYear)
          check_latest_full  <- unique(df$check_full[df$DataSourceYear == latest_source_year])
          # ... and latest series is full then keep only that one
          if (check_latest_full) {
            df <- df[df$DataSourceYear == latest_source_year,]
          } else {
            # ... and latest series is not full, then keep the latest data source record for each age
            df <- df  %>%  dd_latest_source_year 
          }
        }
        
        # tidy up the data frame
        df <- df %>% 
          select(DataSourceYear, AgeStart, AgeEnd, AgeLabel, AgeSpan, DataValue) %>% 
          distinct()

       # if no record for unknown age, set data value to zero
        if (!("Unknown" %in% df$AgeLabel)) {
          df <- df %>% 
            bind_rows(data.frame(AgeStart = -2,
                                 AgeEnd = -2,
                                 AgeSpan = -2,
                                 AgeLabel = "Unknown",
                                 DataSourceYear = NA,
                                 DataValue = 0))
        }
        
      # if the "Total" value is less than the sum over age, discard it
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
        
        # identify the start age of the open age group needed to close the series
        oag_start <- dd_oag_agestart(df, multiple5 = FALSE)
        # flag whether this open age group exists in the series
        oag_check <- paste0(oag_start,"+") %in% df$AgeLabel
        
        # drop records for open age groups that do not close the series
        if (!is_empty(oag_start)) {
        df <- df %>% 
          dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start)) %>% 
          arrange(AgeStart, AgeSpan)
        }
        
        # add AgeSort field that identify standard age groups
        df <- dd_age_standard(df, abridged = FALSE) %>% 
          dplyr::filter(!is.na(DataValue))
        
        # check that df is in fact a complete series starting at age zero and without gaps
        check_cpl <- df %>% 
          dplyr::filter(AgeSpan ==1) %>% 
          summarise(minAge = min(AgeStart),
                    maxAge = max(AgeStart),
                    nAge   = length(unique(AgeStart)))
        check_cpl <- check_cpl$minAge == 0 & check_cpl$nAge == check_cpl$maxAge+1
        
        if (check_cpl==TRUE) {
          
          # compute all possible open age groups given available input 
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
        
        # check again whether any open age group exists
        oag_start <- df %>% dd_oag_agestart
        oag_check <- paste0(oag_start,"+") %in% df$AgeLabel
        
        
      # if total is missing and series is otherwise complete, compute total
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

        # write a note to alert about missing data 
        df$note <- NA
        if (check_cpl == FALSE | oag_check == FALSE) {
          df$note <- "The complete series is missing data for one or more age groups."
        } 
         
         df$SexID <- sex
         
         # now compile these for each sex
         cpl_sex <- rbind(cpl_sex, df) 

  # clean up the environment before beginning next loop
    rm(df, cpl, cpl_std, 
       check_cpl, cpl_oag, check_abr, check_cpl)

    } # close loop for sex
    
    # add series field to data
    if (!is.null(cpl_sex)) {
      cpl_sex <- cpl_sex %>% 
        mutate(abridged = FALSE,
               complete = TRUE,
               series = "complete")
    }

    outdata <- cpl_sex

  return(outdata)
  
}
