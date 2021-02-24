## THIS SCRIPT IMPLEMENTS A WORKFLOW FOR CENSUS POPULATION COUNTS
## EXTRACTING FROM DEMODATA, HARMONIZING AGE GROUPS, IDENTIFYING FULL SERIES,
## SELECTING PREFERRED SERIES, VALIDATING TOTALS AND BY SEX 
## modified 12 Jan 2021 to retain keys for bulk upload to DemoData
## use retainKeys = TRUE to include these key fields in the function output
## modified 14 Jan 2021 to allow user to specify api server address in server parameter
## modified 29 Jan 2021 to retain more key fields for DemoData loader and to 
## eliminate dup country-census records in output
## Valencia: "https://popdiv.dfs.un.org/DemoData/api/" is default
## Paperspace: "http://74.82.31.177/DemoData/api/"

DDharmonize_validate_PopCounts <- function(locid, 
                                           times, 
                                           retainKeys = FALSE, 
                                           server = "https://popdiv.dfs.un.org/DemoData/api/") {
  
  require(DDSQLtools)
  require(DemoTools)
  require(tidyverse)
  
  options(dplyr.summarise.inform=F)
################################
################################
## PART 1: EXTRACT CENSUS POPULATION COUNTS FROM DEMO DATA AND HARMONIZE TO STANDARD 
## ABRIDGED AND COMPLETE AGE GROUPS, BY SERIES

  ## UNPD server housing DemoData
  options(unpd_server = server)
  
# Extract all census population counts for a given country over the period specified in times
  dd_extract <- DDextract_CensusPopCounts(locid      = locid,
                                          start_year = times[1],
                                          end_year   = times[length(times)]) 
  
  if (!is.null(dd_extract)) {
    
    dd_extract <- dd_extract %>% 
    # Discard DataTypeName==“Direct (standard abridged age groups computed)” 
    # or “Direct (standard abridged age groups computed - Unknown redistributed)”
    dplyr::filter(DataTypeName!= 'Direct (standard abridged age groups computed)',
           DataTypeName!= 'Direct (standard abridged age groups computed - Unknown redistributed)') %>% 
    mutate(id = paste(LocID, LocName, DataProcess, ReferencePeriod, DataSourceName, StatisticalConceptName, DataTypeName, DataReliabilityName, sep = " - ")) %>% 
    arrange(id)

  # list of series uniquely identified by Census year - Data Source - Statistical Concept - Data Type
    ids <- unique(dd_extract$id)

  pop_std_all <- list()
  
  for (i in 1:length(ids)) {
    print(ids[i])
  
    # for each series:
    
    pop_raw <- dd_extract %>% 
      dplyr::filter(id == ids[i])

    # 1. isolate records from the "Population5" indicator
    pop5_raw <- pop_raw %>% 
      dplyr::filter(IndicatorShortName == "Population5")
    
    # 2. hamonize the pop5 data into standard age groups
    if (nrow(pop5_raw) > 0) {
      print("harmonizing Population5")
      pop5_std <- DDharmonize_Pop5(indata = pop5_raw)
    } else { pop5_std <- NULL }
    
    # 2.b Often the series for SexID == 0 (other) has records only when the DataValue is non-zero
    # To prevent errors later on, we need to fill in zeros 
    # This is a really clumsy way to do this -- will improve later on
    
    if (0 %in% pop5_std$SexID) {
      sex0_df <- unique(pop5_std[pop5_std$SexID %in% c(1,2,3), 
                                 c("AgeStart", "AgeEnd", "AgeLabel", "AgeSpan", "AgeSort", "abridged", "complete", "series")]) %>% 
        mutate(SexID = 0,
               DataSourceYear = NA,
               DataValue = 0,
               note = NA)
      sex0_orig <- unique(pop5_std[pop5_std$SexID == 0 & pop5_std$DataValue > 0, c("AgeLabel", "DataValue")])
      for (j in 1:nrow(sex0_orig)) {
        sex0_df$DataValue[sex0_df$AgeLabel == sex0_orig$AgeLabel[j]] <- sex0_orig$DataValue[j]
      }
      pop5_std <- rbind(pop5_std[pop5_std$SexID %in% c(1,2),], sex0_df[sex0_df$AgeLabel != "Total",])
      rm(sex0_df, sex0_orig)
    }
    

    # 3. isolate records from the "Population1" indicator
    pop1_raw <- pop_raw %>% 
      dplyr::filter(IndicatorShortName == "Population1")
    
    # 4. hamonize the pop1 data into standard age groups
    if (nrow(pop1_raw) > 0) {
      print("harmonizing Population1")
      pop1_std <- DDharmonize_Pop1(indata = pop1_raw)
    } else { pop1_std <- NULL }
    
    # 4.b As with abridged, deal with incomplete series of SexID = 0
    
    if (0 %in% pop1_std$SexID) {
      sex0_df <- unique(pop1_std[pop1_std$SexID %in% c(1,2), 
                                 c("AgeStart", "AgeEnd", "AgeLabel", "AgeSpan", "AgeSort", "abridged", "complete", "series")]) %>% 
        mutate(SexID = 0,
               DataSourceYear = NA,
               DataValue = 0,
               note = NA)
      sex0_orig <- unique(pop1_std[pop1_std$SexID == 0 & pop1_std$DataValue > 0, c("AgeLabel", "DataValue")])
      for (j in 1:nrow(sex0_orig)) {
        sex0_df$DataValue[sex0_df$AgeLabel == sex0_orig$AgeLabel[j]] <- sex0_orig$DataValue[j]
      }
      pop1_std <- rbind(pop1_std[pop1_std$SexID %in% c(1,2,3),], sex0_df[sex0_df$AgeLabel != "Total",])
      rm(sex0_df, sex0_orig)
    }
    
    # 5. reconcile abridged and complete series, as necessary
    if(!is.null(pop5_std)) {
      pop_abr <- pop5_std %>% 
        dplyr::filter(series == "abridged") # standard abridged age groups
      pop_cpl_from_abr <- pop5_std %>% 
        dplyr::filter(series == "complete from abridged") # single year and other records that were on Population5 series but may be needed for complete
      if (nrow(pop_abr) == 0) {
        pop_abr <- NULL
        pop_abr_cpl <- NULL
      }
    } else {
      pop_abr <- NULL
      pop_cpl_from_abr <- NULL
    }
    if(!is.null(pop1_std)) {
      pop_cpl <- pop1_std
    } else {
      pop_cpl <- NULL
    }
    
    
    
    if (!is.null(pop_abr) & (!is.null(pop_cpl_from_abr) | !is.null(pop_cpl))) {
    pop_abr_cpl <- DDharmonize_AbridgedAndComplete(data_abr = pop_abr,
                                                   data_cpl_from_abr = pop_cpl_from_abr,
                                                   data_cpl = pop_cpl) %>% 
      dplyr::filter(series %in% c("abridged reconciled with complete", "complete reconciled with abridged"))
    
    pop5_std <- pop5_std %>% 
      dplyr::filter(series == "abridged")
    
    }
    
    if(is.null(pop_abr) & !is.null(pop_cpl)) {
      pop5_std <- NULL
      pop_abr_cpl <- NULL
      for (sex in unique(pop_cpl$SexID)) {
      pop_abr_cpl_sex <- dd_single2abridged(data = pop_cpl %>% dplyr::filter(SexID == sex)) %>% 
        mutate(SexID = sex)
      pop_abr_cpl <- rbind(pop_abr_cpl, pop_abr_cpl_sex) 
      rm(pop_abr_cpl_sex)
      }
      pop_abr_cpl <- pop_abr_cpl %>% 
        mutate(abridged = TRUE,
               complete = FALSE,
               series = "abridged reconciled with complete")
    }
    
 
    # 6. Assemble all of the series into a single dataset
    pop_all <- pop5_std %>% 
      bind_rows(pop_cpl) %>% 
      bind_rows(pop_abr_cpl) %>% 
      mutate(id                     = ids[i],
             id_series = paste(id, series, sep = " - "),
             LocName                = pop_raw$LocName[1],
             LocID                  = pop_raw$LocID[1],
             LocTypeName            = pop_raw$LocTypeName[1],
             LocAreaTypeName        = pop_raw$LocAreaTypeName[1],
             SubGroupName           = pop_raw$SubGroupName[1],
             SubGroupTypeName       = pop_raw$SubGroupTypeName[1],
             DataCatalogName        = pop_raw$DataCatalogName[1],
             DataProcess            = pop_raw$DataProcess[1],
             ReferencePeriod        = pop_raw$ReferencePeriod[1],
             TimeUnit               = pop_raw$TimeUnit[1],
             TimeStart              = pop_raw$TimeStart[1],
             TimeEnd                = pop_raw$TimeEnd[1],
             TimeMid                = pop_raw$TimeMid[1],
             DataSourceName         = pop_raw$DataSourceName[1],
             DataSourceAuthor       = pop_raw$DataSourceAuthor[1],
             DataSourceShortName    = pop_raw$DataSourceShortName[1],
             DataSourceYear         = max(pop_raw$DataSourceYear),
             DataStatusName         = pop_raw$DataStatusName[1],
             StatisticalConceptName = pop_raw$StatisticalConceptName[1],
             DataTypeName           = pop_raw$DataTypeName[1],
             DataSeriesID           = pop_raw$SeriesID[1],
             DataReliabilityName    = pop_raw$DataReliabilityName[1],
             DataReliabilitySort    = pop_raw$DataReliabilitySort[1],
             ModelPatternName       = pop_raw$ModelPatternName[1],
             PeriodTypeName         = pop_raw$PeriodTypeName[1],
             PeriodGroupName        = pop_raw$PeriodGroupName[1])

    pop_std_all[[i]] <- pop_all
    
    rm(pop_abr, pop_cpl_from_abr, pop_cpl, pop_abr_cpl, pop5_std, pop1_std)
    
  } # end of id loop
  pop_std_all <- do.call(rbind, pop_std_all)
  
  
################################
################################
## PART 2: FILTER AVAIALBE SERIES, KEEPING ONLY THOSE THAT CONTAIN A FULL AGE DISTRIBUTION
  # AND THE POST-RECONCILIATION ABRIDGED AND COMPLETE SERIES, WHERE APPLICABLE
  
  # 7. dplyr::filter through id_series and keep only those that are full
  # (all age groups present)
  id_sers <- unique(pop_std_all$id_series)
  
  id_series_full <- NULL
  for (i in 1:length(id_sers)) {
    pop_one_series <- pop_std_all %>% 
      dplyr::filter(id_series == id_sers[i])
    
    abridged <- substr(pop_one_series$series[1],1,1) == "a"
    
    check_full_m <- dd_series_isfull(pop_one_series %>% 
                                       dplyr::filter(SexID == 1),
                                     abridged = abridged)
    check_full_f <- dd_series_isfull(pop_one_series %>% 
                                       dplyr::filter(SexID == 2),
                                     abridged = abridged)
    check_full_b <- dd_series_isfull(pop_one_series %>% 
                                       dplyr::filter(SexID == 3),
                                     abridged = abridged)
    check_full <- c(check_full_m, check_full_f, check_full_b) 
    
    n_full <- length(check_full[check_full == TRUE])
    
    # if at least two are full, then identify the series as full
    if (n_full >=2 ) {
      id_series_full <- c(id_series_full, id_sers[i])
    }
  }
  
  pop_std_full <- pop_std_all %>% 
    dplyr::filter(id_series %in% id_series_full) %>% 
    mutate(id_sex = paste(id, SexID, sep = " - "))
  
    
  # 8. for each id-sex combo of full series,
   # keep the reconciled series if it is available and discard the original abridged or complete
  
  
  ids_sex <- unique(pop_std_full$id_sex)
  
  pop_privilege_recon <- NULL
  for (i in 1:length(ids_sex)) {
    
    abr <- pop_std_full %>% 
      dplyr::filter(id_sex == ids_sex[i] & substr(series,1,1) == "a")
    if (nrow(abr) > 0) {
      if ("abridged reconciled with complete" %in% abr$series) {
        abr <- abr %>% 
          dplyr::filter(series == "abridged reconciled with complete")
      }
    }
    
    cpl <- pop_std_full %>% 
      dplyr::filter(id_sex == ids_sex[i] & substr(series,1,1) == "c")
    if (nrow(cpl) > 0) {
      if ("complete reconciled with abridged" %in% cpl$series) {
        cpl <- cpl %>% 
          dplyr::filter(series == "complete reconciled with abridged") 
      }
    }
    
    pop_privilege_recon <- pop_privilege_recon %>% 
      bind_rows(abr) %>% 
      bind_rows(cpl)
    
  }
  
  pop_std_full <- pop_privilege_recon %>% 
    select(-id_sex, -id_series)
  
 
################################
################################
## PART 3: VALIDATE THE REMAINING SERIES, CHECKING FOR BOTH SEX TOTALS THAT MATCH BY SEX,
# CORRECT FOR ANY INSTANCES WHERE DATA FOR SEX-AGE GROUP COMBINATIONS ARE MISSING, AND 
# CHECK WEATHER SUM OVER AGE MATCHES THE REPORTED TOTALS

  ids <- unique(pop_std_full$id)
  
  pop_std_valid <- list()
  for (i in 1:length(ids)) {

  dd_one_id <- pop_std_full %>% 
    dplyr::filter(id == ids[i] & SexID %in% c(0,1,2,3)) %>% 
    select(-note, -series)
  
  # ensure that both sexes values = males + females
  # also redistribute values for SexID == 0 across males and females
  dd_one_id <- dd_validate_totals_over_sex_new(data = dd_one_id)
  
  # reconcile reported and computed totals over age
  dd_one_id <- dd_validate_totals_over_age(data = dd_one_id)
  
  # distribute unkowns by age
  dd_one_id <- dd_distribute_unknowns(data = dd_one_id)
  
  pop_std_valid[[i]] <- dd_one_id
  
  }
  
  pop_std_valid <- do.call(rbind, pop_std_valid) %>% 
    mutate(five_year = abridged == TRUE & AgeSpan %in% c(-1,5),
           abridged = abridged == TRUE & AgeLabel != "0-4")
  
  
  # 10.  When there is more than one id for a given census year, select the most authoritative
  
  pop_valid_id <- pop_std_valid %>% dd_rank_id 
  
  # arrange the data, with priority colums on the left and data loader keys on the right
  first_columns <- c("id", "LocID", "LocName", "DataProcess", "ReferencePeriod", "TimeStart", "TimeMid", "SexID",
                     "AgeStart", "AgeEnd", "AgeLabel", "AgeSpan", "AgeSort", "DataValue", "note", "abridged", "five_year",
                     "complete", "non_standard")
  keep_columns <- names(pop_std_all)
  keep_columns <- keep_columns[!(keep_columns %in% c("series", "id_series", "DataSeriesID", first_columns))]
  
  
  out_all <- pop_valid_id %>% 
    mutate(non_standard = FALSE,
           DataTypeName = "Direct (age standardized)") %>% 
    select(all_of(first_columns), all_of(keep_columns)) 
        
  
  # 11. Look for censuses years that are in raw data, but not in output
  #     If there are series with non-standard age groups, then add these to output as well
  
  first_columns <- first_columns[!(first_columns %in% c("five_year", "abridged", "complete", "non_standard", "note"))]
  skipped <- dd_extract %>% 
    dplyr::filter(!(ReferencePeriod %in% out_all$ReferencePeriod)) %>% 
    select(all_of(first_columns), all_of(keep_columns)) %>% 
    mutate(five_year = FALSE,
           abridged = FALSE,
           complete = FALSE,
           non_standard = TRUE,
           note = "Not harmonized or validated due to non-standard age groups") %>% 
    arrange(id, SexID, AgeSort) %>% 
    distinct()
  
  out_all <- rbind(out_all, skipped) %>% 
    arrange(id, SexID, abridged, AgeSort) %>% 
    mutate(IndicatorName = NA,
           IndicatorName = replace(IndicatorName, abridged == TRUE, "Population5"),
           IndicatorName = replace(IndicatorName, complete == TRUE, "Population1"),
           AgeUnit = "Year",
           SexName = NA,
           SexName = replace(SexName, SexID == 0, "Unknown"),
           SexName = replace(SexName, SexID == 1, "Male"),
           SexName = replace(SexName, SexID == 2, "Female"),
           SexName = replace(SexName, SexID == 3, "Both sexes"))

  if (retainKeys == FALSE) {
    out_all <- out_all %>% 
      select(id, LocID, LocName, ReferencePeriod, TimeMid, DataSourceName, StatisticalConceptName,
             DataTypeName, DataReliabilityName, five_year, abridged, complete, non_standard, SexID, AgeStart, AgeEnd, 
             AgeLabel, AgeSpan, AgeSort, DataValue, note)
  }
  

  } else { # if no census pop counts were extracted from DemoData
    print(paste0("There are no census age distributions available for LocID = ",locid," for the time period ", times[1], " to ", times[length(times)]))
    out_all <- NULL
  }
  
  return(out_all)
  
}





  
