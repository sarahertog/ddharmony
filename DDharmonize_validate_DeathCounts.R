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

DDharmonize_validate_DeathCounts <- function(locid, 
                                           times, 
                                           process = c("census", "vr"),
                                           retainKeys = FALSE, 
                                           server = "https://popdiv.dfs.un.org/DemoData/api/") {
  
  require(DDSQLtools)
  require(DemoTools)
  require(tidyverse)
  
  options(dplyr.summarise.inform=F)
################################
################################
## PART 1: EXTRACT DEATHS BY AGE AND SEX FROM DEMO DATA AND HARMONIZE TO STANDARD 
## ABRIDGED AND COMPLETE AGE GROUPS, BY SERIES

  ## UNPD server housing DemoData
  options(unpd_server = server)
  
  dd_extract <- DDextract_VitalCounts(locid = locid, 
                                      type = "deaths",
                                      process = process,
                                      start_year = times[1],
                                      end_year = times[length(times)])
  
  if (!is.null(dd_extract)) {
    
  dd_extract <- dd_extract %>% 
    dplyr::filter(DataTypeName!= 'Direct (standard abridged age groups computed)',
           DataTypeName!= 'Direct (standard abridged age groups computed - Unknown redistributed)') %>% 
    mutate(id = paste(LocID, LocName, DataProcess, "Deaths", TimeLabel, DataProcessType, DataSourceName, StatisticalConceptName, DataTypeName, DataReliabilityName, sep = " - ")) %>% 
    arrange(id)

  # list of series uniquely identified 
    ids <- unique(dd_extract$id)
    
    vitals_std_all <- list()
    
    for (i in 1:length(ids)) {
      print(ids[i])
      
      # for each series:
      
      vitals_raw <- dd_extract %>% 
        dplyr::filter(id == ids[i])
      
      # 1. isolate records that refer to abridged data
      vitals5_raw <- vitals_raw %>% 
        dplyr::filter(AgeSpan %in% c(-2, -1) | AgeSpan >=5 | AgeLabel %in% c("< 1","1-4")) %>% 
        mutate(AgeLabel = replace(AgeLabel, AgeLabel == "< 1", "0"))
      
      # 2. hamonize the vital5 data into standard age groups
      if (nrow(vitals5_raw[vitals5_raw$AgeSpan == 5,]) > 0) {
        
        print("harmonizing vital counts by 5-year age group")
        vitals5_std <- DDharmonize_Vitals5(indata = vitals5_raw, type = "deaths")
        
      } else { vitals5_std <- NULL }
      
      # 3. isolate records that refer to single year age data
      vitals1_raw <- vitals_raw %>% 
        dplyr::filter(AgeSpan %in% c(-2, -1, 1)) %>% 
        mutate(AgeLabel = replace(AgeLabel, AgeLabel == "< 1", "0"))
      
      # 4. hamonize the pop1 data into standard age groups
      if (nrow(vitals1_raw[vitals1_raw$AgeSpan == 1,]) > 0) {
        
        print("harmonizing vital counts by 1-year age group")
        vitals1_std <- DDharmonize_Vitals1(indata = vitals1_raw)
        
      } else { vitals1_std <- NULL }
      
      if (!is.null(vitals1_std) | !is.null(vitals5_std)) { # continue standardizing if there are any age-specific records
        
        # 5. reconcile abridged and complete series, as necessary
        
        if(!is.null(vitals5_std)) {
          vitals_abr <- vitals5_std 
          if (nrow(vitals_abr) == 0) {
            vitals_abr <- NULL
          }
        } else {
          vitals_abr <- NULL
        }
        if(!is.null(vitals1_std)) {
          vitals_cpl <- vitals1_std
        } else {
          vitals_cpl <- NULL
        }
        
        if (!is.null(vitals_abr) & !is.null(vitals_cpl)) {
          vitals_abr_cpl <- DDharmonize_AbridgedAndComplete(data_abr = vitals_abr,
                                                            data_cpl_from_abr = NULL,
                                                            data_cpl = vitals_cpl) %>% 
            dplyr::filter(series %in% c("abridged reconciled with complete", "complete reconciled with abridged"))
          
        } else { vitals_abr_cpl <- NULL }
        
        if (is.null(vitals_abr) & !is.null(vitals_cpl)) {
          vitals5_std <- NULL
          vitals_abr_cpl <- NULL
          for (sex in unique(vitals_cpl$SexID)) {
            vitals_abr_cpl_sex <- dd_single2abridged(data = vitals_cpl %>% dplyr::filter(SexID == sex)) %>% 
              mutate(SexID = sex)
            vitals_abr_cpl <- rbind(vitals_abr_cpl, vitals_abr_cpl_sex) 
            rm(vitals_abr_cpl_sex)
          }
          vitals_abr_cpl <- dd_fillzeros_births(vitals_abr_cpl %>% select(-AgeSort), abridged = TRUE)  
          
          vitals_abr_cpl <- vitals_abr_cpl %>% 
            mutate(abridged = TRUE,
                   complete = FALSE,
                   series = "abridged reconciled with complete")
        }
        
      } else { # if there are no age-specific records
        vitals_cpl <- NULL
        vitals_abr_cpl <- NULL
      }
      
      # 6. Assemble all of the series into a single dataset
      vitals_all <- vitals5_std %>% 
        bind_rows(vitals_cpl) %>% 
        bind_rows(vitals_abr_cpl) 
      
      if (nrow(vitals_all) > 0) {
        vitals_all <- vitals_all %>% 
          mutate(id                     = ids[i],
                 id_series = paste(id, series, sep = " - "),
                 LocName                = vitals_raw$LocName[1],
                 LocID                  = vitals_raw$LocID[1],
                 LocTypeName            = vitals_raw$LocTypeName[1],
                 LocAreaTypeName        = vitals_raw$LocAreaTypeName[1],
                 SubGroupName           = vitals_raw$SubGroupName[1],
                 SubGroupTypeName       = vitals_raw$SubGroupTypeName[1],
                 DataCatalogID          = vitals_raw$DataCatalogID[1],
                 DataCatalogName        = vitals_raw$DataCatalogName[1],
                 DataProcess            = vitals_raw$DataProcess[1],
                 DataProcessType        = vitals_raw$DataProcessType[1],
                 ReferencePeriod        = vitals_raw$ReferencePeriod[1],
                 TimeUnit               = vitals_raw$TimeUnit[1],
                 TimeStart              = vitals_raw$TimeStart[1],
                 TimeEnd                = vitals_raw$TimeEnd[1],
                 TimeMid                = vitals_raw$TimeMid[1],
                 TimeLabel              = vitals_raw$TimeLabel[1],
                 DataSourceName         = vitals_raw$DataSourceName[1],
                 DataSourceAuthor       = vitals_raw$DataSourceAuthor[1],
                 DataSourceShortName    = vitals_raw$DataSourceShortName[1],
                 DataSourceYear         = max(vitals_raw$DataSourceYear),
                 DataStatusName         = vitals_raw$DataStatusName[1],
                 StatisticalConceptName = vitals_raw$StatisticalConceptName[1],
                 DataTypeName           = vitals_raw$DataTypeName[1],
                 DataSeriesID           = vitals_raw$SeriesID[1],
                 DataReliabilityName    = vitals_raw$DataReliabilityName[1],
                 DataReliabilitySort    = vitals_raw$DataReliabilitySort[1],
                 ModelPatternName       = vitals_raw$ModelPatternName[1],
                 PeriodTypeName         = vitals_raw$PeriodTypeName[1],
                 PeriodGroupName        = vitals_raw$PeriodGroupName[1])
      }
      vitals_std_all[[i]] <- vitals_all
      
      rm(vitals_abr, vitals_cpl, vitals_abr_cpl, vitals5_std, vitals1_std)
      
    } # end of id loop
    vitals_std_all <- do.call(rbind, vitals_std_all)


if (nrow(vitals_std_all) > 0) {
################################
################################
## PART 2: FILTER AVAIALBE SERIES, KEEPING ONLY THOSE THAT CONTAIN A FULL AGE DISTRIBUTION
  # AND THE POST-RECONCILIATION ABRIDGED AND COMPLETE SERIES, WHERE APPLICABLE
  
  # 7. dplyr::filter through id_series and keep only those that are full
  # (all age groups present)
  id_sers <- unique(vitals_std_all$id_series)
  
  id_series_full <- NULL
  for (i in 1:length(id_sers)) {
    pop_one_series <- vitals_std_all %>% 
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
  
  vitals_std_full <- vitals_std_all %>% 
    dplyr::filter(id_series %in% id_series_full) %>% 
    mutate(id_sex = paste(id, SexID, sep = " - "))
  
    
  # 8. for each id-sex combo of full series,
   # keep the reconciled series if it is available and discard the original abridged or complete
  
  
  ids_sex <- unique(vitals_std_full$id_sex)
  
  vitals_privilege_recon <- NULL
  for (i in 1:length(ids_sex)) {
    
    abr <- vitals_std_full %>% 
      dplyr::filter(id_sex == ids_sex[i] & substr(series,1,1) == "a")
    if (nrow(abr) > 0) {
      if ("abridged reconciled with complete" %in% abr$series) {
        abr <- abr %>% 
          dplyr::filter(series == "abridged reconciled with complete")
      }
    }
    
    cpl <- vitals_std_full %>% 
      dplyr::filter(id_sex == ids_sex[i] & substr(series,1,1) == "c")
    if (nrow(cpl) > 0) {
      if ("complete reconciled with abridged" %in% cpl$series) {
        cpl <- cpl %>% 
          dplyr::filter(series == "complete reconciled with abridged") 
      }
    }
    
    vitals_privilege_recon <- vitals_privilege_recon %>% 
      bind_rows(abr) %>% 
      bind_rows(cpl)
    
  }
  
  vitals_std_full <- vitals_privilege_recon %>% 
    select(-id_sex, -id_series)
  
 
################################
################################
## PART 3: VALIDATE THE REMAINING SERIES, CHECKING FOR BOTH SEX TOTALS THAT MATCH BY SEX,
# CORRECT FOR ANY INSTANCES WHERE DATA FOR SEX-AGE GROUP COMBINATIONS ARE MISSING, AND 
# CHECK WEATHER SUM OVER AGE MATCHES THE REPORTED TOTALS

  ids <- unique(vitals_std_full$id)
  
  vitals_std_valid <- list()
  for (i in 1:length(ids)) {

  dd_one_id <- vitals_std_full %>% 
    dplyr::filter(id == ids[i] & SexID %in% c(1,2,3))
  
  # reconcile reported and computed totals over age
  # see note on "Total" record that indicates if difference was greater than 2.5% and thus irreconcilable
  dd_one_id <- dd_validate_totals_over_age(data = dd_one_id)
  
  # distribute unkowns by age
  dd_one_id <- dd_distribute_unknowns(data = dd_one_id)
  
  # ensure that both sexes values = males + females
  dd_one_id <- dd_validate_totals_over_sex(data = dd_one_id)
    
  vitals_std_valid[[i]] <- dd_one_id
  
  }
  
  vitals_std_valid <- do.call(rbind, vitals_std_valid) %>% 
    mutate(five_year = abridged == TRUE & AgeSpan %in% c(-1,5),
           abridged = abridged == TRUE & AgeLabel != "0-4")
  
  
  # 10.  When there is more than one id for a given census year, select the most authoritative
  
  vitals_valid_id <- vitals_std_valid %>% dd_rank_id_vitals 
  
  # arrange the data, with priority colums on the left and data loader keys on the right
  first_columns <- c("id", "LocID", "LocName", "DataProcess", "TimeStart", "TimeMid", "TimeEnd", "SexID",
                     "AgeStart", "AgeEnd", "AgeLabel", "AgeSpan", "AgeSort", "DataValue", "note", "abridged", "five_year",
                     "complete", "non_standard")
  keep_columns <- names(vitals_std_all)
  keep_columns <- keep_columns[!(keep_columns %in% c("series", "id_series", "DataSeriesID", "DataReliabilitySort", first_columns))]
  
  
  out_all <- vitals_valid_id %>% 
    mutate(non_standard = FALSE,
           DataTypeName = "Direct (age standardized)") %>% 
    select(all_of(first_columns), all_of(keep_columns)) 
        
} else { out_all <- NULL }
    
  # 11. Look for years that are in raw data, but not in output
  #     If there are series with non-standard age groups, then add these to output as well
  
  first_columns <- c("id", "LocID", "LocName", "DataProcess", "TimeStart", "TimeMid", "TimeEnd","SexID",
                       "AgeStart", "AgeEnd", "AgeLabel", "AgeSpan", "AgeSort", "DataValue")
    
  skipped <- dd_extract %>% 
    dplyr::filter(!(TimeLabel %in% out_all$TimeLabel)) %>% 
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
           IndicatorName = replace(IndicatorName, abridged == TRUE, "Deaths by age and sex - abridged"),
           IndicatorName = replace(IndicatorName, five_year == TRUE, "Deaths by age and sex - abridged"),
           IndicatorName = replace(IndicatorName, complete == TRUE, "Deaths by age and sex - complete"),
           AgeUnit = "Year",
           SexName = NA,
           SexName = replace(SexName, SexID == 0, "Unknown"),
           SexName = replace(SexName, SexID == 1, "Male"),
           SexName = replace(SexName, SexID == 2, "Female"),
           SexName = replace(SexName, SexID == 3, "Both sexes"))

  if (retainKeys == FALSE) {
    out_all <- out_all %>% 
      select(id, LocID, LocName, TimeMid, TimeEnd, DataSourceName, StatisticalConceptName,
             DataTypeName, DataReliabilityName, five_year, abridged, complete, non_standard, SexID, AgeStart, AgeEnd, 
             AgeLabel, AgeSpan, AgeSort, DataValue, note)
  }
  
  } else { # if no death counts were extracted from DemoData
    print(paste0("There are no death counts by age available for LocID = ",locid," and dataprocess = ", process," for the time period ", times[1], " to ", times[length(times)]))
    out_all <- NULL
  }
  
  return(out_all)
  
}





  
