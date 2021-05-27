## THIS SCRIPT IMPLEMENTS A WORKFLOW FOR Births COUNTS from vr or census
## EXTRACTING FROM DEMODATA, HARMONIZING AGE GROUPS, IDENTIFYING FULL SERIES,
## SELECTING PREFERRED SERIES, VALIDATING TOTALS AND BY SEX AND, EVENTUALLY,
## use retainKeys = TRUE to include these key fields in the function output
## modified 14 Jan 2021 to allow user to specify api server address in server parameter
## Valencia: "https://popdiv.dfs.un.org/DemoData/api/" is default
## Paperspace: "http://74.82.31.177/DemoData/api/"


DDharmonize_validate_BirthCounts <- function(locid, 
                                             times, 
                                             process = c("census", "vr"),
                                             DataSourceShortName = NULL,
                                             DataSourceYear = NULL,
                                             retainKeys = FALSE, 
                                             server = "https://popdiv.dfs.un.org/DemoData/api/") {
  
  require(DDSQLtools)
  require(DemoTools)
  require(tidyverse)
  
  options(dplyr.summarise.inform=F)
################################
################################
## PART 1: EXTRACT VITAL COUNTS (Census and VR) FROM DEMO DATA AND HARMONIZE TO STANDARD 
## ABRIDGED AND COMPLETE AGE GROUPS, BY SERIES

  ## UNPD server housing DemoData
  options(unpd_server = server)
  
# Extract all vital counts for a given country over the period specified in times
  
 dd_extract <- DDextract_VitalCounts(locid = locid, 
                                     type = "births",
                                     process = process,
                                     start_year = times[1],
                                     end_year = times[length(times)],
                                     DataSourceShortName = DataSourceShortName,
                                     DataSourceYear = DataSourceYear) 
 
 
 if (!is.null(dd_extract)) {
   
   # If census data process, extract data catalog info to identify and sub-national censuses
   if (process == "census") {
     DataCatalog <- get_datacatalog(locIds = locid, dataProcessTypeIds = 2, addDefault = "false")
     DataCatalog <- DataCatalog[DataCatalog$isSubnational==FALSE,]
     
     # Keep only those censuses for which isSubnational is FALSE
     dd_extract <- dd_extract %>% 
       filter(DataCatalogID %in% DataCatalog$DataCatalogID)
   }
   
   
   # Temp fix: define LocID and SexID since these have been mistakenly removed from get_recorddata() output
   dd_extract <- dd_extract %>% 
     mutate(LocID = locid,
            SexID = NA)  
   dd_extract$SexID[dd_extract$SexName == "Male"] <- 1
   dd_extract$SexID[dd_extract$SexName == "Female"] <- 2
   dd_extract$SexID[dd_extract$SexName == "Both sexes"] <- 3
   dd_extract$SexID[is.na(dd_extract$SexID)] <- 0
   
   
 dd_extract <- dd_extract %>% 
    # Discard DataTypeName==“Direct (standard abridged age groups computed)” 
    # or “Direct (standard abridged age groups computed - Unknown redistributed)”
    dplyr::filter(DataTypeName!= 'Direct (standard abridged age groups computed)',
           DataTypeName!= 'Direct (standard abridged age groups computed - Unknown redistributed)',
           DataTypeName!= 'Reverse survival method',
           DataTypeName!= 'Birth histories') %>% 
    mutate(id = paste(LocID, LocName, DataProcess, "Births", TimeLabel, DataProcessType, DataSourceName, StatisticalConceptName, DataTypeName, DataReliabilityName, sep = " - ")) %>% 
    arrange(id)
 
 # for births by age of mother, use only both sexes combined
   dd_extract <- dd_extract %>% dplyr::filter(SexID ==3)

  # list of series uniquely identified 
    ids <- unique(dd_extract$id)

  vitals_std_all <- list()
  
  for (i in 1:length(ids)) {
    print(ids[i])
  
    # for each series:
    
    vitals_raw <- dd_extract %>% 
      dplyr::filter(id == ids[i])

    # 1. isolate records that refer to five-year age data
    vitals5_raw <- vitals_raw %>% 
      dplyr::filter(AgeSpan %in% c(-2, -1) | AgeSpan >=5)

    # 2. hamonize the vital5 data into standard age groups
    if (nrow(vitals5_raw[vitals5_raw$AgeSpan == 5,]) > 0) {
      
      print("harmonizing vital counts by 5-year age group")
      vitals5_std <- DDharmonize_Vitals5(indata = vitals5_raw, type = "births")
      
     } else { vitals5_std <- NULL }
    
    # 3. isolate records that refer to single year age data
    vitals1_raw <- vitals_raw %>% 
      dplyr::filter(AgeSpan %in% c(-2, -1, 1))
    
    # 4. hamonize the pop1 data into standard age groups
    if (nrow(vitals1_raw[vitals1_raw$AgeSpan == 1,]) > 0) {
      
      print("harmonizing vital counts by 1-year age group")
      vitals1_std <- DDharmonize_Vitals1(indata = vitals1_raw)
      
    } else { vitals1_std <- NULL }

  if (!is.null(vitals1_std) | !is.null(vitals5_std)) { # continue standardizing if there are any age-specific records
    
    # 4.5 for births, check whether youngest age on abridged is lower than youngest age on complete
    if (!is.null(vitals1_std) & !is.null(vitals5_std)) {
      if ("10-14" %in% vitals5_std$AgeLabel) {
        
      min_age1 <- min(vitals1_std$AgeStart[vitals1_std$AgeSpan == 1])
      min_age5 <- min(vitals5_std$AgeStart[vitals5_std$AgeSpan > 0])
      
      if (min_age1 == 15 & min_age5 == 10) {
        # distribute births at ages 10-14 across single ages 10-14
        # need to get Kirill's Matlab regression for this
        # for now do arbitrary
        b10_14 <- vitals5_std$DataValue[vitals5_std$AgeLabel == "10-14"]
        
        vitals1_std <- dd_births_abridged2single_1014(vitals1_std, births10_14 = b10_14)
        }
      }
    }
    
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
    
    # fill in zeros for births at young ages, if missing
      vitals_abr_cpl1 <- vitals_abr_cpl %>% 
        dplyr::filter(series == "abridged reconciled with complete") 
      if (nrow(vitals_abr_cpl1) > 0) {
      vitals_abr_cpl1 <- dd_fillzeros_births(data = vitals_abr_cpl1 %>% 
                                               select(-AgeSort), abridged = TRUE) %>% 
        mutate(abridged = TRUE,
               complete = FALSE,
               series = "abridged reconciled with complete")
      }
      vitals_abr_cpl2 <- vitals_abr_cpl %>% 
        dplyr::filter(series == "complete reconciled with abridged") 
      if (nrow(vitals_abr_cpl2) > 0) {
      vitals_abr_cpl2 <- dd_fillzeros_births(data = vitals_abr_cpl2 %>% 
                                               select(-AgeSort), abridged = FALSE) %>% 
        mutate(abridged = FALSE,
               complete = TRUE,
               series = "complete reconciled with abridged")
      }
      
      vitals_abr_cpl <- rbind(vitals_abr_cpl1, vitals_abr_cpl2)
      rm(vitals_abr_cpl1, vitals_abr_cpl2)

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
    
    # 6. For births, populate missing abridged age groups with zeros, as appropriate
    if (!is.null(vitals5_std)) {
      vitals5_std <- dd_fillzeros_births(vitals5_std %>% select(-AgeSort), abridged = TRUE) %>% 
        mutate(abridged = TRUE,
               complete = FALSE,
               series = "abridged")
    }

      if (!is.null(vitals_cpl)) {
      if (nrow(vitals_cpl[vitals_cpl$AgeSpan == 1,]) >=5 ) {
      vitals_cpl <- dd_fillzeros_births(vitals_cpl %>% select(-AgeSort), abridged = FALSE) %>% 
        mutate(abridged = FALSE,
               complete = TRUE,
               series = "complete")
        }
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
  
  
################################
################################
## PART 2: FILTER AVAIALBE SERIES, KEEPING ONLY THOSE THAT CONTAIN A FULL AGE DISTRIBUTION
  # AND THE POST-RECONCILIATION ABRIDGED AND COMPLETE SERIES, WHERE APPLICABLE

  if (nrow(vitals_std_all) > 0) {
    
    id_sers <- unique(vitals_std_all$id_series)
    
    id_series_full <- NULL
    
    for (i in 1:length(id_sers)) {
      vitals_one_series <- vitals_std_all %>% 
        dplyr::filter(id_series == id_sers[i])
      
      abridged <- substr(vitals_one_series$series[1],1,1) == "a"
      
      check_full <- dd_series_isfull(vitals_one_series %>% 
                                         dplyr::filter(SexID == 3),
                                       abridged = abridged)

      # if is full, then identify the series to keep
      if (check_full & nrow(vitals_one_series) >= 8) {
        id_series_full <- c(id_series_full, id_sers[i])
      }
    }
    vitals_std_full <- vitals_std_all %>% 
      dplyr::filter(id_series %in% id_series_full) %>% 
      mutate(id_sex = paste(id, SexID, sep = " - "))

  } else { vitals_std_full <- vitals_std_all }
  
  
  # 8. for each id-sex combo of full series,
   # keep the reconciled series if it is available and discard the original abridged or complete
  
  if (nrow(vitals_std_full) > 0) {
  
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
  
  } else { vitals_std_full <- vitals_std_full }
 
################################
################################
## PART 3: VALIDATE THE REMAINING SERIES, CHECKING FOR BOTH SEX TOTALS THAT MATCH BY SEX,
# CORRECT FOR ANY INSTANCES WHERE DATA FOR SEX-AGE GROUP COMBINATIONS ARE MISSING, AND 
# CHECK WEATHER SUM OVER AGE MATCHES THE REPORTED TOTALS

  if (nrow(vitals_std_full) > 0) {
    
  ids <- unique(vitals_std_full$id)
  
  vitals_std_valid <- list()
  for (i in 1:length(ids)) {

  dd_one_id <- vitals_std_full %>% 
    dplyr::filter(id == ids[i])
  
  # reconcile reported and computed totals over age
  dd_one_id <- dd_validate_totals_over_age(data = dd_one_id)
  
  # distribute unkowns by age
  dd_one_id <- dd_distribute_unknowns(data = dd_one_id)
  
  vitals_std_valid[[i]] <- dd_one_id
  
  }
  
  vitals_std_valid <- do.call(rbind, vitals_std_valid) %>% 
    mutate(five_year = abridged == TRUE & AgeSpan %in% c(-1,5),
           abridged = abridged == TRUE & AgeLabel != "0-4")
  
  } else { vitals_std_valid <- vitals_std_full }
  
  # 10.  When there is more than one id for a given census year, select the most authoritative
  
  if (nrow(vitals_std_valid) > 0) {
    
  vitals_valid_id <- vitals_std_valid %>% dd_rank_id_vitals
  
  # arrange the data, with priority colums on the left and data loader keys on the right
  first_columns <- c("id", "LocID", "LocName", "DataProcess", "ReferencePeriod", "TimeStart", "TimeMid", "SexID",
                     "AgeStart", "AgeEnd", "AgeLabel", "AgeSpan", "AgeSort", "DataValue", "note", "abridged", "five_year",
                     "complete", "non_standard")
  keep_columns <- names(vitals_std_all)
  keep_columns <- keep_columns[!(keep_columns %in% c("series", "id_series", "DataSeriesID", first_columns))]
  
  out_all <- vitals_valid_id %>% 
    mutate(non_standard = FALSE,
           DataTypeName = "Direct (age standardized)", 
           note = NA) %>% 
    select(all_of(first_columns), all_of(keep_columns)) 
        
  } else { out_all <- NULL }
  
  # 11. Look for years that are in raw data, but not in output
  #     If there are series with non-standard age groups, then add these to output as well
  
  first_columns <- first_columns[!(first_columns %in% c("five_year", "abridged", "complete", "non_standard", "note"))]
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
           IndicatorName = replace(IndicatorName, abridged == TRUE, "Births by age and sex - abridged"),
           IndicatorName = replace(IndicatorName, five_year == TRUE, "Births by age and sex - abridged"),
           IndicatorName = replace(IndicatorName, complete == TRUE, "Births by age and sex - complete"),
           AgeUnit = "Year",
           SexName = NA,
           SexName = replace(SexName, SexID == 0, "Unknown"),
           SexName = replace(SexName, SexID == 1, "Male"),
           SexName = replace(SexName, SexID == 2, "Female"),
           SexName = replace(SexName, SexID == 3, "Both sexes"))
  
  if (retainKeys == FALSE) {
    out_all <- out_all %>% 
      select(id, LocID, LocName, TimeLabel, TimeMid, TimeEnd, DataProcessType, DataSourceName, StatisticalConceptName,
             DataTypeName, DataReliabilityName, five_year, abridged, complete, non_standard, SexID, AgeStart, AgeEnd, 
             AgeLabel, AgeSpan, AgeSort, DataValue, note)
  }
  
 } else { # if no birth counts were extracted from DemoData
   print(paste0("There are no birth counts by age available for LocID = ",locid," and dataprocess = ", process," for the time period ", times[1], " to ", times[length(times)]))
   out_all <- NULL
 }
  
  return(out_all)
  
}





  
