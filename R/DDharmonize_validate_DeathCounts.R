#' @title
#' DDharmonize_validate_DeathCounts
#'
#' @description
#' This script implements a workflow for death counts from vr or census extracting data from Demodata, harmonizing age groups,
#' identifying full series, selecting preferred series, validating totals by age and by sex and eventually,
#' including key fields in the function output.
#'
#' @details
#' See the \href{https://shelmith-kariuki.github.io/rddharmony/articles/DDharmonize_validate_DeathCounts.html}{DDharmonize_validate_DeathCounts} vignette for more details about this function.
#'
#' @param locid Location id
#' @param times The period of the data to be extracted. You can extract one year data e.g times = 2020 or a longer period of time e.g times = c(1950, 2020).
#' @param process The process through which the data was obtained from various sources i.e either via census or vital registrations (vr). By default, the function pulls data obtained through both of these processes.
#' @param return_unique_ref_period TRUE/FALSE. Specifies whether the data to be returned should contain one unique id (return_unique_ref_period == TRUE) or several ids (return_unique_ref_period == FALSE) per time label.
#' ids are a unique identifier for each unique set of records based on `LocID`, `LocName`, `DataProcess`, `ReferencePeriod`, `DataSourceName`, `StatisticalConceptName`, `DataTypeName` and `DataReliabilityName`.
#' The definitions of these variables are provided later in this article.
#' @param DataSourceShortName NULL
#' @param DataSourceYear NULL
#' @param retainKeys TRUE/FALSE. Specifies whether only a few (retainKeys == FALSE) or all (retainKeys == TRUE) variables should be retained in the output.
#' @param server "https://popdiv.dfs.un.org/DemoData/api/"
#'
#' @import dplyr
#' @import assertthat
#' @import DDSQLtools
#' @import DemoTools
#' @importFrom magrittr %>%
#' @importFrom purrr is_empty
#'
#' @return  A harmonized dataset containing death counts
#'
#' @export
#'
#' @examples
#' \dontrun{
#' kenya_df <- DDharmonize_validate_DeathCounts(404,
#'                                               c(1950,2020),
#'                                               process = c("census", "vr"),
#'                                               return_unique_ref_period = TRUE,
#'                                               DataSourceShortName = NULL,
#'                                               DataSourceYear = NULL,
#'                                               retainKeys = FALSE,
#'                                               server = "https://popdiv.dfs.un.org/DemoData/api/")
#' }
DDharmonize_validate_DeathCounts <- function(locid,
                                             times,
                                             process = c("census", "vr"),
                                             return_unique_ref_period = TRUE, # if true, then only most authoritative series will be returned for each reference period, per dd_rank_id_vitals()
                                             DataSourceShortName = NULL,
                                             DataSourceYear = NULL,
                                             retainKeys = FALSE,
                                             server = "https://popdiv.dfs.un.org/DemoData/api/") {

  ## -------------------------------------------------------------------------------------------------------------------
  ## PART 1: EXTRACT DEATHS BY AGE AND SEX FROM DEMO DATA AND HARMONIZE TO STANDARD ABRIDGED AND COMPLETE AGE GROUPS, BY SERIES
  ## -------------------------------------------------------------------------------------------------------------------

  ## UNPD server housing DemoData
  options(unpd_server = server)

  ## 1. Extract all vital counts for a given country over the period specified in times
  dd_extract <- DDextract_VitalCounts(locid = locid,
                                      type = "deaths",
                                      process = process,
                                      start_year = times[1],
                                      end_year = times[length(times)],
                                      DataSourceShortName = DataSourceShortName,
                                      DataSourceYear = DataSourceYear)

  if (!is.null(dd_extract)) {

    ## Shel added this so that it can be easy to compare the raw data with the clean and harmonized data.
    dd_extract <- dd_extract %>%
      relocate(DataValue, .after = "agesort")

    assign("raw_df", dd_extract, .GlobalEnv)

    # get data process id
    dpi <- ifelse(process == "census", 2, 36)

    ## 2. Drop sub-national censuses (data process "vr" does not work with get_datacatalog?)
    # filter out sub-national censuses (data process "vr" does not work with get_datacatalog?)
    # if (dpi == 2) {
    # Gets all DataCatalog records

    DataCatalog <- get_datacatalog(locIds = locid, dataProcessTypeIds = 2, addDefault = "false")
    DataCatalog <- DataCatalog[DataCatalog$isSubnational==FALSE,]

    if(nrow(DataCatalog) > 0) {
      # Keep only those population series for which isSubnational is FALSE
      dd_extract <- dd_extract %>%
        dplyr::filter(DataProcessID == 36 |(DataProcessID == 2 & DataCatalogID %in% DataCatalog$DataCatalogID))
    }

    # }

    ## 3. Get additional DataSource keys (temporary fix until Dennis adds to DDSQLtools extract)

    ## Added by Shel because locid == 832 exists in get_locations() but not in get_datasources().
    ## To confirm with Sara
    possible_ids <- get_datasources()$LocID
    if (!("DataSourceTypeName" %in% names(dd_extract)) & locid %in% possible_ids) {

      # get additional DataSource keys (temporary fix until Dennis adds to DDSQLtools extract)
      DataSources <- get_datasources(locIds = locid, dataProcessTypeIds = dpi, addDefault = "false") %>%
        dplyr::select(LocID, PK_DataSourceID, DataSourceTypeName, DataSourceStatusName) %>%
        dplyr::rename(DataSourceID = PK_DataSourceID)

      dd_extract <- dd_extract %>%
        left_join(DataSources, by = c("LocID", "DataSourceID"), )
    }

    ## 4. Discard DataTypeName==“Direct (standard abridged age groups computed)” or
    # “Direct (standard abridged age groups computed - Unknown redistributed)” and generate an id
    dd_extract <- dd_extract %>%
      dplyr::filter(DataTypeName!= 'Direct (standard abridged age groups computed)',
                    DataTypeName!= 'Direct (standard abridged age groups computed - Unknown redistributed)') %>%
      mutate(id = paste(LocID, LocName, DataProcess, "Deaths", TimeLabel, DataProcessType, DataSourceName, StatisticalConceptName, DataTypeName, DataReliabilityName, sep = " - ")) %>%
      arrange(id)

    ## 5.***** Shel added this to separate indicator 159 with 170 because indicator 159 data is being dropped in the
    ##DDharmonize_Vitals1() and DDharmonize_Vitals5() functions *****

    dd_extract_188 <- dd_extract %>% filter(IndicatorID == 188)

    ## | AgeLabel == "Total" was added because of the instances where indicator 194 or 195 has no total age record,
    # so we borrow it from indicator 188
    dd_extract_194_195 <- dd_extract %>% filter(IndicatorID == 194 |IndicatorID == 195| AgeLabel == "Total")
    dd_extract_194_195 <- dd_extract_194_195 %>%
      group_by(id, IndicatorID, SexName) %>%
      mutate(n_id = n()) %>%
      ungroup() %>%
      group_by(id, SexName) %>%
      mutate(max_length_id = max(n_id, na.rm = TRUE)) %>%
      mutate(majority_id = ifelse(n_id == max_length_id, IndicatorID[n_id == max_length_id], 0),
             majority_id = unique(majority_id[majority_id!=0]),
             majority_idname = ifelse(majority_id == 194, "Deaths by age and sex - abridged",
                                      ifelse(majority_id == 195, "Deaths by age and sex - complete",
                                             ifelse(majority_id == 188, "Total deaths by sex","")))) %>%
      mutate(IndicatorID = ifelse(AgeLabel == "Total" & IndicatorID == 188 &
                                    !any(IndicatorID %in% c(194, 195) & AgeLabel == "Total"), majority_id,IndicatorID),
             IndicatorName = ifelse(IndicatorID == majority_id, majority_idname, IndicatorName)) %>%
      filter(IndicatorID != 188) %>%
      ungroup() %>%
      select(-n_id ,-max_length_id, -majority_id, -majority_idname)

    if(nrow(dd_extract_194_195) > 0){

      # list of series uniquely identified
      ids <- unique(dd_extract_194_195$id)

      vitals_std_all <- list()

      ## 6. for each id:

      for (i in 1:length(ids)) {

        # print(ids[i])

        vitals_raw <- dd_extract_194_195 %>%
          dplyr::filter(id == ids[i])

        # 7. isolate records that refer to abridged data
        vitals5_raw <- vitals_raw %>%
          dplyr::filter(AgeSpan %in% c(-2, -1) | AgeSpan >=5 | AgeLabel %in% c("< 1","1-4")) %>%
          mutate(AgeLabel = replace(AgeLabel, AgeLabel == "< 1", "0"))

        #   harmonize the vital5 data into standard age groups
        if (nrow(vitals5_raw[vitals5_raw$AgeSpan == 5,]) > 0) {
          print(i)
          print(ids[i])
          print("harmonizing vital counts by 5-year age group")
          vitals5_std <- DDharmonize_Vitals5(indata = vitals5_raw, type = "deaths")

        } else { vitals5_std <- NULL }

        # 8. isolate records that refer to single year age data
        vitals1_raw <- vitals_raw %>%
          dplyr::filter(AgeSpan %in% c(-2, -1, 1)) %>%
          mutate(AgeLabel = replace(AgeLabel, AgeLabel == "< 1", "0"))

        #  harmonize the pop1 data into standard age groups
        if (nrow(vitals1_raw[vitals1_raw$AgeSpan == 1,]) > 0) {

          print("harmonizing vital counts by 1-year age group")
          vitals1_std <- DDharmonize_Vitals1(indata = vitals1_raw)

        } else { vitals1_std <- NULL }

        # continue standardizing if there are any age-specific records

        # if (!is.null(vitals1_std) | !is.null(vitals5_std)) { // Removed

        ## 9. generate two datasets (vitals_abr, vitals_cpl) which are copies of (vitals5_std, vitals1_std) and set this to NULL


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

        # 10. reconcile abridged and complete series, as necessary

        if (!is.null(vitals_abr) & !is.null(vitals_cpl)) {
          vitals_abr_cpl <- DDharmonize_AbridgedAndComplete(data_abr = vitals_abr,
                                                            data_cpl_from_abr = NULL,
                                                            data_cpl = vitals_cpl) %>%
            dplyr::filter(series %in% c("abridged reconciled with complete", "complete reconciled with abridged"))

        } else { vitals_abr_cpl <- NULL }


        ## Added by Shel
        ## Case: 694 - Sierra Leone - VR - Deaths - 2010 - Register - Demographic Yearbook - Year of registration - Direct - Low" wide age groups are being dropped.
        ## If at this point abridged doesn't exist, and complete only 0, Total and Unknown and age labels, let's just
        ## drop it and append all the case id records back to the data using the `skipped` object at the bottom of this script
        ## Without this edit, the complete series will have additional start values and at the end, we will have dropped the wide
        ## age groups labels.
        ## Edit... might remove this since these records are likely to br dropped in part 2 where we are checking whether the series is full
        if (is.null(vitals_abr) & !is.null(vitals_cpl) & all(unique(vitals_cpl$AgeLabel) %in% c("0","Total","Unknown"))) {
          vitals_cpl <- NULL
        }

        # 11. If we only have complete series and not abridged, ...
        if (is.null(vitals_abr) & !is.null(vitals_cpl)) {
          vitals5_std <- NULL
          vitals_abr_cpl <- NULL

          ## Generate abridged data from the complete series
          for (sex in unique(vitals_cpl$SexID)) {

            vitals_abr_cpl_sex <- dd_single2abridged(data = vitals_cpl %>% dplyr::filter(SexID == sex)) %>%
              mutate(SexID = sex) # this returns a dataset containing 5-year age groups generated
            #from single years of age for each sex

            vitals_abr_cpl <- rbind(vitals_abr_cpl, vitals_abr_cpl_sex)
            rm(vitals_abr_cpl_sex)
          }

          ## Fill in the younger ages with 0s if the data values are NA
          vitals_abr_cpl <- dd_fillzeros_births(vitals_abr_cpl %>% select(-AgeSort), abridged = TRUE)

          vitals_abr_cpl <- vitals_abr_cpl %>%
            mutate(abridged = TRUE,
                   complete = FALSE,
                   series = "abridged reconciled with complete")
        }

        # } else { # if there are no age-specific records
        #   vitals_cpl <- NULL
        #   vitals_abr_cpl <- NULL
        # }

        # 12. Assemble all of the series into a single dataset
        vitals_all <- vitals5_std %>%
          bind_rows(vitals_cpl) %>%
          bind_rows(vitals_abr_cpl)

        if (nrow(vitals_all) > 0) {
          vitals_all <- suppressWarnings(vitals_all %>%
            mutate(id                     = ids[i],
                   id_series = paste(id, series, sep = " - "),
                   IndicatorName          = vitals_raw$IndicatorName[1],
                   IndicatorID            = vitals_raw$IndicatorID[1],
                   LocName                = vitals_raw$LocName[1],
                   LocID                  = vitals_raw$LocID[1],
                   LocTypeName            = vitals_raw$LocTypeName[1],
                   LocAreaTypeName        = vitals_raw$LocAreaTypeName[1],
                   SubGroupName           = vitals_raw$SubGroupName[1],
                   SubGroupTypeName       = vitals_raw$SubGroupTypeName[1],
                   DataCatalogID          = vitals_raw$DataCatalogID[1],
                   DataCatalogName        = vitals_raw$DataCatalogName[1],
                   DataProcess            = vitals_raw$DataProcess[1],
                   DataProcessSort        = vitals_raw$DataProcessSort[1],
                   DataProcessType        = vitals_raw$DataProcessType[1],
                   DataProcessTypeSort    = vitals_raw$DataProcessTypeSort[1],
                   ReferencePeriod        = vitals_raw$ReferencePeriod[1],
                   TimeUnit               = vitals_raw$TimeUnit[1],
                   TimeStart              = vitals_raw$TimeStart[1],
                   TimeEnd                = vitals_raw$TimeEnd[1],
                   TimeMid                = vitals_raw$TimeMid[1],
                   TimeLabel              = vitals_raw$TimeLabel[1],
                   DataSourceID           = vitals_raw$DataSourceID[1],
                   DataSourceName         = vitals_raw$DataSourceName[1],
                   DataSourceAuthor       = vitals_raw$DataSourceAuthor[1],
                   DataSourceShortName    = vitals_raw$DataSourceShortName[1],
                   DataSourceYear         = max(vitals_raw$DataSourceYear),
                   DataSourceTypeName     = vitals_raw$DataSourceTypeName[1],
                   DataSourceStatusName   = vitals_raw$DataSourceStatusName[1],
                   DataStatusName         = vitals_raw$DataStatusName[1],
                   DataStatusSort         = vitals_raw$DataStatusSort[1],
                   StatisticalConceptName = vitals_raw$StatisticalConceptName[1],
                   StatisticalConceptSort = vitals_raw$StatisticalConceptSort[1],
                   DataTypeName           = vitals_raw$DataTypeName[1],
                   DataReliabilityName    = vitals_raw$DataReliabilityName[1],
                   DataReliabilitySort    = vitals_raw$DataReliabilitySort[1],
                   ModelPatternName       = vitals_raw$ModelPatternName[1],
                   PeriodTypeName         = vitals_raw$PeriodTypeName[1],
                   PeriodGroupName        = vitals_raw$PeriodGroupName[1],
                   SeriesIDs              = I(list(unique(vitals_raw$SeriesID)))))
        }
        vitals_std_all[[i]] <- vitals_all

        rm(vitals_abr, vitals_cpl, vitals_abr_cpl, vitals5_std, vitals1_std)

      } # end of id loop
      vitals_std_all <- do.call(rbind, vitals_std_all)


      ## -------------------------------------------------------------------------------------------------------------------
      ## PART 2: FILTER AVAILABLE SERIES, KEEPING ONLY THOSE THAT CONTAIN A FULL AGE DISTRIBUTION
      ## AND THE POST-RECONCILIATION ABRIDGED AND COMPLETE SERIES, WHERE APPLICABLE
      ## -------------------------------------------------------------------------------------------------------------------

      if (nrow(vitals_std_all) > 0) {

        ## list the unique id series that exist in the data
        id_sers <- unique(vitals_std_all$id_series)

        ## create a placeholder for the object that will hold the final output (in this case all series that are full)
        id_series_full <- NULL

        ## for each id series
        for (i in 1:length(id_sers)) {

          ## subset the data to only have data for a particular id series
          pop_one_series <- vitals_std_all %>%
            dplyr::filter(id_series == id_sers[i])

          ## check if the series is abridged or abridged reconciled with complete
          abridged <- substr(pop_one_series$series[1],1,1) == "a"

          ## check if each of the gender datasets are full series or not
          ## dd_series_isfull() will produce the following warning in cases where one of the SexIDs does not exist. Refer to https://stackoverflow.com/questions/24282550/no-non-missing-arguments-warning-when-using-min-or-max-in-reshape2
          ## In max(AgeStart) : no non-missing arguments to max; returning -Inf
          ## So we wrap the function in suppressWarnings()
          check_full_m <- suppressWarnings(dd_series_isfull(pop_one_series %>%
                                                              dplyr::filter(SexID == 1),
                                                            abridged = abridged))
          check_full_f <- suppressWarnings(dd_series_isfull(pop_one_series %>%
                                                              dplyr::filter(SexID == 2),
                                                            abridged = abridged))
          check_full_b <- suppressWarnings(dd_series_isfull(pop_one_series %>%
                                                              dplyr::filter(SexID == 3),
                                                            abridged = abridged))

          check_full <- c(check_full_m, check_full_f, check_full_b)

          ## Check how many series are full out of the three
          n_full <- length(check_full[check_full == TRUE])

          # If at least two are full, then identify the series as full
          # If we have both and males, we can compute females.
          # If we have males and females, we can compute both sexes.
          # But if we only have both, these data are not very useful to us.
          if (n_full >=2 ) {
            id_series_full <- c(id_series_full, id_sers[i])
          }
        }

        ## subset the data to only be left with data where the series is full
        vitals_std_full <- vitals_std_all %>%
          dplyr::filter(id_series %in% id_series_full) %>%
          mutate(id_sex = paste(id, SexID, sep = " - "))

      } else { vitals_std_full <- vitals_std_all }

      ## for each id-sex combo of full series,
      ## keep the reconciled series if it is available and discard the original abridged or complete

      if (nrow(vitals_std_full) > 0) {

        ## identify unique id and sex combination
        ids_sex <- unique(vitals_std_full$id_sex)

        ## create a placeholder for the object that will hold the final output (in this case the reconciled series, if it exist)
        vitals_privilege_recon <- NULL

        ## for each id and sex combination
        for (i in 1:length(ids_sex)) {

          ## subset the data to get data where the series is either abridged or `abridged reconciled with complete`
          abr <- vitals_std_full %>%
            dplyr::filter(id_sex == ids_sex[i] & substr(series,1,1) == "a")

          ## if there are some abridged records, and `abridged reconciled with complete` exists, then this is the series we will keep
          ## and discard the original one.

          if (nrow(abr) > 0) {
            if ("abridged reconciled with complete" %in% abr$series) {
              abr <- abr %>%
                dplyr::filter(series == "abridged reconciled with complete")
            }
          }

          ## subset the data to get data where the series is either complete or `complete reconciled with abridged`
          cpl <- vitals_std_full %>%
            dplyr::filter(id_sex == ids_sex[i] & substr(series,1,1) == "c")
          ## if there are some complete records, and `complete reconciled with abridged` exists, then this is the series we will keep
          ## and discard the original one.

          if (nrow(cpl) > 0) {
            if ("complete reconciled with abridged" %in% cpl$series) {
              cpl <- cpl %>%
                dplyr::filter(series == "complete reconciled with abridged")
            }
          }

          ## Append the reconciled series together to form one dataset
          vitals_privilege_recon <- vitals_privilege_recon %>%
            bind_rows(abr) %>%
            bind_rows(cpl)

        }
        ## drop id_sex and id_series variables
        vitals_std_full <- vitals_privilege_recon %>%
          select(-id_sex, -id_series)
      } else { vitals_std_full <- vitals_std_full }

      ## -------------------------------------------------------------------------------------------------------------------
      ## PART 3: VALIDATE THE REMAINING SERIES, CHECKING FOR BOTH SEX TOTALS THAT MATCH BY SEX,
      # CORRECT FOR ANY INSTANCES WHERE DATA FOR SEX-AGE GROUP COMBINATIONS ARE MISSING, AND
      # CHECK WEATHER SUM OVER AGE MATCHES THE REPORTED TOTALS
      ## -------------------------------------------------------------------------------------------------------------------

      if (nrow(vitals_std_full) > 0) {

        ## identify the unique ids in the data
        ids <- unique(vitals_std_full$id)

        ## create a place holder for the final output which is ...
        vitals_std_valid <- list()

        for (i in 1:length(ids)) {

          ## subset the data to only be left with data for one id
          dd_one_id <- vitals_std_full %>%
            dplyr::filter(id == ids[i] & SexID %in% c(1,2,3))

          ## validate totals over age (if the reported and actual totals exist, if computed is greater than reported,
          ## then replace reported with computed, if computed is less than reported, then add difference to "Unknown" age)
          ## and distribute unknowns
          # reconcile reported and computed totals over age
          # see note on "Total" record that indicates if difference was greater than 2.5% and thus irreconcilable
          dd_one_id <- dd_validate_totals_over_age(data = dd_one_id)

          # distribute unknowns by age
          dd_one_id <- dd_distribute_unknowns(data = dd_one_id)

          # ensure that both sexes values = males + females
          dd_one_id <- dd_validate_totals_over_sex(data = dd_one_id)

          # throw it out if the open age group is below 50
          oag_check <- max(dd_one_id$AgeStart)
          if (oag_check < 50) {
            dd_one_id <- NULL
          }
          rm(oag_check)

          vitals_std_valid[[i]] <- dd_one_id

        }

        if(length(vitals_std_valid) >0 ){
          vitals_std_valid <- do.call(rbind, vitals_std_valid) %>%
            mutate(five_year = abridged == TRUE & AgeSpan %in% c(-1,5),
                   abridged = abridged == TRUE & AgeLabel != "0-4")

        }else
        {
          vitals_std_valid <- data.frame() ## maybe change this to vitals_std_valid <- vitals_std_valid
        }
      } else { vitals_std_valid <- vitals_std_full }



      ## 29th Oct change
      # arrange the data, with priority columns on the left and data loader keys on the right
      first_columns <- c("id", "LocID", "LocName", "DataProcess", "TimeStart", "TimeMid", "TimeEnd", "SexID",
                         "AgeStart", "AgeEnd", "AgeLabel", "AgeSpan", "AgeSort", "DataValue", "note", "abridged", "five_year",
                         "complete", "non_standard")

      ## Made this change on 5th November because of
      ##  404, Kenya,deaths, 2010, 2011. Since the vitals_std_all == NULL is non existent, keep_columns was returning a NULL object
      if(nrow(vitals_std_all) == 0){
        keep_columns <- names(vitals_raw)

      }else{
        keep_columns <- names(vitals_std_all)
      }
      keep_columns <- keep_columns[!(keep_columns %in% c("series", "id_series", "DataReliabilitySort", first_columns))]


      ## When there is more that one id for a given census year, select the most authoritative one

      if (nrow(vitals_std_valid) > 0) {

        #  When there is more than one id for a given census year, select the most authoritative

        if (return_unique_ref_period == TRUE) {

          vitals_valid_id <- vitals_std_valid %>% dd_rank_id_vitals

        } else { vitals_valid_id <- vitals_std_valid }

        out_all <- vitals_valid_id %>%
          mutate(non_standard = FALSE,
                 DataTypeName = "Direct (age standardized)") %>%
          select(all_of(first_columns), all_of(keep_columns))

      } else { out_all <- NULL }

      ## -------------------------------------------------------------------------------------------------------------------
      ## PART 4: Finalize
      ## -------------------------------------------------------------------------------------------------------------------


      first_columns <- c("id", "LocID", "LocName", "DataProcess", "DataProcessType", "TimeStart","TimeLabel", "TimeMid", "TimeEnd","SexID",
                         "AgeStart", "AgeEnd", "AgeLabel", "AgeSpan", "AgeSort", "DataValue")

      # ref_pds <- unique(out_all$TimeLabel)

      skipped <- dd_extract_194_195 %>%
        dplyr::filter(!(TimeLabel %in% out_all$TimeLabel)) %>%
        group_by(id) %>%
        mutate(SeriesIDs = I(list(unique(SeriesID)))) %>%
        ungroup() %>%
        select(IndicatorID, IndicatorName, all_of(first_columns), all_of(keep_columns)) %>%
        mutate(five_year = FALSE,
               abridged = FALSE,
               complete = FALSE,
               non_standard = TRUE,
               note = "Not harmonized or validated due to non-standard age groups") %>%
        arrange(id, SexID, AgeSort) %>%
        distinct()

      out_all <- bind_rows(out_all, skipped) ## 29th Oct change

      ## 29th Oct change
      ## Modified this part because of:
      ## Case: 583 - Micronesia (Fed. States of) - VR - Deaths - 1994 - Register - Demographic Yearbook - Year of occurrence - Direct - Low
      ## We have cases where none of the series ids is full so out_all is NULL, but when merged with skipped, we regain the
      ## original data. But, we do not want to lose indicator id and indicator name
      if(any(is.na(out_all$note))){## 29th Oct change

      out_all2 <- out_all %>%
        arrange(id, SexID, abridged, AgeSort) %>%
        select(-IndicatorID, -IndicatorName) %>%
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

      out_all2 <- out_all2 %>%
        mutate(IndicatorID = ifelse(IndicatorName == "Deaths by age and sex - abridged", 194,
                                    ifelse(IndicatorName == "Deaths by age and sex - complete", 195,  NA))) %>%
        select(IndicatorID, IndicatorName, everything())



      ## We have cases where we have complete cases that only contain 0, 1,2,3,4,5+. I (Shel) propose we drop this in cases where abridged cases exist
      # throw it out if the open age group is below 50

      out_all2 <- out_all2 %>%
        group_by(id, SexID, complete) %>%
        mutate(max_agestart = max(AgeStart, na.rm = TRUE)) %>%
        filter(max_agestart > 50) %>%
        ungroup() %>%
        select(-max_agestart)
      }else{
        out_all2 <- out_all ## 29th Oct change
      }

      ## This is causing an error bc it requires DataReliabilitySort, but that field is no longer on the data
      ##  Combine the harmonized data with indicator 188 data and clean it

      # if(nrow(dd_extract_188) >0){
      #   out_all_appended <- dd_append_tcs_cas(indata = out_all2, ## 29th Oct change
      #                                         type = "deaths",
      #                                         tcs_data = dd_extract_188,
      #                                         ind = 188)
      # }else
      # {
      #   out_all_appended <- out_all2 ## 29th Oct change
      # }
      out_all_appended <- out_all2

      ## Retain variables of interest

      if(nrow(out_all_appended) >0){

        if (retainKeys == FALSE) {
          out_all_appended <- out_all_appended %>%
            select(id, LocID, LocName, IndicatorID, IndicatorName, TimeLabel, TimeMid, TimeEnd, DataProcessType, DataSourceName, StatisticalConceptName,
                   DataTypeName, DataReliabilityName, five_year, abridged, complete, non_standard, SexID, AgeStart, AgeEnd,
                   AgeLabel, AgeSpan, AgeSort, DataValue, note)
        }

      }else{
        print(paste0("No full data series exists for LocID = ",locid," for the time period ", times[1], " to ", times[length(times)]))
        out_all_appended <- NULL
      }

      ## Print a text message showing the locid and the locname of the data extracted
      cat("\n","Location ID: ", unique(dd_extract$LocID),"\n",
          "Location Name: ", unique(dd_extract$LocName),"\n")

    }else{## 	Deaths by age and sex not available
      print(paste0("Deaths by age and sex do not exist for LocID = ",locid," for the time period ", times[1], " to ", times[length(times)]))
      out_all_appended <- NULL
    }

  } else{## if no death counts were extracted from DemoData
    if(locid %in% get_locations()$PK_LocID){
      print(paste0("There are no death counts available for LocID = ",locid," for the time period ", times[1], " to ", times[length(times)]))
      out_all_appended <- NULL
    }
    out_all_appended <- NULL
  }


  ## To be removed later
  ## missing_timelabs should be NULL
  missing_timelabs<- unique(dd_extract$TimeLabel[which(!dd_extract$TimeLabel %in% out_all_appended$TimeLabel)])
  assign("missing_timelabs", missing_timelabs, .GlobalEnv)

  if(length(missing_timelabs) >0){
    missing_data <- dd_extract %>%
      filter(TimeLabel %in% missing_timelabs) %>%
      select(any_of(names(out_all_appended)))

    assign("missing_data", missing_data, .GlobalEnv)
  }else{
    missing_data <- NULL
  }

return(out_all_appended)

}





