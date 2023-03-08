#' @title
#' DDharmonize_validate_PopCounts
#'
#' @description
#' This script implements a work flow for census population counts extracting from DemoData, harmonizing age groups,
#' identifying full series, selecting preferred series, validating totals and by sex and eventually
#' including key fields in the function output.
#'
#' @details
#' See the \href{https://shelmith-kariuki.github.io/rddharmony/articles/DDharmonize_validate_PopCounts.html}{DDharmonize_validate_PopCounts} vignette for more details about this function.
#'
#' @param locid Location id
#' @param dd_extract A data frame extracted from DemoData using the DDextract_PopCounts() function. If NULL then the function will query DemoData for a new extract.
#' @param times The period of the data to be extracted. You can extract one year data e.g times = 2020 or a longer period of time e.g times = c(1950, 2020).
#' @param process The process through which the data was obtained from various sources i.e either via census , register or estimates. By default, the function pulls data obtained through both of these processes.
#' @param locType The location type for which data are extracted i.e. Whole, Urban, Rural
#' @param return_unique_ref_period TRUE / FALSE. Specifies whether the data to be returned should contain one unique id (return_unique_ref_period == TRUE) or several ids (return_unique_ref_period == FALSE) per time label.
#' ids are a unique identifier for each unique set of records based on `LocID`, `LocName`, `DataProcess`, `ReferencePeriod`, `DataSourceName`, `StatisticalConceptName`, `DataTypeName` and `DataReliabilityName`.
#' The definitions of these variables are provided later in this article.
#' @param DataSourceShortName NULL
#' @param DataSourceYear NULL
#' @param retainKeys  TRUE / FALSE. Specifies whether only a few (retainKeys == FALSE) or all (retainKeys == TRUE) variables should be retained in the output.
#' @param server "https://popdiv.dfs.un.org/DemoData/api/"
#'
#' @import dplyr
#' @import assertthat
#' @import DDSQLtools
#' @import DemoTools
#' @importFrom magrittr %>%
#' @importFrom purrr is_empty
#'
#' @return  A harmonized dataset containing population counts
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sweden_df <- DDharmonize_validate_PopCounts(locid = 752,
#'                                               times = c(1950,2020),
#'                                               process = c("census", "estimate", "register"),
#'                                               return_unique_ref_period = TRUE,
#'                                               DataSourceShortName = NULL,
#'                                               DataSourceYear = NULL,
#'                                               retainKeys = FALSE,
#'                                               server = "https://popdiv.dfs.un.org/DemoData/api/")
#'}

DDharmonize_validate_PopCounts <- function(locid,
                                           dd_extract = NULL, 
                                           times,
                                           process = c("census", "estimate", "register"),
                                           locType = "whole",
                                           return_unique_ref_period = TRUE, # if true, then only most authoritative series will be returned for each reference period, per dd_rank_id()
                                           DataSourceShortName = NULL,
                                           DataSourceYear = NULL,
                                           retainKeys = FALSE,
                                           server = "https://popdiv.dfs.un.org/DemoData/api/") {
options(dplyr.summarise.inform=F)

  ## -------------------------------------------------------------------------------------------------------------------
  ## PART 1: EXTRACT POPULATION COUNTS FROM DEMO DATA AND HARMONIZE TO STANDARD ABRIDGED AND COMPLETE AGE GROUPS, BY SERIES
  ## -------------------------------------------------------------------------------------------------------------------

  ## UNPD server housing DemoData
  options(unpd_server = server)
  
  if (is.null(dd_extract)) {
    ## 1. Extract population counts for a given country and process over the period specified in times
    dd_extract <- DDextract_PopCounts(locid   = locid,
                                      process = process,
                                      locType = locType,
                                      start_year = times[1],
                                      end_year   = times[length(times)],
                                      DataSourceShortName = DataSourceShortName,
                                      DataSourceYear = DataSourceYear)
  }

  if (!is.null(dd_extract)) {

  ## Shel added this so that it can be easy to compare the raw data with the clean and harmonized data.
  dd_extract <- dd_extract %>%
    relocate(DataValue, .after = "agesort")

  assign("raw_df", dd_extract, .GlobalEnv)

  ## 2. Drop sub-national censuses (data process "vr" does not work with get_datacatalog?)
    # get data process id
    dpi <- NA
    dpi <- ifelse(process == "census", 2, dpi)
    dpi <- ifelse(process == "estimate", 6, dpi)
    dpi <- ifelse(process == "register", 9, dpi)


    ## Shel: Edited this part after I kept getting the error: Error in open.connection(con, "rb") : HTTP error 500.
    # DataCatalog <- get_datacatalog(locIds = locid, dataProcessTypeIds = dpi, addDefault = "false")
    # DataCatalog <- DataCatalog[DataCatalog$isSubnational==FALSE,]

    DC <- NULL
    for (i in 1:length(dpi)){

    DataCatalog <- get_datacatalog(locIds = locid, dataProcessTypeIds = dpi[i], addDefault = "false")
    DC <- rbind(DC, DataCatalog)
    }
    DataCatalog <- DC
    DataCatalog <- DataCatalog[DataCatalog$isSubnational==FALSE,]
    rm(DC)

    if(nrow(DataCatalog) > 0) {
    # Keep only those population series for which isSubnational is FALSE
    dd_extract <- dd_extract %>%
      dplyr::filter(DataCatalogID %in% DataCatalog$DataCatalogID)
    }

    # if data process is estimate or register, then the ReferencePeriod field is empty
    # fill it in with TimeLabel
    if (any(process %in% c("estimate", "register"))) {## Shel added any to remove the warning
      dd_extract$ReferencePeriod <- dd_extract$TimeLabel
    }

    ## 3. Get additional DataSource keys (temporary fix until Dennis adds to DDSQLtools extract)

    if (!("DataSourceTypeName" %in% names(dd_extract))) {
      # get additional DataSource keys (temporary fix until Dennis adds to DDSQLtools extract)
      DataSources <- get_datasources(locIds = c(locid, 900), addDefault = "false") %>%
        dplyr::select(LocID, PK_DataSourceID, DataSourceTypeName, DataSourceStatusName) %>%
        dplyr::rename(DataSourceID = PK_DataSourceID) %>%
        mutate(LocID = locid) %>%
        distinct()

      dd_extract <- dd_extract %>%
        left_join(DataSources, by = c("LocID", "DataSourceID"), )
    }

    ## 4. Drop Discard DataTypeName==“Direct (standard abridged age groups computed) or “Direct (standard abridged age groups computed - Unknown redistributed)” or protocol adjusted
    dd_extract <- dd_extract %>%
      dplyr::filter(DataTypeID %in% c(16, 89, 183, 58, 141)) %>%
      mutate(id = paste(LocID, LocName, DataProcess, ReferencePeriod, DataSourceName, StatisticalConceptName, DataTypeName, DataReliabilityName, sep = " - ")) %>%
      arrange(id)

       # list of series uniquely identified by Reference Period - Data Source - Statistical Concept - Data Type
       ids <- unique(dd_extract$id)

       pop_std_all <- list()

       for (i in 1:length(ids)) {

         print(i)
        print(ids[i])

         ## 5. For each id
         pop_raw <- dd_extract %>%
         dplyr::filter(id == ids[i])

         # # This block of code has been maintained in order to test the lines of code that follow in this for loop.
         # pop_raw <- dd_extract %>%
         #              filter(id == "410 - Republic of Korea - Census - 1955 - Demographic Yearbook - De-facto - Population by age and sex - Fair") %>%
         #              filter(SexID == 1)

         ## 6. isolate records from the "Population5" indicator and harmonize the pop5 data into standard age groups
         pop5_raw <- pop_raw %>%
         dplyr::filter(IndicatorShortName == "Population5")

         if (nrow(pop5_raw) > 0) {
         print("harmonizing Population5")
         pop5_std <- DDharmonize_Pop5(indata = pop5_raw)
         } else { pop5_std <- NULL }


         ## 7. isolate records from the "Population1" indicator and harmonize the pop1 data into standard age groups
         pop1_raw <- pop_raw %>%
           dplyr::filter(IndicatorShortName == "Population1")

         if (nrow(pop1_raw) > 0) {
           print("harmonizing Population1")
           pop1_std <- DDharmonize_Pop1(indata = pop1_raw)
         } else { pop1_std <- NULL }

        ## 8. Often the series for SexID == 0 (other) has records only when the DataValue is non-zero
        ## To prevent errors later on, we need to fill in zeros
        ## This is a really clumsy way to do this -- will improve later on

         ### Population5
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

         ### Population1
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

         ## 9. Generate pop_abr, pop_cpl and pop_cpl_from_abr

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


         ## 10. reconcile abridged and complete series, as necessary

            if (!is.null(pop_abr) & (!is.null(pop_cpl_from_abr) | !is.null(pop_cpl))) {
              pop_abr_cpl <- DDharmonize_AbridgedAndComplete(data_abr = pop_abr,
                                                     data_cpl_from_abr = pop_cpl_from_abr,
                                                   data_cpl = pop_cpl) %>%
                  dplyr::filter(series %in% c("abridged reconciled with complete", "complete reconciled with abridged"))

            pop5_std <- pop5_std %>%
            dplyr::filter(series == "abridged")

            }

         ## 11. If we only have complete series and not abridged, ...

            if(is.null(pop_abr) & !is.null(pop_cpl)) {
                pop5_std <- NULL
                pop_abr_cpl <- NULL

                ## Generate abridged data from the complete series

              for (sex in unique(pop_cpl$SexID)) {
                pop_abr_cpl_sex <- dd_single2abridged(data = pop_cpl %>% dplyr::filter(SexID == sex)) %>%
                                      mutate(SexID = sex)
                # this returns a dataset containing 5-year age groups generated from single years of age for each sex
                pop_abr_cpl <- rbind(pop_abr_cpl, pop_abr_cpl_sex)
                rm(pop_abr_cpl_sex)
                }
               pop_abr_cpl <- pop_abr_cpl %>%
                                  mutate(abridged = TRUE,
                                  complete = FALSE,
                                  series = "abridged reconciled with complete")
          }


    # 12. Assemble all of the series into a single dataset
    pop_all <- suppressWarnings(pop5_std %>%
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
             DataCatalogID          = pop_raw$DataCatalogID[1],
             DataCatalogName        = pop_raw$DataCatalogName[1],
             DataProcess            = pop_raw$DataProcess[1],
             DataProcessSort        = pop_raw$DataProcessSort[1],
             DataProcessType        = pop_raw$DataProcessType[1],
             DataProcessTypeSort    = pop_raw$DataProcessTypeSort[1],
             ReferencePeriod        = pop_raw$ReferencePeriod[1],
             TimeUnit               = pop_raw$TimeUnit[1],
             TimeStart              = pop_raw$TimeStart[1],
             TimeEnd                = pop_raw$TimeEnd[1],
             TimeMid                = pop_raw$TimeMid[1],
             TimeLabel              = pop_raw$TimeLabel[1],
             DataSourceID           = pop_raw$DataSourceID[1],
             DataSourceName         = pop_raw$DataSourceName[1],
             DataSourceAuthor       = pop_raw$DataSourceAuthor[1],
             DataSourceShortName    = pop_raw$DataSourceShortName[1],
             DataSourceYear         = max(pop_raw$DataSourceYear),
             DataSourceTypeName     = pop_raw$DataSourceTypeName[1],
             DataSourceStatusName   = pop_raw$DataSourceStatusName[1],
             DataStatusName         = pop_raw$DataStatusName[1],
             StatisticalConceptName = pop_raw$StatisticalConceptName[1],
             DataTypeName           = pop_raw$DataTypeName[1],
             DataReliabilityName    = pop_raw$DataReliabilityName[1],
             DataReliabilitySort    = pop_raw$DataReliabilitySort[1],
             ModelPatternName       = pop_raw$ModelPatternName[1],
             PeriodTypeName         = pop_raw$PeriodTypeName[1],
             PeriodGroupName        = pop_raw$PeriodGroupName[1],
             SeriesIDs              = I(list(unique(pop_raw$SeriesID)))))

    pop_std_all[[i]] <- pop_all

    rm(pop_abr, pop_cpl_from_abr, pop_cpl, pop_abr_cpl, pop5_std, pop1_std)

  } # end of id loop
  pop_std_all <- do.call(rbind, pop_std_all)


  ## -------------------------------------------------------------------------------------------------------------------
  ## PART 2: FILTER AVAIALBE SERIES, KEEPING ONLY THOSE THAT CONTAIN A FULL AGE DISTRIBUTION
  ## AND THE POST-RECONCILIATION ABRIDGED AND COMPLETE SERIES, WHERE APPLICABLE
  ## -------------------------------------------------------------------------------------------------------------------

  if (nrow(pop_std_all) > 0) {

  ## list the unique id series that exist in the data
  id_sers <- unique(pop_std_all$id_series)

  ## create a placeholder for the object that will hold the final output (in this case all series that are full)
  id_series_full <- NULL

  ## for each id series
  for (i in 1:length(id_sers)) {

    ## subset the data to only have data for a particular id series
    pop_one_series <- pop_std_all %>%
      dplyr::filter(id_series == id_sers[i])

    ## check if the series is abridged or abridged reconciled with complete
    abridged <- substr(pop_one_series$series[1],1,1) == "a"

    ## if abridged or abridged reconciled with complete, the minimum number of rows should be 11, otherwise 51
    if (abridged) {
      minrows <- 11
    } else {
      minrows <- 51
    }

    ## xysrev
    nrows <- min(nrow(pop_one_series %>%
                        dplyr::filter(SexID == 1)),
                 nrow(pop_one_series %>%
                        dplyr::filter(SexID == 2)))

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

    max_oag <- max(pop_one_series$AgeStart)

    # if at least two are full, then identify the series as full
    if (n_full >=2  & max_oag >= 50 & nrows > minrows) {
      id_series_full <- c(id_series_full, id_sers[i])
    }
    rm(nrows, minrows, n_full, max_oag, check_full_m, check_full_f, check_full_b)
  }

  ## subset the data to only be left with data where the series is full
  pop_std_full <- pop_std_all %>%
    dplyr::filter(id_series %in% id_series_full) %>%
    mutate(id_sex = paste(id, SexID, sep = " - "))

  } else { pop_std_full <- pop_std_all }


## for each id-sex combo of full series,
## keep the reconciled series if it is available and discard the original abridged or complete

if (nrow(pop_std_full) > 0) {

  ## identify unique id and sex combination
  ids_sex <- unique(pop_std_full$id_sex)

  ## create a placeholder for the object that will hold the final output (in this case the reconciled series, if it exist)
  pop_privilege_recon <- NULL

  ## for each id and sex combination
  for (i in 1:length(ids_sex)) {

    ## subset the data to get data where the series is either abridged or `abridged reconciled with complete`
    abr <- pop_std_full %>%
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
    cpl <- pop_std_full %>%
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
    pop_privilege_recon <- pop_privilege_recon %>%
      bind_rows(abr) %>%
      bind_rows(cpl)

  }

  ## drop id_sex and id_series variables
  pop_std_full <- pop_privilege_recon %>%
    select(-id_sex, -id_series)
} else { pop_std_full <- pop_std_full }


  ## -------------------------------------------------------------------------------------------------------------------
  ## PART 3: VALIDATE THE REMAINING SERIES, CHECKING FOR BOTH SEX TOTALS THAT MATCH BY SEX,
  # CORRECT FOR ANY INSTANCES WHERE DATA FOR SEX-AGE GROUP COMBINATIONS ARE MISSING, AND
  # CHECK WEATHER SUM OVER AGE MATCHES THE REPORTED TOTALS
  ## -------------------------------------------------------------------------------------------------------------------

if (nrow(pop_std_full) > 0) {

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

  # throw it out if the open age group is below 50
  oag_check <- max(dd_one_id$AgeStart)
  if (oag_check < 50) {
    dd_one_id <- NULL
  }
  rm(oag_check)
  pop_std_valid[[i]] <- dd_one_id

  }

  if(length(pop_std_valid) >0 ){
    pop_std_valid <- do.call(rbind, pop_std_valid) %>%
      mutate(five_year = abridged == TRUE & AgeSpan %in% c(-1,5),
             abridged = abridged == TRUE & AgeLabel != "0-4")

  }else
  {
    pop_std_valid <- pop_std_valid
  }
} else {
    pop_std_valid <- pop_std_full
    }


  # define how we want to arrange the data, with priority columns on the left and data loader keys on the right
  first_columns <- c("id", "LocID", "LocName", "DataProcess", "TimeStart","TimeLabel", "TimeMid", "SexID",
                     "AgeStart", "AgeEnd", "AgeLabel", "AgeSpan", "AgeSort", "DataValue", "note", "abridged", "five_year",
                     "complete", "non_standard")
  ## Made this change on 5th November because of
  ##  404, Kenya,deaths, 2010, 2011. Since the vitals_std_all == NULL is non existent, keep_columns was returning a NULL object
  if(nrow(pop_std_all) == 0){
    keep_columns <- names(pop_raw)

  }else{
    keep_columns <- names(pop_std_all)
  }
  keep_columns <- keep_columns[!(keep_columns %in% c("series", "id_series", "DataSeriesID", first_columns))]

  ## When there is more that one id for a given census year, select the most authoritative one

   if (nrow(pop_std_valid) > 0) {

    if (return_unique_ref_period == TRUE) {

      pop_valid_id <- pop_std_valid %>% dd_rank_id

    } else { pop_valid_id <- pop_std_valid }

    out_all <- pop_valid_id %>%
      mutate(non_standard = FALSE,
             DataTypeName = "Direct (age standardized)") %>%
      select(all_of(first_columns), all_of(keep_columns))

  } else {
    out_all <- NULL
  }

  ## -------------------------------------------------------------------------------------------------------------------
  ## PART 4: Finalize
  ## -------------------------------------------------------------------------------------------------------------------

  ## Look for years that are in the raw data but not in the output. If there are series with non-standard age groups, then add these to the output as well.

  # ref_pds <- unique(out_all$ReferencePeriod)


  first_columns <- c("id", "LocID", "LocName", "DataProcess","TimeStart","TimeLabel", "TimeMid", "TimeEnd","SexID",
                     "AgeStart", "AgeEnd", "AgeLabel", "AgeSpan", "AgeSort", "DataValue")

  skipped <- dd_extract %>%
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

  if(any(is.na(out_all$note))){## 29th Oct change

    out_all2 <- out_all %>%
      arrange(id, SexID, abridged, AgeSort) %>%
      select(-IndicatorID, -IndicatorName) %>%
      mutate(IndicatorName = NA,
           IndicatorName = replace(IndicatorName, abridged == TRUE, "Population5"),
           IndicatorName = replace(IndicatorName, complete == TRUE, "Population1"),
           AgeUnit = "Year",
           SexName = NA,
           SexName = replace(SexName, SexID == 0, "Unknown"),
           SexName = replace(SexName, SexID == 1, "Male"),
           SexName = replace(SexName, SexID == 2, "Female"),
           SexName = replace(SexName, SexID == 3, "Both sexes"))

    #Population by age and sex - abridged(58), #Population by age and sex - complete(60)
    out_all2 <- out_all2 %>%
      mutate(IndicatorID = ifelse(IndicatorName == "Population5", 58,
                                  ifelse(IndicatorName == "Population1", 60,  NA))) %>%
      select(IndicatorID, IndicatorName, everything())


  }else{
    out_all2 <- out_all ## 29th Oct change
  }

  ## Retain variables of interest

  out_all_appended <- out_all2

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

  } else{## if no pop counts were extracted from DemoData
    if(locid %in% get_locations()$PK_LocID){
        print(paste0("There are no census age distributions available for LocID = ",locid," for the time period ", times[1], " to ", times[length(times)]))
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












