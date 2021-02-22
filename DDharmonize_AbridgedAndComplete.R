# We need a single wrapper function that takes location id and time references as input paramaters
# and processes/harmonizes all census population counts for that country through the procedures
# developed by Marilia

# indata data.frame. One abridged series extracted from DemoData 
# Refers to one LocID, Year, DataSource)

# requires c(IndicatorShortName, DataSourceYear, DataStatusName, SexID, 
#          AgeStart, AgeEnd, AgeLabel, AgeSpan, DataValue)

##########
##########
# 
# library(tidyverse)
# library(devtools)
# library(DDSQLtools)
# library(DemoTools)
# source("R/DDextract_CensusPopCounts.R")
# source("R/DDharmonize_Pop1.R")
# source("R/DDharmonize_Pop5.R")
# 
# options(dplyr.summarise.inform=F)
# 
# # source helper functions
# lf <- list.files("R/")
# lf <- lf[substr(lf,1,3) == "dd_"]
# for (i in 1:length(lf)) {
#   source(paste0("R/",lf[i]))
# }
# 
# ## development server for UNPD testing (paperspace) DemoData api
# options(unpd_server = "http://74.82.31.177/DemoData/api/")
# 
# 
# # burundi 1990
# pop_counts <- DDextract_CensusPopCounts(locid = 840,
#                                         start_year = 1950,
#                                         end_year = 2020)
# 
# # pop_counts <- DDextract_CensusPopCounts(locid = 492,
# #                                         start_year = 1990,
# #                                         end_year = 1990)
# id_list <- unique(pop_counts$id)
# id_list
# df_one_series <- pop_counts %>% 
#   dplyr::filter(id == id_list[13]) %>% 
#   select(IndicatorShortName, DataSourceYear, DataStatusName, SexID, 
#          AgeStart, AgeEnd, AgeLabel, AgeSpan, DataValue, SeriesID, DataSourceID, id)
# 
# pop5 <- DDharmonize_Pop5(indata = df_one_series)
# pop1 <- DDharmonize_Pop1(indata = df_one_series)
# 
# if (!is.null(pop5)) {
# data_abr <- pop5 %>% dplyr::filter(series == "abridged")
# data_cpl_from_abr <- pop5 %>% dplyr::filter(series == "complete from abridged") } else {
#   data_abr <- NULL
#   data_cpl_from_abr <- NULL
# }
# if (!is.null(pop1)) {
# data_cpl <- pop1 %>% dplyr::filter(series == "complete")  } else {
#   data_cpl <- NA
# }
# 
# all.out <- list()
# 
# test <- DDharmonize_AbridgedAndComplete(data_abr = data_abr,
#                                         data_cpl_from_abr = data_cpl_from_abr,
#                                         data_cpl = data_cpl)

# This wrapper takes output of harmonizations and reconciled abridged and complete series
DDharmonize_AbridgedAndComplete <- function (data_abr,
                                             data_cpl_from_abr,
                                             data_cpl) {

  abr_sex <- NULL
  cpl_sex <- NULL
  
  for (sex in unique(c(data_abr$SexID, data_cpl$SexID))) {
  
  # create some flags for data availability
  has_abr          <- nrow(data_abr[!is.na(data_abr$DataValue),]) >0
    has_abr <- ifelse(is_empty(has_abr), FALSE, has_abr)
  has_cpl_from_abr <- nrow(data_cpl_from_abr[!is.na(data_cpl_from_abr$DataValue),]) >0
    has_cpl_from_abr <- ifelse(is_empty(has_cpl_from_abr), FALSE, has_cpl_from_abr)
  has_cpl          <- nrow(data_cpl[!is.na(data_cpl$DataValue),]) >0
    has_cpl <- ifelse(is_empty(has_cpl), FALSE, has_cpl)
  
  if (has_abr) {  
    
    df_abr <- data_abr %>% dplyr::filter(SexID == sex) %>% 
      select(-note, -SexID)
    total_abr <- df_abr$DataValue[df_abr$AgeLabel == "Total"]
    
  } else { df_abr <- NULL }
  
  if (has_cpl_from_abr) {  
    df_cpl_from_abr <- data_cpl_from_abr %>% dplyr::filter(SexID == sex) %>% 
      select(-note, -SexID)
  } else { df_cpl_from_abr <- NULL }
  
  if (has_cpl) {  
    df_cpl <- data_cpl %>% dplyr::filter(SexID == sex ) %>% 
      select(-note, -SexID)
    total_cpl <- df_cpl$DataValue[df_cpl$AgeLabel == "Total"]
    df_abr_from_cpl <- df_cpl %>% dd_single2abridged
    
  } else { # if no data_cpl
    if (has_cpl_from_abr) {
      # use data_cpl_from_abr
      if (all(data_cpl_from_abr$AgeSpan < 0)) { data_cpl_from_abr <- NULL }
      df_cpl <- df_cpl_from_abr
      if (!is.null(df_cpl)) {
        total_cpl <- df_cpl$DataValue[df_cpl$AgeLabel == "Total"]
        df_abr_from_cpl <- df_cpl %>% dd_single2abridged
        has_cpl <- TRUE
      }
      
    } else {
      df_cpl <- NULL 
      df_abr_from_cpl <- NULL 
      }
  }
  
  
  if (has_abr & has_cpl) {
    
    total_diff <- total_abr - total_cpl
    total_match <- total_diff == 0
    if (is_empty(total_match)) { total_match = TRUE }
    
  } else { total_match <- FALSE }
    
  if (exists('total_abr') & exists('total_cpl')) {
    total_match <- ifelse(is_empty(total_abr) & is_empty(total_cpl), FALSE, total_match)
  }
  
 
  # if the totals agree or if total for one series is missing a total then fill in
  if (total_match) {
    
    ###########################
    # reconcile abridged series with records from complete
    ###########################
    
    df_abr <- df_abr %>%
      bind_rows(df_abr_from_cpl %>% dplyr::filter(!(AgeLabel %in% df_abr$AgeLabel)))

    # drop records for open age groups that do not close the series
    oag_start_abr <- dd_oag_agestart(df_abr, multiple5 = TRUE)
    
    if (!is_empty(oag_start_abr)) {
    df_abr <- df_abr %>% 
      dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start_abr))
    }
    
    # compute all possible open age groups
    oag_abr <- dd_oag_compute(df_abr, age_span = 5)
    if(!is.null(oag_abr)) {
    df_abr <- df_abr %>% 
      bind_rows(oag_abr %>% dplyr::filter(!(AgeLabel %in% df_abr$AgeLabel)))
    }
    
    # drop records for open age groups that do not close the series
    oag_start_abr <- dd_oag_agestart(df_abr, multiple5 = TRUE)
    if (!is_empty(oag_start_abr)) {
    df_abr <- df_abr %>% 
      dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start_abr)) %>% 
      mutate(series = "abridged reconciled with complete") %>% 
      arrange(AgeSort)
    }
    
    isfull_abr <- dd_series_isfull(df_abr, abridged = TRUE)
    
    df_abr$note <- ifelse(isfull_abr, NA, "The abridged series is missing data for one or more age groups.")
    df_abr$SexID <- sex 
    
    ###########################
    # reconcile complete series with records from abridged
    ###########################
    
    if (!is.null(df_cpl_from_abr)) {
    df_cpl <- df_cpl %>% 
      bind_rows(df_cpl_from_abr %>% dplyr::filter(!(AgeLabel %in% df_cpl$AgeLabel)))
    }
    
    # only process if there are multiple closed age groups in the series
    
    if (nrow(df_cpl[df_cpl$AgeSpan == 1,]) > 1) {
      
    # drop records for open age groups that do not close the series
    oag_start_cpl <- dd_oag_agestart(df_cpl, multiple5 = FALSE)
    df_cpl <- df_cpl %>% 
      dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start_cpl))
    
    # compute all possible open age groups
      oag_cpl <- dd_oag_compute(df_cpl, age_span = 1)
      if (!is.null(oag_cpl)) {
      df_cpl <- df_cpl %>% 
        bind_rows(oag_cpl %>% dplyr::filter(!(AgeLabel %in% df_cpl$AgeLabel)))
      }
    
    # identify the open age group that is a multiple of five
    oag_start_cpl <- dd_oag_agestart(df_cpl, multiple5 = TRUE)
    
    df_cpl <- df_cpl %>% 
      dplyr::filter(!(AgeStart > 0 & AgeSpan == -1 & AgeStart != oag_start_cpl)) %>% 
      dplyr::filter(!(AgeSpan ==1 & AgeStart >= oag_start_cpl)) %>% 
      mutate(series = "complete reconciled with abridged") %>% 
      arrange(AgeSort)
    
    isfull_cpl <- dd_series_isfull(df_cpl, abridged = FALSE)
    
    df_cpl$note <- ifelse(isfull_cpl, NA, "The complete series is missing data for one or more age groups.")
    df_cpl$SexID = sex
    } else { df_cpl <- NULL }
    
    # if the only remaining record on complete is "Unknown" or "Total" then discard the whole series
    
    if (all(unique(df_cpl$AgeLabel) %in% c("Unknown","Total"))) {
      df_cpl <- NULL
    }
    
  } else {
    if (!is.null(df_abr)) {
      df_abr$note <- "Different totals on abridged and complete preclude reconciliation"
      df_abr$SexID <- sex
    }
    if (!is.null(df_cpl)) {
      df_cpl$note <- "Different totals on abridged and complete preclude reconciliation"
      df_cpl$SexID <- sex
    }
    
  }
  
  abr_sex <- rbind(abr_sex, df_abr) 
  cpl_sex <- rbind(cpl_sex, df_cpl) 
  
  if (!is.null(abr_sex)) {
    abr_sex <- abr_sex %>% 
      mutate(abridged = TRUE,
             complete = FALSE)
  }
  if (!is.null(cpl_sex)) {
    cpl_sex <- cpl_sex %>% 
      mutate(abridged = FALSE,
             complete = TRUE)
  }
} # close loop for sex


  outdata <- rbind(abr_sex , 
                   cpl_sex ) 

  return(outdata)
  
}
