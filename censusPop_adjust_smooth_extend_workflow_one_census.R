# This function implements census population adjustment and smoothing in accordance with 
# Peter Johnson methods protocol
# still needs testing to ensure PJ protocols carried out exactly as specified
# it further extends age distributions to open age = 100+ using OPAG

library(DemoTools)
library(tidyverse)

#####

censusPop_adjust_smooth_extend_workflow_one_census <- function(dd_census_extract, 
                                                               pes_directory="dev/PES/",
                                                               nLxFemale = NULL,
                                                               nLxMale = NULL,
                                                               Age_nLx = NULL,
                                                               AgeInt_nLx = NULL,
                                                               nLxDatesIn = NULL,
                                                               AsfrMat = NULL,
                                                               AsfrDatesIn = NULL,
                                                               SRB = NULL,
                                                               SRBDatesIn = NULL,
                                                               OAnew = 101) {
  
  # 1. check that data meet some basic requirements
  nids <- length(unique(dd_census_extract$id))
  maxage <- max(dd_census_extract$AgeStart)
  stopifnot(nids == 1 & maxage >= 50)
  
  locid <- dd_census_extract$LocID[1]
  census_year <- as.numeric(dd_census_extract$ReferencePeriod[1])
  census_reference_date <- as.numeric(dd_census_extract$TimeMid[1])
  
  # load some preliminaries
  
  # load census enumeration model results
  load(paste0(pes_directory,"TotNetEnum.RData"))
  load(paste0(pes_directory,"DiffNCE1.RData"))
  load(paste0(pes_directory,"DiffNCEAbr.RData"))
  load(paste0(pes_directory,"Covariates.RData"))
  
  # parse years of education by sex to use as criterion for selecting levels of smoothing
  EduYrs_m <-  dplyr::filter(Covariates,LocID==locid, Year==census_year)$EducYrsM
  EduYrs_f <-  dplyr::filter(Covariates,LocID==locid, Year==census_year)$EducYrsF

  # 2. Parse the census population counts by single year of age
  pop1 <- dd_census_extract %>% dplyr::filter(complete == TRUE) %>% 
    arrange(SexID, AgeStart)
  
  maxage <- ifelse(nrow(pop1) > 0, max(pop1$AgeStart), 0)

  pop_smoothed_out <- NULL  
  # 3. Proceed with single age data if the open age group is age 50 or higher
  if (nrow(pop1) > 0 & maxage >= 50) {
    
    census_pop_in <- pop1 %>% 
      dplyr::filter(AgeLabel != "Total" & SexID %in% c(1,2)) %>% 
      select(SexID, AgeStart, DataValue)
    
    popM   <- census_pop_in$DataValue[census_pop_in$SexID ==1]
    popF   <- census_pop_in$DataValue[census_pop_in$SexID ==2]
    Age    <- census_pop_in$AgeStart[census_pop_in$SexID ==2]
    
    
    # 3.a. collapse open age over 100 to 100+  
    if (maxage > 100) {
      popM   <- c(popM[Age < 100], sum(popM[Age >= 100]))
      popF   <- c(popF[Age < 100], sum(popF[Age >= 100]))
      Age    <- c(Age[Age<100], 100) 
      maxage <- 100
    }
    
    # 3.b. If the open age group is larger than OpenAge5, then truncate back
    OpenAge5 <- as.integer(maxage / 5) * 5
    if (maxage > OpenAge5) {
      popM   <- c(popM[Age < OpenAge5], sum(popM[Age >= OpenAge5]))
      popF   <- c(popF[Age < OpenAge5], sum(popF[Age >= OpenAge5]))
      Age    <- c(Age[Age < OpenAge5], OpenAge5)
      maxage <- OpenAge5
    }
    rm(OpenAge5)
    
    # 3.c. Parse net census enumeration model results for the given locid and census year
    NCE_total <- dplyr::filter(TotNetEnum, LocID == locid, Year == census_year)$NetEnum
    NCE_m     <- dplyr::filter(DiffNCE1, LocID == locid, Year == census_year)$NetEnum_M_Diff
    NCE_f     <- dplyr::filter(DiffNCE1, LocID == locid, Year == census_year)$NetEnum_F_Diff
    NCE_age   <- dplyr::filter(DiffNCE1, LocID == locid, Year == census_year)$Age
    
      # truncate sex-age specific nce diff to the ages of the available population data
      NCE_m   <- c(NCE_m[NCE_age < maxage], mean(NCE_m[NCE_age >= maxage]))
      NCE_f   <- c(NCE_f[NCE_age < maxage], mean(NCE_f[NCE_age >= maxage]))
    
    
    # 3.d. Perform adjustment per net census enumeration model results based on analysis of post-enumeration surveys
    pop_adjusted_out <- censusPop_adjust_nce(popM = popM,
                                             popF = popF,
                                             Age  = Age,
                                             NCE_total = NCE_total,
                                             NCE_m = NCE_m,
                                             NCE_f = NCE_f) 
    pop_m_adj_total <- sum(pop_adjusted_out$pop_m_adj)
    pop_f_adj_total <- sum(pop_adjusted_out$pop_f_adj)

    
    # 3.e. Assess age heaping and smooth per Peter Johnson methods protocol
    
    pop_smoothed_out <- getSmoothedPops_generic(popM = pop_adjusted_out[[1]],
                                                popF = pop_adjusted_out[[2]],
                                                Age  = Age)

    # 3.f. identify the best level of smoothing given bachi index and other criteria (Peter Johnson protocol)
    
    pop_smoothed_out$Summary$BestMavN <- getBestMavN(SummaryTbl = pop_smoothed_out$Summary, EduYrs_f = EduYrs_f, EduYrs_m = EduYrs_m)
    
    # 3.g. identify whether to use the unsmoothed or smoothed 5-year data
    
    pop_smoothed_out$Summary$BestGrad5 <- getBestGrad5(SummaryTbl = pop_smoothed_out$Summary, EduYrs_f = EduYrs_f, EduYrs_m = EduYrs_m)
    
    # 3.h. name the best smoothed series
    if (is.na(pop_smoothed_out$Summary$BestMavN[1])) {
        
      pop_smoothed_out$Summary$BestSmthLabel <- paste0("p1mav",sprintf("%02d", 50 + pop_smoothed_out$Summary$BestGrad5))
      pop_smoothed_out$Summary$BestSmthColName <- paste0("Grad5mav", pop_smoothed_out$Summary$BestGrad5)
        
      } else {
        
        pop_smoothed_out$Summary$BestSmthLabel <- paste0("p1mav",sprintf("%02d", pop_smoothed_out$Summary$BestMavN))
        pop_smoothed_out$Summary$BestSmthColName <- paste0("Pop", pop_smoothed_out$Summary$BestMavN)
        
      }
    
    # 3.i. Parse the best single age data
    pop_smoothed_out$PopCompare$BestSmth <- unlist(select(pop_smoothed_out$PopCompare, !!pop_smoothed_out$Summary$BestSmthColName[1]))
    
    pop_m_best_total <- sum(pop_smoothed_out$PopCompare$BestSmth[pop_smoothed_out$PopCompare$SexID == 1])
    pop_f_best_total <- sum(pop_smoothed_out$PopCompare$BestSmth[pop_smoothed_out$PopCompare$SexID == 2])
    
    # 3.j. adjust the best smoothed to NCE-adjusted pop level by sex
    pop_m_best_adj <- pop_smoothed_out$PopCompare$BestSmth[pop_smoothed_out$PopCompare$SexID == 1] * pop_m_adj_total/pop_m_best_total
    pop_f_best_adj <- pop_smoothed_out$PopCompare$BestSmth[pop_smoothed_out$PopCompare$SexID == 2] * pop_f_adj_total/pop_f_best_total
    
    pop_smoothed_out$PopCompare$BestSmthAdj <- c(pop_m_best_adj, pop_f_best_adj)
    
    } # end for single year data
  
  if (is.null(pop_smoothed_out)) {  # if there are no single age data to process, then use abridged/five-year
    
    # 4. Parse the abridged series
    pop5 <- dd_census_extract %>% dplyr::filter(abridged == TRUE | five_year == TRUE) %>% 
      arrange(SexID, AgeStart)
    
    abr <- "1-4" %in% pop5$AgeLabel # identify whether the abridged series is available
    
    if (abr) {
      pop5 <- pop5 %>% dplyr::filter(abridged)
    } else {
      pop5 <- pop5 %>% dplyr::filter(five_year)
    }
    
    maxage <- ifelse(nrow(pop5) > 0, max(pop5$AgeStart), 0)
    
    # 5. Proceed with abridged/five year data if the open age group is age 50 or higher
    if (nrow(pop5) > 0 & maxage >= 50) {
      
      census_pop_in <- pop5 %>% 
        dplyr::filter(AgeLabel != "Total" & SexID %in% c(1,2)) %>% 
        select(SexID, AgeStart, DataValue)
      
      popM   <- census_pop_in$DataValue[census_pop_in$SexID ==1]
      popF   <- census_pop_in$DataValue[census_pop_in$SexID ==2]
      Age    <- census_pop_in$AgeStart[census_pop_in$SexID ==2]

      # 5.a. collapse open age over 100 to 100+  
      if (maxage > 100) {
        popM   <- c(popM[Age < 100], sum(popM[Age >= 100]))
        popF   <- c(popF[Age < 100], sum(popF[Age >= 100]))
        Age    <- c(Age[Age<100], 100) 
        maxage <- 100
      }
      
      # 5.b. Parse net census enumeration model results for the given locid and census year
      NCE_total <- dplyr::filter(TotNetEnum, LocID == locid, Year == census_year)$NetEnum
      NCE_m     <- dplyr::filter(DiffNCEAbr, LocID == locid, Year == census_year)$NetEnum_M_Diff
      NCE_f     <- dplyr::filter(DiffNCEAbr, LocID == locid, Year == census_year)$NetEnum_F_Diff
      NCE_age   <- dplyr::filter(DiffNCEAbr, LocID == locid, Year == census_year)$Age
      
        # truncate sex-age specific nce diff to the ages of the available population data
        NCE_m   <- c(NCE_m[NCE_age < maxage], mean(NCE_m[NCE_age >= maxage]))
        NCE_f   <- c(NCE_f[NCE_age < maxage], mean(NCE_f[NCE_age >= maxage]))
        
        if (!abr) { # if only five year values, then collapse the adjustment for the first age group
          NCE_m <- c(0.2 * NCE_m[1] + 0.8 * NCE_m[2], NCE_m[3:length(NCE_m)])
          NCE_f <- c(0.2 * NCE_f[1] + 0.8 * NCE_f[2], NCE_f[3:length(NCE_f)])
        }
      
      # 5.c. Perform adjustment per net census enumeration model results based on analysis of post-enumeration surveys
      pop_adjusted_out <- censusPop_adjust_nce(popM = popM,
                                               popF = popF,
                                               Age  = Age,
                                               NCE_total = NCE_total,
                                               NCE_m = NCE_m,
                                               NCE_f = NCE_f) 
      pop_m_adj_total <- sum(pop_adjusted_out$pop_m_adj)
      pop_f_adj_total <- sum(pop_adjusted_out$pop_f_adj)
      
      
      # 5.d. Smooth per Peter Johnson methods protocol
      
      pop_smoothed_out <- getSmoothedPops_generic(popM = pop_adjusted_out[[1]],
                                                  popF = pop_adjusted_out[[2]],
                                                  Age  = Age)
      
      # 5.e. identify whether to use the unsmoothed or smoothed 5-year data
      
      pop_smoothed_out$Summary$BestGrad5 <- getBestGrad5(SummaryTbl = pop_smoothed_out$Summary, EduYrs_f = EduYrs_f, EduYrs_m = EduYrs_m)
      
      # 5.f. name the best smoothed series
      pop_smoothed_out$Summary$BestSmthLabel <- paste0("p5mav",sprintf("%02d", pop_smoothed_out$Summary$BestGrad5))
      pop_smoothed_out$Summary$BestSmthColName <- paste0("Grad5mav", pop_smoothed_out$Summary$BestGrad5)
        
      # 5.g. Parse the best single age data
      pop_smoothed_out$PopCompare$BestSmth <- unlist(select(pop_smoothed_out$PopCompare, !!pop_smoothed_out$Summary$BestSmthColName[1]))
      
      pop_m_best_total <- sum(pop_smoothed_out$PopCompare$BestSmth[pop_smoothed_out$PopCompare$SexID == 1])
      pop_f_best_total <- sum(pop_smoothed_out$PopCompare$BestSmth[pop_smoothed_out$PopCompare$SexID == 2])
      
      # 5.h. adjust the best smoothed to NCE-adjusted pop level by sex
      pop_m_best_adj <- pop_smoothed_out$PopCompare$BestSmth[pop_smoothed_out$PopCompare$SexID == 1] * pop_m_adj_total/pop_m_best_total
      pop_f_best_adj <- pop_smoothed_out$PopCompare$BestSmth[pop_smoothed_out$PopCompare$SexID == 2] * pop_f_adj_total/pop_f_best_total
      
      pop_smoothed_out$PopCompare$BestSmthAdj <- c(pop_m_best_adj, pop_f_best_adj)
      
      # 5.j. graduate the NCE-adjusted population series and include it on the PopCompare file as Pop1
      pop1M <- graduate_mono(pop_adjusted_out$pop_m_adj, Age = Age, AgeInt = DemoTools::age2int(Age), OAG = TRUE)
      pop1F <- graduate_mono(pop_adjusted_out$pop_f_adj, Age = Age, AgeInt = DemoTools::age2int(Age), OAG = TRUE)
      pop_smoothed_out$PopCompare$Pop1 <- c(pop1M, pop1F)
      
    } # end adjustment and smoothing of  abridged/five-year data
  }
  
  # stop if there was no complete, abridged, or five-year series available with maxage >= 50
  stopifnot(!is.null(pop_smoothed_out))
  
  # 6. Perform basepop adjustment of children per PJ protocol

  BP_out <- BP_single(LocID = locid, 
                      SummaryTbl = pop_smoothed_out$Summary,
                      Pop1Compare = pop_smoothed_out$PopCompare,
                      RefDate = census_reference_date,
                      nLxFemale = nLxFemale,
                      nLxMale   = nLxMale,
                      nLxDatesIn = nLxDatesIn,
                      AsfrMat = AsfrMat,
                      AsfrDatesIn = AsfrDatesIn,
                      SRB = SRB,
                      SRBDatesIn = SRBDatesIn)
    
    # SummaryTbl$BestSmthAdjBPLabel[SummaryTbl$id == ids[i]] <- BPout$BestSmthAdjBPLabel
    # Pop1Compare$BestSmthAdjBP[Pop1Compare$id == ids[i]] <- BPout$BestSmthAdjBP$BestSmthAdjBP
    # 
    # SummaryTbl$TotPopBestAdjBP[SummaryTbl$id == ids[i]][1] <- sum(BPout$BestSmthAdjBP$BestSmthAdjBP[BPout$BestSmthAdjBP$SexID == 1])
    # SummaryTbl$TotPopBestAdjBP[SummaryTbl$id == ids[i]][2] <- sum(BPout$BestSmthAdjBP$BestSmthAdjBP[BPout$BestSmthAdjBP$SexID == 2])
  
  # 7. Extend to open age group OAnew (as needed), using OPAG
  
  if (maxage < OAnew) {
    OPAG_m <- OPAG_wrapper(Pop = BP_out$BestSmthAdjBP$BestSmthAdjBP[BP_out$BestSmthAdjBP$SexID == 1],
                           Age = BP_out$BestSmthAdjBP$AgeStart[BP_out$BestSmthAdjBP$SexID == 1],
                           LocID = locid, 
                           Year = census_year, 
                           sex = "male", 
                           nLx = nLxMale,
                           Age_nLx = Age_nLx,
                           AgeInt_nLx = AgeInt_nLx,
                           cv_tolerance = 0.75,
                           OAnew = OAnew)
    
    OPAG_f <- OPAG_wrapper(Pop = BP_out$BestSmthAdjBP$BestSmthAdjBP[BP_out$BestSmthAdjBP$SexID == 2],
                           Age = BP_out$BestSmthAdjBP$AgeStart[BP_out$BestSmthAdjBP$SexID == 2],
                           LocID = locid, 
                           Year = census_year, 
                           sex = "female", 
                           nLx = nLxFemale,
                           Age_nLx = Age_nLx,
                           AgeInt_nLx = AgeInt_nLx,
                           cv_tolerance = 0.75,
                           OAnew = OAnew)
    
    OPAG_out <- list(OPAG_m = OPAG_m,
                     OPAG_f = OPAG_f)
    
    census_pop_out <- data.frame(SexID = c(rep(1,OAnew+1), rep(2,OAnew+1)),
                                 AgeStart = rep(0:OAnew,2),
                                 DataValue = c(OPAG_m$pop_ext, OPAG_f$pop_ext))
  
  } else { # if no extension necessary
    
    OPAG_out <- NULL
    census_pop_out <- data.frame(SexID = c(rep(1,OAnew+1), rep(2,OAnew+1)),
                                 AgeStart = rep(0:OAnew,2),
                                 DataValue = c(BP_out$BestSmthAdjBP$BestSmthAdjBP[BP_out$BestSmthAdjBP$SexID == 1], 
                                               BP_out$BestSmthAdjBP$AgeStart[BP_out$BestSmthAdjBP$SexID == 2]))
  }
  
  # append some identifying information to the output census series to make it easier to transform to matrix later on
  census_pop_out$LocID <- locid  
  census_pop_out$census_year <- census_year
  census_pop_out$census_reference_date <- census_reference_date
  
  # compile all of the outputs
  census_adjust_smooth_extend_out <- list(LocID = locid,
                                          LocName = dd_census_extract$LocName[1],
                                          census_year = census_year,
                                          census_reference_date = census_reference_date,
                                          census_data_source = dd_census_extract$id[1],
                                          census_pop_in = census_pop_in,
                                          census_pop_out = census_pop_out,
                                          pop_adjusted_out = pop_adjusted_out,
                                          pop_smoothed_out = pop_smoothed_out,
                                          BP_out = BP_out,
                                          OPAG_out = OPAG_out)
  
  return(census_adjust_smooth_extend_out)  
  
}


