
        
census_workflow_for_one_census <- function(dd_census_extract,
                                           census_reference_date,
                                           adjust_pes = TRUE,
                                           adjust_smooth = TRUE,
                                           adjust_basepop = TRUE,
                                           lxMale = NULL, # single or abridged
                                           lxFemale = NULL, # single or abridged
                                           Age_lx = NULL, # single or abridged
                                           nLxMatMale = NULL, # abridged
                                           nLxMatFemale = NULL, # abridged
                                           nLxMatDatesIn = NULL,
                                           AsfrMat = NULL, # 5-year age groups from 15 to 45 only
                                           AsfrDatesIn = NULL,
                                           SRB = NULL,
                                           SRBDatesIn = NULL,
                                           radix = NULL,
                                           EduYrs = NULL,
                                           PES_factor_Total = NULL,
                                           PES_factor_single_Male = NULL,
                                           PES_factor_single_Female = NULL,
                                           PES_factor_abridged_Male = NULL,
                                           PES_factor_abridged_Female = NULL) {
  
  
    # initialize output 
      census_workflow_out <- NULL
      
    ###############################################################################
    ###############################################################################
    # A. Decide whether to work with single-year, abridged, or five-year age groups

        # Single year series is available and OAG >= 50?
        has_single_oag50 <- dd_census_extract %>% census_has_single_oag50
        # Five-year grouped age series is available and OAG >= 50?
        has_five_year_oag50 <- dd_census_extract %>% census_has_five_year_oag50
        # Abridged grouped age series is available and OAG >= 50?
        has_abridged_oag50 <- dd_census_extract %>% census_has_abridged_oag50
        
        if (has_single_oag50 == TRUE) {
          
          # Parse the census population counts by single year of age
          pop <- dd_census_extract %>% dplyr::filter(complete == TRUE) %>% 
            arrange(SexID, AgeStart)
          
          use_series <- "single"
          
        } else { 
          
          if (has_abridged_oag50 == TRUE) {
          
          # Parse the census population counts by abridged age groups
          pop <- dd_census_extract %>% dplyr::filter(abridged == TRUE) %>% 
            arrange(SexID, AgeStart)
          
          use_series <- "abridged"
          
          } else {
          
            if (has_five_year_oag50 == TRUE) {
            
            # Parse the census population counts by five-year age groups
            pop <- dd_census_extract %>% dplyr::filter(five_year == TRUE) %>% 
              arrange(SexID, AgeStart)
            
            use_series <- "five_year"
            
            } else {
          
              pop <- NULL
              use_series <- NULL
              print(paste0("WARNING: There are no suitable data series available for the ", census_refpd, " census of ", dd_census_extract$LocName[1]))
              
            }
          }
        }
    
        
    ###############################################################################
    ###############################################################################
        
    if (!is.null(pop)) {
          
          # set aside the input population series
          pop_in <- pop %>% 
            dplyr::filter(AgeLabel != "Total" & SexID %in% c(1,2)) %>% 
            select(SexID, AgeStart, DataValue, id)
          
    ###############################################################################
    ###############################################################################
    # B. Ensure that the start age for the open age group is <= 100 and is a multiple of 5

          # Ensure that open age group <= 100 and is a multiple of 5
          pop_working <- census_workflow_oag100_mult5(Age = pop_in$AgeStart[pop_in$SexID ==2],
                                                      popF = pop_in$DataValue[pop_in$SexID ==2],
                                                      popM = pop_in$DataValue[pop_in$SexID ==1])
          
          
    ###############################################################################
    ###############################################################################
    # C. Adjust for under/overcount
    
        if (adjust_pes == TRUE) {
          
          if (use_series == "single") {
            NCE_m <- PES_factor_single_Male
            NCE_f <- PES_factor_single_Female
            NCE_age <- 1:length(NCE_m)-1
          } else {
            NCE_m <- PES_factor_abridged_Male
            NCE_f <- PES_factor_abridged_Female
            NCE_age <- c(0,1,seq(5, 5*(length(NCE_m)-2),5))
          }
          
          # Perform the adjustment per net census enumeration model results based on analysis of post-enumeration surveys
          pop_working <- census_workflow_adjust_pes(Age = pop_working$Age, 
                                                     popF = pop_working$popF, 
                                                     popM = pop_working$popM,
                                                     census_reference_date = census_reference_date,
                                                     NCE_total = PES_factor_Total, 
                                                     NCE_m = NCE_m, 
                                                     NCE_f = NCE_f,
                                                     NCE_age = NCE_age)

          # set aside the adjusted series
          nAge <- nrow(pop_working)
          pop_adjusted <- data.frame(SexID = c(rep(1,nAge),rep(2,nAge)),
                                     AgeStart = rep(pop_working$Age,2),
                                     DataValue = c(pop_working$popM, pop_working$popF))
          
        } else {
            pop_adjusted <- NULL
        }
          
          # set the unsmoothed series aside for basepop later
          nAge <- nrow(pop_working)
          pop_unsmoothed <- data.frame(SexID = c(rep(1,nAge),rep(2,nAge)),
                                       AgeStart = rep(pop_working$Age,2),
                                       DataValue = c(pop_working$popM, pop_working$popF))
        
    ###############################################################################
    ###############################################################################
    # D. Extend such that the open age group is  for ages 105+

          OPAG_out <- census_workflow_extend_to_105(popM = pop_working$popM,
                                                    popF = pop_working$popF,
                                                    Age = pop_working$Age,
                                                    LocID = locid_DemoTools, 
                                                    Year = min(floor(census_reference_date), 2019), 
                                                    lxM = lxMale, # vector of lx representing stable standard
                                                    lxF = lxFemale,
                                                    Age_lx = Age_lx,
                                                    AgeInt_lx = AgeInt_lx,
                                                    cv_tolerance = 0.75, # coefficient of variation tolerance for join point
                                                    min_age_redist = min(max(pop_working$Age), 65),
                                                    OAnew = 105)
          
          pop_working <- data.frame(Age = OPAG_out$Age_ext,
                                popF = OPAG_out$popF_ext,
                                popM = OPAG_out$popM_ext)

          # set aside the extended series
          nAge <- nrow(pop_working)
          pop_extended <- data.frame(SexID = c(rep(1,nAge),rep(2,nAge)),
                                     AgeStart = rep(pop_working$Age,2),
                                     DataValue = c(pop_working$popM, pop_working$popF))
          
          
    ###############################################################################
    ###############################################################################
    # E. Smooth over age
          
        if (adjust_smooth == TRUE)  {
          
          # intialize bachi
          bachi_child <- NA
          bachi_adult <- NA
          
          # if inputs are by single year of age
          if (use_series == "single") {
            
            # assess single year age heaping for children and smooth accordingly 
            pop_smooth_child <- getSmoothedPop1 (Age = pop_working$Age,
                                                 popF = pop_working$popF,
                                                 popM = pop_working$popM, 
                                                 bachi_age = 3:17, 
                                                 age_ratio_age = c(0,10),
                                                 EduYrs = EduYrs, 
                                                 subgroup = "child") 
            bachi_child <- pop_smooth_child$bachi
            
            # assess single year age heaping for adults and smooth accordingly 
            pop_smooth_adult <- getSmoothedPop1 (Age = pop_working$Age,
                                                 popF = pop_working$popF,
                                                 popM = pop_working$popM,  
                                                 bachi_age = 23:(min(77,OPAG_out$age_redist_start)),
                                                 age_ratio_age = c(15, min(70, OPAG_out$age_redist_start)),
                                                 EduYrs = EduYrs, 
                                                 subgroup = "adult") 
            bachi_adult <- pop_smooth_adult$bachi
            
          } else {
            
            # assess grouped age heaping for children and smooth accordingly 
            pop_smooth_child <- getSmoothedPop5(Age = pop_working$Age,
                                                popF = pop_working$popF,
                                                popM = pop_working$popM, 
                                                age_ratio_age = c(0, 10),
                                                EduYrs = EduYrs, 
                                                subgroup = "child") 
            
            # assess grouped age heaping for adults and smooth accordingly 
            pop_smooth_adult <- getSmoothedPop5(Age = pop_working$Age,
                                                popF = pop_working$popF,
                                                popM = pop_working$popM, 
                                                age_ratio_age = c(15, min(70, OPAG_out$age_redist_start)),
                                                EduYrs = EduYrs, 
                                                subgroup = "adult") 
            
          }
          
          # blend the smoothed child and adult series, with transition at ages 15-19
          wts <- c(rep(1,16),0.8, 0.6, 0.4, 0.2, rep(0, max(pop_working$Age)-19))
          
          popM_smoothed <- (pop_smooth_child$popM_smooth * wts) + (pop_smooth_adult$popM_smooth * (1-wts))
          popF_smoothed <- (pop_smooth_child$popF_smooth * wts) + (pop_smooth_adult$popF_smooth * (1-wts))
          
          # re-adjust to ensure that after smoothing we are still matching the total
          popM_smoothed <- popM_smoothed * sum(pop_working$popM)/sum(popM_smoothed)
          popF_smoothed <- popF_smoothed * sum(pop_working$popF)/sum(popF_smoothed)
          
          pop_working <- data.frame(Age = 0:105,
                                    popF = popF_smoothed,
                                    popM = popM_smoothed)
          
          # set asside the smoothed series
          nAge <- nrow(pop_working)
          pop_smoothed <- data.frame(SexID = c(rep(1,nAge),rep(2,nAge)),
                                     AgeStart = rep(pop_working$Age,2),
                                     DataValue = c(pop_working$popM, pop_working$popF))
          
          # pull some diagnostics into memory 
          ageRatio_adult_orig <- pop_smooth_adult$AgeRatioScore_orig
          ageRatio_child_orig <- pop_smooth_child$AgeRatioScore_orig
          ageRatio_adult_mav2 <- pop_smooth_adult$AgeRatioScore_mav2
          ageRatio_child_mav2 <- pop_smooth_child$AgeRatioScore_mav2
          best_smooth_adult <- pop_smooth_adult$best_smooth_method
          best_smooth_child <- pop_smooth_child$best_smooth_method
      
        } else { # if no smoothing
          
          # if data are not single, then graduate the grouped data
          if (!(use_series == "single")) {
          # graduate the extended series
          popM_grad <- DemoTools::graduate_mono(pop_working$popM, Age = pop_working$Age, AgeInt = DemoTools::age2int(pop_working$Age))
          popF_grad <- DemoTools::graduate_mono(pop_working$popF, Age = pop_working$Age, AgeInt = DemoTools::age2int(pop_working$Age))

          pop_working <- data.frame(Age = 0:max(pop_working$Age),
                                    popF = popF_grad,
                                    popM = popM_grad)
          }
          
          
          pop_smoothed <- NULL
          bachi_adult <- NA
          bachi_child <- NA
          ageRatio_adult_orig <- NA
          ageRatio_child_orig <- NA
          ageRatio_adult_mav2 <- NA
          ageRatio_child_mav2 <- NA
          best_smooth_adult <- NA
          best_smooth_child <- NA
          
        }
          
    ###############################################################################
    ###############################################################################
    # F: Adjust for missing children (basepop) 
          
        if (adjust_basepop == TRUE)  {
            
        # group to abridged age groups
          popM_abr <- DemoTools::single2abridged(pop_working$popM)
          popF_abr <- DemoTools::single2abridged(pop_working$popF)
          Age_abr  <- as.numeric(row.names(popM_abr))
          
          # run basepop_five()
          BP1 <- DemoTools::basepop_five(location = locid_DemoTools,
                                         refDate = census_reference_date,
                                         Age = Age_abr,
                                         Females_five = popF_abr,
                                         Males_five = popM_abr, 
                                         nLxFemale = nLxMatFemale,
                                         nLxMale   = nLxMatMale,
                                         nLxDatesIn = nLxMatDatesIn,
                                         AsfrMat = AsfrMat,
                                         AsfrDatesIn = AsfrDatesIn,
                                         SRB = SRB,
                                         SRBDatesIn = SRBDatesIn,
                                         radix = radix,
                                         verbose = FALSE)
          
          # graduate result to single year of age
          popM_BP1 <- DemoTools::graduate_mono(Value = BP1[[2]], Age = Age_abr, AgeInt = DemoTools::age2int(Age_abr), OAG = TRUE)
          popF_BP1 <- DemoTools::graduate_mono(Value = BP1[[1]], Age = Age_abr, AgeInt = DemoTools::age2int(Age_abr), OAG = TRUE)
          
          # what is the minimum age at which BP1 is not higher than input population for both males and females
          Age <- 1:length(popM_BP1)-1
          BP1_higher <- popM_BP1 > pop_working$popM & popF_BP1 > pop_working$popF
          minLastBPage1 <- min(Age[!BP1_higher],10) - 1
          
          # get the unsmoothed series and graduate to single age if necessary
          if (use_series == "single") {
            popM_unsmoothed <- pop_unsmoothed$DataValue[pop_unsmoothed$SexID ==1]
            popF_unsmoothed <- pop_unsmoothed$DataValue[pop_unsmoothed$SexID ==2]
          } else {
            popM_unsmoothed <- DemoTools::graduate_mono(Value = pop_unsmoothed$DataValue[pop_unsmoothed$SexID ==1],
                                                       Age = pop_unsmoothed$AgeStart[pop_unsmoothed$SexID ==1],
                                                       AgeInt = DemoTools::age2int(pop_unsmoothed$AgeStart[pop_unsmoothed$SexID ==1]),
                                                       OAG = TRUE)
            popF_unsmoothed <- DemoTools::graduate_mono(Value = pop_unsmoothed$DataValue[pop_unsmoothed$SexID ==2],
                                                       Age = pop_unsmoothed$AgeStart[pop_unsmoothed$SexID ==2],
                                                       AgeInt = DemoTools::age2int(pop_unsmoothed$AgeStart[pop_unsmoothed$SexID ==2]),
                                                       OAG = TRUE)
          }
          
          # splice the BP1 series for ages at or below minLastBPage1 with unsmoothed single age series to age 15 and smoothed series thereafter
          popM_BP2 <- c(popM_BP1[Age <= minLastBPage1], popM_unsmoothed[Age > minLastBPage1 & Age < 15], pop_working$popM[pop_working$Age >= 15]) 
          popF_BP2 <- c(popF_BP1[Age <= minLastBPage1], popF_unsmoothed[Age > minLastBPage1 & Age < 15], pop_working$popF[pop_working$Age >= 15]) 
          
          # if we are smoothing, then smooth BP2 using the best method from child smoothing before
          if (adjust_smooth) {
            
            adj_method <- pop_smooth_child$best_smooth_method
            
            if (substr(adj_method, 1, 8) == "bestMavN") { # if the best smoothing was mav on one year data
              mavN <- as.numeric(substr(adj_method, nchar(adj_method)-1, nchar(adj_method)))
              popM_BP3_mav <- mavPop1(popM_BP2, Age)
              popM_BP3     <- unlist(select(popM_BP3_mav$MavPopDF, !!paste0("Pop", mavN)))
              popF_BP3_mav <- mavPop1(popF_BP2, Age)
              popF_BP3     <- unlist(select(popF_BP3_mav$MavPopDF, !!paste0("Pop", mavN)))
            } else { # if the best smoothing was on five-year data
              
              popM5_BP2 <- DemoTools::groupAges(popM_BP2, N=5)
              popF5_BP2 <- DemoTools::groupAges(popF_BP2, N=5)
              Age5      <- seq(0,max(Age_abr),5)
              
              bestGrad5 <- as.numeric(substr(adj_method, nchar(adj_method), nchar(adj_method)))
              
              if (bestGrad5 == 1) {
                popM_BP3 <- DemoTools::graduate_mono(popM5_BP2, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
                popF_BP3 <- DemoTools::graduate_mono(popF5_BP2, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
              }
              if (bestGrad5 == 2) {
                popM5_BP2_mav2 <- DemoTools::smooth_age_5(popM5_BP2, Age5, method = "MAV", n = 2)
                popF5_BP2_mav2 <- DemoTools::smooth_age_5(popF5_BP2, Age5, method = "MAV", n = 2)
                popM_BP3 <- DemoTools::graduate_mono(popM5_BP2_mav2, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
                popF_BP3 <- DemoTools::graduate_mono(popF5_BP2_mav2, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
              }
              
            }
            popM_BP3 <- c(popM_BP3[Age < 15], pop_working$popM[Age >=15])
            popF_BP3 <- c(popF_BP3[Age < 15], pop_working$popF[Age >=15])
            
          } else { # if no smoothing then BP3 = BP2
            popM_BP3 <- popM_BP2
            popF_BP3 <- popF_BP2
            
          }
          
          # what is the minimum age at which BP1 is higher than BP3 for both males and females
          BP1_higher <- popM_BP1 >= popM_BP3 & popF_BP1 >= popF_BP3 
          minLastBPage3 <- min(Age[!BP1_higher],10) - 1
          
          # splice the BP1 up to age minLastBPage3 with the BP3
          popM_BP4 <- c(popM_BP1[Age <= minLastBPage3], popM_BP3[Age > minLastBPage3 & Age < 15], pop_working$popM[Age >= 15])
          popF_BP4 <- c(popF_BP1[Age <= minLastBPage3], popF_BP3[Age > minLastBPage3 & Age < 15], pop_working$popF[Age >= 15])
          
          
          pop_working <- data.frame(Age = 0:max(pop_working$Age),
                                    popF = popF_BP4,
                                    popM = popM_BP4)

          nAge <- nrow(pop_working)
          pop_basepop <- data.frame(SexID = rep(c(rep(1,nAge),rep(2,nAge)),4),
                                    AgeStart = rep(Age,8),
                                    BPLabel = c(rep("BP1",nAge*2),
                                                rep("BP2",nAge*2),
                                                rep("BP3",nAge*2),
                                                rep("BP4",nAge*2)),
                                    DataValue = c(popM_BP1, popF_BP1,
                                                  popM_BP2, popF_BP2,
                                                  popM_BP3, popF_BP3,
                                                  popM_BP4, popF_BP4))

        } else {
          pop_basepop <- NULL
        }
          
    ###############################################################################
    ###############################################################################
    # END: Compile the output
        
        # Final results
          nAge <- nrow(pop_working)
          census_pop_out <- data.frame(SexID = c(rep(1,nAge),rep(2,nAge)),
                                       AgeStart = rep(pop_working$Age,2),
                                       DataValue = c(pop_working$popM, pop_working$popF))
          
          # compile all of the outputs
          census_workflow_out <- list(LocID = locid,
                                                  LocName = dd_census_extract$LocName[1],
                                                  census_reference_period = dd_census_extract$ReferencePeriod[1],
                                                  census_reference_date = census_reference_date,
                                                  census_data_source = dd_census_extract$id[1],
                                                  DataCatalogID = dd_census_extract$DataCatalogID[1],
                                                  pes_adjustment = PES_factor_Total,
                                                  best_smooth_adult = best_smooth_adult,
                                                  best_smooth_child = best_smooth_child,
                                                  bachi_adult = bachi_adult,
                                                  bachi_child = bachi_child,
                                                  ageRatio_adult_orig = ageRatio_adult_orig,
                                                  ageRatio_child_orig = ageRatio_child_orig,
                                                  ageRatio_adult_mav2 = ageRatio_adult_mav2,
                                                  ageRatio_child_mav2 = ageRatio_child_mav2,
                                                  EduYrs = EduYrs,
                                                  census_input_age_structure = use_series,
                                                  census_input_max_age = max(pop_in$AgeStart),
                                                  age_redist_start = OPAG_out$age_redist_start,
                                                  census_pop_in = pop_in,
                                                  pop_adjusted = pop_adjusted,
                                                  pop_extended = pop_extended,
                                                  pop_smoothed = pop_smoothed,
                                                  pop_basepop = pop_basepop,
                                                  census_pop_out = census_pop_out) 
          

          
    } # close for !is.null(pop)
        
        return(census_workflow_out)  
        
}
        
        
          
          
          
        
     
    
