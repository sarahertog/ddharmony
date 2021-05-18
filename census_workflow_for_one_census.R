# implements the census workflow steps for a single census
# as described in flowchart XX
        
census_workflow_for_one_census <- function(dd_census_extract, # a census extract returned by DDharmonize_validate_PopCounts()
                                           LocID,
                                           locid_DemoTools = LocID,
                                           census_reference_date, # decimal year
                                           adjust_pes = TRUE, # should census be adjusted based on pes models?
                                           adjust_smooth = TRUE, # should census be smoothed according to age heaping assessment?
                                           adjust_basepop = TRUE, # should child counts be adjusted per basepop analysis?
                                           lxMale = NULL, # single or abridged male lx at census reference year. if NULL then will use DemoToolsData
                                           lxFemale = NULL, # single or abridged female lx at census reference year.
                                           Age_lx = NULL, # single or abridged
                                           nLxMatMale = NULL, # matrix of abridged nLx for males. if NULL then will use DemoToolsData
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
          
          pop_smooth_output <- census_workflow_adjust_smooth(popM = pop_working$popM,
                                                             popF = pop_working$popF,
                                                             Age = pop_working$Age,
                                                             bachi_age_child = 3:17,
                                                             bachi_age_adult = 23:min(77, OPAG_out$age_redist_start),
                                                             age_ratio_age_child = c(0,10),
                                                             age_ratio_age_adult = c(15,min(70, OPAG_out$age_redist_start)),
                                                             EduYrs = EduYrs)
 
          
          pop_working <- data.frame(Age = 0:105,
                                    popF = pop_smooth_output$popF,
                                    popM = pop_smooth_output$popM)
          
          # set aside the smoothed series
          nAge <- nrow(pop_working)
          pop_smoothed <- data.frame(SexID = c(rep(1,nAge),rep(2,nAge)),
                                     AgeStart = rep(pop_working$Age,2),
                                     DataValue = c(pop_working$popM, pop_working$popF))
          

      
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
          
          pop_smooth_output <- list(Age = NA,
                                    popF_smoothed = NA,
                                    popM_smoothed = NA,
                                    bachi_child = NA,
                                    bachi_adult = NA,
                                    ageRatio_adult_orig = NA,
                                    ageRatio_child_orig = NA,
                                    ageRatio_adult_mav2 = NA,
                                    ageRatio_child_mav2 = NA,
                                    best_smooth_adult   = NA,
                                    best_smooth_child   = NA)
          pop_smoothed        <- NULL

        }
          
    ###############################################################################
    ###############################################################################
    # F: Adjust for missing children (basepop) 
          
        if (adjust_basepop == TRUE)  {
            
          pop_basepop <- census_workflow_adjust_basepop(popM1 = pop_working$popM,
                                                   popF1 = pop_working$popF,
                                                   popM_unsmoothed = pop_unsmoothed$DataValue[pop_unsmoothed$SexID ==1],
                                                   popF_unsmoothed = pop_unsmoothed$DataValue[pop_unsmoothed$SexID ==2],
                                                   Age_unsmoothed = pop_unsmoothed$AgeStart[pop_unsmoothed$SexID == 2],
                                                   smooth_method = pop_smooth_output$best_smooth_child,
                                                   LocID = locid_DemoTools,
                                                   census_reference_date = census_reference_date,
                                                   nLxMatFemale = nLxMatFemale, 
                                                   nLxMatMale = nLxMatMale, 
                                                   nLxMatDatesIn = nLxMatDatesIn, 
                                                   AsfrMat = AsfrMat, 
                                                   AsfrDatesIn = AsfrDatesIn, 
                                                   SRB = SRB, 
                                                   SRBDatesIn = SRBDatesIn, 
                                                   radix = radix)

          
          pop_working <- data.frame(Age = pop_basepop$AgeStart[pop_basepop$BPLabel == "BP4" & pop_basepop$SexID == 2],
                                    popF = pop_basepop$DataValue[pop_basepop$BPLabel == "BP4" & pop_basepop$SexID == 2],
                                    popM = pop_basepop$DataValue[pop_basepop$BPLabel == "BP4" & pop_basepop$SexID == 1])


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
          census_workflow_out <- list(LocID = LocID,
                                                  LocName = dd_census_extract$LocName[1],
                                                  census_reference_period = dd_census_extract$ReferencePeriod[1],
                                                  census_reference_date = census_reference_date,
                                                  census_data_source = dd_census_extract$id[1],
                                                  DataCatalogID = dd_census_extract$DataCatalogID[1],
                                                  pes_adjustment = PES_factor_Total,
                                                  best_smooth_adult = pop_smooth_output$best_smooth_adult,
                                                  best_smooth_child = pop_smooth_output$best_smooth_child,
                                                  bachi_adult = pop_smooth_output$bachi_adult,
                                                  bachi_child = pop_smooth_output$bachi_child,
                                                  ageRatio_adult_orig = pop_smooth_output$ageRatio_adult_orig,
                                                  ageRatio_child_orig = pop_smooth_output$ageRatio_child_orig,
                                                  ageRatio_adult_mav2 = pop_smooth_output$ageRatio_adult_mav2,
                                                  ageRatio_child_mav2 = pop_smooth_output$ageRatio_child_mav2,
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
        
        
          
          
          
        
     
    
