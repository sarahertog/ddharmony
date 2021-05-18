# This function carries out the basepop adjustment for children missing from census counts
# See census workflow chart XX for a description of the logic/steps


census_workflow_adjust_basepop <- function(popM1, # male pop counts by single year of age
                                           popF1, # female pop counts by single year of age
                                           popM_unsmoothed, # male pop counts from before smoothing step of workflow (can be single, abridged or five-year)
                                           popF_unsmoothed, # female pop counts from before smoothing step of workflow
                                           Age_unsmoothed, # starting age of age groups for unsmoothed series
                                           smooth_method = NA, # smoothing method used
                                           LocID, 
                                           census_reference_date, # decimal year
                                           nLxMatFemale = NULL, # matrix of nLx life table values for females. If NULL then values from DemoToolsData will be used.
                                           nLxMatMale = NULL, # matrix of nLx life table values for males
                                           nLxMatDatesIn = NULL, # dates associated with nLx matrices
                                           AsfrMat = NULL, # matrix of age-specific fertility rates. If NULL then DemoToolsData values are used.
                                           AsfrDatesIn = NULL, # dates associated with ASFR matrix
                                           SRB = NULL, # vector of sex ratio at birth
                                           SRBDatesIn = NULL, # dates associated with SRB vector
                                           radix = NULL)  { # radix associated with nLx values  
                                           
  
  Age1 <- 1:length(popM1)-1
  # group to abridged age groups
  popM_abr <- DemoTools::single2abridged(popM1)
  popF_abr <- DemoTools::single2abridged(popF1)
  Age_abr  <- as.numeric(row.names(popM_abr))

  # run basepop_five()
  BP1 <- DemoTools::basepop_five(location = LocID,
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
  BP1_higher <- popM_BP1 > popM1 & popF_BP1 > popF1
  minLastBPage1 <- min(Age1[!BP1_higher],10) - 1

  if (minLastBPage1 >= 0) {
  
  # graduate the unsmoothed series to single age if necessary
  AgeInt_unsmoothed <- DemoTools::age2int(Age_unsmoothed)
  if (!(max(AgeInt_unsmoothed, na.rm=TRUE)==1)) {
    popM_unsmoothed <- DemoTools::graduate_mono(Value = popM_unsmoothed,
                                                Age = Age_unsmoothed,
                                                AgeInt = AgeInt_unsmoothed,
                                                OAG = TRUE)
    popF_unsmoothed <- DemoTools::graduate_mono(Value = popF_unsmoothed,
                                                Age = Age_unsmoothed,
                                                AgeInt = AgeInt_unsmoothed,
                                                OAG = TRUE)
  }
  Age1_unsmoothed <- 1:length(popM_unsmoothed) - 1

  # splice the BP1 series for ages at or below minLastBPage1 with unsmoothed single age series to age 15 and smoothed series thereafter
  popM_BP2 <- c(popM_BP1[Age1 <= minLastBPage1], popM_unsmoothed[Age1_unsmoothed > minLastBPage1 & Age1_unsmoothed < 15], popM1[Age1 >= 15]) 
  popF_BP2 <- c(popF_BP1[Age1 <= minLastBPage1], popF_unsmoothed[Age1_unsmoothed > minLastBPage1 & Age1_unsmoothed < 15], popF1[Age1 >= 15]) 

    # if we are smoothing, then smooth BP2 using the best method from child smoothing before
    if (!is.na(smooth_method)) {
      
      if (substr(smooth_method, 1, 8) == "bestMavN") { # if the best smoothing was mav on one year data
        
        mavN <- as.numeric(substr(smooth_method, nchar(smooth_method)-1, nchar(smooth_method)))
        popM_BP3_mav <- mavPop1(popM_BP2, Age1)
        popM_BP3     <- unlist(select(popM_BP3_mav$MavPopDF, !!paste0("Pop", mavN)))
        popF_BP3_mav <- mavPop1(popF_BP2, Age1)
        popF_BP3     <- unlist(select(popF_BP3_mav$MavPopDF, !!paste0("Pop", mavN)))
        
      } else { # if the best smoothing was on five-year data
        
        popM5_BP2 <- DemoTools::groupAges(popM_BP2, N=5)
        popF5_BP2 <- DemoTools::groupAges(popF_BP2, N=5)
        Age5      <- seq(0,max(Age_abr),5)
        
        bestGrad5 <- as.numeric(substr(smooth_method, nchar(smooth_method), nchar(smooth_method)))
        
        if (bestGrad5 == 1) {
          popM_BP3 <- DemoTools::graduate_mono(popM5_BP2, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
          popF_BP3 <- DemoTools::graduate_mono(popF5_BP2, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
        }
        if (bestGrad5 == 2) {
          popM5_BP2_mav2 <- DemoTools::smooth_age_5(popM5_BP2, Age5, method = "MAV", n = 2)
          popF5_BP2_mav2 <- DemoTools::smooth_age_5(popF5_BP2, Age5, method = "MAV", n = 2)
          # splice infants from BP1 to 1-4 year olds from smoothed BP2 and remaining smoothed BP2 thereafter
          popMabr_BP2_mav2 <- c(popM_BP1[1], popM5_BP2_mav2[1]-popM_BP1[1], popM5_BP2_mav2[2:length(popM5_BP2_mav2)])
          popFabr_BP2_mav2 <- c(popF_BP1[1], popF5_BP2_mav2[1]-popF_BP1[1], popF5_BP2_mav2[2:length(popF5_BP2_mav2)])
          
          popM_BP3 <- DemoTools::graduate_mono(popMabr_BP2_mav2, AgeInt = DemoTools::age2int(Age_abr), Age = Age_abr, OAG = TRUE)
          popF_BP3 <- DemoTools::graduate_mono(popFabr_BP2_mav2, AgeInt = DemoTools::age2int(Age_abr), Age = Age_abr, OAG = TRUE)
        }
        
      }
      popM_BP3 <- c(popM_BP3[Age1 < 15], popM1[Age1 >=15])
      popF_BP3 <- c(popF_BP3[Age1 < 15], popF1[Age1 >=15])
      
    } else { # if no smoothing then BP3 = BP2
      popM_BP3 <- popM_BP2
      popF_BP3 <- popF_BP2
      
    }
    
    # what is the minimum age at which BP1 is higher than BP3 for both males and females
    BP1_higher <- popM_BP1 >= popM_BP3 & popF_BP1 >= popF_BP3 
    minLastBPage3 <- min(Age1[!BP1_higher],10) - 1
    
    # splice the BP1 up to age minLastBPage3 with the BP3
    popM_BP4 <- c(popM_BP1[Age1 <= minLastBPage3], popM_BP3[Age1 > minLastBPage3 & Age1 < 15], popM1[Age1 >= 15])
    popF_BP4 <- c(popF_BP1[Age1 <= minLastBPage3], popF_BP3[Age1 > minLastBPage3 & Age1 < 15], popF1[Age1 >= 15])
    
  } else { # IF BP1 CAME IN LOWER THAN ORIGINAL AT ALL CHILD AGES, THEN WE SKIP THE STEPS AND JUST RETURN ORIGINAL
    
    popM_BP2 <- popM1
    popM_BP3 <- popM1
    popM_BP4 <- popM1
    popF_BP2 <- popF1
    popF_BP3 <- popF1
    popF_BP4 <- popF1
    
  }

nAge <- length(Age1)
pop_basepop <- data.frame(SexID = rep(c(rep(1,nAge),rep(2,nAge)),4),
                          AgeStart = rep(Age1,8),
                          BPLabel = c(rep("BP1",nAge*2),
                                      rep("BP2",nAge*2),
                                      rep("BP3",nAge*2),
                                      rep("BP4",nAge*2)),
                          DataValue = c(popM_BP1, popF_BP1,
                                        popM_BP2, popF_BP2,
                                        popM_BP3, popF_BP3,
                                        popM_BP4, popF_BP4))

return(pop_basepop)

}


