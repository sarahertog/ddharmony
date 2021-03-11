
# for abridged or five-year data, smoothes with moving average at different levels

# inputs are population by abridged or 5-year age groups

getSmoothedPop5 <- function(popM, 
                            popF,
                            Age,
                            age_ratio_age = NULL,
                            EduYrs,
                            subgroup = c("adult", "child")) {
  
  maxage = max(Age)
  
  popM_grad <- DemoTools::graduate_mono(Value = popM, Age = Age, AgeInt = DemoTools::age2int(Age), OAG = TRUE)
  popF_grad <- DemoTools::graduate_mono(Value = popF, Age = Age, AgeInt = DemoTools::age2int(Age), OAG = TRUE)
  
    # check if series is abridged
    abr <- 1 %in% Age
    
    # if abridged, then collapse the first two age groups (we still have age 0-1 on the graduated series)
    if (abr) {
      popM5 <- c(sum(popM[1:2]), popM[3:(length(popM))])
      popF5 <- c(sum(popF[1:2]), popF[3:(length(popF))])
      Age5  <- seq(0, maxage, 5)
      
    }  else {
      popM5 <- popM
      popF5 <- popF
      Age5  <- Age
    }
    nAge5    <- length(Age5)
       
    # smooth the 5-year series using mav2
    popM5_mav2 <- DemoTools::smooth_age_5(popM5, seq(0, maxage, 5), method = "MAV", n = 2)
    popF5_mav2 <- DemoTools::smooth_age_5(popF5, seq(0, maxage, 5), method = "MAV", n = 2)
    
    # smooth the 5-year series using mav4
    popM5_mav4 <- DemoTools::smooth_age_5(popM5, seq(0, maxage, 5), method = "MAV", n = 4)
    popM5_mav4[2] <- popM5_mav2[2]
    popM5_mav4[nAge5 - 2] <- popM5_mav2[nAge5 - 2]
    popF5_mav4 <- DemoTools::smooth_age_5(popF5, seq(0, maxage, 5), method = "MAV", n = 4)
    popF5_mav4[2] <- popF5_mav2[2]
    popF5_mav4[nAge5 - 2] <- popF5_mav2[nAge5 - 2]
    
    # compute age ratio scores for 5-yr age groups from 5 to 75 for adults or from 0 to 20 for children
    if (is.null(age_ratio_age)) {
      if (subgroup == "adult") {
        ageMin <- 5
        ageMax <- min(75, maxage)
      } else {
        ageMin <- 0
        ageMax <- 20
      }
    } else {
      ageMin = min(age_ratio_age)
      ageMax = max(age_ratio_age)
    }
    
    # first on the unsmoothed 5-year data
    ageRatioScoreM_orig <- DemoTools::ageRatioScore(Value = popM5, Age = Age5,
                                                    ageMin = ageMin, ageMax = ageMax)
    ageRatioScoreF_orig <- DemoTools::ageRatioScore(Value = popF5, Age = Age5,
                                                    ageMin = ageMin, ageMax = ageMax)
    # then on the mav2 smoothed 5-year data
    ageRatioScoreM_mav2 <- DemoTools::ageRatioScore(Value = popM5_mav2, Age = Age5,
                                                    ageMin = ageMin, ageMax = ageMax)
    ageRatioScoreF_mav2 <- DemoTools::ageRatioScore(Value = popF5_mav2, Age = Age5,
                                                    ageMin = ageMin, ageMax = ageMax)
    
    # now identify the best smoothing approach based on age ratio scores and education
    bestGrad5 <- getBestGrad5(AgeRatioScore_orig = max(ageRatioScoreM_orig, ageRatioScoreF_orig), 
                              AgeRatioScore_mav2 = max(ageRatioScoreM_mav2, ageRatioScoreF_mav2), 
                              EduYrs = EduYrs,
                              subgroup = subgroup)
    
    if (bestGrad5 == 1) {
      popM_smooth <- DemoTools::graduate_mono(popM5, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
      popF_smooth <- DemoTools::graduate_mono(popF5, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
    }
    if (bestGrad5 == 2) {
      popM_smooth <- DemoTools::graduate_mono(popM5_mav2, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
      popF_smooth <- DemoTools::graduate_mono(popF5_mav2, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
    }
    if (bestGrad5 == 4) {
      popM_smooth <- DemoTools::graduate_mono(popM5_mav4, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
      popF_smooth <- DemoTools::graduate_mono(popF5_mav4, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
    }
    
    best_smooth_method <- paste("bestGrad5 = ", bestGrad5)
    
    ### Should we be splicing that first age group back in????
    
  out.data <- list(popM_grad = popM_grad,
                   popF_grad = popF_grad,
                   popM_smooth = popM_smooth,
                   popF_smooth = popF_smooth,
                   best_smooth_method = best_smooth_method)
  
  return(out.data) 
  
  
}

