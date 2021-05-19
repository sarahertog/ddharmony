

# for single year data, assesses heaping with bachi index and smoothes with moving avearage at different levels
# for abridged or five-year data, smoothes with moving average at different levels

# inputs are population by single year of age and sex

getSmoothedPop1 <- function(popM, 
                            popF,
                            Age,
                            bachi_age = NULL,
                            age_ratio_age = NULL, # must be multiples of 5
                            EduYrs,
                            subgroup = c("adult", "child")) {
  
  maxage = max(Age)
  
  if (is.null(bachi_age)) {
    if (subgroup == "adult") {
      ageMin <- 23
      ageMax <- min(77, maxage)
    } else {
      ageMin <- 3
      ageMax <- 17
    }
  } else {
    ageMin <- min(bachi_age)
    ageMax <- max(bachi_age)
    
  }
  
  # compute bachi
  bachi_m <- check_heaping_bachi(Value = popM,
                                       Age = Age,
                                       ageMin = ageMin,
                                       ageMax = ageMax,
                                       method = "pasex",
                                       details = TRUE)
  bachi_f <- check_heaping_bachi(Value = popF,
                                       Age = Age,
                                       ageMin = ageMin,
                                       ageMax = ageMax,
                                       method = "pasex",
                                       details = TRUE)
  
  # compute proportion of heaping concentrated in digits 0 and 5
  BachiProp0and5_m <- (bachi_m$pct[1] + bachi_m$pct[6] - 20) / bachi_m$index
  BachiProp0and5_f <- (bachi_f$pct[1] + bachi_f$pct[6] - 20) / bachi_f$index
  
  # compute proportion of heaping concentrated in the favorite two digits
  BachiPropMax2_m <- (sum(bachi_m$pct[order(-bachi_m$pct)][c(1,2)]) - 20) / bachi_m$index
  BachiPropMax2_f <- (sum(bachi_f$pct[order(-bachi_f$pct)][c(1,2)]) - 20) / bachi_f$index
  
  # identify the preferred smoothing based on bachi, digit preference and level of education
  bestMavN <- getBestMavN(Bachi = max(bachi_m$index, bachi_f$index), 
                          BachiProp0and5 = min(BachiProp0and5_m, BachiProp0and5_f),
                          BachiPropMax2 = min(BachiPropMax2_m, BachiPropMax2_f), 
                          EduYrs = EduYrs,
                          subgroup = subgroup) 
  
  # smooth based on bestMavN
  if (!is.na(bestMavN)) {
    popM_smooth_mav <- mavPop1(popM, Age = Age)
    popF_smooth_mav <- mavPop1(popF, Age = Age)
    
    # Parse the best single age data
    popM_smooth <- unlist(select(popM_smooth_mav$MavPopDF, !!paste0("Pop", bestMavN)))
    popF_smooth <- unlist(select(popF_smooth_mav$MavPopDF, !!paste0("Pop", bestMavN)))
  
    best_smooth_method <- paste("bestMavN = ", bestMavN)
    
    AgeRatioScore_orig <- NA # We dont need age ratio score if we are using single age data so set this to NULL for output
    AgeRatioScore_mav2 <- NA
    
  } else { # if bestMavN is NA then group to five year data
    
    # group single year series to five year age groups
    popM5    <- DemoTools::groupAges(popM, Age = Age, N = 5, OAnew = maxage)
    popF5    <- DemoTools::groupAges(popF, Age = Age, N = 5, OAnew = maxage)
    Age5     <- seq(0, maxage, 5)
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
    
    # compute age ratio scores for 5-yr age groups from 15-19 to 70-74 for adults or from 0 to 10-14 for children
    if (is.null(age_ratio_age)) {
      if (subgroup == "adult") {
        ageMin <- 15
        ageMax <- min(70, maxage)
      } else {
        ageMin <- 0
        ageMax <- 10
      }
    } else {
      ageMin = min(age_ratio_age)
      ageMax = max(age_ratio_age)
    }
    
    # first on the unsmoothed 5-year data
    ageRatioScoreM_orig <- DemoTools::ageRatioScore(Value = popM5, Age = Age5,
                                                    ageMin = ageMin, ageMax = ageMax, OAG = FALSE)
    ageRatioScoreF_orig <- DemoTools::ageRatioScore(Value = popF5, Age = Age5,
                                                    ageMin = ageMin, ageMax = ageMax, OAG = FALSE)
    AgeRatioScore_orig = max(ageRatioScoreM_orig, ageRatioScoreF_orig)
    
    # then on the mav2 smoothed 5-year data
    ageRatioScoreM_mav2 <- DemoTools::ageRatioScore(Value = popM5_mav2, Age = Age5,
                                                    ageMin = ageMin, ageMax = ageMax, OAG = FALSE)
    ageRatioScoreF_mav2 <- DemoTools::ageRatioScore(Value = popF5_mav2, Age = Age5,
                                                    ageMin = ageMin, ageMax = ageMax, OAG = FALSE)
    AgeRatioScore_mav2 = max(ageRatioScoreM_mav2, ageRatioScoreF_mav2)
    
    # now identify the best smoothing approach based on age ratio scores and education
    bestGrad5 <- getBestGrad5(AgeRatioScore_orig = AgeRatioScore_orig, 
                              AgeRatioScore_mav2 = AgeRatioScore_mav2, 
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
    
  } # done with smoothing
  
  out.data <- list(popM_smooth = popM_smooth,
                   popF_smooth = popF_smooth,
                   best_smooth_method = best_smooth_method,
                   bachi = max(bachi_m$index, bachi_f$index),
                   AgeRatioScore_orig = AgeRatioScore_orig,
                   AgeRatioScore_mav2 = AgeRatioScore_mav2)
  
  return(out.data) 
  
  
}

