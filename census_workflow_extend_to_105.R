# Extend and redistribute open age group to 105+

census_workflow_extend_to_105 <- function(popM, 
                                          popF,
                                          Age, 
                                          LocID, 
                                          Year,
                                          cv_tolerance = 0.75, 
                                          min_age_redist = min(max(Age), 65), 
                                          OAnew = 105,
                                          lxM = NULL,
                                          lxF = NULL,
                                          Age_lx = NULL,
                                          AgeInt_lx = NULL) {
  
  maxage   <- max(Age)
  
  stopifnot(maxage >= 50 & OAnew > maxage)
  
  single <- DemoTools::is_single(Age)
  
  # group to five-year age groups
  popM5 <- DemoTools::groupAges(popM, Age=Age, N=5)
  popF5 <- DemoTools::groupAges(popF, Age=Age, N=5)
  Age5 <- seq(0, maxage, 5)
  
  # If OAnew is not a multiple of 5, round up
  OpenAge5 <- as.integer(OAnew / 5) * 5
  OAnew5 <- ifelse(OAnew == OpenAge5, OAnew, OpenAge5+5)

  # extend pop to OAnew using OPAG
  if(popM5[Age5 == maxage] > 5) { # male pop at older ages tends to be smaller than female, so use this as criteria for extension
    
    if (is.null(lxM)) {
    
      # download abridged life table for males
      ltM_abr <- fertestr::FetchLifeTableWpp2019(locations = LocID, 
                                                year = Year,
                                                sex = "male")
      
      # download abridged life table for males
      ltF_abr <- fertestr::FetchLifeTableWpp2019(locations = LocID, 
                                                 year = Year,
                                                 sex = "female")
      
      lxM <- ltM_abr$lx
      lxF <- ltF_abr$lx
      Age_lx <- ltM_abr$x
    
    }
    
    # keep only lx for abridged age groups
    ages_abr <- c(0,1,seq(5, max(Age_lx), 5))
    lxM <- lxM[Age_lx %in% ages_abr]
    lxF <- lxF[Age_lx %in% ages_abr]

    # recompute life table, extending as necessary
    nLxM <- lt_abridged(lx = lxM, Age = ages_abr, Sex = "m", OAnew = OAnew5)$nLx
    nLxF <- lt_abridged(lx = lxF, Age = ages_abr, Sex = "f", OAnew = OAnew5)$nLx
    
    # collapse first two age groups
    nLxM <- c(nLxM[1]+nLxM[2],nLxM[3:length(nLxM)])
    nLxF <- c(nLxF[1]+nLxF[2],nLxF[3:length(nLxF)])
    Age_nLx <- seq(0,OAnew5,5)
    
    redist_ages <- seq(min_age_redist, maxage, 5)
    nra <- length(redist_ages)
    
    pop_extM <- list()
    pop_extF <- list()
    cvM <- list()
    cvF <- list()
    
    # extend the old-age populations using different starting ages
    for (k in 1:nra) {
      
      pop_extM[[k]] <- OPAG(Pop = popM5,
                            Age_Pop = Age5,
                            AgeInt_Pop = age2int(Age5, OAvalue = 5),
                            nLx = nLxM,
                            Age_nLx = Age_nLx,
                            method = "uniform",
                            Redistribute_from = redist_ages[k],
                            OAnew = OAnew5)
      
      cvM[[k]] <- sd(diff(pop_extM[[k]]$Pop_out[seq(0,OAnew5,5) %in% c((redist_ages[k]-10):(redist_ages[k]+5))]))/abs(mean(diff(pop_extM[[k]]$Pop_out[seq(0,OAnew5,5) %in% c((redist_ages[k]-10):(redist_ages[k]+5))])))
      
      pop_extF[[k]] <- OPAG(Pop = popF5,
                            Age_Pop = Age5,
                            AgeInt_Pop = age2int(Age5, OAvalue = 5),
                            nLx = nLxF,
                            Age_nLx = Age_nLx,
                            method = "uniform",
                            Redistribute_from = redist_ages[k],
                            OAnew = OAnew5)
      
      cvF[[k]] <- sd(diff(pop_extF[[k]]$Pop_out[seq(0,OAnew5,5) %in% c((redist_ages[k]-10):(redist_ages[k]+5))]))/abs(mean(diff(pop_extF[[k]]$Pop_out[seq(0,OAnew5,5) %in% c((redist_ages[k]-10):(redist_ages[k]+5))])))
      
    }
    
    cvM <- do.call(rbind, cvM)
    cvF <- do.call(rbind, cvF)
    
    cvM_below_tolerance <- min(cvM) <= cv_tolerance
    cvF_below_tolerance <- min(cvF) <= cv_tolerance
    
    if (cvM_below_tolerance & cvF_below_tolerance) {
      
      # identify the max age at which the cv tolerance is below the threshold for both males and females
      age_redist <- min(max(redist_ages[cvM <= cv_tolerance]), max(redist_ages[cvF <= cv_tolerance]))
    
    } else {
      
      # use the minimum of the age with minimum cv for males and age with minimum cv for females
      age_redist <- min(redist_ages[cvM == min(cvM)], redist_ages[cvF == min(cvF)])
      
    }
    Agein <- Age
    Age <- pop_extM[[1]]$Age_out
    
    popM_ext <- pop_extM[[c(1:nra)[redist_ages==age_redist]]]$Pop_out
    popF_ext <- pop_extF[[c(1:nra)[redist_ages==age_redist]]]$Pop_out
    
    ############
    ############
    ############
    
  } else { # if male pop in maxage group is less than 5, then extend both males and females to to OAnew5 with zeros
    
    age_redist <- maxage
    cv_out <- NA
    popM_ext <- c(popM5, rep(0, (OAnew5 - maxage) / 5))
    popF_ext <- c(popM5, rep(0, (OAnew5 - maxage) / 5))
    
    Agein <- Age
    Age <- seq(0,OAnew5,5)
    
    
  }

  
  if (single) { # if input is single, then graduate output
    
  # Now graduate to single year of age
  popM_ext <- DemoTools::graduate_mono(popM_ext, AgeInt = rep(5,(OAnew5 + 5)/5), Age= seq(0,OAnew5,5), OAG = TRUE)
  popF_ext <- DemoTools::graduate_mono(popF_ext, AgeInt = rep(5,(OAnew5 + 5)/5), Age= seq(0,OAnew5,5), OAG = TRUE)
  Age     <- 0:OAnew5
  # truncate back to the originally requested OAnew
  popM_ext <- DemoTools::groupOAG(Value = popM_ext, Age = Age, OAnew = OAnew)
  popF_ext <- DemoTools::groupOAG(Value = popF_ext, Age = Age, OAnew = OAnew)
  Age     <- 0:OAnew
  
  }
  
  # splice together, using opag extension for only ages above the start of the opag redistribution
  
  popM_ext_final <- c(popM[Agein < age_redist],
                      popM_ext[Age >= age_redist])
  popF_ext_final <- c(popF[Agein < age_redist],
                      popF_ext[Age >= age_redist])
  Age_ext_final <- c(Agein[Agein < age_redist],
                     Age[Age >= age_redist])
  
  opag_out <- list(popM_ext = popM_ext_final,
                   popF_ext = popF_ext_final,
                   Age_ext = Age_ext_final,
                   age_redist_start = age_redist)
  
  return(opag_out)
  
}