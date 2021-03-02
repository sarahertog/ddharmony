# Extend and redistribute open age group to 100+

OPAG_wrapper <- function(Pop, 
                         Age, 
                         LocID, 
                         Year, 
                         sex = "female", 
                         cv_tolerance = 0.75, 
                         min_age_redist = min(max(Age), 65), 
                         OAnew = OAnew,
                         nLx = NULL,
                         Age_nLx = NULL,
                         AgeInt_nLx = NULL,
                         graduate_output = TRUE) {
  
  maxage   <- max(Age)
  
  stopifnot(maxage >= 50 & OAnew > maxage)
  
  # group to five-year age groups
  Pop5 <- DemoTools::groupAges(Pop, Age=Age, N=5)
  Age5 <- seq(0, maxage, 5)
  
  # extend pop to OAnew using OPAG
  if(Pop5[Age5 == maxage] > 5) {
    
    if (is.null(nLx)) {
    #nLx   <- downloadnLx(NULL, LocID, sex, Year) # WPP19 Lx has oag = 100+
    
    # download life table
    lt_abr <- fertestr::FetchLifeTableWpp2019(locations = LocID, 
                                              year = Year,
                                              sex = sex)
    # If OAnew is not a multiple of 5, round up
    OpenAge5 <- as.integer(OAnew / 5) * 5
    OAnew5 <- ifelse(OAnew == OpenAge5, OAnew, OpenAge5+5)
    
    # recompute life table, extending as necessary
    nLx <- lt_abridged(nMx = lt_abr$mx, Age = lt_abr$x, Sex = substr(sex,1,1), OAnew = OAnew5)$nLx
    Age_nLx <- c(0,1,seq(5,OAnew5,5))
    AgeInt_nLx <- DemoTools::age2int(Age_nLx, OAvalue = 1)
    }
    
    age_redist <- maxage
    pop_ext <- OPAG(Pop = Pop5,
                    Age_Pop = Age5,
                    AgeInt_Pop = age2int(Age5, OAvalue = 1),
                    nLx = nLx,
                    Age_nLx = Age_nLx,
                    AgeInt_nLx = AgeInt_nLx,
                    method = "uniform",
                    Redistribute_from = age_redist,
                    OAnew = OAnew5)
    
    # compute coefficient of variation of population from maxage-10 to maxage+5 to detect any discontinuity
    cv_out <- sd(diff(pop_ext$Pop_out[Age5 %in% c((age_redist-10):(age_redist+5))]))/abs(mean(diff(pop_ext$Pop_out[Age5 %in% c((age_redist-10):(age_redist+5))])))
    
    # iteratively move the Redistribute_from start age back in increments of 5 until the output cv is reasonably smooth
    while (cv_out > cv_tolerance & age_redist >=65 ) {
      age_redist <- age_redist - 5
      
      pop_ext <- OPAG(Pop = Pop5,
                      Age_Pop = Age5,
                      AgeInt_Pop = age2int(Age5, OAvalue = 1),
                      nLx = nLx,
                      Age_nLx = Age_nLx,
                      AgeInt_nLx = AgeInt_nLx,
                      method = "uniform",
                      Redistribute_from = age_redist)
      
      cv_out <- sd(diff(pop_ext$Pop_out[Age5 %in% c((age_redist-10):(age_redist+5))]))/abs(mean(diff(pop_ext$Pop_out[Age5 %in% c((age_redist-10):(age_redist+5))])))
      
    }
    
    pop_ext <- pop_ext$Pop_out
    
  } else { # if pop in maxage group is less than 5, then extend to OAnew5 with zeros
    
    age_redist <- maxage
    # If OAnew is not a multiple of 5, round up
    OpenAge5 <- as.integer(OAnew / 5) * 5
    OAnew5 <- ifelse(OAnew == OpenAge5, OAnew, OpenAge5+5)
    cv_out <- NA
    pop_ext <- c(Pop5, rep(0, (OAnew5 - maxage) / 5))
    
  }
  
  Agein <- Age
  if (graduate_output) {
  # Now graduate to single year of age
  pop_ext <- DemoTools::graduate_mono(pop_ext, AgeInt = rep(5,(OAnew5 + 5)/5), Age= seq(0,OAnew5,5), OAG = TRUE)
  Age     <- 0:OAnew5
  # truncate back to the originally requested OAnew
  pop_ext <- DemoTools::groupOAG(Value = pop_ext, Age = Age, OAnew = OAnew)
  Age     <- 0:OAnew
  }
  
  # splice together, using opag extension for only ages above the start of the opag redistribution
  
  pop_ext_final <- c(Pop[Agein < age_redist],
                     pop_ext[Age >= age_redist])
  Age_ext_final <- c(Agein[Agein < age_redist],
                     Age[Age >= age_redist])
  
  opag_out <- list(pop_ext = pop_ext_final,
                   Age_ext = Age_ext_final,
                   age_redist_start = age_redist,
                   cv = cv_out)
  
  return(opag_out)
  
}