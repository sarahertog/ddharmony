

# for single year data, assesses heaping with bachi index and smoothes with moving avearage at different levels
# for abridged or five-year data, smoothes with moving average at different levels

# inputs are population by age and sex

getSmoothedPops_generic <- function(popM, 
                            popF,
                            Age) {
  
  nAge <- length(Age)
  stopifnot(length(popM) == nAge & length(popF) == nAge)
  
  # initialize outputs
  
  PopCompare <- list(tibble(SexID = 1,
                            AgeStart = as.numeric(0:max(Age))),
                     tibble(SexID = 2,
                            AgeStart = as.numeric(0:max(Age))))
  Summary    <- list(tibble(SexID = 1),
                     tibble(SexID = 2))
  Pop5un     <- list(tibble(SexID = 1),
                     tibble(SexID = 2))
  
  # identify whether data are abridged or complete (single)
  
  AgeInt <- DemoTools::age2int(Age)
  single <- max(AgeInt, na.rm = TRUE) == 1
  
  if (single) {
    
      for (sex in c(1,2)) {
        
        if (sex == 1) {
          pop <- popM } else {
            pop <- popF
          }
        
        maxage <- max(Age)
        
        # compute bachi
        bachi <- check_heaping_bachi(Value = pop,
                                     Age = Age,
                                     ageMin = 23,
                                     ageMax = min(77, maxage),
                                     method = "pasex",
                                     details = TRUE)
        
        
        # smooth with moving average for various levels
        MavPop1temp <- mavPop1(pop, Age)
        
        # assemble the various single year population series into a data frame
        PopCompare[[sex]] <- cbind(PopCompare[[sex]], MavPop1temp$MavPopDF[,-1]) 
        
        # put together summary indices
        Summary[[sex]]$BachiIndex =  bachi$index
        Summary[[sex]]$BachiPct0 = bachi$pct[1]
        Summary[[sex]]$BachiPct1 = bachi$pct[2]
        Summary[[sex]]$BachiPct2 = bachi$pct[3]
        Summary[[sex]]$BachiPct3 = bachi$pct[4]
        Summary[[sex]]$BachiPct4 = bachi$pct[5]
        Summary[[sex]]$BachiPct5 = bachi$pct[6]
        Summary[[sex]]$BachiPct6 = bachi$pct[7]
        Summary[[sex]]$BachiPct7 = bachi$pct[8]
        Summary[[sex]]$BachiPct8 = bachi$pct[9]
        Summary[[sex]]$BachiPct9 = bachi$pct[10]
        Summary[[sex]]$BachiProp0and5 = (bachi$pct[1]+bachi$pct[5]-20)/bachi$index
 
      } # close loop for sex
  } # close for if single
        
  #--- do some operations on 5-year data ---------------------------
  # for age and sex scores assume 75+ is open-ended 
  # unless it is lower ageMax= min(75,OpenAge)
  
  for (sex in c(1,2)) {
    
    if (sex == 1) {
      pop <- popM } else {
        pop <- popF
      }
    
    maxage <- max(Age)
    
    pop5    <- DemoTools::groupAges(pop, Age = Age, N = 5, OAnew = maxage)
    Age5    <- seq(0, maxage, 5)
    nAge5   <- length(Age5)
    Age5un  <- seq(0, min(75,  maxage), by = 5)
    nAge5un <- length(Age5un)

    # save for sex ratio and overall indices
    Pop5un[[sex]] <- pop5[1:nAge5un]
        
    # age/sex scores only to age 75 = 16 age groups
    ageScore1 <- DemoTools::ageRatioScore(pop5[1:nAge5un], Age5un)
        
    # save for sex ratio and overall indices
    Pop5un[[sex]] <- pop5[1:nAge5un]
        
    # resplit into single ages
    Pop1split <- DemoTools::graduate_mono(pop5)
        
    # Use Mav2 on 5-year data and regraduate
    Pop5mav2 <- DemoTools::smooth_age_5(pop5, seq(0, maxage, 5), method = "MAV", n = 2)
    ageScore2 <- DemoTools::ageRatioScore(Pop5mav2[1:nAge5un], Age5un)
    
    # Use Mav4 on 5-year data and regraduate
    Pop5mav4 <- DemoTools::smooth_age_5(pop5, seq(0, maxage, 5), method = "MAV", n = 4)
    # temp fix for ages 5-9 and second 5-yr from top
    Pop5mav4[2] <- Pop5mav2[2]
    Pop5mav4[nAge5 - 2] <- Pop5mav2[nAge5 - 2]
    
    # preserve age 0 -- > IS THIS CORRECT??? THERE WAS A NOTE IN PETERS CODE ABOUT DEALING WITH ABRIDGED DIFFERENTLY THAN FIVE-YEAR?
    # But I don't think age 0 was being preserved on the same process for single year of age?
    if (1 %in% AgeInt[!is.na(AgeInt)]) {
      Pop5mav2 <- c(pop[1], Pop5mav2[1] - pop[1], Pop5mav2[2:length(Pop5mav2)])
      Pop5mav4 <- c(pop[1], Pop5mav4[1] - pop[1], Pop5mav4[2:length(Pop5mav4)])
      Age5 <- c(0,1,seq(5, maxage, 5))
    }

    Grad5mav2 <- graduate_mono(Pop5mav2, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
    Grad5mav4 <- graduate_mono(Pop5mav4, AgeInt = DemoTools::age2int(Age5), Age = Age5, OAG = TRUE)
        
    # add the various single year population series to the PopCompare output
    PopCompare[[sex]]$Grad5mav1=Pop1split
    PopCompare[[sex]]$Grad5mav2=Grad5mav2
    PopCompare[[sex]]$Grad5mav4=Grad5mav4

    # add age scores to Summary table
    Summary[[sex]]$AgeScore1 = ageScore1
    Summary[[sex]]$AgeScore2 = ageScore2
        
  } # end loop for sex
      
  PopCompare <- do.call(rbind, PopCompare)
  Summary    <- do.call(rbind, Summary)
  Pop5un     <- reshape2::melt(Pop5un)
  names(Pop5un) <- c("age5", "pop5", "SexID")
  
  # compute some additional indices for the summary table
  # sex ratio score
  Summary$SexRatioScore <- DemoTools::sexRatioScore(Pop5un$pop5[Pop5un$SexID==1], 
                                                    Pop5un$pop5[Pop5un$SexID==2], 
                                                    Pop5un$age5[Pop5un$SexID==1], 
                                                    ageMax = max(Pop5un$age5))
  
  # age sex accuracy
  Summary$AgeSexAccuracy <- DemoTools::ageSexAccuracy(Pop5un$pop5[Pop5un$SexID==1], 
                                                      Pop5un$pop5[Pop5un$SexID==2], 
                                                      Pop5un$age5[Pop5un$SexID==1], 
                                                      ageMax = max(Pop5un$age5))
  
 
  

      
  out.data <- list(Summary = Summary,
                   PopCompare = PopCompare,
                   Pop5un = Pop5un)
  
  return(out.data)
  
}



