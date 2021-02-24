

#Get BestMavN from Bachi Indices ---------------------------------------------
getBestMavN <- function(SummaryTbl, EduYrs_f, EduYrs_m) {
  
  #---- select best MavN -------
  BachiLevels <-  c(0, 0.75,  2,  4,  8, 30, Inf)
  MavNs <-        c(  1,  2,  4,  6,  10, NA)
  
  MaxBachi <- max(SummaryTbl$BachiIndex, na.rm=TRUE)
  
  InitialBest <- MavNs[as.double(cut(MaxBachi, breaks = BachiLevels, labels = FALSE))]
  
  FinalBest <- InitialBest
  
  if (!is.na(FinalBest)) {
    
    Max2prop <- c(NA,NA)
    # compute proportion in max 2 digits
    for (isex in 1:2) {
      percents <- t(SummaryTbl[isex,3:12])
      maxPct <- max(percents)
      isMax <- percents == maxPct
      percents <- percents * (1 - isMax)
      rank2pct <- max(percents)
      Max2prop[isex] <- (maxPct + rank2pct - 20) / SummaryTbl$BachiIndex[isex]
    }
    
    
    # these could be nested if else.. but this is easier to read
    if (InitialBest == 6) {
      if(min(SummaryTbl$BachiProp0and5) > 0.65) {
        FinalBest <- 10} else {
          if (min(EduYrs_f, EduYrs_m) >= 8) FinalBest <- 4 
        }
      
    } 
    
    if (InitialBest == 4) {
      if(min(SummaryTbl$BachiProp0and5) > 0.60) {
        FinalBest <- 6 } else {
          if (min(EduYrs_f, EduYrs_m) >= 8) FinalBest <- 2
        }
      
    }
    
    if (InitialBest == 2) {
      if(min(Max2prop) > 0.70) {
        FinalBest <- 4} else {
          if (min(EduYrs_f, EduYrs_m) >= 8) FinalBest <- 1
        }
      
    }
    
    if (InitialBest == 1) {
      if(min(Max2prop) > 0.55) {
        FinalBest <- 2}
    }
    
  } # close for if !is.na(FinalBest)
  
  return(FinalBest)
  
}

