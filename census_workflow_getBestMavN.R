

#Get BestMavN from Bachi Indices ---------------------------------------------
getBestMavN <- function(Bachi, BachiProp0and5, BachiPropMax2, EduYrs, subgroup = c("adult", "child")) {
  
  
  ## NEED TO IMPLEMENT DIFFERENT FILTERING BASED ON SUBGROUP????
  
  #---- select best MavN -------
  BachiLevels <-  c(0, 0.75,  2,  4,  8, 30, Inf)
  MavNs <-        c(  1,  2,  4,  6,  10, NA)
  
  InitialBest <- MavNs[as.double(cut(Bachi, breaks = BachiLevels, labels = FALSE))]
  
  FinalBest <- InitialBest
  
  if (!is.na(FinalBest)) {
    
    # these could be nested if else.. but this is easier to read
    if (InitialBest == 6) {
      if(BachiProp0and5 > 0.65) {
        FinalBest <- 10} else {
          if (EduYrs >= 8) FinalBest <- 4 
          if (EduYrs < 8) FinalBest <- 6 
        }
      
    } 
    
    if (InitialBest == 4) {
      if(BachiProp0and5 > 0.60) {
        FinalBest <- 6 } else {
          if (EduYrs >= 8) FinalBest <- 2
          if (EduYrs < 8) FinalBest <- 4
        }
      
    }
    
    if (InitialBest == 2) {
      if(BachiPropMax2 > 0.70) {
        FinalBest <- 4} else {
          if (EduYrs >= 8) FinalBest <- 1
        }
      
    }
    
    if (InitialBest == 1) {
      if(BachiPropMax2 > 0.55) {
        FinalBest <- 2}
    }
    
  } # close for if !is.na(FinalBest)
  
  return(FinalBest)
  
}
