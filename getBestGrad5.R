

#Get BestGrad5 from smoothing results
getBestGrad5 <- function(SummaryTbl, EduYrs_f, EduYrs_m) {
  
  # select whether to use the straight  5-year data 
  # or use Mav2 or mav4 of the 5-year data
  BestGrad5 <- NA
  if (max(SummaryTbl$AgeScore1) < 4 ) {
    BestGrad5 <- 1
  } else {
    if (min(EduYrs_f, EduYrs_m) >= 4) {
      BestGrad5 <-1
    } else {
      if (max(SummaryTbl$AgeScore2) < 4) {
        BestGrad5 <- 2
      } else {
        BestGrad5 <- 4
      }
    }
  }
  
  return(BestGrad5)
  
}

