
# Ensure that the open age group is less than or equal to 100 and is a multiple of five

census_workflow_oag100_mult5 <- function(Age, popF, popM) {

  Age <- Age
  popM <- popM
  popF <- popF
  maxage <- max(Age)
  
  
  # collapse open age over 100 to 100+  
  if (maxage > 100) {
    popM   <- c(popM[Age < 100], sum(popM[Age >= 100]))
    popF   <- c(popF[Age < 100], sum(popF[Age >= 100]))
    Age    <- c(Age[Age<100], 100) 
    maxage <- 100
    nAge   <- length(Age)
  }

  # If the open age group is larger than OpenAge5, then truncate back
  OpenAge5 <- as.integer(maxage / 5) * 5
  if (maxage > OpenAge5) {
    popM   <- c(popM[Age < OpenAge5], sum(popM[Age >= OpenAge5]))
    popF   <- c(popF[Age < OpenAge5], sum(popF[Age >= OpenAge5]))
    Age    <- c(Age[Age < OpenAge5], OpenAge5)
    maxage <- OpenAge5
    nAge   <- length(Age)
  }
  
  outdata <- data.frame(Age = Age,
                        popF = popF,
                        popM = popM)
  
  return(outdata)

}
