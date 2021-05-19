# Test of functions to create moving average estimates of population by single years of age
# and birth cohort estimates based on single year data and reference date
# note Bachi defaults ageMin=23 and ageMax=77
# 11/19/20  rc code to fill in youngest oldest ages using seq of mavs
# 11/25/20 fixed mutate
# 12/17/20 more fixes 
# 2021-01-20 remove BirthCohort1 (switch to DT function )
#            No longer do birth cohorts here
# 2021-01-29 remove sex, CenDate, and CenYr entries from mavPop1

library(DemoTools)
library(tidyverse)
#-----------------------------------------------

SCcount <- function(Pop) {
  # count number of sign changes age to age
  # get Pop(a) - Pop(a-1) only look at ages 23-67
  PopCh <- Pop[25:68] - Pop[24:67]
  PopCHsign <- sign(PopCh)
  PopCHsignChange <- PopCHsign[1:43] != PopCHsign[2:44]
  sum(PopCHsignChange)
}

#-----------------------------------------------

# new version of function with help from rc 11/19/20
mavPop1 <- function(Pop, Age) {
  
  # store pop data for sending back
  MavPopDF <- tibble(Age = Age, Pop1 = Pop)
  Nage <- length(Age) - 2

  # get results for original data
  BachiN <- check_heaping_bachi(Pop, Age, method = "pasex")
  MySCcount <- SCcount(Pop)
  MavSummaryDF <- tibble(MavN = 1,BachiN, SCcountN = MySCcount)

  for  (MavN in 2:10)  {
    MyMavNew <- mav(Pop, n = MavN, Age = Age)
    MavPopDF <- cbind(MavPopDF, MyMavNew)
    colnames(MavPopDF)[ncol(MavPopDF)]  <- paste("Pop", MavN, sep = "")
  }
  
  OpenAge <- Nage + 1

  MavPopDF <- mutate( MavPopDF,  across(3:11, ~(
    function(x) {
      for(i in 1:length(x))
      {
        if(is.na(x[i]) & (Age[i] == 0 | Age[i] >= (OpenAge - 1)))
          x[i] <- Pop1[i]
        
        else if(is.na(x[i]) & (Age[i] == 1 | Age[i] == (OpenAge - 2)))
          x[i] <- Pop2[i]
        
        else if(is.na(x[i]) & (Age[i] == 2 | Age[i] == (OpenAge - 3)))
          x[i] <- Pop4[i]
        
        else if(is.na(x[i]) & (Age[i] == 3 | Age[i] == (OpenAge - 4)))
          x[i] <- Pop6[i]
        
        else if(is.na(x[i]) & (Age[i] == 4 | Age[i] == (OpenAge - 5)))
          x[i] <- Pop8[i]
        
      }
      return(x)
    } ) # end of function spec
    (.) ) # end of across spec  I still don't know the function of (.)
  ) # end of mutate
  
  
  for (i in 3:ncol(MavPopDF)){ 
    BachiN <- check_heaping_bachi(MavPopDF[[i]], Age, method = "pasex")
    SCcountN <- SCcount(MavPopDF[[i]])
    
    MavSummaryDF <- rbind(MavSummaryDF,c(MavN=i, BachiN, SCcountN))
  } 

  rownames(MavPopDF) <- MavPopDF$Age
  
  list(MavPopDF = MavPopDF,
       MavSummaryDF = MavSummaryDF)
  
} # end of mavPop1 function
