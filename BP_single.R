# test base_pop with previously smoothed data
# use beerA0 type graduation of the under 10 pop 0, 1-4, 5-9
#   2021-01-22 reorg to work with processed data
#   2021-01-23 found error called label with BP BestPopBPLabel instead of
#              BestSmthBPLabel
# 2021-01-29 lots of var name changes Mav -> Best
# 2021-01-30 remove some prints, change verbose to FALSE in basepop (for now)
#            plot to age 20, adjust y min and max
# 2021-02-07 fix columns, NEED TO SET TotPopBP
#        -08 use BestSmthInf smoothed and inflated to adjusted totals by sex
#            save TotPopBestBP to summary
#      02-11 change field names



BP_single <- function(SummaryTbl, 
                      Pop1Compare, 
                      LocID, 
                      RefDate,
                      nLxFemale = NULL,
                      nLxMale = NULL,
                      nLxDatesIn = NULL,
                      AsfrMat = NULL,
                      AsfrDatesIn = NULL,
                      SRB = NULL,
                      SRBDatesIn = NULL,
                      radix = NULL) {
  
      PopData <- Pop1Compare
      RefDate <- RefDate
      
      #get the Mav1=observed data
      ObsPop_Tbl <- select(PopData, SexID, AgeStart, Pop1)
      ObsPopF <- deframe(select(dplyr::filter(ObsPop_Tbl, SexID==2), AgeStart,Pop1))
      ObsPopM <- deframe(select(dplyr::filter(ObsPop_Tbl, SexID==1), AgeStart,Pop1))

      if (is.na( ObsPopF[1]) ) {
        # use Grad5mav1 in place of obs if no single year data
        ObsPop_Tbl <- select(PopData, SexID, AgeStart, Pop1, Grad5mav1)
        ObsPopF <- deframe(select(dplyr::filter(ObsPop_Tbl, SexID==2), AgeStart,
                                  Grad5mav1))
        ObsPopM <- deframe(select(dplyr::filter(ObsPop_Tbl, SexID==1), AgeStart,
                                  Grad5mav1))
        ObsLabel <- "Grad5mav1"
        
      } else { ObsLabel <- "Obs"}
        
      OPAG <- length(ObsPopF) - 1

      #Convert to abridged ages
      ObsPop5F <- single2abridged(ObsPopF)
      ObsPop5M <- single2abridged(ObsPopM)

      BPobs5List <- DemoTools::basepop_five(country = LocID,
                                            refDate = RefDate,
                                            Females_five = ObsPop5F,
                                            Males_five = ObsPop5M, 
                                            nLxFemale = nLxFemale,
                                            nLxMale   = nLxMale,
                                            nLxDatesIn = nLxDatesIn,
                                            AsfrMat = AsfrMat,
                                            AsfrDatesIn = AsfrDatesIn,
                                            SRB = SRB,
                                            SRBDatesIn = SRBDatesIn,
                                            radix = radix,
                                            verbose = FALSE)

      BPobs5F <- BPobs5List[[1]]
      BPobs5M <- BPobs5List[[2]]
      
      #get the BestMavN data
      BestLabel <- dplyr::filter(SummaryTbl, SexID==2)$BestSmthLabel
      BestPopF <- deframe(select(dplyr::filter(PopData, SexID==2), AgeStart, BestSmthAdj))
      BestPopM <- deframe(select(dplyr::filter(PopData, SexID==1), AgeStart, BestSmthAdj))
      
      #Convert to abridged ages
      BestPop5F <- single2abridged(BestPopF)
      BestPop5M <- single2abridged(BestPopM)

      BPBestList <- DemoTools::basepop_five(country = LocID,
                                            refDate = RefDate,
                                            Females_five = BestPop5F,
                                            Males_five = BestPop5M, 
                                            nLxFemale = nLxFemale,
                                            nLxMale   = nLxMale,
                                            nLxDatesIn = nLxDatesIn,
                                            AsfrMat = AsfrMat,
                                            AsfrDatesIn = AsfrDatesIn,
                                            SRB = SRB,
                                            SRBDatesIn = SRBDatesIn,
                                            radix = radix,
                                            verbose = FALSE)

      BPBest5F <-  BPBestList[[1]]  
      BPBest5M <-  BPBestList[[2]]  
      
      Age5 <- as.integer(names(BPBest5F))
      # BPBestSplitF <- DemoTools::graduate_beers(BPBest5F, Age = Age5, method="ord", johnson=TRUE)
      # BPBestSplitM <- DemoTools::graduate_beers(BPBest5M, Age = Age5, method="ord", johnson=TRUE)
      # #develop final adjusted estimates (<10) combined with BestMav
      # BPfinalF <- FinalBP(BestPopF, BPBestSplitF)
      # BPfinalM <- FinalBP(BestPopM, BPBestSplitM)
      
      # Peter says better to select on grouped five-year data and then graduate_mono to single year of age
      BPfinalF <- FinalBP5(BestPop5F, BPBest5F)
      BPfinalM <- FinalBP5(BestPop5M, BPBest5M)
      BPFinalSplitF <- DemoTools::graduate_mono(BPfinalF, Age = Age5)
      BPFinalSplitM <- DemoTools::graduate_mono(BPfinalM, Age = Age5)
      
      # output information that can be appended to SummaryTbl and Pop1Compare
      BestSmthAdjBPLabel <- c(NA,NA)
      BestSmthAdjBP <- Pop1Compare[,c("SexID","AgeStart")]
      BestSmthAdjBP$BestSmthAdjBP <- NA
      
      for (sex in c(1,2)) {
        
        if (sex == 1) {
          bp_final <- BPFinalSplitM
          best_pop <- BestPopM
        } else {
          bp_final <- BPFinalSplitF
          best_pop <- BestPopF
        }
        
        if (bp_final[1] <= best_pop[1]) { # BP est too low, so use original mav data
          BestSmthAdjBPLabel[sex] <- BestLabel
          BestSmthAdjBP$BestSmthAdjBP[BestSmthAdjBP$SexID == sex] <- Pop1Compare$BestSmthAdj[Pop1Compare$SexID == sex]
        } else { # use the final BP values
          BestSmthAdjBPLabel[sex] <- paste0(BestLabel, "BP") 
          BestSmthAdjBP$BestSmthAdjBP[BestSmthAdjBP$SexID == sex] <- bp_final
        }
        
      }
      
      out.data <- list(BestSmthAdjBPLabel = BestSmthAdjBPLabel,
                       BestSmthAdjBP = BestSmthAdjBP)
      
      return(out.data)
      
}

# this is for use on graduated data
FinalBP <- function(Adj, BP) {
  
  BPG <- TRUE
  f <- Adj
  
  for (i in 1:10) {
    
    if (BPG) {
      if (BP[i] > Adj[i]) {
        f[i] <- BP[i]
        
      } else {
        BPG <- FALSE
      }
    }
  }
  f
}

# use this one for grouped 5-year data
FinalBP5 <- function(Adj, BP) {
  
  BPG <- TRUE
  f <- Adj
  
  for (i in 1:2) {
    
    if (BPG) {
      if (BP[i] > Adj[i]) {
        f[i] <- BP[i]
        
      } else {
        BPG <- FALSE
      }
    }
  }
  f
}

