
# Adjust census populations using results of net census enumeration from PES models

censusPop_adjust_nce <- function(popM, 
                                 popF,
                                 Age,
                                 NCE_total,
                                 NCE_m,
                                 NCE_f) { 

    nAge <- length(Age)
    stopifnot(length(popM) == nAge &  length(popF) == nAge & length(NCE_m) == nAge & length(NCE_f) == nAge)
      
    maxage <- max(Age)
    
    stopifnot(maxage >= 50)

    # PES adjustment

      NCEAge_m <- NCE_total/100 + NCE_m/100
      NCEAge_f <- NCE_total/100 + NCE_f/100
      
      # adjust population
      pop_m_adj <- popM  / (1 + NCEAge_m)
      pop_f_adj <- popF  / (1 + NCEAge_f)
      
      # totals
      TotBSpopAdj <- sum(pop_m_adj + pop_f_adj)
      TotBStargetPop <- sum(popM + popF) / (1 + NCE_total/100)
      Adj2factor <- TotBStargetPop / TotBSpopAdj
      
      # adjust again per second adjustment factor
      pop_m_adj <- pop_m_adj * Adj2factor
      pop_f_adj <- pop_f_adj * Adj2factor
      
      
      out.data <- list(pop_m_adj = pop_m_adj,
                       pop_f_adj = pop_f_adj)
      
      return(out.data)
      
} 
      
      
   