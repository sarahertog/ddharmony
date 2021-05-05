
# Adjust census populations per pes or user-defined adjustment factors

census_workflow_adjust_pes <- function(Age, 
                                       popF, 
                                       popM,
                                       census_reference_date,
                                       NCE_total = NULL, # overall adjustment factor
                                       NCE_m = NULL, # age-specific adjustment relative to total for males
                                       NCE_f = NULL, # age-specific adjustment relative to total for females
                                       NCE_age = NULL) { # ages associated with age-specific adjustments   

    # detect age structure of input data
    sgl <- DemoTools::is_single(Age)
    abr <- DemoTools::is_abridged(Age)

    # if only five year values, then collapse the adjustment for the first age group
    if (!sgl & !abr) { 
      NCE_m <- c(0.2 * NCE_m[1] + 0.8 * NCE_m[2], NCE_m[3:length(NCE_m)])
      NCE_f <- c(0.2 * NCE_f[1] + 0.8 * NCE_f[2], NCE_f[3:length(NCE_f)])
      NCE_age <- c(0, seq(5, max(NCE_age), 5))
    }

    maxage <- max(Age)
    
    # truncate sex-age specific nce diff to the ages of the available population data
    NCE_m   <- c(NCE_m[NCE_age < maxage], mean(NCE_m[NCE_age >= maxage]))
    NCE_f   <- c(NCE_f[NCE_age < maxage], mean(NCE_f[NCE_age >= maxage]))
    
    # PES adjustment

      # compute age- and sex-specific adjustment factors
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
      
      
      out.data <- data.frame(Age = Age,
                             popF = pop_f_adj,
                             popM = pop_m_adj)
      
      return(out.data)
      
} 
      
      
   