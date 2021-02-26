## --------------------------------------------------------------------------------------------
## This function:
##  i) Loads and manipulates all the datasets.
## ii) Generates the age specific birth rate estimates.
## ii) Outputs the final dataset which contains the pop counts, birth counts and estimates for each
##     unique set of records.
## iii) Generates another dataset that only contains the age specific birth rate estimates.
## --------------------------------------------------------------------------------------------

binder_complete <- function(lid){
  
  options(scipen = 99999)  
  
  ### 0.) Source the functions--------------------------------------------
  source("Shel/functions/pkg_load.R")
  source("Shel/functions/births_load.R")
  source("Shel/functions/pop_complete_load.R")
  source("Shel/functions/estimates_complete_func.R")
  
  ### 1.) Load the packages that are required.--------------------------------------------
  
  pkg_load()
  
  ### 2.) Load location data --------------------------------------
  
  load("Shel/data/Locations.RData", envir = .GlobalEnv)
  
  ### 3). Specify the location  --------------------------------------
  
  lid <- lid

  ### 4.) Load births data  --------------------------------------
  
  births_load(lid)
  
  ### 5.) Load the 5-year population data  --------------------------------------
  
  pop_complete_load(lid)
  
  
  ### 6.) Generate a data set that contains the population counts, the birth counts and the asbr estimates  --------------------------------------
  
  if(!exists("births_vr")){
    df_vr <- data.frame()
  }else
    if(exists("births_vr") & any(class(births_vr) %in% "data.frame")){ 
      df_vr <-estimates_complete_func(births_vr)
      df_vr$process <- "vr"  
    }else
      df_vr <- data.frame() 
    
    if(!exists("births_census")){
      df_census <- data.frame()
    }else
      if(exists("births_census") & any(class(births_census) %in% "data.frame")){ 
        df_census <- estimates_complete_func(births_census)
        df_census$process <- "census"  
      }else
        df_census <- data.frame() 
      
      
      final_df <- bind_rows(df_vr, df_census)
      
      ### 7.) Re-order the variables
      vars <- c(paste(0:49), "50", "50+", paste(51:99), "100", "100+")
      vars2 <- grep("\\d", names(final_df), value = TRUE)
      vars2 <- vars2[!vars2 %in% "ISO3Alpha"]
      vars3 <-  vars2[!vars2 %in% vars]
      if(nrow(final_df) >0){
        
        final_df <- final_df %>% 
          select(LocID, LocName, id, process, TimeLabel:category, any_of(vars) ,any_of(vars3),  Total)
        
        ## Fill in the missing LocNames
        final_df <- final_df %>% 
          mutate(LocName = ifelse(is.na(LocName), unique(LocName[!is.na(LocName)]), LocName))
        
      }
      assign("complete_final_df", final_df , envir = .GlobalEnv)
      
      ### 8.) Extract the final estimates.  --------------------------------------
      
      if(nrow(final_df) >0){
        
        asbr_df <- final_df %>% 
          filter(category == "asbr")
      }
      assign("complete_estimates_df", asbr_df , envir = .GlobalEnv)
}
