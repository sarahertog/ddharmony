## --------------------------------------------------------------------------------------------
## This function:
##  i) Loads and manipulates all the datasets.
## ii) Generates the age specific birth rate estimates.
## ii) Outputs the final dataset which contains the pop counts, birth counts and estimates for each
##     unique set of records.
## iii) Generates another dataset that only contains the age specific birth rate estimates.
## --------------------------------------------------------------------------------------------

binder_5 <- function(lid){

options(scipen = 99999)  
  
### 0.) Source the functions--------------------------------------------
source("Shel/functions/pkg_load.R")
source("Shel/functions/births_load.R")
source("Shel/functions/pop_load.R")
source("Shel/functions/estimates_func.R")

### 1.) Load the packages that are required.--------------------------------------------

pkg_load()

### 2.) Load location data --------------------------------------

load("Shel/data/Locations.RData", envir = .GlobalEnv)

### 3). Specify the location  --------------------------------------

lid <- lid

### 4.) Load births data  --------------------------------------

births_load()

### 5.) Load the 5-year population data  --------------------------------------

pop_load()


### 6.) Generate a data set that contains the population counts, the birth counts and the asbr estimates  --------------------------------------

if(!exists("births_vr")){
  df_vr <- data.frame()
}else
  if(exists("births_vr") & any(class(births_vr) %in% "data.frame")){ 
    df_vr <- estimates_func(births_vr)
    df_vr$process <- "vr"  
  }else
    df_vr <- data.frame() 

if(!exists("births_census")){
  df_census <- data.frame()
}else
  if(exists("births_census") & any(class(births_census) %in% "data.frame")){ 
    df_census <- estimates_func(births_census)
    df_census$process <- "census"  
  }else
    df_census <- data.frame() 


final_df <- bind_rows(df_vr, df_census)

### 7.) Re-order the variables
vars <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30+" , "30-34","35-39","40+", "40-44", "45+", "45-49", "50+", "50-54", "55+", "55-59", "60+", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")
vars <- grep("\\d", names(final_df), value = TRUE)
vars <- vars[!vars %in% "ISO3Alpha"]

if(nrow(final_df) >0){

final_df <- final_df %>% 
  select(id, process, TimeLabel:category, any_of(vars) ,  Total)

}
assign("final_df", final_df , envir = .GlobalEnv)

### 8.) Extract the final estimates.  --------------------------------------

if(nrow(final_df) >0){
  
asbr_df <- final_df %>% 
             filter(category == "asbr")
}
assign("estimates_df", asbr_df , envir = .GlobalEnv)
}

