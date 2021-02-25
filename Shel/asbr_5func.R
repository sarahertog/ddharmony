asbr_5func <- function(lid){
  
#### 0.) Source the functions--------------------------------------------
source("Shel/pkg_load.R")
source("Shel/births_load.R")
source("Shel/pop_load.R")
source("Shel/estimates_func.R")

#### 1.) Load the packages that are required.--------------------------------------------

pkg_load()

#### 2.) Load location data --------------------------------------

load("Shel/Locations.RData", envir = .GlobalEnv)

#### 3). Specify the location  --------------------------------------

lid <- lid

#### 3.) Load births data  --------------------------------------

births_load()

#### 4.) Load the 5-year population data  --------------------------------------

pop_load()


#### 5. Generate a data set that contains the population counts, the birth counts and the asbr estimates  --------------------------------------

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

## order the variables
vars <- c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30+" , "30-34","35-39","40+", "40-44", "45+", "45-49", "50+", "50-54", "55+", "55-59", "60+", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+")
vars <- grep("\\d", names(final_df), value = TRUE)
vars <- vars[!vars %in% "ISO3Alpha"]

final_df <- final_df %>% 
  select(id, TimeLabel:category, any_of(vars) ,  Total)

assign("final_df", final_df , envir = .GlobalEnv)

#### 6.) Extract the final estimates.  --------------------------------------

asbr_df <- final_df %>% 
             filter(category == "asbr")

assign("estimates_df", asbr_df , envir = .GlobalEnv)
}

lid <- 710
asbr_5func(lid)