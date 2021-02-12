## Date: Feb 4th
## Author: Shelmith Kariuki
## Description: Calculation of 1-year and 5-year age specific birth rates

## 0. Load the packages required -------------------------------

###  create a vector of packages to be installed
pkgs <- c("devtools","httr", "tidyverse", "readxl", "zoo")

###  Check if there are packages you want to load, that are not already installed. 
miss_pkgs <- pkgs[!pkgs %in% installed.packages()[,1]] 

###  Installing the missing packages
if(length(miss_pkgs)>0){
  install.packages(miss_pkgs)
}

###  Loading all the packages
invisible(lapply(pkgs,library,character.only=TRUE))
library(DDSQLtools)

###  Remove the objects that are no longer required
rm(miss_pkgs)
rm(pkgs)

## 1. Load the 5-year population data -------------------------------
pop5_df <- read_xlsx("Shel/WPP2019_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE.xlsx")
pop5_df <- pop5_df[-1:-11, ] 

colnames(pop5_df) <- pop5_df[1,]
pop5_df <- pop5_df[-1, ]

## 2. Reshape the data so that we have age labels on one column, and population counts in another, and rename the agelabel variable to AgeLabel_denom.-------
pop5_df2 <- pop5_df %>% 
  gather("AgeLabel_denom", "pop.count", contains("-"), contains("+")) %>% 
  rename(LocID = `Country code`,
         TimeLabel = `Reference date (as of 1 July)`) %>% 
  filter(LocID == 608)

## Deal with scientific notation
pop5_df2 <- pop5_df2 %>% 
  mutate(sci_exists = ifelse(pop.count %in% grep("E", pop.count, value = T, ignore.case = FALSE),
                             1, 0),
         sci_not = ifelse(sci_exists == 1, as.numeric(trimws(gsub(".*E", "", pop.count))), NA),
         pop.count = ifelse(sci_exists == 1, trimws(gsub("E.*", "", pop.count)), trimws(pop.count)),
         pop.count = as.numeric(pop.count)) %>% 
  mutate(pop.count = ifelse(sci_exists == 1, pop.count * 10^sci_not, pop.count)) %>% 
  select(-sci_exists, -sci_not)

## 3. Load location data
load("Shel/Locations.RData")

## 4. Load births data -------------------------------------------------------------------

req <- GET("https://api.github.com/repos/sarahertog/ddharmony/git/trees/main?recursive=1")
stop_for_status(req)
filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)

for (filename in filelist) {
  
  one_function <- paste0("https://github.com/sarahertog/ddharmony/blob/main/", filename, "?raw=TRUE")
  source_url(one_function)
  rm(one_function)
}
rm(req, filelist, filename)


# Births by age of mother per Philippines VR
philippines_vr <- DDharmonize_validate_BirthCounts(locid = 608, process = "vr", times = 1950:2020, retainKeys = TRUE)

## 5. Merge this data with the Location data  -------------------------------------------------------------------
philippines_vr2 <- Locations %>% 
  right_join(.,philippines_vr , by = c("LocID", "LocName")) %>% 
  rename(AgeLabel_num = AgeLabel)

## 5. Extract the 5-year counts and rename the agelabel variable to AgeLabel_num.-------------------------------
philippines_vr2_5 <- philippines_vr2 %>% 
  filter(five_year == TRUE)

## Generate a variable that shows the maximum age label
philippines_vr2_5 <- philippines_vr2_5 %>% 
  separate(AgeLabel_num, into = c("ll", "ul"), sep = "-", remove = FALSE) %>% 
  mutate(ul = ifelse(is.na(ul) & 
                       AgeLabel_num %in% grep("\\d+", AgeLabel_num, value = TRUE), AgeLabel_num, ul)) %>% 
  mutate(ul = as.numeric(trimws(gsub("\\+", "", ul)))) %>% 
  group_by(id) %>% 
  mutate(max_age = ifelse(any(ul == max(ul, na.rm = TRUE)), 
                          AgeLabel_num[ul == max(ul, na.rm = TRUE)], NA)) %>% 
  ungroup()

## 6. Merge the population data with the births data.----------------------------------------------------------- 
merged_df <- pop5_df2 %>% 
  mutate(LocID = as.numeric(trimws(LocID)),
         TimeLabel = as.numeric(trimws(TimeLabel))) %>% 
  full_join(., philippines_vr2_5, by = c("LocID", "TimeLabel", "AgeLabel_denom" = "AgeLabel_num")) %>% 
  rename(births.count = DataValue, 
         AgeLabel = AgeLabel_denom) %>% 
  relocate(c(TimeLabel, AgeLabel, pop.count, births.count), .after = last_col()) %>% 
  arrange(TimeLabel)

## Fill in the missing values
merged_df <- merged_df %>% 
  mutate(id = na.locf0(id, fromLast = TRUE),
         max_age = na.locf0(max_age, fromLast = TRUE))

## Split age labe into ll and ul. 
merged_df <- merged_df %>% 
  separate(AgeLabel, into = c("ll", "ul"), sep = "-", remove = FALSE) %>% 
  mutate(ul = ifelse(is.na(ul) & 
                       AgeLabel %in% grep("\\d+", AgeLabel, value = TRUE), AgeLabel, ul)) %>% 
  mutate(max_age2 = max_age) %>% 
  mutate(across(c(max_age2, ll, ul), ~as.numeric(trimws(gsub("\\+", "", .))))) %>% 
  mutate(juncture = ifelse(ul >= max_age2, 1, 0)) %>% 
  mutate(AgeLabel = ifelse(juncture == 1 & AgeLabel!= "Total", max_age, AgeLabel )) %>% 
  group_by(id) %>% 
  mutate(pop.count = ifelse(juncture == 1, sum(pop.count[juncture == 1], na.rm = TRUE),
                            pop.count)) %>% 
  ungroup()

## Drop the ages that have been collapsed into a smaller bracket
merged_df <- merged_df %>% 
  mutate(todrop = ifelse(AgeLabel == max_age & is.na(births.count) & !is.na(pop.count),
                         "drop", "keep")) %>% 
  filter(todrop == "keep") %>% 
  group_by(id) %>% 
  mutate(pop.count = ifelse(is.na(pop.count), sum(pop.count, na.rm = TRUE), pop.count)) %>% 
  select(-ll, -ul, -max_age, -juncture, -todrop, -max_age2) %>% 
  ungroup()
  

## 7. Calculate the age specific birth rates for each of the years and age labels.-------------------------------
merged_df <- merged_df %>% 
  mutate(asbr = births.count / pop.count)

## 8. Reshape the data so that we have category on one variable (pop.count, births.count, asbr).-------------------------------


## 9. Extract each of the datasets i.e pop_df, births_df, asbr_df.-------------------------------