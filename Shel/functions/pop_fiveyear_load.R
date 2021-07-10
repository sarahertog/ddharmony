## --------------------------------------------------------------------------------
## Load and manipulate the 5 year population data
## --------------------------------------------------------------------------------

pop_fiveyear_load <- function(lid){

pop5_df0 <- read_xlsx("Shel/data/WPP2019_POP_F15_3_ANNUAL_POPULATION_BY_AGE_FEMALE.xlsx")
pop5_df <- pop5_df0[-1:-11, ] 

colnames(pop5_df) <- pop5_df[1,]
pop5_df <- pop5_df[-1, ]


#### Reshape the data so that we have age labels on one column, and population counts in another, and rename the agelabel variable to AgeLabel_denom.

pop5_df <- pop5_df %>% 
  mutate(`Country code` = as.numeric(trimws(`Country code`))) %>% 
  gather("AgeLabel_denom", "pop.count", contains("-"), contains("+")) %>% 
  rename(LocID = `Country code`,
         TimeLabel = `Reference date (as of 1 July)`) %>%  
  ## Deal with scientific notation
  mutate(sci_exists = ifelse(pop.count %in% grep("E", pop.count, value = T, ignore.case = FALSE),
                             1, 0),
         sci_not = ifelse(sci_exists == 1, as.numeric(trimws(gsub(".*E", "", pop.count))), NA),
         pop.count = ifelse(sci_exists == 1, trimws(gsub("E.*", "", pop.count)), trimws(pop.count)),
         pop.count = as.numeric(pop.count)) %>% 
  mutate(pop.count = ifelse(sci_exists == 1, pop.count * 10^sci_not, 
                            pop.count)) %>% 
  select(-sci_exists, -sci_not) %>% 
  arrange(TimeLabel) %>% 
  filter(LocID == lid)

assign("pop_fiveyear_df", pop5_df , envir = .GlobalEnv)

}