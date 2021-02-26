## --------------------------------------------------------------------------------
## Load and manipulate the 5 year population data
## --------------------------------------------------------------------------------

pop_complete_load <- function(lid){
  
  pop1_df0 <- read_xlsx("Shel/data/WPP2019_INT_F03_3_POPULATION_BY_AGE_ANNUAL_FEMALE.xlsx")
  
  pop1_df <- pop1_df0[-1:-11, ] 
  
  colnames(pop1_df) <- pop1_df[1,]
  pop1_df <- pop1_df[-1, ]
  
  agelabels <- grep("\\d", names(pop1_df), value = TRUE)
  agelabels <- agelabels[!agelabels %in% "Reference date (as of 1 July)"]
  
  pop1_df <- pop1_df %>% 
    mutate(`Country code` = as.numeric(trimws(`Country code`))) %>% 
    gather("AgeLabel_denom", "pop.count", agelabels) %>% 
    rename(LocID = `Country code`,
           TimeLabel = `Reference date (as of 1 July)`) 
  
  ## Deal with scientific notation
  pop1_df <- pop1_df %>% 
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
  
assign("pop_complete_df", pop1_df , envir = .GlobalEnv)

}