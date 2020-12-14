# ddharmony

## Load ddharmony functions and return harmonized, validated census population counts by sex and
## abridged/single-year age groups

## requires packages tidyverse, devtools, httr, DDSQLtools, DemoTools

## This set of functions:
## 1. Extracts census population counts (abridged age groups and single year of age ("complete"))
## 2. Harmonizes into standard abridged and complete series
## 3. Validates totals over age and by sex
## 4. Selects the most authoratative, based on 
##    a) prefer "Final" data status
##    b) the availability of a complete series
##    c) better data reliability
##    d) De facto preferred over De jure
##    e) Has the highest available start age for the last age group
##    f) if still more than one available, prefer the Demographic Yearbook sourse

## Returns a dataframe with one abridged or complete series per ReferencePeriod (census year) and SexID

## Source all of the required functions from Sara's temporary github repository 
## ! very much under development -- we will be revising to deal with some tricky cases as they arise

  require(devtools)
  require(httr)
  
  req <- GET("https://api.github.com/repos/sarahertog/ddharmony/git/trees/main?recursive=1")
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  
  for (filename in filelist) {
    
    one_function <- paste0("https://github.com/sarahertog/ddharmony/blob/main/", filename, "?raw=TRUE")
    source_url(one_function)
    rm(one_function)
  }
  rm(req, filelist, filename)

## obtain harmonized, validated census population counts by age and sex for one country
  census_pop_counts <- DDharmonize_validate_PopCounts(locid = 404,       # LocID for country
                                                      times = 1950:2020) # time frame for censuses to extract
