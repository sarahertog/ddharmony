## --------------------------------------------------------------------------------
## Load the packages that are required.
## --------------------------------------------------------------------------------

pkg_load <- function(){

###  create a vector of packages to be installed
pkgs <- c("tidyverse","data.table","DT","lubridate", "readxl","googlesheets4",
          "openxlsx", "kableExtra","zoo", "stringr", "EnvStats", "trend", "beepr",
          "devtools","httr")

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


}