

census_cohort_interpolation_1x1_wrapper <- function (locname, census_output, refdates, midyear = FALSE) {

library(lubridate)

# loop through all of the available censuses
icm_all <- NULL
icf_all <- NULL
for (i in 1:(length(refdates)-1)) {
  
  # parse reference dates of first and second censuses
  date1 <- date_decimal(refdates[i])
  date2 <- date_decimal(refdates[i+1])
  
  # convert to data format required by interp_coh
  date1 <- format(as.POSIXct(date1,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
  date2 <- format(as.POSIXct(date2,format='%Y/%m/%d %H:%M:%S'),format='%Y-%m-%d')
  
  # parse the male and female populations by single year of age 0:100 for the two censuses
  popm1 <- dplyr::filter(census_output, census_reference_date == refdates[i] & SexID == 1)$DataValue
  popm2 <- dplyr::filter(census_output, census_reference_date == refdates[i+1] & SexID == 1)$DataValue
  popf1 <- dplyr::filter(census_output, census_reference_date == refdates[i] & SexID == 2)$DataValue
  popf2 <- dplyr::filter(census_output, census_reference_date == refdates[i+1] & SexID == 2)$DataValue
  
  # interpolate males
  icm <- DemoTools::interp_coh(
    country = locname,
    sex = "male",
    c1 = popm1,
    c2 = popm2,
    date1 = date1,
    date2 = date2,
    age1 = 0:(length(popm1)-1),
    age2 = 0:(length(popm2)-1),
    midyear = midyear)
  icm_all <- cbind(icm_all, icm)

  # interpolate females
  icf <- DemoTools::interp_coh(
    country = locname,
    sex = "female",
    c1 = popf1,
    c2 = popf2,
    date1 = date1,
    date2 = date2,
    age1 = 0:(length(popf1)-1),
    age2 = 0:(length(popf2)-1),
    midyear = midyear)
  icf_all <- cbind(icf_all, icf)
  
}

ic_all <- list(icm_all = icm_all,
               icf_all = icf_all)

return(ic_all)

}



# right now the function outputs matrices, which is what is needed for the residual migration functions
# could tranform to long data frames with the below code
# ic_m$AgeStart <- 0:100
# # make long
# ic_m <- reshape(ic_m, idvar = "AgeStart", v.names = "DataValue", timevar = "Year", times = names(ic_m)[1:(ncol(ic_m)-1)],
#                  varying = list(1:(ncol(ic_m)-1)), direction = "long")
# ic_m$SexID <- 1


