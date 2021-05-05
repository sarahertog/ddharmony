# return TRUE/FALSE for whether census extract has a single year of age series with open age group 50 or higher

census_has_single_oag50 <- function(indata) {
  
  df <- indata %>% 
    dplyr::filter(complete == TRUE)
  
  has_single_oag50 <- FALSE
  if (nrow(df) > 1) {
    has_single_oag50 <- max(df$AgeStart) >= 50
  }
  
  return(has_single_oag50)
}

# return TRUE/FALSE for whether census extract has a five-year grouped age series with open age group 50 or higher

census_has_five_year_oag50 <- function(indata) {
  
  df <- indata %>% 
    dplyr::filter(five_year == TRUE)
  
  has_five_year_oag50 <- FALSE
  if (nrow(df) > 1) {
    has_five_year_oag50 <- max(df$AgeStart) >= 50
  }

  return(has_five_year_oag50)
  
}


# return TRUE/FALSE for whether census extract has an abridged grouped age series with open age group 50 or higher

census_has_abridged_oag50 <- function(indata) {
  
  df <- indata %>% 
    dplyr::filter(abridged == TRUE)

  abr <- "1-4" %in% df$AgeLabel
  
  has_abridged_oag50 <- FALSE
  if (abr & nrow(df) > 1) {
    has_abridged_oag50 <- max(df$AgeStart) >= 50
  }
  
  return(has_abridged_oag50)
  
}
