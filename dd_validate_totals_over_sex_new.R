#----------------------------------------------------------------------
# Compute total over sex as max of reported total or sum over sex
# compute any NA values based on differences
# redistribute SexID = 0 across males and females

# ---------
# Function
# ---------
dd_validate_totals_over_sex_new <- function(data){
  
  df <- data %>% 
    mutate(SexID = paste0("X", SexID)) %>% 
    spread(key = SexID, value = DataValue)

  if (!("X0" %in% names(df))) {
    df$X0 <- 0
  } 
  if (!("X1" %in% names(df))) {
    df$X1 <- 0
  } 
  if (!("X2" %in% names(df))) {
    df$X2 <- 0
  } 
  if (!("X3" %in% names(df))) {
    df$X3 <- 0
  } 

  data.out <- df %>% 
    mutate(X0 = replace(X0, is.na(X0), 0),
           X1 = replace(X1, is.na(X1), 0),
           X2 = replace(X2, is.na(X2), 0),
           X3 = replace(X3, is.na(X3), 0),
           total_over_sex = X0 + X1 + X2,
           X3 = ifelse(X3 > total_over_sex, X3, total_over_sex), # total as max of reported total or sum over sex
           X1 = ifelse(X1 == 0 & X3 > total_over_sex, X3 - total_over_sex, X1),
           total_over_sex = X0 + X1 + X2,
           X2 = ifelse(X2 == 0 & X3 > total_over_sex, X3 - total_over_sex, X2),
           total_over_sex = X0 + X1 + X2,
           X0 = ifelse(X3 > total_over_sex, X0 + (X3 - total_over_sex), X0 ),
           pct_m = X1 / (X1 + X2), # percentage of males among males plus females
           pct_m = replace(pct_m, is.na(pct_m), 0), # avoids divide by zero problems
           X1 = X1 + (X0 * pct_m),
           X2 = X2 + (X0 * (1-pct_m)),
           X3 = X1 + X2) %>% 
    select(-X0, -total_over_sex, -pct_m) %>% 
    gather(key = "SexID", value = "DataValue", c("X1","X2","X3")) %>% 
    mutate(SexID = as.numeric(substr(SexID,2,2)), 
           note = NA) %>% 
    arrange(SexID, AgeSort)
  
  
 return(data.out)
}

