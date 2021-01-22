# distribute births to ages 10-14 across single ages
# abritrary distribution of births across ages 10-14 for now. Need to update with Kirill's regression

dd_births_abridged2single_1014 <- function(indata, births10_14) {

    df <- indata
    births_single_1014 <- births10_14 * c(0,0,0.05,0.25,0.70) 
    for (j in 5:1) {
      df_add <- df[df$AgeLabel == 15,] %>% 
        mutate(AgeStart = 9+j,
               AgeEnd = 10+j,
               AgeLabel = AgeStart,
               DataSourceYear = NA,
               DataValue = births_single_1014[j])
      
      df <- rbind(df_add, df)
      
    }
    
    return(df)

}