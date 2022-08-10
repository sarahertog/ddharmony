#' print_dropped_ids
#'
#' Print ids that have been dropped, i.e those that are not full series

#' @param indata_before vitals_std_all
#' @param indata_after vitals_std_full
#'
#' @return ids that have been dropped because they result in data that is not full
#' @export
#' @keywords internal
print_dropped_ids<- function(indata_before, indata_after){

  dropped_id_series <- unique(indata_before$id_series[!indata_before$id_series  %in%  indata_after$id_series])

  if(length(dropped_id_series) >0 ){

  # universal_agelabels <- c("0", "1-4", "0-4", "5-9", "10-14","15-19", "20-24","25-29", "30-34", "35-39", "40-44", "45-49","50+","Total","Unknown")
  universal_agelabels_abr <- std_age_function() %>%
    filter(AgeSort <= 12 |
             AgeSort == 167) %>%
    distinct(AgeLabel) %>%
    pull()

  universal_agelabels_comp <- std_age_function() %>%
    filter((AgeSort >= 29 & AgeSort <= 77)|
             AgeSort == 167) %>%
    distinct(AgeLabel) %>%
    pull()

  for (i in 1: length(dropped_id_series)){
    series_df <- indata_before %>%
      filter(id_series == dropped_id_series[i])

    available_agelabels <- series_df %>%
      distinct(AgeLabel) %>%
      pull()

    if (length(unique(series_df$series)) == 2){
      universal_agelabels <- c(universal_agelabels_abr, universal_agelabels_comp)
      missing_agelabels <- universal_agelabels[!universal_agelabels %in% available_agelabels]

      print(paste0("id: ", dropped_id_series[i]))
      print(unique(series_df$series))
      print(paste0("Age labels present: ", paste0(available_agelabels, collapse = " , ")))
      print(paste0("Age labels missing: ", paste0(missing_agelabels, collapse = " , ")))

      cat("\n")
    }else
      if(length(unique(series_df$series)) == 1 &
         (series_df$series == "abridged" | series_df$series == "abridged reconciled with complete")){
        missing_agelabels <- universal_agelabels_abr[!universal_agelabels_abr %in% available_agelabels]

        print(paste0("id: ", dropped_id_series[i]))
        print(unique(series_df$series))
        print(paste0("Age labels present: ", paste0(available_agelabels, collapse = " , ")))
        print(paste0("Age labels missing: ", paste0(missing_agelabels, collapse = " , ")))

        cat("\n")
      }else{
        if(length(unique(series_df$series)) == 1 &
           (series_df$series == "abridged" | series_df$series == "abridged reconciled with complete")){
          missing_agelabels <- universal_agelabels_comp[!universal_agelabels_comp %in% available_agelabels]

          print(paste0("id: ", dropped_id_series[i]))
          print(unique(series_df$series))
          print(paste0("Age labels present: ", paste0(available_agelabels, collapse = " , ")))
          print(paste0("Age labels missing: ", paste0(missing_agelabels, collapse = " , ")))

          cat("\n")
        }

      }
  }

  cat("\n")
  print(paste0(length(dropped_id_series), " out of ", length(unique(indata_before$id_series)), " have been dropped at this point, resulting into ",
               nrow(indata_before) - nrow(indata_after), " records out of ",  nrow(indata_before), " dropped ", "(", round(((nrow(indata_before) - nrow(indata_after))/nrow(indata_before))*100,1)  ,"%)"))

}else{
    "No records were dropped"
  }


}
