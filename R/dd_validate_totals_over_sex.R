#' @title
#' dd_validate_totals_over_sex
#'
#' @description
#' Check if population counts by sex are equal to population counts for both sexes and adjust in case of any difference.
#' Criteria: If Female Population + Male Population is different from the population for Both sexes, then Both sexes = Female + Male
#'
#' @details
#' See the \href{https://shelmith-kariuki.github.io/rddharmony/articles/dd_validate_totals_over_sex.html}{"Validating totals over sex" vignette} for more details about this function.
#'
#' @param data Data to be harmonized
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
#'
#' @return A dataset where Both sexes == Female + Male for each id and SexId.
#'
#' @export
#' @examples
#' \dontrun{
#' df <- validate_totals_over_sex
#' df <- dd_validate_totals_over_sex(df)
#'}
dd_validate_totals_over_sex <- function(data){

  # abridged
  df_abr <- data %>%
    dplyr::filter(abridged == TRUE) %>%
    select(-series, -note)

  if (nrow(df_abr) > 0) {

  sexes <- unique(df_abr$SexID)

  # if has data for males and females, ensure total is sum of both sexes
  if (all(c(1,2) %in% sexes)) {

    full_m <- dd_series_isfull(data = df_abr[df_abr$SexID == 1,], abridged = TRUE)
    full_f <- dd_series_isfull(data = df_abr[df_abr$SexID == 2,], abridged = TRUE)

    # Shel replaced == with %in% to remove the following warning, in cases where not all female age labels are
    # present in the male records:
    # Warning message:
    #   In df_abr$AgeLabel[df_abr$SexID == 1] == df_abr$AgeLabel[df_abr$SexID ==  :
    #                                                              longer object length is not a multiple of shorter object length
    # ages_same <- all(df_abr$AgeLabel[df_abr$SexID == 1] == df_abr$AgeLabel[df_abr$SexID == 2])
    # Case: 666: Saint Pierre, deaths

    ages_same <- all(df_abr$AgeLabel[df_abr$SexID == 1] %in% df_abr$AgeLabel[df_abr$SexID == 2])


    if (all(full_m, full_f, ages_same)) {
    df_abr <- df_abr %>%
      mutate(SexID = paste0("X", SexID)) %>%
      spread(key = SexID, value = DataValue) %>%
      mutate(X3 = X1 + X2) %>% # ensure that both sexes is sum of male and female
      gather(key = "SexID", value = "DataValue", c("X1","X2","X3")) %>%
      mutate(SexID = as.numeric(substr(SexID,2,2)),
             note = NA) %>%
      dplyr::filter(!is.na(DataValue)) %>% # need this bc NAs are introduced if both sexes has more age groups than by sex
      arrange(SexID, AgeSort)
    } else {
      df_abr <- df_abr %>%
        mutate(note = "Inconsistent age groups for males and females; Both sexes not reconciled")
    }

    rm(full_m, full_f, ages_same)
  }
  if (!("note" %in% names(df_abr))) {
    df_abr$note <- NA
  }
  }


  # complete
  df_cpl <- data %>%
    dplyr::filter(complete == TRUE) %>%
    select(-series, -note)

  if (nrow(df_cpl) > 0) {

  sexes <- unique(df_cpl$SexID)

  # if has data for males and females, ensure total is sum of both sexes
  if (all(c(1,2) %in% sexes)) {

    full_m <- dd_series_isfull(data = df_cpl[df_cpl$SexID == 1,], abridged = FALSE)
    full_f <- dd_series_isfull(data = df_cpl[df_cpl$SexID == 2,], abridged = FALSE)


    # Shel replaced == with %in% to remove the following warning, in cases where not all female age labels are
    # present in the male records:
    # Warning message:
    # In df_cpl$AgeLabel[df_cpl$SexID == 1] == df_cpl$AgeLabel[df_cpl$SexID ==  :
    #                                                            longer object length is not a multiple of shorter object length
    # ages_same <- all(df_abr$AgeLabel[df_abr$SexID == 1] == df_abr$AgeLabel[df_abr$SexID == 2])
    # Case: 414: Kuwait, deaths

    # ages_same <- all(df_cpl$AgeLabel[df_cpl$SexID == 1] == df_cpl$AgeLabel[df_cpl$SexID == 2])
    ages_same <- all(df_cpl$AgeLabel[df_cpl$SexID == 1] %in% df_cpl$AgeLabel[df_cpl$SexID == 2])

    if (all(full_m, full_f, ages_same)) {
      df_cpl <- df_cpl %>%
        mutate(SexID = paste0("X", SexID)) %>%
        spread(key = SexID, value = DataValue) %>%
        mutate(X3 = X1 + X2) %>% # ensure that both sexes is sum of male and female
        gather(key = "SexID", value = "DataValue", c("X1","X2","X3")) %>%
        mutate(SexID = as.numeric(substr(SexID,2,2)),
               note = NA) %>%
        dplyr::filter(!is.na(DataValue)) %>% # need this bc NAs are introduced if both sexes has more age groups than by sex
        arrange(SexID, AgeSort)
    } else {
      df_cpl <- df_cpl %>%
        mutate(note = "Inconsistent age groups for males and females; Both sexes not reconciled")
    }
    rm(full_m, full_f, ages_same)
  }
  if (!("note" %in% names(df_cpl))) {
    df_cpl$note <- NA
  }
  }


  data.out <- rbind(df_abr, df_cpl)

  return(data.out)
}

