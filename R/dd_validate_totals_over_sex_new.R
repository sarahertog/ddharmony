#' @title
#' dd_validate_totals_over_sex_new
#'
#' @description
#' Compute total over sex as max of reported total or sum over sex, compute any NA values based on differences,
#' redistribute SexID = 0 across males and females then validate totals over sex.
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
#' df <- validate_totals_over_sex_pop
#' df <- dd_validate_totals_over_sex_new(df)
#'}

dd_validate_totals_over_sex_new <- function(data){

# harmonize open age group by truncating to minimum open age group of males and females
# Filter the data to only have abridged records
  abr <- data %>%
    dplyr::filter(abridged == TRUE)

  ## Determine the minimum open age group in the males and females data
  oa_min <- suppressWarnings(min(abr$AgeStart[abr$AgeSpan == -1 & abr$AgeLabel != "Total" & abr$SexID %in% c(1,2)]))

  ## Extract th record that contains this minimum open age group
  oa_record <- abr %>%
    dplyr::filter(AgeStart == oa_min & AgeSpan == -1) %>%
    select(-SexID, -DataValue) %>%
    distinct()


  if (1 %in% abr$SexID) {
    oa_record_m <- oa_record %>%
      mutate(SexID =1,
             DataValue = sum(abr$DataValue[abr$AgeStart >= oa_min & abr$SexID ==1]))
  } else {
    oa_record_m <- NULL
  }
  if (2 %in% abr$SexID) {
    oa_record_f <- oa_record %>%
      mutate(SexID =2,
             DataValue = sum(abr$DataValue[abr$AgeStart >= oa_min & abr$SexID ==2]))
  } else {
    oa_record_f <- NULL
  }
  if (3 %in% abr$SexID) {
    oa_record_b <- oa_record %>%
      mutate(SexID =3,
             DataValue = sum(abr$DataValue[abr$AgeStart >= oa_min & abr$SexID ==3]))
  } else {
    oa_record_b <- NULL
  }

  if (0 %in% abr$SexID) {
    oa_record_o <- oa_record %>%
      mutate(SexID =0,
             DataValue = sum(abr$DataValue[abr$AgeStart >= oa_min & abr$SexID ==0]))
  } else {
    oa_record_o <- NULL
  }

  abr <- abr %>%
    dplyr::filter(AgeStart < oa_min) %>%
    dplyr::filter(!(AgeSpan == -1 & AgeLabel != "Total" & AgeStart < oa_min)) %>% # remove any oa that is below oa_min (eg. for both sexes)
    bind_rows(oa_record_m, oa_record_f, oa_record_b, oa_record_o) %>%
    arrange(SexID, AgeStart)

  cpl <- data %>%
    dplyr::filter(complete == TRUE)

  if (nrow(cpl) > 50){ # only bother with this if complete series is usable

    ## Added SupressWarning to remove the warning:
    ## Warning message:
    # In min(cpl$AgeStart[cpl$AgeSpan == -1 & cpl$AgeLabel != "Total" &  :
    #                       no non-missing arguments to min; returning Inf
    # The warning appears when the content inside min() is NULL
    # Case: "68 - Bolivia (Plurinational State of) - Estimate - 2006 - Demographic Yearbook - De-facto - Population by age and sex - Low"
      oa_min <- suppressWarnings(min(cpl$AgeStart[cpl$AgeSpan == -1 & cpl$AgeLabel != "Total" & cpl$SexID %in% c(1,2)]))
      oa_record <- cpl %>%
        dplyr::filter(AgeStart == oa_min & AgeSpan == -1) %>%
        select(-SexID, -DataValue) %>%
        distinct()
      if (1 %in% cpl$SexID) {
      oa_record_m <- oa_record %>%
        mutate(SexID =1,
               DataValue = sum(cpl$DataValue[cpl$AgeStart >= oa_min & cpl$SexID ==1]))
      } else {
        oa_record_m <- NULL
      }
      if (2 %in% cpl$SexID) {
      oa_record_f <- oa_record %>%
        mutate(SexID =2,
               DataValue = sum(cpl$DataValue[cpl$AgeStart >= oa_min & cpl$SexID ==2]))
      } else {
        oa_record_f <- NULL
      }
      if (3 %in% cpl$SexID) {
      oa_record_b <- oa_record %>%
        mutate(SexID =3,
               DataValue = sum(cpl$DataValue[cpl$AgeStart >= oa_min & cpl$SexID ==3]))
      } else {
        oa_record_b <- NULL
      }
      if (0 %in% cpl$SexID) {
      oa_record_o <- oa_record %>%
        mutate(SexID =0,
               DataValue = sum(cpl$DataValue[cpl$AgeStart >= oa_min & cpl$SexID ==0]))
      } else {
        oa_record_o <- NULL
      }

      cpl <- cpl %>%
        dplyr::filter(AgeStart < oa_min) %>%
        dplyr::filter(!(AgeSpan == -1 & AgeLabel != "Total" & AgeStart < oa_min)) %>%
        bind_rows(oa_record_m, oa_record_f, oa_record_b, oa_record_o) %>%
        arrange(SexID, AgeStart)
  } else {
    cpl <- NULL
  }

  df <- rbind(abr, cpl)


  # now validate totals over sex
  df <- df %>%
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
           X1 = ifelse(X1 == 0 & total_over_sex > 0 & X3 > total_over_sex, X3 - total_over_sex, X1),
           total_over_sex = X0 + X1 + X2,
           X2 = ifelse(X2 == 0 & total_over_sex > 0 & X3 > total_over_sex, X3 - total_over_sex, X2),
           total_over_sex = X0 + X1 + X2,
           X0 = ifelse(X3 > total_over_sex & total_over_sex > 0, X0 + (X3 - total_over_sex), X0 ),
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

