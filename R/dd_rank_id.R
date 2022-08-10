#' @title
#' dd_rank_id
#'
#' @description
#' Ranking id; For each country- year, we rank the available ids for population counts.
#'
#' @details
#' See Part 3 of the \href{https://shelmith-kariuki.github.io/rddharmony/articles/Harmonization_Workflow.html}{Harmonization Workflow article} for details about this function.
#'
#' @param indata The data to be harmonized
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset with only one id per country and year
#'
#' @export
#'
dd_rank_id <- function(indata){

  # Discard DataTypeName==“Direct (standard abridged age groups computed)”
  # or “Direct (standard abridged age groups computed - Unknown redistributed)”
  out <- indata %>% dplyr::filter(DataTypeName!= 'Direct (standard abridged age groups computed)',
                           DataTypeName!= 'Direct (standard abridged age groups computed - Unknown redistributed)')

  # Check id duplication for each country-census year
  out <-  out %>%
    group_by(ReferencePeriod) %>%
    mutate(num.id = length(unique(id))) %>%
    ungroup()

  # If there is more than 1 id for each country-census year:
  if (max(out$num.id)>1) {
    out1 <-  out %>%
      dplyr::filter(num.id>=2) %>%
      group_by(id) %>%
      mutate(num.serie = length(unique(abridged[AgeLabel != "0-4"])),
             maxage = max(AgeStart)) %>%
      ungroup %>%

      # First: Prefer ids that have both abridged and complete available
      group_by(ReferencePeriod) %>%
      dplyr::filter(num.serie == max(num.serie)) %>%

      # Second:Prefer better DataReliability (the minimum of the DataReliabilitySort field)
      dplyr::filter(DataReliabilitySort == min(DataReliabilitySort)) %>%

      # Third: Prefer StatisticalConceptName == “De-facto” over “De-jure”
      mutate(has_de_facto = ifelse('De-facto' %in% StatisticalConceptName, TRUE, FALSE),
             keep_de_facto = ifelse(has_de_facto == TRUE, 'De-facto', StatisticalConceptName)) %>%
      dplyr::filter(StatisticalConceptName == keep_de_facto) %>%

      # Fourth: Prefer the series with the highest open age group
      dplyr::filter(maxage == max(maxage)) %>%

      # Fifth: Prefer DataSourceName = "Demographic Yearbook"
      mutate(has_dyb = ifelse('Demographic Yearbook' %in% DataSourceName, TRUE, FALSE),
             keep_dyb = ifelse(has_dyb == TRUE, 'Demographic Yearbook', DataSourceName)) %>%
      dplyr::filter(DataSourceName == keep_dyb) %>%

      # Sixth: If IPUMS is a duplicate source then drop it since many of these series are samples
      mutate(nonipums_flag = DataSourceShortName != "IPUMS",
             has_nonipums = any(nonipums_flag),
             keep_nonipums = ifelse(has_nonipums == FALSE | (has_nonipums == TRUE & DataSourceShortName != "IPUMS"), TRUE, FALSE)) %>%
      dplyr::filter(keep_nonipums == TRUE) %>%

      # Seventh, keep most recent data source year
      dplyr::filter(DataSourceYear == max(DataSourceYear)) %>%
      ungroup() %>%

      # Finally discard a couple of duplicate series (if in sample) that have been hardcoded here bc they are not eliminated by above criteria

      # dplyr::filter(!(id %in% discard_these_dups)) %>%

      dplyr::filter(!(id %in% dups$dups)) %>%
      select(-num.serie, -maxage, -has_de_facto, -keep_de_facto, -has_dyb, -keep_dyb, -nonipums_flag, -has_nonipums, -keep_nonipums)

  }  else { out1 <- NULL }

  # Only 1 id for each country-census year:
  out2 <- out %>%
    dplyr::filter(num.id == 1)

  outdata <- rbind(out1, out2)

  # check number of ids again
  lookForDups <- unique(outdata[,c("ReferencePeriod","id")]) %>%
    group_by(ReferencePeriod) %>%
    mutate(n = 1:length(ReferencePeriod)) %>%
    dplyr::filter(n == 1) # keep only the first id for each census year
  outdata <- outdata %>%
    dplyr::filter(id %in% lookForDups$id)

  outdata <- outdata %>%
    arrange(ReferencePeriod) %>%
    select(!c(num.id))

} # end function

