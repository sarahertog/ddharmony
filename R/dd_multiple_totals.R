#' @title
#' dd_multiple_totals
#'
#' @description
#' If there exists more than one `Total` age label and there is one that is equal to the computed total, drop the others and keep this one.
#'
#' @param data The data to be harmonised
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return  A data frame with only one `Total` age label per id, SexId and indicator id.
#' @export

dd_multiple_totals <- function(data){
  ## Added by Shel (26th October at 14:26)
  ## Case study (births): id == "578 - Norway - VR - Births - 2002 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair"
  ## Case study (births):id == "831 - Channel Islands - Guernsey - VR - Births - 1998 - Register - Demographic Yearbook - Year of occurrence - Direct - Fair"

data <- data %>%
  group_by(AgeStart) %>%
  mutate(counter = length(unique(AgeLabel))) %>%
  ungroup() %>%
  mutate(misplaced_oag = ifelse(AgeLabel %in% grep("\\+", AgeLabel, value = TRUE, ignore.case = TRUE) & counter >1, TRUE, FALSE)) %>%
  mutate(calc_total = sum(DataValue[AgeLabel!="Total" & misplaced_oag!=TRUE], na.rm = TRUE),
         total_todrop = ifelse(any(AgeLabel == "Total") & length(AgeLabel[AgeLabel=="Total"])>1 &
                                 AgeLabel=="Total" & any(DataValue == calc_total) & DataValue != calc_total, "drop","")) %>%
  filter(total_todrop != "drop") %>%
  select(-calc_total, -total_todrop)

return(data)
}
