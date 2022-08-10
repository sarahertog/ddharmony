#' @title
#' check_locid
#'
#' @description
#' Throws an error message if a location id is not part of the location ids in the UNDP database, and a confirmation message showing the location name if the location id is valid.
#'
#' @details
#' Run `check_locid(insert locid here)` to check whether a location id is valid (part of the locations in the UNPD website). Running `check_locid(insert locid here)` with a valid id returns a message confirming that the location id is valid and also gives the location name of that particular id. Running the same code with an invalid id returns a message directing the user to run `View(get_locations())` in order to get a list of plausible location ids.
#' You can run `getAnywhere(check_locid)` to see how the function is defined.
#'
#' @param locid Location id
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return The message: "`locid` is not a valid location id. Please run `View(get_locations())` to get a list of plausible location ids. They are listed in the `PK_LocID` variable" in the case
#' where the user has inserted an invalid location id, and in cases where the location id is valid, the following message is returned: "`locid` is a valid location id. The location name is `location name`"
#'
#' @export
#'
#' @examples
#' \dontrun{
#' check_locid(12345) ## invalid location id
#' check_locid(404) ## valid location id
#' }
check_locid <- function(locid){

  ## Create a list of plausible location ids
  possible_ids <- DDSQLtools::get_locations() %>% distinct(PK_LocID, Name)

  if (!locid %in% possible_ids$PK_LocID) {

    ## If a location id is not part of the possible ids, throw an error message.
    print( paste0(locid," is not a valid location id. Please run View(get_locations()) to get a list of plausible location ids. They are listed in the `PK_LocID` variable"))

  }else{
    ## If a location id is part of the possible ids, show a confirmatory message.
    print( paste0(locid," is a valid location id. The location name is ", possible_ids$Name[possible_ids$PK_LocID == locid]))

    }
}
