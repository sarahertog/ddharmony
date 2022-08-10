#' @title
#' std_age_function
#'
#' @description
#' Creates a data frame with all of the standard age groups we would expect for abridged and complete series (up to 130+ open age group)
#'
#' @details
#' You can run `getAnywhere(std_age_function)` to view the definition of this function.
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
#' @return A dataset with standard age groups
#'
#' @export
#'
#' @examples
#' \dontrun{
#' std_ages <- std_age_function()
#' }

std_age_function <- function() {

  # create a data frame with all of the standard age groups we would expect for abridged
  # and complete series (up to 130+ open age group)
  #
  # abridged
  AgeStart = c(0,1,0,seq(5,125,5),seq(5,130,5),0,-2)
  AgeEnd = c(1,5,seq(5,130,5),rep(0,26),-1,-2)
  AgeLabel = c(0,
               paste(AgeStart[2:28], AgeEnd[2:28]-1, sep="-"),
               paste0(AgeStart[29:54],"+"),
               "Total","Unknown")
  AgeSpan = c(AgeEnd[1:28] - AgeStart[1:28], rep(-1,27), -2)

  std_age_abridged <- as.data.frame(cbind(AgeStart,AgeEnd,AgeLabel,AgeSpan)) %>%
    mutate(abridged = TRUE,
           complete = c(TRUE, rep(FALSE,27), rep(TRUE, 28)))

  # complete
  AgeStart = c(seq(1,129,1))
  AgeEnd = c(seq(2,130,1))
  AgeLabel = as.character(AgeStart)
  AgeSpan = rep(1,129)

  std_age_complete <- as.data.frame(cbind(AgeStart,AgeEnd,AgeLabel,AgeSpan)) %>%
    mutate(abridged = FALSE,
           complete = TRUE)


  std_ages <- rbind(std_age_abridged,
                    std_age_complete) %>%
    mutate(AgeStart = as.numeric(AgeStart),
           AgeEnd = as.numeric(AgeEnd),
           AgeSpan = as.numeric(AgeSpan),
           AgeSort = c(seq(1,28,1), seq(158,185,1), seq(29,157,1))) %>%
    arrange(AgeSort)

  return(std_ages)

}




