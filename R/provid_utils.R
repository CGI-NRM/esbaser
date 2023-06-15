#' Validate ProvID entered by the user
#'
#' @description Validate a provid, on the form Q2022-12345
#'
#' @return Logical value wether the provid_str is a valid provid that can be parsed or not.
#'
#' @param provid_str The ProvID to be validated
#'
#' @importFrom stringr str_detect
#' @export
provid_validate <- function(provid_str) {
  str_detect(provid_str, "^[Q][\\d]{4}-[\\d]{5}$")
}


#' Add a value onto an ProvID
#'
#' @description Given a parsed ProvID, add to the value of it
#'
#' @param provid_list The ProvID to add onto
#' @param amount The value which will be added. Normally a 1 to get the next ProvID
#' @export
provid_add <- function(provid_list, amount) {
  list(letter = provid_list$letter, year = provid_list$year, value = provid_list$value + amount)
}

#' Parse ProvID in string form
#'
#' @description If it is valid a provid, generate a list containing 'letter', 'year' and 'value' based on it.
#'
#' @return A list containing the 'letter', 'year' and numeric 'value' of the provid
#'
#' @param provid_str The ProvID to be parsed
#' @export
provid_parse <- function(provid_str) {
  if (!provid_validate(provid_str)) {
    return(NULL)
  }

  letter <- substring(provid_str, 1, 1)
  year <- as.numeric(substring(provid_str, 2, 5))
  value <- as.numeric(substring(provid_str, 7))

  list(letter = letter, year = year, value = value)
}

#' Format an provid letter and value to the standard format
#'
#' @description Format the parsed provid_list for printing.
#'
#' @param provid_list The ProvID to format
#' @return A string of the nicely formated ProvID
#' @examples
#' provid_str <- "Q2022-12354"
#' provid_list <- provid_parse(provid_str)
#' provid_next <- provid_add(provid_list, 3)
#' print(provid_sprint(provid_next))
#' @export
provid_sprint <- function(provid_list) {
  sprintf("%s%04d-%05d", provid_list$letter, provid_list$year, provid_list$value)
}
