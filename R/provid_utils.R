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
#' @param provid_tib The ProvID to add onto
#' @param amount The value which will be added. Normally a 1 to get the next ProvID
#' @export
provid_add <- function(provid_tib, amount) {
  tibble(letter = provid_tib$letter, year = provid_tib$year, value = provid_tib$value + amount)
}

#' Parse ProvID in string form
#'
#' @description If it is valid a provid, generate a tibble containing 'letter', 'year' and 'value' based on it.
#'
#' @return A tibble containing the 'letter', 'year' and numeric 'value' of the provid
#'
#' @param provid_str The ProvID to be parsed
#' @export
provid_parse <- function(provid_str) {
  valid <- provid_validate(provid_str)
  provid_str[!valid] <- NA

  if (any(!valid)) {
    warning("Invalid ProvID, NAs introduced by coercion.")
  }

  letter <- substring(provid_str, 1, 1)
  year <- as.numeric(substring(provid_str, 2, 5))
  value <- as.numeric(substring(provid_str, 7))

  tibble(letter = letter, year = year, value = value)
}

#' Format an provid letter and value to the standard format
#'
#' @description Format the parsed provid_tib for printing.
#'
#' @param provid_tib The ProvID to format
#' @return A string of the nicely formated ProvID
#' @examples
#' provid_str <- "Q2022-12354"
#' provid_tib <- provid_parse(provid_str)
#' provid_next <- provid_add(provid_tib, 3)
#' print(provid_sprint(provid_next))
#' @export
provid_sprint <- function(provid_tib) {
  sprintf("%s%04d-%05d", provid_tib$letter, provid_tib$year, provid_tib$value)
}
