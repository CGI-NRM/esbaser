#' Validate AccNR entered by the user
#'
#' @description Validate a accnr, allowing both A2022/12345 and A202212345
#'
#' @return Logical value wether the accnr_str is a valid accnr that can be parsed or not.
#'
#' @param accnr_str The AccNR to be validated
#'
#' @importFrom stringr str_detect
#' @export
accnr_validate <- function(accnr_str) {
  str_detect(accnr_str, "^[A-Za-z][\\d]{4}/?[\\d]{5}$")
}


#' Add a value onto an AccNR
#'
#' @description Given a parsed AccNR, add to the value of it
#'
#' @param accnr_list The AccNR to be validated
#' @param amount The value which will be added. Normally a 1 to get the next AccNR
#' @export
accnr_add <- function(accnr_list, amount) {
  list(letter = accnr_list$letter, year = accnr_list$year, value = accnr_list$value + amount)
}

#' Parse AccNR in string form
#'
#' @description If it is valid a accnr, generate a list containing 'letter', 'year' and 'value' based on it.
#'
#' @return A list containing the 'letter', 'year' and numeric 'value' of the accnr
#'
#' @param accnr_str The AccNR to be parsed, either with or without an '/'
#' @export
accnr_parse <- function(accnr_str) {
  if (!accnr_validate(accnr_str)) {
    return(list(letter = "A", year = 0, value = 0))
  }

  letter <- substring(accnr_str, 1, 1)
  year <- as.numeric(substring(accnr_str, 2, 5))
  if (grepl("/", accnr_str, fixed = TRUE)) {
    value <- as.numeric(substring(accnr_str, 7))
  } else {
    value <- as.numeric(substring(accnr_str, 6))
  }


  list(letter = letter, year = year, value = value)
}

#' Format an accnr letter and value to the standard format
#'
#' @description Format the parsed accnr_list for printing.
#'
#' @param accnr_list The AccNR to be parsed
#' @return A string of the nicely formated AccNR
#' @examples
#' accnr_str <- "A2022/12354"
#' accnr_list <- parse_accnr(accnr_str)
#' accnr_next <- add_accnr(accnr_list, 3)
#' print(sprint_accnr(accnr_next))
#' @export
accnr_sprint <- function(accnr_list) {
  sprintf("%s%04d/%05d", accnr_list$letter, accnr_list$year, accnr_list$value)
}
