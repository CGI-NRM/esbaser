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
  str_detect(accnr_str, "^[ABCDGHLXP][\\d]{4}/?[\\d]{5}$")
}


#' Add a value onto an AccNR
#'
#' @description Given a parsed AccNR, add to the value of it
#'
#' @param accnr_list The AccNR to add onto
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
    return(NULL)
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
#' @param accnr_list The AccNR to format
#' @return A string of the nicely formated AccNR
#' @examples
#' accnr_str <- "A2022/12354"
#' accnr_list <- accnr_parse(accnr_str)
#' accnr_next <- accnr_add(accnr_list, 3)
#' print(accnr_sprint(accnr_next))
#' @export
accnr_sprint <- function(accnr_list) {
  sprintf("%s%04d/%05d", accnr_list$letter, accnr_list$year, accnr_list$value)
}

#' Convert from human-readable accnr to the accnr saved in the db
#'
#' @param accnr_list The AccNR to format
#' @return A string of the accnr formated for the db
#' @export
accnr_to_database_format <-  function(accnr_list) {
  if (!(accnr_list$letter %in% c("A", "B", "C", "D", "G", "H", "L", "X", "P"))) {
    warning("AccNR's letter was not one of ABCDGHLXP and cannot be converted to database format.")
    return(NULL)
  }
  year_str <- sprintf("%04d", accnr_list$year)
  first_num <- list("A" = "1", "B" = "2", "C" = "3", "D" = "4", "G" = "7", "H" = "8", "L" = "5", "P" = "9", "X" = "6")[[accnr_list$letter]]
  sprintf("%s%s%05d", first_num, substring(year_str, 2, 4), accnr_list$value)
}

#' Convert from database accnr to human-readable accnr
#'
#' @param accnr_list The AccNR str from the database
#' @return A parsed accnr_list
#' @export
accdb_parse_to_accnr <- function(accdb_str) {
  if (!str_detect(accdb_str, "[1-9][0-9]{8}")) {
    warning("AccDB str is not correctly formatted and cannot be converted to AccNR.")
    return(NULL)
  }
  letter <- list(
    "1" = "A", "2" = "B", "3" = "C", "4" = "D", "7" = "G",
    "8" = "H", "5" = "L", "9" = "P", "6" = "X"
  )[[substring(accdb_str, 1, 1)]]
  millenium <- ifelse(substring(accdb_str, 2, 2) == "9", "1", "2")
  year <- as.numeric(paste0(millenium, substring(accdb_str, 2, 4)))
  value <- as.numeric(substring(accdb_str, 5, 9))

  list(letter = letter, year = year, value = value)
}
