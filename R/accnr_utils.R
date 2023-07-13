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
#' @description Given (a) parsed AccNR(s), add to the value of it(them)
#'
#' @param accnr_tib The parsed AccNRs to add onto
#' @param amount The value which will be added. Normally a 1 to get the next AccNR
#' @export
accnr_add <- function(accnr_tib, amount) {
  tibble(letter = accnr_tib$letter, year = accnr_tib$year, value = accnr_tib$value + amount)
}

#' Parse AccNR in string form
#'
#' @description If it is valid a accnr, generate a list containing 'letter', 'year' and 'value' based on it.
#'
#' @return A tibble with the columns 'letter', 'year' and numeric 'value' of the accnrs
#'
#' @param accnr_str The AccNR to be parsed (single or vector), either with or without an '/'
#' @export
accnr_parse <- function(accnr_str) {
  valid <- accnr_validate(accnr_str)
  accnr_str[!valid] <- NA

  if (any(!valid, na.rm = TRUE)) {
    warning("Invalid AccNR, NAs introduced by coercion.")
  }

  letter <- substring(accnr_str, 1, 1)
  year <- as.numeric(substring(accnr_str, 2, 5))

  accnr_str <- gsub("/", "", accnr_str)
  value <- as.numeric(substring(accnr_str, 6))

  tibble(letter = letter, year = year, value = value)
}

#' Format an accnr letter and value to the standard format
#'
#' @description Format the parsed accnr_list for printing.
#'
#' @param accnr_tib The AccNR to format
#' @return A string of the nicely formated AccNR
#' @examples
#' accnr_str <- c("A2022/12354", "B2023/00001")
#' accnr_tib <- accnr_parse(accnr_str)
#' accnr_next <- accnr_add(accnr_tib, c(1, 3))
#' print(accnr_sprint(accnr_next))
#' @export
accnr_sprint <- function(accnr_tib) {
  sprintf("%s%04d/%05d", accnr_tib$letter, accnr_tib$year, accnr_tib$value)
}

#' Convert from human-readable accnr to the accnr saved in the db
#'
#' @param accnr_tib The AccNR to format
#' @return A string of the accnr formated for the db
#' @export
accnr_db_sprint <-  function(accnr_tib) {
  valid <- accnr_tib$letter %in% c("A", "B", "C", "D", "G", "H", "L", "X", "P")
  accnr_tib[!valid, ] <- NA

  if (any(!valid, na.rm = TRUE)) {
    warning("AccNR's letter was not one of ABCDGHLXP and cannot be converted to database format.")
  }

  year_str <- sprintf("%04d", accnr_tib$year)
  first_num <- c("A" = "1", "B" = "2", "C" = "3", "D" = "4", "G" = "7", "H" = "8", "L" = "5", "P" = "9", "X" = "6")[accnr_tib$letter]
  sprintf("%s%s%05d", first_num, substring(year_str, 2, 4), accnr_tib$value)
}

#' Validate AccDB entered by the user
#'
#' @description Validate a accdb, on the form 202200000
#'
#' @return Logical value wether the accdb_str is a valid accdb that can be parsed or not.
#'
#' @param accdb_str The AccDB to be validated
#'
#' @importFrom stringr str_detect
#' @export
accdb_validate <- function(accdb_str) {
  str_detect(accdb_str, "[1-9][0-9]{8}")
}

#' Convert from database accdb to parsed accnr-tibble
#'
#' @param accdb_str The AccNR str from the database
#' @return A parsed accnr_tib
#' @export
accdb_parse <- function(accdb_str) {
  valid <- str_detect(accdb_str, "[1-9][0-9]{8}")
  accdb_str[!valid] <- NA

  if (any(!valid, na.rm = TRUE)) {
    warning("AccDB str is not correctly formatted and cannot be converted to AccNR.")
  }

  letter <- c(
    "1" = "A", "2" = "B", "3" = "C", "4" = "D", "7" = "G",
    "8" = "H", "5" = "L", "9" = "P", "6" = "X"
  )[substring(accdb_str, 1, 1)]

  millenium <- ifelse(substring(accdb_str, 2, 2) == "9", "1", "2")
  year <- as.numeric(paste0(millenium, substring(accdb_str, 2, 4)))
  value <- as.numeric(substring(accdb_str, 5, 9))

  tibble(letter = letter, year = year, value = value)
}

#' Convert from accnr to database accdb
#'
#' @param accnr_str The AccNR str
#' @return An accdb str
#' @export
accnr_to_accdb <- function(accnr_str) {
  accnr_db_sprint(accnr_parse(accnr_str))
}

#' Convert from database accdb to accnr
#'
#' @param accdb_str The AccNR str from the database
#' @return An accnr str
#' @export
accdb_to_accnr <- function(accdb_str) {
  accnr_sprint(accdb_parse(accdb_str))
}
