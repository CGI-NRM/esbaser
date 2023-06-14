#' Get Biologdata from AccNR
#'
#' Given an AccNR, get the biologdata that already exists in the DB
#'
#' @param accnr The AccNR to get the data for
#'
#' @return All biologdata that already exists for the AccNR
#' @importFrom stats runif
#' @export
get_accnr_biologdata <- function(accnr_str) {
  colnames <- get_biologdata_colnames(pretty = FALSE)
  if (accnr_str == "" || accnr_str == "-") {
    df <- data.frame(
      annat_nrmnr = character(1),
      accnr = accnr_str |> as.character(),
      alder = double(1),
      kroppsvikt = double(1),
      totallangd = double(1),
      kroppslangd = double(1),
      kon = factor("", levels = c("", "F", "M")),
      gonadvikt = double(1),
      gonad_sparad = factor("", levels = c("", "J", "N")),
      levervikt = double(1),
      lever_kvar = double(1),
      parasit = double(1),
      skrottvikt = double(1),
      mage_sparad = factor("", c("", "J", "N")),
      notering = character(1)
    )
  } else {
    accnr_list <- accnr_parse(accnr_str)
    set.seed(accnr_list$value)
    df <- data.frame(
      annat_nrmnr = character(1),
      accnr = accnr_str |> as.character(),
      alder = runif(1, 0, 10) |> as.numeric(),
      kroppsvikt = runif(1, 120, 250) |> as.numeric(),
      totallangd = runif(1, 8, 30) |> as.numeric(),
      kroppslangd = runif(1, 9, 24) |> as.numeric(),
      kon = sample(c("M", "F"), 1) |> factor(levels = c("", "F", "M")),
      gonadvikt = runif(1, 2, 10) |> as.numeric(),
      gonad_sparad = sample(c("J", "N"), 1) |> factor(levels = c("", "J", "N")),
      levervikt = runif(1, 4, 12) |> as.numeric(),
      lever_kvar = runif(1, 2, 10) |> as.numeric(),
      parasit = runif(1, 5, 100) |> as.numeric(),
      skrottvikt = runif(1, 4, 10) |> as.numeric(),
      mage_sparad = sample(c("J", "N"), 1) |> factor(c("", "J", "N")),
      notering = character(1)
    )
  }

  df
}

#' Get columnnames of biologdata
#'
#' @param pretty TRUE or FALSE, wether to return pretty values, 'Lever kvar (g)' or 'lever_kvar'
#' @return The colnames of the biologdata
#' @export
get_biologdata_colnames <- function(pretty) {
  if (pretty) {
    colnames <- c("Annat NRMnr.", "Acc.nr.", "Ålder (år)",
                  "Kroppsvikt (g)", "Totallängd (cm)",
                  "Kroppslängd (cm)", "Kön", "Gonadvikt (g)",
                  "Gonad sparad J/N", "Levervikt (g)", "Lever kvar (g)",
                  "Parasit (g)", "Skrottvikt (g)", "Mage sparad J/N",
                  "Notering/Avvikelse")
  } else {
    colnames <- c(
      "annat_nrmnr", "accnr", "alder", "kroppsvikt", "totallangd", "kroppslangd",
      "kon", "gonadvikt", "gonad_sparad", "levervikt", "lever_kvar", "parasit",
      "skrottvikt", "mage_sparad", "notering")
  }
  colnames
}

#' Get stödlista med arter
#'
#' Returns a helplist containing all species that can be used for checking data input
#'
#' @return Dataframe med kolumner: Katalog, svenskt namn, latinskt namn, engelskt namn
#' @importFrom tibble tibble
#' @importFrom utils data
#' @export
get_stodlista_arter <- function() {
  esbaser::arter
}

#' Get stödlista med lokaler
#'
#' Returns a helplist containing all loakler that can be used for checking data input
#'
#' @return Dataframe with omrade, narmsta_ort, sjodistrikt, landskap, lan, land
#' @importFrom tibble tibble
#' @importFrom utils data
#' @export
get_stodlista_lokaler <- function() {
  esbaser::lokaler
}
