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
    df <- data.frame(as.list(rep("", length(colnames))))
    colnames(df) <- colnames
    df[1, "accnr"] <- accnr_str
  } else {
    accnr_list <- accnr_parse(accnr_str)
    set.seed(accnr_list$value)
    df <- data.frame(
      list("",
           accnr_str,
           runif(1, 0, 10),
           runif(1, 120, 250),
           runif(1, 8, 30),
           runif(1, 9, 24),
           sample(c("M", "F"), 1),
           runif(1, 2, 10),
           sample(c("J", "N"), 1),
           runif(1, 4, 12),
           runif(1, 2, 10),
           runif(1, 5, 100),
           runif(1, 4, 10),
           sample(c("J", "N"), 1),
           ""
      ))
    colnames(df) <- colnames
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
