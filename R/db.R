#' Get Biologdata from AccNR
#'
#' Given an AccNR, get the biologdata that already exists in the DB
#'
#' @param accnr The AccNR to get the data for
#'
#' @return All biologdata that already exists for the AccNR
#' @importFrom stats runif
#' @export
get_accnr_biologdata <- function(accnr) {
  set.seed(accnr)
  colnames <- c("Annat NRMnr.", "Acc.nr.", "Ålder (år)",
                "Kroppsvikt (g)", "Totallängd (cm)",
                "Kroppslängd (cm)", "Kön", "Gonadvikt (g)",
                "Gonad sparad J/N", "Levervikt (g)", "Lever kvar (g)",
                "Parasit (g)", "Skrottvikt (g)", "Mage sparad J/N",
                "Notering/Avvikelse")

  df <- data.frame(
    list("",
         accnr,
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
  df
}
