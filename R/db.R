#' Get Biologdata from AccNR
#'
#' Given an AccNR, get the biologdata that already exists in the DB
#'
#' @param accnr_str The AccNR to get the data for
#'
#' @return All biologdata that already exists for the AccNR
#' @importFrom stats runif
#' @export
get_accnr_biologdata <- function(accnr_str) {
  colnames <- get_biologdata_colnames(pretty = FALSE)
  genders <- get_options_gender()
  if (accnr_str == "" || accnr_str == "-") {
    df <- data.frame(
      helpnr_at_testprep = as.character(NA),
      accnr = accnr_str |> as.character(),
      alder = as.double(NA),
      kroppsvikt = as.double(NA),
      totallangd = as.double(NA),
      kroppslangd = as.double(NA),
      kon = factor(NA, levels = genders[, "id", drop = TRUE], labels = genders[, "representation", drop = TRUE]),
      gonadvikt = as.double(NA),
      gonad_sparad = factor(NA, levels = c("", "J", "N")),
      levervikt = as.double(NA),
      lever_kvar = as.double(NA),
      parasit = as.double(NA),
      skrottvikt = as.double(NA),
      mage_sparad = factor(NA, c("", "J", "N")),
      notering = as.character(NA)
    )
  } else {
    accnr_list <- accnr_parse(accnr_str)
    set.seed(accnr_list$value)
    df <- data.frame(
      helpnr_at_testprep = as.character(NA),
      accnr = accnr_str |> as.character(),
      alder = runif(1, 0, 10) |> as.numeric(),
      kroppsvikt = runif(1, 120, 250) |> as.numeric(),
      totallangd = runif(1, 8, 30) |> as.numeric(),
      kroppslangd = runif(1, 9, 24) |> as.numeric(),
      kon = sample(genders[, "id", drop = TRUE]) |>
      factor(levels = genders[, "id", drop = TRUE], labels = genders[, "representation", drop = TRUE]),
      gonadvikt = runif(1, 2, 10) |> as.numeric(),
      gonad_sparad = sample(c("J", "N"), 1) |> factor(levels = c("", "J", "N")),
      levervikt = runif(1, 4, 12) |> as.numeric(),
      lever_kvar = runif(1, 2, 10) |> as.numeric(),
      parasit = runif(1, 5, 100) |> as.numeric(),
      skrottvikt = runif(1, 4, 10) |> as.numeric(),
      mage_sparad = sample(c("J", "N"), 1) |> factor(c("", "J", "N")),
      notering = as.character(NA)
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
    colnames <- c("Hjälpnr. vid provber.", "Acc.nr.", "Ålder (år)",
                  "Kroppsvikt (g)", "Totallängd (cm)",
                  "Kroppslängd (cm)", "Kön", "Gonadvikt (g)",
                  "Gonad sparad J/N", "Levervikt (g)", "Lever kvar (g)",
                  "Parasit (g)", "Skrottvikt (g)", "Mage sparad J/N",
                  "Notering/Avvikelse")
  } else {
    colnames <- c(
      "helpnr_at_testprep", "accnr", "alder", "kroppsvikt", "totallangd", "kroppslangd",
      "kon", "gonadvikt", "gonad_sparad", "levervikt", "lever_kvar", "parasit",
      "skrottvikt", "mage_sparad", "notering")
  }
  colnames
}

#' Paste Collapse
#'
#' Removes all NA:s and empty strings from x, then pastes and collapses them together
#'
#' @param x The vector which non-Na non-empty-string values should be pasted and collapsed
#' @param collapse Passed to the paste function
#'
#' @return A string of the collapsed values with the collapse param between ignoring empty strings and NAs
paste_collapse <- function(x, collapse = ", ") {
  paste(x[x != "" & !is.na(x)], collapse = collapse)
}

#' Join Part
#'
#' Used to compile helplists where for example the species table has a catalog_id, which ponts to the catalog table
#' @param tib A tibble contaning an id column, and one or more columns that should be joined into a string representation
#' @param id_in_tib The name of the id column in the tibble tib
#' @param repr_column_in_res The string of the column-name the representation column should take
#' @param id_column_in_res The name the id column should have in the returned tibble
#' @param cols A vector with the columns in tib that should be joined
#' @param collapse The string that should separate the columns cols
#' @return A tibble taking the id_column from tib, and the combined cols separated by collapse
join_part <- function(
  tib,
  id_in_tib = "id",
  repr_column_in_res = "repr",
  id_column_in_res = "id",
  cols = c("code", "swe_name"),
  collapse = "-"
) {
  t <- tibble(
    id = tib[, id_in_tib, drop = TRUE],
    repr = apply(
      tib[, cols],
      1,
      paste_collapse,
      collapse = collapse
    )
  )
  colnames(t) <- c(id_column_in_res, repr_column_in_res)
  t
}

#' Get options lokaler
#'
#' Returns the possble options for the locality
#'
#' @return Tibble with id, representation
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @export
get_options_lokaler <- function() {
  data <- esbaser::locality[, c("id", "name", "closecity")]

  county_part <- join_part(esbaser::county, repr_column_in_res = "county_repr")
  province_part <- join_part(esbaser::province, repr_column_in_res = "province_repr")
  coast_part <- join_part(esbaser::coast, repr_column_in_res = "coast_repr")
  country_part <- join_part(esbaser::country, repr_column_in_res = "country_repr")

  data <- bind_cols(
    data,
    county_part[match(esbaser::locality[, "county_id", drop = TRUE],
                      county_part[, "id", drop = TRUE]), "county_repr"],
    province_part[match(esbaser::locality[, "province_id", drop = TRUE],
                        province_part[, "id", drop = TRUE]), "province_repr"],
    coast_part[match(esbaser::locality[, "coast_id", drop = TRUE],
                     coast_part[, "id", drop = TRUE]), "coast_repr"],
    country_part[match(esbaser::locality[, "country_id", drop = TRUE],
                       country_part[, "id", drop = TRUE]), "country_repr"]
  )

  join_part(
    data,
    repr_column_in_res = "representation",
    cols = c("county_repr", "province_repr", "coast_repr",
             "country_repr", "name", "closecity"), collapse = ", ")
}

#' Get options gender
#'
#' Returns the possble options for the gender
#'
#' @return Tibble with id, representation
#' @importFrom tibble tibble
#' @export
get_options_gender <- function() {
  join_part(esbaser::gender, repr_column_in_res = "representation", cols = c("code", "swe_name"), collapse = ", ")
}

#' Get options species
#'
#' Returns the possble options for the species
#'
#' @return Tibble with id, representation
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @export
get_options_species <- function() {
  catalog_part <- join_part(esbaser::catalog, repr_column_in_res = "catalog_repr", cols = c("name"))

  data <- bind_cols(
    esbaser::species[, c("id", "swe_name", "eng_name", "lat_name")],
    catalog_part[match(esbaser::species[, "catalog_id", drop = TRUE],
                       catalog_part[, "id", drop = TRUE]), "catalog_repr"]
  )

  join_part(data, repr_column_in_res = "representation", cols = c("swe_name", "eng_name", "lat_name", "catalog_repr"), collapse = ", ")
}

#' Get options project
#'
#' Returns the possble options for the project
#'
#' @return Tibble with id, representation
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @export
get_options_project <- function() {
  join_part(esbaser::project, repr_column_in_res = "representation", cols = c("name", "number", "note"), collapse = ", ")
}

#' Get options material_type
#'
#' Returns the possble options for the material_type
#'
#' @return Tibble with id, representation
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @export
get_options_material_type <- function() {
  join_part(esbaser::material_type, repr_column_in_res = "representation", cols = c("code", "swe_name"), collapse = "-")
}
