#' Get accessions between
#'
#' Gets all accessions between lower and upper
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param accnr_start The lower accession_id (inclusive)
#' @param accnr_end The upper accession_id (inclusive)
#' @return A tibble
#' @export
get_accessions_between <- function(conn, accnr_start, accnr_end) {
  accdb_start <- accnr_to_accdb(accnr_start)
  accdb_end <- accnr_to_accdb(accnr_end)

  tbl(conn, "accession") |>
  filter(between(id, accdb_start, accdb_end)) |>
  select(id, project_id, locality_id, accdate, arrival_date, species_id, discovery_id, discovery_date_start,
         discovery_date_end, sender_id, collector_id, created_by, updated_by, note, complete, latitude, longitude,
         findplace_note, coordinate_precision_id, oldnumber, description, catalog_id, created, updated) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get fish between
#'
#' Gets all fish between lower and upper
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param accnr_start The lower accession_id (inclusive)
#' @param accnr_end The upper accession_id (inclusive)
#' @return A tibble
#' @export
get_fish_between <- function(conn, accnr_start, accnr_end) {
  accdb_start <- accnr_to_accdb(accnr_start)
  accdb_end <- accnr_to_accdb(accnr_end)

  tbl(conn, "fish") |>
  filter(between(accession_id, accdb_start, accdb_end)) |>
  select(accession_id, nourishment_id, gender_id, liverweight, totallength,
         decay_id, reproduction_phase_id, othernumber, bodylength, gonadweight) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get egg between
#'
#' Gets all egg between lower and upper
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param accnr_start The lower accession_id (inclusive)
#' @param accnr_end The upper accession_id (inclusive)
#' @return A tibble
#' @export
get_egg_between <- function(conn, accnr_start, accnr_end) {
  accdb_start <- accnr_to_accdb(accnr_start)
  accdb_end <- accnr_to_accdb(accnr_end)

  tbl(conn, "egg") |>
  filter(between(accession_id, accdb_start, accdb_end)) |>
  select(accession_id, length, width, weight, field_number, shell_thickness1,
         shell_thickness2, shell_thickness3, shell_index, embryo_weight, embryo_length,
         embryo, hatch_egg_count, hatch_chick_count, note, hatch_note, egg_sent_material,
         hinna, content, decay_id) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get clam between
#'
#' Gets all clam between lower and upper
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param accnr_start The lower accession_id (inclusive)
#' @param accnr_end The upper accession_id (inclusive)
#' @return A tibble
#' @export
get_clam_between <- function(conn, accnr_start, accnr_end) {
  accdb_start <- accnr_to_accdb(accnr_start)
  accdb_end <- accnr_to_accdb(accnr_end)

  tbl(conn, "clam") |>
  filter(between(accession_id, accdb_start, accdb_end)) |>
  select(accession_id, length, width, wet_weight, dry_weight, shell_weight, note) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get mammal between
#'
#' Gets all mammal between lower and upper
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param accnr_start The lower accession_id (inclusive)
#' @param accnr_end The upper accession_id (inclusive)
#' @return A tibble
#' @export
get_mammal_between <- function(conn, accnr_start, accnr_end) {
  accdb_start <- accnr_to_accdb(accnr_start)
  accdb_end <- accnr_to_accdb(accnr_end)

  tbl(conn, "mammal") |>
  filter(between(accession_id, accdb_start, accdb_end)) |>
  select(accession_id, nourishment_id, gender_id, rearfoot_length, tail_length, fat_neck_ventral, fat_breast_ventral,
         fat_abdomen_ventral, fat_hip_ventral, fat_neck_right, fat_breast_right, fat_abdomen_right, fat_hip_right, fat_neck_back,
         fat_breast_back, fat_abdomen_back, fat_hip_back, fat, circumference_neck, circumference_breast, circumference_abdomen,
         circumference_hip, autopsy_date, autopsy_journal, autopsy_journal_old, autopsy, birth_year, svanumber,
         decay_id, xrays, xray, fieldnumber, othernumber, sent_material_id, liverweight, totallength, anus) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get bird between
#'
#' Gets all bird between lower and upper
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param accnr_start The lower accession_id (inclusive)
#' @param accnr_end The upper accession_id (inclusive)
#' @return A tibble
#' @export
get_bird_between <- function(conn, accnr_start, accnr_end) {
  accdb_start <- accnr_to_accdb(accnr_start)
  accdb_end <- accnr_to_accdb(accnr_end)

  tbl(conn, "bird") |>
  filter(between(accession_id, accdb_start, accdb_end)) |>
  select(accession_id, nourishment_id, gender_id, liverweight, totallength, xrays, svanumber,
         ringnumber, fieldnumber, othernumber, autopsy_date, autopsy, xray, decay_id) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get specimen between
#'
#' Gets all specimen between lower and upper accessionid
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param accnr_start The lower accession_id (inclusive)
#' @param accnr_end The upper accession_id (inclusive)
#' @return A tibble
#' @export
get_specimen_between <- function(conn, accnr_start, accnr_end) {
  accdb_start <- accnr_to_accdb(accnr_start)
  accdb_end <- accnr_to_accdb(accnr_end)

  tbl(conn, "specimen") |>
  filter(between(id, accdb_start, accdb_end)) |>
  select(id, note, storagenote, deathdate_start, deathdate_end, age_type_id, age_start, age_end, weight) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get material between
#'
#' Gets all material between lower and upper
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param accnr_start The lower accession_id (inclusive)
#' @param accnr_end The upper accession_id (inclusive)
#' @return A tibble
#' @export
get_material_between <- function(conn, accnr_start, accnr_end) {
  accdb_start <- accnr_to_accdb(accnr_start)
  accdb_end <- accnr_to_accdb(accnr_end)

  tbl(conn, "material") |>
  filter(between(accession_id, accdb_start, accdb_end)) |>
  select(id, accession_id, material_type_id, amount_original, amount_left, storage_id, storage_type_id,
         storage_note, created_by, updated_by, created, updated) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Localitys
#'
#' Get the entire locality table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_locality <- function(conn) {
  tbl(conn, "locality") |>
  select(id, county_id, province_id, coast_id, country_id, name,
         closecity, created_by, updated_by, note, created, updated) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Countys
#'
#' Get the entire county table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_county <- function(conn) {
  tbl(conn, "county") |>
  select(id, code, swe_name, eng_name) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Countrys
#'
#' Get the entire country table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_country <- function(conn) {
  tbl(conn, "country") |>
  select(id, code, swe_name, eng_name, created_by, updated_by, created, updated) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Provinces
#'
#' Get the entire province table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_province <- function(conn) {
  tbl(conn, "province") |>
  select(id, code, swe_name, eng_name) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Coasts
#'
#' Get the entire coast table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_coast <- function(conn) {
  tbl(conn, "coast") |>
  select(id, code, swe_name, eng_name) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Catalog
#'
#' Get the entire catalog table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_catalog <- function(conn) {
  tbl(conn, "catalog") |>
  select(id, name) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Species
#'
#' Get the entire species table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_species <- function(conn) {
  tbl(conn, "species") |>
  select(id, swe_name, eng_name, type, lat_name, created_by,
         updated_by, catalog_id, created, updated) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Nourishment
#'
#' Get the entire nourishment table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_nourishment <- function(conn) {
  tbl(conn, "nourishment") |>
  select(id, code, swe_name, eng_name) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Age
#'
#' Get the entire age table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_age <- function(conn) {
  tbl(conn, "age") |>
  select(id, code, swe_name, eng_name) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Material Type
#'
#' Get the entire material_type table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_material_type <- function(conn) {
  tbl(conn, "material_type") |>
  select(id, code, swe_name, eng_name) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Material Storange
#'
#' Get the entire material_storage table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_material_storage <- function(conn) {
  tbl(conn, "material_storage") |>
  select(id, name, sortbyme) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Gender
#'
#' Get the entire gender table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_gender <- function(conn) {
  tbl(conn, "gender") |>
  select(id, code, swe_name, eng_name) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Person
#'
#' Get the entire person table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_person <- function(conn) {
  tbl(conn, "person") |>
  select(id, firstname, lastname, institution, address, postnumber, town,
         country, phone, email, type, note, username, password, rights,
         created_by, updated_by, created, updated) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Project
#'
#' Get the entire project table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_project <- function(conn) {
  tbl(conn, "project") |>
  select(id, name, number, contact_id, start, end, note, finished, created_by,
         updated_by, catalog_id, analysis_project, created, updated) |>
  collect() |>
  set_chr_column_encoding()
}

#' Get Analysis Type
#'
#' Get the entire analysis_type table from the database
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @return A tibble
#' @export
get_analysis_type <- function(conn) {
  tbl(conn, "analysis_type") |>
  select(id, name) |>
  collect() |>
  set_chr_column_encoding()
}
