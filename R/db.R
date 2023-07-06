#' @import dplyr
NULL

#' Connect to database
#'
#' Initializes connection to database.
#'
#' @param dbname The name of the database, is 'test' on my local computer, 'mgg2' on the production machine
#' @param host The ip of the mariadb database to connect to
#' @param username The username when connecting to database
#' @param password The password when connecting to database
#' @return conn The database connection
#' @importFrom DBI dbConnect
#' @export
connect_to_database <- function(
  host = "172.17.0.1",
  username = "docker",
  password = "dockerpass",
  dbname = "test"
) {
  conn <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    username = username,
    password = password,
    host = host,
    dbname = dbname
  )

  conn
}

#' Disconnect from database
#'
#' Close connection to database
#'
#' @param conn A database connection returned by \link[esbaser]{connect_to_database}
#' @importFrom DBI dbDisconnect
#' @export
disconnect_from_database <- function(conn) {
  conn <- DBI::dbDisconnect(conn)
}

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
  accdb_start <- accnr_start |> accnr_parse() |> accnr_to_database_format()
  accdb_end <- accnr_end |> accnr_parse() |> accnr_to_database_format()

  tbl(conn, "accession") |>
  filter(between(id, accdb_start, accdb_end)) |>
  select(id, project_id, locality_id, accdate, arrival_date, species_id, discovery_id, discovery_date_start,
         discovery_date_end, sender_id, collector_id, created_by, updated_by, note, complete, latitude, longitude,
         findplace_note, coordinate_precision_id, oldnumber, description, catalog_id, created, updated) |>
  collect()
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
  accdb_start <- accnr_start |> accnr_parse() |> accnr_to_database_format()
  accdb_end <- accnr_end |> accnr_parse() |> accnr_to_database_format()

  tbl(conn, "fish") |>
  filter(between(accession_id, accdb_start, accdb_end)) |>
  select(accession_id, nourishment_id, gender_id, liverweight, totallength,
         decay_id, reproduction_phase_id, othernumber, bodylength, gonadweight) |>
  collect()
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
  accdb_start <- accnr_start |> accnr_parse() |> accnr_to_database_format()
  accdb_end <- accnr_end |> accnr_parse() |> accnr_to_database_format()

  tbl(conn, "egg") |>
  filter(between(accession_id, accdb_start, accdb_end)) |>
  select(accession_id, length, width, weight, field_number, shell_thickness1,
         shell_thickness2, shell_thickness3, shell_index, embryo_weight, embryo_length,
         embryo, hatch_egg_count, hatch_chick_count, note, hatch_note, egg_sent_material,
         hinna, content, decay_id) |>
  collect()
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
  accdb_start <- accnr_start |> accnr_parse() |> accnr_to_database_format()
  accdb_end <- accnr_end |> accnr_parse() |> accnr_to_database_format()

  tbl(conn, "clam") |>
  filter(between(accession_id, accdb_start, accdb_end)) |>
  select(accession_id, length, width, wet_weight, dry_weight, shell_weight, note) |>
  collect()
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
  accdb_start <- accnr_start |> accnr_parse() |> accnr_to_database_format()
  accdb_end <- accnr_end |> accnr_parse() |> accnr_to_database_format()

  tbl(conn, "mammal") |>
  filter(between(accession_id, accdb_start, accdb_end)) |>
  select(accession_id, nourishment_id, gender_id, rearfoot_length, tail_length, fat_neck_ventral, fat_breast_ventral,
         fat_abdomen_ventral, fat_hip_ventral, fat_neck_right, fat_breast_right, fat_abdomen_right, fat_hip_right, fat_neck_back,
         fat_breast_back, fat_abdomen_back, fat_hip_back, fat, circumference_neck, circumference_breast, circumference_abdomen,
         circumference_hip, autopsy_date, autopsy_journal, autopsy_journal_old, autopsy, birth_year, svanumber,
         decay_id, xrays, xray, fieldnumber, othernumber, sent_material_id, liverweight, totallength, anus) |>
  collect()
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
  accdb_start <- accnr_start |> accnr_parse() |> accnr_to_database_format()
  accdb_end <- accnr_end |> accnr_parse() |> accnr_to_database_format()

  tbl(conn, "bird") |>
  filter(between(accession_id, accdb_start, accdb_end)) |>
  select(accession_id, nourishment_id, gender_id, liverweight, totallength, xrays, svanumber,
         ringnumber, fieldnumber, othernumber, autopsy_date, autopsy, xray, decay_id) |>
  collect()
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
  collect()
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
  collect()
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
  collect()
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
  collect()
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
  collect()
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
  collect()
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
  collect()
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
  collect()
}
