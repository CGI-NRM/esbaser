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
  accnr_start_list <- accnr_parse(accnr_start)
  accnr_end_list <- accnr_parse(accnr_end)
  accdb_start <- accnr_to_database_format(accnr_start_list)
  accdb_end <- accnr_to_database_format(accnr_end_list)

  tbl(conn, "accession") |>
  filter(between(id, accdb_start, accdb_end)) |>
  select(id,
         project_id,
         locality_id,
         accdate,
         arrival_date,
         species_id,
         discovery_id,
         discovery_date_start,
         discovery_date_end,
         sender_id,
         collector_id,
         created_by,
         updated_by,
         note,
         complete,
         latitude,
         longitude,
         findplace_note,
         coordinate_precision_id,
         oldnumber,
         description,
         catalog_id,
         created,
         updated
  ) |>
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
  select(id,
         county_id,
         province_id,
         coast_id,
         country_id,
         name,
         closecity,
         created_by,
         updated_by,
         note,
         created,
         updated
  ) |>
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
  select(id,
         code,
         swe_name,
         eng_name
  ) |>
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
  select(id,
         code,
         swe_name,
         eng_name,
         created_by,
         updated_by,
         created,
         updated
  ) |>
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
  select(id,
         code,
         swe_name,
         eng_name,
  ) |>
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
  select(id,
         code,
         swe_name,
         eng_name,
  ) |>
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
  select(id,
         name
  ) |>
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
  select(id,
         swe_name,
         eng_name,
         type,
         lat_name,
         created_by,
         updated_by,
         catalog_id,
         created,
         updated
  ) |>
  collect()
}
