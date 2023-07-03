#' Connect to database
#'
#' Initializes connection to database.
#'
#' @param dbname The name of the database, is 'test' on my local computer, 'mgg2' on the production machine
#' @return conn, The database connection
#' @import dplyr
#' @importFrom DBI dbConnect
#' @export
connect_to_database <- function(dbname = "test") {
  conn <- DBI::dbConnect(
    RMariaDB::MariaDB(),
    username = "docker",
    password = "dockerpass",
    host = "172.17.0.1",
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
#' @return Returns a tibble
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
         note,
         complete,
         latitude,
         longitude,
         findplace_note,
         coordinate_precision_id,
         oldnumber,
         description,
         catalog_id
  ) |>
  collect()
}
