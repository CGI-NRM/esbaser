#' @import dplyr
#' @import glue
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
