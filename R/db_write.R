#' Insert new material
#'
#' Insert new material rows
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param account_id The id of the logged in user to be put in the created_by,updated_by column
#' @param accdbs The accession ids in database form to add to point the materials to
#' @param material_type_id The inital material_type_id for the new material rows
#' @return Success of write
#' @importFrom DBI dbSendQuery
#' @importFrom DBI dbBind
#' @importFrom DBI dbFetch
#' @importFrom DBI dbClearResult
#' @importFrom lubridate today
#' @export
insert_new_material <- function(conn, account_id, accdbs, material_type_id) {
  account_id <- as.integer(account_id)
  if (any(!accdb_validate(accdbs))) {
    stop("Invalid accdbs")
    return()
  }
  material_type_id <- as.integer(material_type_id)

  new_rows <- tibble(
    accession_id = accdbs, material_type_id = material_type_id,
    created_by = account_id, updated_by = account_id,
    created = format(today()), updated = format(today()))

  for (row in seq_len(nrow(new_rows))) {
    row <- unlist(new_rows[row, ], use.names = FALSE)
    sql <- glue_sql(
      "INSERT INTO material 
      (accession_id, material_type_id, created_by, updated_by, created, updated)
      VALUES
      ({row*});",
      .con = conn)

    print(sql)
  }

  return(TRUE)
}
