#' Insert new material
#'
#' Insert new material rows
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param account_id The id of the logged in user to be put in the created_by,updated_by column
#' @param accdbs The accession ids in database form to add to point the materials to
#' @param material_type_id The inital material_type_id for the new material rows
#' @return The number of affected rows
#' @importFrom DBI dbSendStatement
#' @importFrom DBI dbGetRowsAffected
#' @importFrom DBI dbClearResult
#' @importFrom lubridate today
#' @export
insert_new_material <- function(conn, account_id, accdbs, material_type_id) {
  account_id <- as.integer(account_id)
  if (any(!accdb_validate(accdbs))) {
    stop("Invalid accdbs")
    return()
  }

  if (length(accdbs) > 1000) {
    warning("Can only insert a thousand rows at once.")
  }
  material_type_id <- as.integer(material_type_id)

  new_rows <- tibble(
    accession_id = accdbs, material_type_id = material_type_id,
    created_by = account_id, updated_by = account_id,
    created = format(today()), updated = format(today()))

  data <- lapply(seq_len(nrow(new_rows)), function(row) {
    row <- unlist(new_rows[row, ], use.names = FALSE)
    glue_sql("({row*}, '', NULL, NULL, NULL, NULL)", .con = conn)
  }) |> unlist() |> paste0(collapse = ", ")

  sql <- paste0(
    "INSERT INTO material ",
    "(accession_id, material_type_id, created_by, updated_by, created, updated, ",
    "storage_note, amount_original, amount_left, storage_type_id, storage_id) ",
    "VALUES ",
    data,
    ";")

  stat <- dbSendStatement(conn, sql)
  rows_affected <- dbGetRowsAffected(stat)
  dbClearResult(stat)

  return(rows_affected)
}

#' Update material rows
#'
#' Update material rows based on moified tibble returned by \link[esbaser]{get_material_between} based on the id
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param account_id The id of the logged in user to be put in the created_by,updated_by column
#' @param material A tibble of the material, containing the columns 'id, material_type_id, amount_original,
#' amount_left, storage_id, storage_type_id, storage_note.
#' @return The number of affected rows
#' @importFrom DBI dbSendStatement
#' @importFrom DBI dbGetRowsAffected
#' @importFrom DBI dbClearResult
#' @importFrom lubridate today
#' @export
update_material <- function(conn, account_id, material) {
  for (row in seq_len(nrow(material))) {
    id <- material[row, "id", drop = TRUE]
    material_type_id <- material[row, "material_type_id", drop = TRUE]
    amount_original <- material[row, "amount_original", drop = TRUE]
    amount_left <- material[row, "amount_left", drop = TRUE]
    storage_id <- material[row, "storage_id", drop = TRUE]
    storage_type_id <- material[row, "storage_type_id", drop = TRUE]
    storage_note <- material[row, "storage_note", drop = TRUE]
    updated_by <- account_id
    updated <- format(today())

    sql <- glue_sql("
             UPDATE material
             SET `material_type_id` = {material_type_id}, `amount_original` = {amount_original}, `amount_left` = {amount_left},
             `storage_id` = {storage_id}, `storage_type_id` = {storage_type_id}, `storage_note` = {storage_note},
             `updated_by` = {updated_by}, `updated` = {updated}
             WHERE `id` = {id}
             ", .con = conn)

    print(sql)
  }
}
