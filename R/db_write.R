#' Insert new material
#'
#' Insert new material rows
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param account_id The id of the logged in user to be put in the created_by,updated_by column. Must be a numeric.
#' @param accdbs The accession ids in database form to add to point the materials to
#' @param material_type_id The inital material_type_id for the new material rows
#' @return The number of affected rows as affected_rows in a list
#' @importFrom DBI dbSendStatement
#' @importFrom DBI dbGetRowsAffected
#' @importFrom DBI dbClearResult
#' @importFrom lubridate today
#' @export
insert_new_material <- function(conn, account_id, accdbs, material_type_id) {
  if (!is.numeric(account_id)) {
    stop("Account id is not numeric.")
    return(list(rows_affected = 0))
  }

  if (any(!accdb_validate(accdbs))) {
    stop("Invalid accdbs")
    return(list(rows_affected = 0))
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

  return(list(rows_affected = rows_affected))
}

#' Update material rows
#'
#' Update material rows based on moified tibble returned by \link[esbaser]{get_material_between} based on the id
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param account_id The id of the logged in user to be put in the created_by,updated_by column
#' @param material A tibble of the material, containing the columns 'id, material_type_id, amount_original,
#' amount_left, storage_id, storage_type_id, storage_note.
#' @return The number of affected rows as affected_rows in a list
#' @importFrom DBI dbSendStatement
#' @importFrom DBI dbGetRowsAffected
#' @importFrom DBI dbClearResult
#' @importFrom lubridate today
#' @export
update_material <- function(conn, account_id, material) {
  if (!is.numeric(account_id)) {
    stop("Account id is not numeric.")
    return(list(rows_affected = 0))
  }

  affected_rows <- 0

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
             WHERE `id` = {id};
             ", .con = conn)

    stat <- dbSendStatement(conn, sql)
    affected_rows <- affected_rows + dbGetRowsAffected(stat)
    dbClearResult(stat)
  }

  return(list(rows_affected = affected_rows))
}

#' Update specimen rows
#'
#' Update material rows based on moified tibble returned by \link[esbaser]{get_specimen_between} based on the accession_id
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param account_id The id of the logged in user to be put in the created_by,updated_by column
#' @param specimen A tibble of the specimen, containing the columns 'id, note, storagenote, deathdate_start, deathdate_end,
#' age_type_id, age_start, age_end, weight
#' @importFrom DBI dbSendStatement
#' @importFrom DBI dbGetRowsAffected
#' @importFrom DBI dbClearResult
#' @importFrom lubridate now
#' @return The number of affected rows as affected_rows in a list
#' @export
update_specimen <- function(conn, account_id, specimen) {
  if (!is.numeric(account_id)) {
    stop("Account id is not numeric.")
    return(list(rows_affected = 0))
  }

  affected_rows <- 0

  for (row in seq_len(nrow(specimen))) {
    id <- specimen[row, "id", drop = TRUE]
    note <- specimen[row, "note", drop = TRUE]
    storagenote <- specimen[row, "storagenote", drop = TRUE]
    deathdate_start <- specimen[row, "deathdate_start", drop = TRUE]
    deathdate_end <- specimen[row, "deathdate_end", drop = TRUE]
    age_type_id <- specimen[row, "age_type_id", drop = TRUE]
    age_start <- specimen[row, "age_start", drop = TRUE]
    age_end <- specimen[row, "age_end", drop = TRUE]
    weight <- specimen[row, "weight", drop = TRUE]
    updated_by <- account_id
    updated <- format(now())

    sql_specimen <- glue_sql("
             UPDATE specimen
             SET `note` = {note}, `storagenote` = {storagenote}, `deathdate_start` = {deathdate_start},
             `deathdate_end` = {deathdate_end}, `age_type_id` = {age_type_id}, `age_start` = {age_start},
             `age_end` = {age_end}, `weight` = {weight}
             WHERE `id` = {id};
             ", .con = conn)

    stat <- dbSendStatement(conn, sql_specimen)
    affected_rows <- affected_rows + dbGetRowsAffected(stat)
    dbClearResult(stat)

    sql_accession <- glue_sql(
    "UPDATE accession
    SET `updated_by` = {updated_by}, `updated` = {updated}
    WHERE `id` = {id};
    ", .con = conn)

    stat <- dbSendStatement(conn, sql_accession)
    affected_rows <- affected_rows + dbGetRowsAffected(stat)
    dbClearResult(stat)
  }

  return(list(rows_affected = affected_rows))
}

#' Update fish rows
#'
#' Update fish rows based on moified tibble returned by \link[esbaser]{get_fish_between} based on the accession_id
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param account_id The id of the logged in user to be put in the created_by,updated_by column
#' @param fish A tibble of the fish, containing the columns 'accession_id, nourishment_id, gender_id, liverweight, totallength,
#' decay_id, reproduction_phase_id, othernumber, bodylength
#' @importFrom DBI dbSendStatement
#' @importFrom DBI dbGetRowsAffected
#' @importFrom DBI dbClearResult
#' @importFrom lubridate now
#' @return The number of affected rows as affected_rows in a list
#' @export
update_fish <- function(conn, account_id, fish) {
  if (!is.numeric(account_id)) {
    stop("Account id is not numeric.")
    return(list(affected_rows = 0))
  }

  affected_rows <- 0

  for (row in seq_len(nrow(fish))) {
    accession_id <- fish[row, "accession_id", drop = TRUE]
    nourishment_id <- fish[row, "nourishment_id", drop = TRUE]
    gender_id <- fish[row, "gender_id", drop = TRUE]
    liverweight <- fish[row, "liverweight", drop = TRUE]
    totallength <- fish[row, "totallength", drop = TRUE]
    decay_id <- fish[row, "decay_id", drop = TRUE]
    reproduction_phase_id <- fish[row, "reproduction_phase_id", drop = TRUE]
    othernumber <- fish[row, "othernumber", drop = TRUE]
    bodylength <- fish[row, "bodylength", drop = TRUE]
    gonadweight <- fish[row, "gonadweight", drop = TRUE]

    updated_by <- account_id
    updated <- format(now())

    sql_fish <- glue_sql(
      "UPDATE fish
      SET `nourishment_id` = {nourishment_id}, `gender_id` = {gender_id}, `liverweight` = {liverweight},
      `totallength` = {totallength}, `decay_id` = {decay_id}, `reproduction_phase_id` = {reproduction_phase_id},
      `othernumber` = {othernumber}, `bodylength` = {bodylength}, `gonadweight` = {gonadweight}
      WHERE `accession_id` = {accession_id};
      ", .con = conn)

    stat <- dbSendStatement(conn, sql_fish)
    affected_rows <- affected_rows + dbGetRowsAffected(stat)
    dbClearResult(stat)

    sql_accession <- glue_sql(
    "UPDATE accession
    SET `updated_by` = {updated_by}, `updated` = {updated}
    WHERE `id` = {accession_id};
    ", .con = conn)

    stat <- dbSendStatement(conn, sql_accession)
    affected_rows <- affected_rows + dbGetRowsAffected(stat)
    dbClearResult(stat)
  }

  return(list(affected_rows = affected_rows))
}

#' Insert analysisrecord
#'
#' Insert new analysisrecord
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param account_id The id of the logged in user to be put in the created_by,updated_by column. Must be a numeric.
#' @param project_id The id of the project this analysis is connected to
#' @param creator_id The person_id of the creator of the analysis
#' @param contact_id The person_id of the contact for this analysis
#' @param date Provberedningsdatum
#' @param shippingdate The date the sample was sent
#' @param analysis_type_id Based on analysis_type
#' @param result Notis, comment about the results of the analysis
#' @param analysis_type_note Analystypnotis, comment about the analysis_type
#' @importFrom DBI dbSendStatement
#' @importFrom DBI dbGetRowsAffected
#' @importFrom DBI dbClearResult
#' @importFrom DBI dbGetQuery
#' @importFrom lubridate today
#' @return The number of affected rows as affected_rows and the id of the newly created analysisrecord as new_row_id
#' @export
insert_analysisrecord <- function(conn, account_id, project_id, creator_id, contact_id, date,
                                  shippingdate, analysis_type_id, result, analysis_type_note) {
  if (!is.numeric(account_id)) {
    stop("Account id is not numeric.")
    return(list(affected_rows = 0, new_row_id = NULL))
  }

  created <- format(now())

  sql <- glue_sql("
                  INSERT INTO analysisrecord
                  (project_id, species_id, creator_id, contact_id, date, 
                  shippingdate, analysis_type_id, result, analysis_type_note, 
                  created_by, updated_by, created, updated) 
                  VALUES ({project_id}, 0, {creator_id}, {contact_id}, {date},
                  {shippingdate}, {analysis_type_note}, {result}, {analysis_type_note},
                  {account_id}, {account_id}, {created}, {created});", .con = conn)

  stat <- dbSendStatement(conn, sql)
  rows_affected <- dbGetRowsAffected(stat)
  dbClearResult(stat)

  new_row_id <- dbGetQuery(conn, "SELECT LAST_INSERT_ID() as id")$id

  return(list(rows_affected = rows_affected, new_row_id = new_row_id))
}

#' Insert analysisrecord_row
#'
#' Insert new analysisrecord_row
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param amount The size of the sample
#' @param homogenate_amount If a homogenate, the partial weight
#' @param tag The provid of the sample
#' @importFrom DBI dbSendStatement
#' @importFrom DBI dbGetRowsAffected
#' @importFrom DBI dbClearResult
#' @importFrom DBI dbGetQuery
#' @return The number of affected rows as affected_rows and the id of the newly created analysisrecord as new_row_id
#' @export
insert_analysisrecord_row <- function(conn, amount, homogenate_amount, tag) {
  sql <- glue_sql("
                  INSERT INTO analysisrecord_row
                  (amount, homogenate_amount, tag)
                  VALUES ({amount}, {homogenate_amount}, {tag});", .con = conn)

  stat <- dbSendStatement(conn, sql)
  rows_affected <- dbGetRowsAffected(stat)
  dbClearResult(stat)

  new_row_id <- dbGetQuery(conn, "SELECT LAST_INSERT_ID() as id")$id

  return(list(rows_affected = rows_affected, new_row_id = new_row_id))
}

#' Insert analysisrecord_row_assn
#'
#' Insert new analysisrecord_row_assn, connect two ids of analysisrecord and analysisrecord_row
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param analysisrecord_id The id of the newly inserted analysisrecord
#' @param analysisrecord_row_id The id of the newly inserted analysisrecord_row
#' @importFrom DBI dbSendStatement
#' @importFrom DBI dbGetRowsAffected
#' @importFrom DBI dbClearResult
#' @return The number of affected rows as affected_rows in a list
#' @export
insert_analysisrecord_row_assn <- function(conn, analysisrecord_id, analysisrecord_row_id) {
  sql <- glue_sql("
                  INSERT INTO analysisrecord_row_assn
                  (analysis_record_row_id, analysis_record_id)
                  VALUES ({analysisrecord_row_id}, {analysisrecord_id});", .con = conn)

  stat <- dbSendStatement(conn, sql)
  rows_affected <- dbGetRowsAffected(stat)
  dbClearResult(stat)

  return(list(rows_affected = rows_affected))
}

#' Insert analysisrecord_material_assn
#'
#' Insert new analysisrecord_material_assn, connect two ids of analysisrecord and material
#'
#' @param conn The database connection returned by \link[esbaser]{connect_to_database}
#' @param analysisrecord_row_id The id of the analysisrecord_row to link to a material
#' @param material_id The id of the material to link to an analysisrecord_row
#' @importFrom DBI dbSendStatement
#' @importFrom DBI dbGetRowsAffected
#' @importFrom DBI dbClearResult
#' @return The number of affected rows as affected_rows in a list
#' @export
insert_analysisrecord_material_assn <- function(conn, analysisrecord_row_id, material_id) {
  sql <- glue_sql("
                  INSERT INTO analysisrecord_material_assn
                  (analysisrecordRow_id, material_id)
                  VALUES ({analysisrecord_row_id}, {material_id});", .con = conn)

  stat <- dbSendStatement(conn, sql)
  rows_affected <- dbGetRowsAffected(stat)
  dbClearResult(stat)

  return(list(rows_affected = rows_affected))
}
