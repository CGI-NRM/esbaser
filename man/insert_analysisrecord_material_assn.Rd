% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_write.R
\name{insert_analysisrecord_material_assn}
\alias{insert_analysisrecord_material_assn}
\title{Insert analysisrecord_material_assn}
\usage{
insert_analysisrecord_material_assn(conn, analysisrecord_row_id, material_id)
}
\arguments{
\item{conn}{The database connection returned by \link[esbaser]{connect_to_database}}

\item{analysisrecord_row_id}{The id of the analysisrecord_row to link to a material}

\item{material_id}{The id of the material to link to an analysisrecord_row}
}
\value{
The number of affected rows as affected_rows in a list
}
\description{
Insert new analysisrecord_material_assn, connect two ids of analysisrecord and material
}
