% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_read.R
\name{get_clam_between}
\alias{get_clam_between}
\title{Get clam between}
\usage{
get_clam_between(conn, accnr_start, accnr_end)
}
\arguments{
\item{conn}{The database connection returned by \link[esbaser]{connect_to_database}}

\item{accnr_start}{The lower accession_id (inclusive)}

\item{accnr_end}{The upper accession_id (inclusive)}
}
\value{
A tibble
}
\description{
Gets all clam between lower and upper
}
