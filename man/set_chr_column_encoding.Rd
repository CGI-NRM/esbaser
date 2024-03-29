% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{set_chr_column_encoding}
\alias{set_chr_column_encoding}
\title{Set Character Encoding to Chr Columns}
\usage{
set_chr_column_encoding(tib, encoding = "latin1")
}
\arguments{
\item{tib}{The tibble to convert columns in}

\item{encoding}{Default 'latin1', the encoding to specify all columns as}
}
\description{
Set the encoding using 'Encoding(col) <- encoding' to all column of character type
}
