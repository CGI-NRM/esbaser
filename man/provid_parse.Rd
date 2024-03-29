% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/provid_utils.R
\name{provid_parse}
\alias{provid_parse}
\title{Parse ProvID in string form}
\usage{
provid_parse(provid_str)
}
\arguments{
\item{provid_str}{The ProvID to be parsed}
}
\value{
A tibble containing the 'letter', 'year' and numeric 'value' of the provid
}
\description{
If it is valid a provid, generate a tibble containing 'letter', 'year' and 'value' based on it.
}
