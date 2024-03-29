% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db.R
\name{connect_to_database}
\alias{connect_to_database}
\title{Connect to database}
\usage{
connect_to_database(
  host = "172.17.0.1",
  username = "docker",
  password = "dockerpass",
  dbname = "test"
)
}
\arguments{
\item{host}{The ip of the mariadb database to connect to}

\item{username}{The username when connecting to database}

\item{password}{The password when connecting to database}

\item{dbname}{The name of the database, is 'test' on my local computer, 'mgg2' on the production machine}
}
\value{
conn The database connection
}
\description{
Initializes connection to database.
}
