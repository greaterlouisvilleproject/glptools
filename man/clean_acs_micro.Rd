% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/microdata.R
\name{clean_acs_micro}
\alias{clean_acs_micro}
\title{Process ACS microdata from IPUMS.}
\usage{
clean_acs_micro(df, gq = T, pull_peers = T, remove_vars = T)
}
\arguments{
\item{df}{A dataframe of microdata from IPUMS.}

\item{gq}{Include group quarters residents? Defaults to \code{FALSE}.}

\item{pull_peers}{Subset the data to peers? Defaults to \code{TRUE}.
Subsets to peer MSAs if present. Otherwise, subsets to peers counties.}
}
\description{
Adds FIPS codes and the Tulsa MSA code, if appropriate. Process race, sex, and education variables if present.
}
