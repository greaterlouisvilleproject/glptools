## CROSSWALKS

#' A crosswalk of MSA codes to MSA names.
#'
#' @format A data frame with two variables:
#' \describe{
#' \item{\code{MSA}}{MSA code}
#' \item{\code{city}}{city}
#' }
"MSA_names"

#' A crosswalk of MSA codes to the FIPS codes of their core counties.
#'
#' @format A data frame with two variables:
#' \describe{
#' \item{\code{MSA}}{MSA code}
#' \item{\code{FIPS}}{FIPS code}
#' }
"MSA_FIPS"

#' A crosswalk of MSA codes to the PUMAs they contain. 
#' 
#' 
#' @format A data frame with four variables:
#' \describe{
#' \item{\code{MSA}}{MSA code}
#' \item{\code{STATEFIP}}{State FIPS code}
#' \item{\code{PUMA}}{Public Use Microdata Area}
#' \item{\code{year}}{year. 
#' PUMAs based on the 2000 census are labelled with the years 2000 to 2012 to mirror ACS microdata available through IPUMS. 
#' PUMAS based on the 2010 census are labelled with the years 2013 to 2017.}
#' }
"MSA_PUMA"


## OTHER DATA

#' Population data with unmerged St. Louis Counties and a numeric FIPS column.
#'
#' @format A data frame with three variables:
#' \describe{
#' \item{\code{FIPS}}{FIPS codes (numeric)}
#' \item{\code{year}}{year}
#' \item{\code{population}}{population}
#' }
#'
"population_df"

#' Population
#'
#' Population data with merged and unmerged St. Louis counties and a character FIPS column.
#'
#' @format A data frame with three variables:
#' \describe{
#' \item{\code{FIPS}}{FIPS codes (character)}
#' \item{\code{year}}{year}
#' \item{\code{population}}{population}
#' }
#'
"population_df_merged"

#' Inflation and cost of living data. 
#' The data frame is used by the function \code{COLA} to adjust dollar amounts to 2017 Louisville dollars.
#'
#' @format A data frame with four variables:
#' \describe{
#' \item{\code{FIPS}}{FIPS code}
#' \item{\code{year}}{year}
#' \item{\code{rpp_index}}{The RPP of Louisville divided by the RPP of \code{FIPS} for that particular year. 
#' RPP values prior to 2008 are copied from 2008. RPP values from after 2015 are copied from 2015.}
#' \item{\code{cpi_index}}{The CPI index for 2017 divided by the CPI index for \code{year}}
#' }
#'
#' For further details, see \url{http://www.ssa.gov/oact/NOTES/as120/LifeTables_Body.html#wp1168594}
#'
"COLA_df"
