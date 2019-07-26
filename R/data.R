## GLP DEFINITIONS

#' A list of GLP peer counties.
#'
#' @format A data frame with four variables:
#' \describe{
#' \item{\code{FIPS}}{FIPS code}
#' \item{\code{city}}{city name}
#' \item{\code{baseline}}{1 if the city was an original peer, otherwise 0}
#' \item{\code{current}}{1 if the city is a current peer, otherwise 0}
#' }
"FIPS_df"

#' A list of GLP peer MSAs.
#'
#' @format A data frame with four variables:
#' \describe{
#' \item{\code{MSA}}{MSA code}
#' \item{\code{city}}{city name}
#' \item{\code{baseline}}{1 if the city was an original peer, otherwise 0}
#' \item{\code{current}}{1 if the city is a current peer, otherwise 0}
#' }
"MSA_df"



## CROSSWALKS

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
#' @format A data frame with four variables:
#' \describe{
#' \item{\code{MSA}}{MSA code}
#' \item{\code{year}}{year.
#'   PUMAs based on the 2000 census are labelled with the years 2000 to 2012 to mirror ACS microdata available through IPUMS.
#'   PUMAS based on the 2010 census are labelled with the years 2013 to 2017.}
#' \item{\code{STATEFIP}}{State FIPS code}
#' \item{\code{PUMA}}{Public Use Microdata Area}

#' }
"MSA_PUMA"

#' A crosswalk of FIPS codes to the PUMAs they contain.
#'
#' @format A data frame with four variables:
#' \describe{
#' \item{\code{FIPS}}{FIPS code}
#' \item{\code{year}}{year.
#'   PUMAs based on the 2000 census are labelled with the years 2000 to 2012 to mirror ACS microdata available through IPUMS.
#'   PUMAS based on the 2010 census are labelled with the years 2013 to 2017.}
#' \item{\code{STATEFIP}}{State FIPS code}
#' \item{\code{PUMA}}{Public Use Microdata Area}

#' }
"FIPS_PUMA"

#' A crosswalk of MSA codes to the zip codes they contain.
#'
#' @format A data frame with four variables:
#' \describe{
#' \item{\code{MSA}}{MSA code}
#' \item{\code{zip}}{zip code}
#' \item{\code{population_total}}{total population of zip code}
#' \item{\code{population_in_MSA}}{population of zip code in MSA}
#' }
"MSA_zip"

#' A crosswalk of FIPS codes to the zip codes they contain.
#'
#' @format A data frame with four variables:
#' \describe{
#' \item{\code{FIPS}}{FIPS code}
#' \item{\code{zip}}{zip code}
#' \item{\code{population_total}}{total population of zip code}
#' \item{\code{population_in_MSA}}{population of zip code in FIPS}
#' }
"FIPS_zip"



## OTHER DATA

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
"COLA_df"

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



## MAP DEFINITIONS

#' A crosswalk from tracts to neighborhoods.
#'
#' @format A data frame with two variables:
#' \describe{
#' \item{\code{GEO_ID}}{20-digit GEO_ID code}
#' \item{\code{neighborhood}}{neighborhood name}
#' }
"nh_tract"

#' A crosswalk from tracts to market areas.
#'
#' @format A data frame with two variables:
#' \describe{
#' \item{\code{GEO_ID}}{20-digit GEO_ID code}
#' \item{\code{market_area}}{market area name}
#' }
"ma_tract"

#' Identifies which tracts are mostly inside the Watterson.
#'
#' @format A data frame with two variables:
#' \describe{
#' \item{\code{GEO_ID}}{20-digit GEO_ID code}
#' \item{\code{watterson}}{1 if the tract is mostly inside the Watterson, otherwise 0}
#' }
"watterson_tract"



# MAPS

#' A map of block groups in Louisville
#'
#' @format A SpatialPolygonsDataFrame
#' \describe{
#' \item{\code{GEO_ID}}{15-digit GEO_ID code consisting of FIPS, tract, and block numbers}
#' \item{\code{tract}}{6-character tract code}
#' \item{\code{block_group}}{numeric block group code}
#' \item{\code{line1}}{Block group \code{block}}
#' }
"map_block_group"

#' A map of 2000 tracts in Louisville
#'
#' @format A SpatialPolygonsDataFrame
#' \describe{
#' \item{\code{tract}}{6-character tract code}
#' \item{\code{line1}}{Tract \code{tract}}
#' }
"map_tract_2000"

#' A map of zip codes in Louisville
#'
#' @format A SpatialPolygonsDataFrame
#' \describe{
#' \item{\code{zip}}{5-digit zip code}
#' \item{\code{line1}}{Zip code \code{zip}}
#' }
"map_zip"

#' A map of market areas in Louisville
#'
#' @format A SpatialPolygonsDataFrame
#' \describe{
#' \item{\code{market}}{Market area}
#' \item{\code{line1}}{\code{Market area}}
#' }
"map_market"

#' A map of counties in the Louisville MSA
#'
#' @format A SpatialPolygonsDataFrame
#' \describe{
#' \item{\code{FIPS}}{5-character FIPS code}
#' \item{\code{county}}{county name}
#' \item{\code{line1}}{\code{county}}
#' }
"map_county"

