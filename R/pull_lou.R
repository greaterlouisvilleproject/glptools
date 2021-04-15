##NOTE FOR Harrison - this works except when i run it for MSA the only FIPS it preserves is 21111
#and it shows NA for the rest. Where have I gone wrong?

#' Subset a data frame containing MSA or county data to peer cities and add current and baseline peer data.
#'
#' @param df A data frame containing the column MSA or FIPS
#' @param subset_to_Lou Either \code{TRUE} or \code{FALSE}.
#'   Subset the data to Louisville geographies? Defaults to \code{TRUE}.
#' @param geog Level of gegraphy to use for adding information and filtering.
#'   Can be \code{FIPS} or \code{MSA}
#'   Defaults to the MSA if present, followed by the FIPS column if present.
#'   (To keep MSA-level data using FIPS as the ID column, geog should be \code{MSA}.)
#'
#' @export pull_Lou
pull_Lou <- function(df, geog = "", subset_to_Lou = T){

  # If no geography provided, use MSA column. If no MSA column, use FIPS column.
  if (geog == ""){
    if ("MSA" %in% names(df)) geog <- "MSA"
    else if ("FIPS" %in% names(df)) geog <- "FIPS"
  }
  if(geog == "") stop("MSA and FIPS columns are missing from the data frame.")

  # subset to Lou based on geog
  if(subset_to_Lou) {
    if (geog == "FIPS") df %<>% filter(FIPS == "21111")
    if (geog == "MSA") df %<>% filter(MSA == "31140")
    }
  df %<>% organize()

  df
}


