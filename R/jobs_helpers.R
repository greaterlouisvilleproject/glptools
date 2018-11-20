#' COLA
#'
#' Reads in BRFSS data
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()
#'
COLA <- function(df, ..., remove_calc = TRUE){

  if(c("rpp_index") %!in% names(df)){
    df %<>% left_join(COLA_df, by = c("FIPS", "year"))
  }

  df %<>% mutate_at(vars(...), funs(. * rpp_index * cpi_index))

  if(remove_calc){
    df %<>% select(-rpp_index, -cpi_index)
  }

  df

}
