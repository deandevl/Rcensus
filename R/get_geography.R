#' get_geography()
#'
#' Get the list of geography entities available (state, county, tract, etc)
#'    available for a specific dataset and optionally a vintage.
#'
#' @description Function produces a data.table/dataframe of the
#'   geography names available in submitting a data request to the
#'   Census Bureau's API.
#'
#' @param dataset A string that sets the name of the data set of interest (e.g. "acs/acs5")
#' @param vintage An optional numeric that sets the year of interest.
#'
#' @import data.table httr jsonlite
#'
#' @return A data.table
#'
#' @author Rick Dean
#'
#' @export
get_geography <- function(dataset, vintage = NULL){

  # Create a string url based on the submitted parameters
  a_url <- .get_url(dataset, vintage)

  a_url <- paste(a_url, "geography.json", sep="/")

  # Make a web request
  resp <- httr::GET(a_url)

  # Check the response as valid JSON
  .check_response(resp)

  # Parse the response and return raw JSON
  raw_json <- .parse_response(resp)
  # Return a data.table
  fips_dt <- data.table::setDT(raw_json$fips)

  return(fips_dt)
}