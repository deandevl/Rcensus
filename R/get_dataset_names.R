#' get_dataset_names()
#'
#' Get the names and descriptions of the Census Bureau's datasets.
#'
#' @description Function produces a data.table/dataframe of the
#'   Census Bureau's dataset names that can be used in other \code{Rcensus::}
#'   functions calling for a dataset name. See Census Bureau's publicly available
#'    \href{https://www.census.gov/data/developers/data-sets.html}{datasets} for
#'    descriptions.
#'
#' @param filter_str A character string by which to filter the dataset names
#'   containing the string.
#'
#' @import data.table httr jsonlite
#'
#' @return A data.table
#'
#' @author Rick Dean
#'
#' @export
get_dataset_names <- function(filter_str = NULL){
  # Create the url
  a_url <- "https://api.census.gov/data.json"

  # Make a web request
  resp <- httr::GET(a_url)

  # Check the response as valid JSON
  check <- .check_response(resp)

  # Parse the response and return raw JSON
  raw_json <- .parse_response(resp)

  datasets_df <- jsonlite::flatten(raw_json[["dataset"]])
  colnames(datasets_df) <- gsub("c_","",colnames(datasets_df))

  datasets_dt <- data.table::setDT(datasets_df)

  change_name <- function(x){
    paste(x[["dataset"]], collapse = "/")
  }
  change_url <- function(x){
    return(x[["distribution"]][["accessURL"]])
  }

  datasets_dt[, name := apply(datasets_dt, 1, change_name)]
  datasets_dt[, url := apply(datasets_dt, 1, change_url)]

  select_cols <- c("name","vintage","title","url","isTimeseries","description","modified")
  datasets_dt <- datasets_dt[, ..select_cols]

  if(!is.null(filter_str)){
    datasets_dt <- datasets_dt[grepl(filter_str, datasets_dt$name, fixed = TRUE)]
  }

  datasets_dt <- datasets_dt[order(name, vintage)]

  return(datasets_dt)
}