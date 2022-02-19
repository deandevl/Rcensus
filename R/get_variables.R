
#' get_variables()
#'
#' Get the variable IDs of Census Bureau and their descriptive parameters
#'
#' @description Function produces a dataframe of variable IDs and their
#'    descriptive parameters from the Census Bureau's publicly available
#'    \href{https://www.census.gov/data/developers/data-sets.html}{datasets}.
#' @param dataset A string that sets the name of the dataset of interest (e.g. "acs/acs5").
#'   See \code{Rcensus::get_dataset_names()} for available dataset names.
#' @param vintage An optional numeric or vector of numerics that sets the year(s) of interest.
#' @param group An optional string that sets the group name associated with a set of variables
#' @param brief If TRUE out of the 10 columns, will return only columns `name`, `label`, `predicateType`.
#' @param filter_str A character string by which to filter the variable IDs.
#'
#' @import data.table httr jsonlite
#'
#' @return A data.table
#'
#' @author Rick Dean
#'
#' @export
get_variables <- function(
  dataset,
  vintage = NULL,
  group = NULL,
  brief = FALSE,
  filter_str = NULL){

  getDataTable <- function(x){
    return(data.table::setDT(x))
  }

  getDT <- function(vintage){
    # Create a string url based on the submitted parameters
    a_url <- .get_url(dataset, vintage)

    if(!is.null(group)){
      a_url <- paste0(a_url, "/groups/", group, ".json")
    }else {
      a_url <- paste(a_url, "variables.json", sep = "/")
    }

    # Make a web request
    resp <- httr::GET(a_url)

    # Check the response as valid JSON
    check <- .check_response(resp)

    # Parse the response and return raw JSON
    raw_json <- .parse_response(resp)

    # Get a listing that makes each variable a data frame with their parameter info
    var_datatable <- lapply(raw_json$variables, getDataTable)

    # Filter out those data frame variables that were set to NA
    var_datatable <- var_datatable[!is.na(var_datatable)]

    # Combine the list of data frames into one data frame
    dt <- data.table::rbindlist(var_datatable, fill = T)

    # Add a column to the data table that has the names of the variables
    name <- names(var_datatable)
    dt <- cbind(name,dt)

    if(brief){
      sel_col <- c("name","label", "predicateType")
      dt <- dt[,..sel_col]
    }

    if(!is.null(filter_str)){
      dt <- dt[grepl(filter_str, dt$name, fixed = TRUE)]
    }

    return(dt)
  }

  if(length(vintage) > 1){
    dt <- getDT(vintage[[1]])

    dt[, vintage := vintage[[1]]]

    for(i in 2:length(vintage)){
      a_dt <- getDT(vintage[[i]])

      a_dt[, vintage := vintage[[i]]]

      dt <- rbind(dt, a_dt)
    }
    return(dt)
  }else{
    return(getDT(vintage))
  }
}
