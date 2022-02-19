#' get_multi_vintage_data()
#'
#' Get Census Bureau data for a specific dataset, variables, and region
#'   in the form of a dataframe for multiple vintages.
#'
#' @description Function produces a data.table/dataframe of
#'   Census Bureau data for multiple vintages.  The function requires an access key
#'   issued from the Bureau.
#'
#' @param dataset A string that sets the name of the data set of interest (e.g. "acs/acs5")
#' @param vintage_v A numeric vector that sets the vintages of interest.
#' @param vars Can be either a string vector of variable names to be acquired.
#'   Also can be a named list of string vectors to separate groups of variables.
#' @param vars_init A string vector of variable names to append to \code{vars}, The default
#'   is \code{c("GEO_ID", "NAME")}. Set it to NULL to disregard it.
#' @param region A string that specifies the geography of the request.
#'    See \code{Rcensus::get_geography()} for assistance in obtaining
#'    these values.
#' @param regionin A string that sets a qualifier for \code{region}.
#' @param key A key string issued by the Census Bureau in making data requests.
#'
#' @import data.table httr jsonlite
#'
#' @return A data.table
#'
#' @author Rick Dean
#'
#' @export
get_multi_vintage_data <- function(
  dataset,
  vintage_v,
  vars,
  vars_init = c("GEO_ID", "NAME"),
  region = NULL,
  regionin = NULL,
  key = Sys.getenv("CENSUS_KEY")
) {
  # Check for key in environment
  key_from_env <- Sys.getenv("CENSUS_KEY")
  if(key_from_env == "" & key == key_from_env){
    stop("'key' argument is missing. A Census Bureau key is required.")
  }

  getDT <- function(vintage){
    # Create a string url based on the submitted parameters
    a_url <- .get_url(dataset, vintage)

    var_names <- NULL
    meas <- NULL
    if(is.list(vars)){
      var_names <- unlist(vars)
      meas <- vars
    }else{
      var_names <- vars
      meas <- list(vars)
    }

    # Get the data.table
    dt <- .get_dt(
      a_url = a_url,
      key = key,
      group = NULL,
      vars = var_names,
      vars_init = vars_init,
      region = region,
      regionin = regionin
    )

    if(is.list(vars)){
      dt <- data.table::melt(
        dt,
        measure = meas
      )

      var_labels <- c()
      a_var <- vars[[1]]
      for(i in seq_along(a_var)){
        var_labels <- c(var_labels, substr(a_var[[i]], 1, nchar(a_var[[i]])-1))
      }
      dt[, variable := factor(variable, levels = 1:length(a_var), labels = var_labels)]
    }else{
      dt <- data.table::melt(
        dt,
        measure = meas,
        value.name = c("value")
      )
    }

    return(dt)
  }

  dt <- getDT(vintage_v[[1]])

  dt[, vintage := vintage_v[[1]]]

  for(i in 2:length(vintage_v)){
    a_dt <- getDT(vintage_v[[i]])

    a_dt[, vintage := vintage_v[[i]]]

    dt <- rbind(dt, a_dt)
  }
  return(dt)
}