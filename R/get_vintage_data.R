#' get_vintage_data()
#'
#' Get Census Bureau data for a specific dataset, variables, and region
#'   in the form of a dataframe.
#'
#' @description Function produces a data.table/dataframe of
#'   Census Bureau data.  The function requires an access key
#'   issued from the Bureau.
#'
#' @param dataset A string that sets the name of the data set of interest (e.g. "acs/acs5")
#' @param vintage An optional numeric that sets the year of interest.
#' @param vars Can be either a string vector of variable names to be acquired.
#'   Also can be a named list of string vectors to separate groups of variables.
#' @param vars_init A string vector of variable names to append to \code{vars}, The default
#'   is \code{c("GEO_ID", "NAME")}. Set it to NULL to disregard it.
#' @param group A string that names the group that has a collection of variables. A dataframe
#'   is returned in the "long" format with a factor column named "variables" with their respective
#'   values.
#' @param group_values A string vector that sets the type of output values.
#'   Permissible values are "estimate" (for estimates) "moe" (for margin of error).
#'   The default is \code{c("estimate", "moe")} for both.
#' @param region A string that specifies the geography of the request.
#'    See \code{Rcensus::get_geography()} for assistance in obtaining
#'    these values.
#' @param regionin A string that sets a qualifier for \code{region}.
#' @param na_cols If TRUE will remove all rows with missing values. If a
#'   vector of column names/integers will check their values for missing values.
#' @param shape A string that sets the shape of the returned dataframe. Permissible
#'   values are "long" (the default) or "wide".
#' @param melt_meas A \code{list} that contains character vectors of column names/indexes from
#'   \code{dataset} to be consolidated. It is the \code{measure.vars} argument in data.table's
#'   wide-to-long \href{https://rdatatable.gitlab.io/data.table/reference/melt.data.table.html}{melt}
#'   reshaping tool.
#' @param melt_values A Vector that assigns a value to each of the consolidated columns.
#' @param key A key string issued by the Census Bureau in making data requests.
#'
#' @import data.table httr jsonlite
#'
#' @return A data.table
#'
#' @author Rick Dean
#'
#' @export
get_vintage_data <- function(
  dataset,
  vintage = NULL,
  vars,
  vars_init = c("GEO_ID", "NAME"),
  group = NULL,
  group_values = c("estimate", "moe"),
  region = NULL,
  regionin = NULL,
  na_cols = NULL,
  shape = "long",
  melt_meas = NULL,
  melt_values = NULL,
  key = Sys.getenv("CENSUS_KEY")
){
  dt <- NULL

  # Check for key in environment
  key_from_env <- Sys.getenv("CENSUS_KEY")
  if(key_from_env == "" & key == key_from_env){
    stop("'key' argument is missing. A Census Bureau key is required.")
  }

  # Create a string url based on the submitted parameters
  a_url <- .get_url(dataset, vintage)

  if(!is.null(group)){
    # Get the data.table for the group
    dt <- .get_dt(
      a_url = a_url,
      key = key,
      group = group,
      vars = NULL,
      region = region,
      regionin = regionin
    )

    # Just get estimates and margin of error columns
    E_M_list <- .get_E_M_both(dt,group_values)

    dt <- E_M_list[["dt"]]

    # Reshape from wide to long
    if(shape == "long") {
      if(length(group_values) == 2){
        dt <- data.table::melt(
          data = dt,
          measure = list(E_M_list[["E"]], E_M_list[["M"]]),
          value.name = c("estimate", "moe")
        )
        dt[, variable := factor(variable, levels = 1:length(E_M_list[["E"]]), labels = substr(E_M_list[["E"]],start = 1,stop = 10))]
        return(dt)
      }else if(group_values[[1]] == "estimate"){
        dt <- data.table::melt(
          data = dt,
          measure = list(E_M_list[["E"]]),
          value.name = "estimate"
        )
        return(dt)
      }else if(group_values[[1]] == "moe"){
        dt <- data.table::melt(
          data = dt,
          measure = list(E_M_list[["M"]]),
          value.name = "moe"
        )
      }
    }
  }else{ # non-group variables
    var_names <- vars
    meas <- NULL

    if(is.list(vars)){
      var_names <- unlist(vars)
      meas <- vars
    }else{
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

    if(!is.null(melt_meas)){
      shape = "wide"
    }

    if(shape == "long"){
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
    }

    if(!is.null(melt_meas)){
      dt <- data.table::melt(dt, measure = melt_meas)
      if(!is.null(melt_values)){
        dt[, variable := factor(variable, levels = 1:length(melt_values), labels = melt_values)]
      }
    }
  }

  if(!is.null(na_cols)){
    if(is.logical(na_cols)){
      dt <- na.omit(dt)
    }else if(is.vector(na_cols)){
      dt <- na.omit(dt, cols = na_cols)
    }
  }

  return(dt)
}

