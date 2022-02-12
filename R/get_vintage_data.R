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
#' @param vars A string vector of variable names to be acquired.
#' @param group A string that names the group that has a collection of variables. A dataframe
#'   is returned in the "long" format with a factor column named "variables" with their respective
#'   values.
#' @param region A string that specifies the geography of the request.
#'    See \code{Rcensus::get_geography()} for assistance in obtaining
#'    these values.
#' @param regionin A string that sets a qualifier for \code{region}.
#' @param na_cols If TRUE will remove all rows with missing values. If a
#'   vector of column names/integers will check their values for missing values.
#' @param suffixes A vector string where column names ending with the suffixes will be kept.
#' @param melt_meas A \code{list} that contains character vectors of column names/indexes from
#'   \code{dataset} to be consolidated. It is the \code{measure.vars} argument in data.table's
#'   wide-to-long \href{https://rdatatable.gitlab.io/data.table/reference/melt.data.table.html}{melt}
#'   reshaping tool.
#' @param melt_var_name A character string (or character vector) that defines the new name(s) of the
#'   consolidated variable name column.
#' @param melt_val_name A character string (or character vector) that defines the new name(s) of the
#'   consolidated value column(s) defined in \code{melt_meas} above. It is the \code{value.name} argument
#'   in data.table's wide-to-long reshaping tool.
#' @param dcast_for The LHS ~ RHS formula to use in data.table's
#'   \href{https://rdatatable.gitlab.io/data.table/reference/dcast.data.table.html}{decast}
#'   long-to-wide reshaping tool.
#' @param dcast_var A vector of column names/indexes that defines the \code{value.var}
#'   argument in data.table's long-to-wide dcast.
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
  group = NULL,
  region = NULL,
  regionin = NULL,
  na_cols = NULL,
  suffixes = NULL,
  melt_meas = NULL,
  melt_var_name = "variable",
  melt_val_name = "value",
  dcast_for = NULL,
  dcast_var = NULL,
  key = Sys.getenv("CENSUS_KEY")
){

  # Check for key in environment
  key_from_env <- Sys.getenv("CENSUS_KEY")
  if(key_from_env == "" & key == key_from_env){
    stop("'key' argument is missing. A Census Bureau key is required.")
  }

  # Create a string url based on the submitted parameters
  a_url <- .get_url(dataset, vintage)

  get_var = NULL
  if(!is.null(group)){
    get_var = paste0("NAME,group(", group, ")")
    suffixes = c("E","M")
  }else {
    get_var <- paste(vars, sep = "", collapse = ",")
  }

  resp <- httr::GET(
    a_url,
    query = list(
      key = key,
      get = get_var,
      "for" = region,
      "in" = regionin
    )
  )
  # Check the response as valid JSON
  check <-  .check_response(resp)

  # Parse the response and return raw JSON
  raw_json <- .parse_response(resp)

  dt <- data.table::as.data.table(raw_json)

  colnames(dt) <- raw_json[1,]

  dt <- dt[-1]

  if(!is.null(suffixes)){
    dt_colnames <- colnames(dt)

    keep_cols <- endsWith(dt_colnames, suffixes[1])
    temp_dt <- dt[,..keep_cols]

    for(i in 2:length(suffixes)){
      keep_cols <- endsWith(dt_colnames, suffixes[i])
      temp_dt <- cbind(temp_dt, dt[,..keep_cols])

    }
    cols_unique <- unique(colnames(temp_dt))
    dt <- temp_dt[,..cols_unique]
  }

  if(!is.null(group)){
    col_names <- colnames(dt)
    col_names_E <- col_names[endsWith(col_names, "E")]
    col_names_E <- col_names_E[!col_names_E %in% c("NAME")]
    col_names_M <- col_names[endsWith(col_names, "M")]
    col_names_M <- col_names_M[!col_names_M %in% c("NAME")]

    dt <- data.table::melt(
      data = dt,
      ids.vars = "NAME",
      measure = list(col_names_E, col_names_M),
      value.name = c("Estimate", "MOE")
    )

    dt[, variable := factor(variable, levels = 1:length(col_names_E), labels = substr(col_names_E,start = 1,stop = 10))]
    dt[, MOE := ifelse(MOE == "-555555555",NA,MOE)]
  }


  if(!is.null(melt_meas)) {
    dt <- data.table::melt(dt, measure = melt_meas, variable.name = melt_var_name, value.name = melt_val_name)
  }else if(!is.null(dcast_for) & !is.null(dcast_var)){
    dt <- data.table::dcast(dt, formula = dcast_for, value.var = dcast_var)
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
