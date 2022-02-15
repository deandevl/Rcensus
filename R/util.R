#' @keywords internal
#'
.check_response <- function(resp){
  status <- resp[["status_code"]]
  if(!(status %in% c(200, 201, 202))){
    if(status == 404){
      stop("Invalid request, (404) not found.")
    }else if(status == 400){
      stop(
        paste0(
          "The Census Bureau returned the following error message:\n",
          resp[["error_message"]],
          "\n Your API call was: ", resp[["url"]]
        )
      )
    }else if(status == 204){
      stop(
        paste0(
          "204, No content was returned. \n",
          "Your API call was: ", resp[["url"]]
        )
      )
    }
  }else {
    return(TRUE)
  }
}

#' @keywords internal
#'
.get_url <- function(dataset, vintage){
  if(is.null(vintage)){
    apiurl <- paste("https://api.census.gov/data", dataset, sep = "/")
  }else {
    apiurl <- paste("https://api.census.gov/data", vintage, dataset, sep = "/")
  }
  return(apiurl)
}

#' @keywords internal
#'
.parse_response <- function(resp){
  content <- httr::content(resp, as="text")

  if(jsonlite::validate(content) == FALSE){
    stop(
      paste0(
        "The Census Bureau returned the following error message:",
        content
      )
    )
  }else{
    return(jsonlite::fromJSON(content))
  }
}

#' @keywords internal
#'
.get_dt <- function(a_url, key, group, vars, vars_init, region, regionin){
  get_vars <- NULL
  if(!is.null(group)){
    get_vars <- paste0("group(", group, ")")
  }else{
    if(is.null(vars_init)){
      get_vars <- paste(vars, sep = "", collapse = ",")
    }else{
      get_vars <- paste(c(vars_init, vars), sep = "", collapse = ",")
    }
  }

  resp <- httr::GET(
    url = a_url,
    query = list(
      "key" = key,
      "get" = get_vars,
      "for" = region,
      "in" = regionin
    )
  )

  # Check the response as valid JSON
  check <-  .check_response(resp)

  # Parse the response and return raw JSON
  raw_json <- .parse_response(resp)

  # Create data,table
  dt <- data.table::as.data.table(raw_json)

  colnames(dt) <- raw_json[1,]

  dt <- dt[-1]

  return(dt)
}

#' @keywords internal
#'
.get_E_M_both <- function(dt,values){
  dt_colnames <- colnames(dt)
  dt_colnames <- dt_colnames[!dt_colnames %in% c("NAME")]

  cols_E <- endsWith(dt_colnames, "E")
  cols_E <- dt_colnames[cols_E]

  cols_M <- endsWith(dt_colnames, "M")
  cols_M <- dt_colnames[cols_M]

  if(length(values) == 2){
    cols_all <- c("GEO_ID", "NAME")
    for(i in seq_along(cols_E)){
      cols_all <- c(cols_all, cols_E[[i]], cols_M[[i]])
    }
    dt <- dt[, ..cols_all]
  }else if(values[[1]] == "estimate"){
    cols_all <- c("GEO_ID", "NAME", cols_E)
    dt <- dt[, ..cols_all]
  }else if(values[[1]] == "moe"){
    cols_all <- c("GEO_ID", "NAME", cols_M)
    dt <- dt[, ..cols_all]
  }

  return(list(
    "dt" = dt,
    "E" = cols_E,
    "M" = cols_M
  ))
}
