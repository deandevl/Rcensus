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
