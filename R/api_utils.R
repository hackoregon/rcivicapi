#' @title GET from Civic API
#' @name get_civic_api
#' @description GETs data from a Civic API endpoint
#' @importFrom httr GET
#' @importFrom httr status_code
#' @importFrom httr content
#' @importFrom jsonlite fromJSON
#' @export get_civic_api
#' @param url the URL to GET
#' @param verbose print status info
#' @return a list of two items
#' \itemize{
#'   \item status_code the HTTP status code (200 for success)
#'   \item response if status_code == 200, the JSON object;
#'     otherwise, the raw text.
#' }
#' @examples
#' \dontrun{
#' response <- rcivicapi::get_civic_api(
#' "https://api.mysportsfeeds.com/v2.0/pull/nba/2018-playoff/games.json"
#' )}

get_civic_api <- function(url, verbose) {

  # you shouldn't have to change these!
  tries <- 5
  sleep_seconds <- 10

  # only some response codes should be retried
  retry_allowed <- c()

  for (ixtry in 1:tries) {
    if (verbose) print(url)
    response <- httr::GET(url)
    status_code <- httr::status_code(response)

    # was the GET successful?
    if (status_code == 200) {
      return(list(
        status_code = status_code,
        data = jsonlite::fromJSON(
          httr::content(response, as = "text", encoding = "UTF-8"),
          flatten = TRUE
        )
      ))
    }

    # is it a fatal code?
    if (!(status_code %in% retry_allowed)) {
      return(list(
        status_code = status_code,
        data = httr::content(
          response, as = "text", encoding = "UTF-8"
        )
      ))
    }

    # GET failed but we can retry - sleep and continue retry loop
    if (verbose) {
      print(paste(
        status_code, "sleeping", sleep_seconds
      ))
    }
    Sys.sleep(sleep_seconds)
  }

  # no retry succeeded - return the raw stuff
  return(list(
    status_code = status_code,
    data = httr::content(
      response, as = "text", encoding = "UTF-8")
  ))
}

utils::globalVariables(c(
))
