#' FSF query function
#'
#' This function constructs a fsf API request:
#' @param api.cat is one of FSF's 7 API categories
#' @param api is one of FSF's 18 APIs
#' @param arg is a query argument
#' @keywords fsf.query
#' @export
#' @import httr

fsf.query <- function(api.cat, api, arg) {

  # Create path:
  path <- paste(pkg.env$api.version, api.cat, api, arg, sep = "/")

  # Query FSF API:
  url <- modify_url("https://api.firststreet.org/", path = path)
  resp <- GET(url, query = list(key = pkg.env$api.key))

  if(resp$status_code == "200") {
    return(resp)
  } else {
    return(resp$status_code)
  }

}
