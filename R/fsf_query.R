#' FSF query function
#'
#' This function constructs a fsf API request:
#' @param api.cat is one of FSF's 7 API categories
#' @param api is one of FSF's 18 APIs
#' @param location is a formatted location lookup object (see location.lookup fxn)
#' @keywords fsf.query
#' @export
#' @import httr

fsf.query <- function(api.cat, api, location) {

  # Create path:
  path <- paste(pkg.env$version, api.cat, api, location, sep = "/")

  # Query FSF API:
  url <- modify_url("https://api.firststreet.org/", path = path)
  resp <- GET(url, query = list(key = pkg.env$api.key))

  if(resp$status_code == "200") {
    return(resp)
  } else {
    return(resp$status_code)
  }

}
