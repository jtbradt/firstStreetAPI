#' Location-lookup function
#'
#' This function constructs a location lookup:
#' @param lookup is the location lookup type
#' @param type is the location type for the lookup
#' @param arg is the location lookup argument
#' @keywords location.lookup
#' @export

location.lookup <- function(lookup, type, arg) {

  if(lookup == "fsid"){
    lookup <- paste(type, arg, sep = "/")
  }

  else if (lookup == "coordinate") {
    lat = strsplit(arg, ",")[[1]][1]
    lon = strsplit(arg, ",")[[1]][2]
    lookup <- paste0(type, "?lat=", lat, "&lng=", lon)
  }

  else if (lookup == "address") {
    lookup <- paste0(type, "?address=", arg)
  }

  else {
    stop("Lookup must be of type fsid, coordinate, or address.")
  }

  return(lookup)

}
