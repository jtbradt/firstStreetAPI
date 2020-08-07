#' Adaptation-projects function
#'
#' This function queries the Adaptation Summary API:
#' @param lookup.type is the lookup type
#' @param loc.type is the type of location used in lookup
#' @param lookup.arg is the location used for the lookup
#' @keywords adaptation.projects
#' @export
#' @import tidyverse
#' @import httr
#' @import jsonlite

adaptation.projects <- function(lookup.type, loc.type, lookup.arg) {
  # Generate location lookup:
  lookup <- lapply(lookup.arg, function(l){
    location.lookup(lookup = lookup.type,
                    type = loc.type,
                    arg = l)
  })

  # Query FSF API:
  resp <- lapply(lookup, function (l) {
    temp.resp <- fsf.query("adaptation", "summary", l)

    # If unsuccessful, return HTTP code:
    if (typeof(temp.resp) == "integer") {
      stop(paste0("FSF API query returned the following HTTP code:", temp.resp))
    }

    # Parse plain text list of adaptation IDs:
    return <- temp.resp$content
    return(return)
  })

  return(resp)
}
