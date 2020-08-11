#' Probability function
#'
#' This function queries the Adaptation Summary API:
#' @param lookup.type is the lookup type
#' @param loc.type is the type of location used in lookup
#' @param lookup.arg is the location used for the lookup
#' @param probability.api is the specific probability API (depth, chance, cumulative, count) to query
#' @keywords probability
#' @export

probability <- function(lookup.type, loc.type, lookup.arg, probability.api) {
    # If invalid location type-probability API combination requested, throw error:
    if ((loc.type != "property" & probability.api != "count") | (loc.type == "property" & probability.api == "count")) {
        stop(paste0("The ", probability.api, " API is not available for the ", loc.type, " location type. Request valid API."))
    }
    
    # Retrieve list of probability data for lookup arguments from indicated probability API:
    resp <- lapply(lookup.arg, function(l) {
        # Generate location lookup:
        lookup <- location.lookup(lookup = lookup.type, type = loc.type, arg = l)
        
        # Query indicated FSF probability API:
        temp.resp <- fsf.query("probability", probability.api, lookup)
        
        # If unsuccessful, return HTTP code:
        if (typeof(temp.resp) == "integer") {
            stop(paste0("FSF API query returned the following HTTP code:", temp.resp))
        }
        
        # Parse probability data:
        parsed <- jsonlite::fromJSON(httr::content(temp.resp, "text"), simplifyVector = TRUE)
        
        # Format conditional on location type:
        if (loc.type == "property") {
            # Extract and format parsed property probability data:
            return <- parsed[[2]] %>% tidyr::unnest(data) %>% data.table::data.table(.)
        } else {
            # Extract and format parsed non-property probability
            return <- parsed[[2]] %>% tidyr::unnest(data) %>% tidyr::unnest(data) %>% data.table::data.table(.)
        }
        
        # Extract fsid:
        return$fsid <- parsed[[1]]
        
        return(return)
    })
}
