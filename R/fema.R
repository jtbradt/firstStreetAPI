#' FEMA function
#'
#' This function queries the FEMA API:
#' @param lookup.type is the lookup type
#' @param loc.type is the type of location used in lookup
#' @param lookup.arg is the location used for the lookup
#' @keywords fema
#' @export

fema <- function(lookup.type, loc.type, lookup.arg) {
    # FEMA NFIP API only accepts zip, county, tract, and state lookups:
    if (!(loc.type %in% c("zcta", "county", "tract", "state"))) {
        stop("Invalid location type. FEMA NFIP API only accepts the following location lookups: 'zcta', 'county', 'tract', and 'state'.")
    }
    
    # Retrieve list of fema return objects for lookup arguments
    resp <- lapply(lookup.arg, function(l) {
        # Generate location lookup:
        lookup <- location.lookup(lookup = lookup.type, type = loc.type, arg = l)
        
        # Retrieve fema data for given location lookup from FSF API:
        temp.resp <- fsf.query("fema", "nfip", lookup)
        
        # If unsuccessful, return HTTP code:
        if (typeof(temp.resp) == "integer") {
            stop(paste0("FSF API query returned the following HTTP code:", temp.resp))
        }
        
        # Parse fema data:
        parsed <- jsonlite::fromJSON(httr::content(temp.resp, "text"), simplifyVector = TRUE)
        
        # Format parsed fema data:
        return <- data.table::data.table(t(parsed))
        return$fsid.type <- loc.type
        
        return(return)
    }) %>% do.call(rbind, .)
    
    return(resp)
}
