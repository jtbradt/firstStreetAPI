#' Environmental function
#'
#' This function queries the historic API:
#' @param lookup.type is the lookup type
#' @param loc.type is the type of location used in lookup
#' @param lookup.arg is the location used for the lookup
#' @param api is the specific environmental API to query
#' #' @param geometry is a boolean indicating whether geometry data should be returned
#' @keywords environmental
#' @export

environmental <- function(lookup.type, loc.type, lookup.arg, api, geometry = FALSE) {
    # If valid API not requested, return error:
    if (api != "precipitation" & api != "sea-level") {
        stop("Invalid request. Must request one of the two environmental APIs: 'precipitation' or 'sea-level.'")
    }
    
    # If non-state location type requested for sea-level API, return error:
    if (api == "sea-level" & loc.type != "state") {
        stop("Invalid request. Sea level API only works with state lookups.")
    }
    
    # If geography larger than county requested for the precipitation API, return error:
    if (api == "precipitation" & loc.type == "state") {
        stop("Invalid request. Precipiration API returns data at the county-level and requires county- or sub-county-level lookups.")
    }
    
    # Retriev list of environmental data for lookup arguments:
    resp <- lapply(lookup.arg, function(l) {
        # Generate location lookup:
        lookup <- location.lookup(lookup = lookup.type, type = loc.type, arg = l)
        
        # Retrieve environmental data obtained from given location lookup:
        temp.resp <- fsf.query("environmental", api, lookup)
        
        # If unsuccessful, return HTTP code:
        if (typeof(temp.resp) == "integer") {
            stop(paste0("FSF API query returned the following HTTP code:", temp.resp))
        }
        
        # Parse environmental data:
        parsed <- jsonlite::fromJSON(httr::content(temp.resp, "text"), simplifyVector = TRUE)
        
        # Format parsed data:
        return <- cbind(parsed[[1]], parsed[[2]])
        data.table::setnames(return, old = "parsed[[1]]", new = "fsid")
        return$fsid.type <- loc.type
        
        # If sea level data requested, retrieve detailed sea level data:
        if (api == "sea-level") {
            # Extract tideStationId from API response:
            tide <- return$tideStationId
            
            # For each tideStationId, retrieve detailed response from tide station API:
            tide.resp <- lapply(tide, function(s) {
                # Retrieve response from tide station API:
                temp.resp.tide <- fsf.query("environmental", "tide-station", s)
                
                # Parse response from tide station API:
                parsed.tide <- jsonlite::fromJSON(httr::content(temp.resp.tide, "text"), simplifyVector = TRUE)
                
                # Generate station id data:
                station <- data.frame(t(unlist(parsed.tide[1:3])))
                
                # Generate historic SLR data object:
                if (length(parsed.tide$slrHistoric) != 0) {
                  slr.historic <- cbind(station, dplyr::distinct(parsed.tide$slrHistoric))
                } else {
                  slr.historic <- station
                }
                
                # Generate future SLR data object:
                if (length(parsed.tide$slrProjected) != 0) {
                  slr.projected <- cbind(station, unique(parsed.tide$slrProjected$year), dplyr::distinct(parsed.tide$slrProjected$data))
                  data.table::setnames(slr.projected, old = "unique(parsed.tide$slrProjected$year)", new = "year")
                } else {
                  slr.projected <- station
                }
                
                
                # Process geometry data:
                if (geometry == TRUE) {
                  # Extract geometry data from parsed response:
                  geometry.data <- process.geometry(parsed.tide$center)
                  
                  # If returned geometry data is non-empty:
                  if (class(geometry.data)[1] != "character") {
                    # Bind geometry data to non-geometry data:
                    slr.historic <- cbind(data.table::data.table(slr.historic), geometry.data) %>% sf::st_as_sf(.)
                    slr.projected <- cbind(data.table::data.table(slr.projected), geometry.data) %>% sf::st_as_sf(.)
                  }
                }
                
                # Generate return:
                temp.return <- list(slr.historic, slr.projected)
                return(temp.return)
            })
            return <- lapply(1:2, function(x) {
                temp <- lapply(1:length(tide.resp), function(y) {
                  tide.resp[[y]][[x]]
                }) %>% data.table::rbindlist(., fill = TRUE)
                temp$fsid <- parsed[[1]]
                temp$fsid.type <- loc.type
                return(temp)
            })
        }
        
        return(return)
    })
    
    # Format return for each API type:
    if (api == "precipitation") {
        resp <- do.call(rbind, resp)
    } else {
        resp <- lapply(1:2, function(x) {
            temp <- lapply(1:length(resp), function(y) {
                resp[[y]][[x]]
            }) %>% data.table::rbindlist(., fill = TRUE)
            # If geometry requested, convert final return to sf object:
            if ("geometry.data" %in% colnames(temp)) {
                temp <- sf::st_as_sf(temp)
            }
            return(temp)
        })
        
    }
    
    
    return(resp)
}
