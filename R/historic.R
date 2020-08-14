#' Historic function
#'
#' This function queries the historic API:
#' @param lookup.type is the lookup type
#' @param loc.type is the type of location used in lookup
#' @param lookup.arg is the location used for the lookup
#' @param event is a boolean indicating whether detailed event information should be returned
#' @param geometry is a boolean indicating whether geometry data should be returned
#' @keywords historic
#' @export

historic <- function(lookup.type, loc.type, lookup.arg, event = TRUE, geometry = TRUE) {
    # Retrieve list of historic events for lookup arguments:
    resp <- lapply(lookup.arg, function(l) {
        # Generate location lookup:
        lookup <- location.lookup(lookup = lookup.type, type = loc.type, arg = l)
        
        # Retrieve list of historic events obtained from given location lookup from FSF API:
        temp.resp <- fsf.query("historic", "summary", lookup)
        
        # If unsuccessful, return HTTP code:
        if (typeof(temp.resp) == "integer") {
            stop(paste0("FSF API query returned the following HTTP code:", temp.resp))
        }
        
        # Parse historic event summary data:
        parsed <- jsonlite::fromJSON(httr::content(temp.resp, "text"), simplifyVector = TRUE)
        
        # Generate fsid data:
        fsid <- data.table::data.table(parsed$fsid)
        names(fsid) <- "fsid"
        fsid$fsid.type <- loc.type
        
        # If summary requested, format summary response:
        if (event == FALSE) {
            # If event not requested but geometry requested, throw error
            if (geometry == TRUE) {
                stop("Cannot return geometry data from the summary API. Specify option 'event == TRUE' to return geometry data.")
            }
            
            # Generate summary historic data:
            if (length(parsed[[2]]) != 0) {
                historic.summary <- parsed[[2]] %>% unnest(data) %>% data.table::data.table(.)
            } else {
                historic.summary <- NULL
            }
            
            # Bind return object:
            return <- cbind(fsid, historic.summary)
        } else {
            # If no historic events for queried geometry, null return:
            if (length(parsed[[2]]) == 0) {
                return <- fsid
            } else {
                # Extract eventId from summary API response:
                event.id <- parsed$historic$eventId
                
                # For each eventId, retrieve response from event API:
                event.resp <- lapply(event.id, function(e) {
                  # Retrieve response from event API:
                  temp.resp.event <- fsf.query("historic", "event", e)
                  
                  # Parse response from event API:
                  parsed.event <- jsonlite::fromJSON(httr::content(temp.resp.event, "text"), simplifyVector = TRUE)
                  
                  # Process non-geometry data:
                  temp.return <- parsed.event
                  temp.return[[length(temp.return)]] <- NULL
                  temp.return <- data.frame(t(unlist(temp.return)))
                  
                  # If geometry requested:
                  if (geometry == TRUE) {
                    # Extract geometry data from parsed response:
                    geometry.data <- process.geometry(parsed.event$geometry$polygon)
                    
                    # If returned geometry data is non-empty:
                    if (class(geometry.data)[1] != "character") {
                      # Bind geometry data to non-geometry data:
                      temp.return <- cbind(temp.return, geometry.data) %>% sf::st_as_sf(.)
                    }
                  }
                  return(temp.return)
                }) %>% do.call(rbind, .)
                return <- event.resp
                return$fsid <- fsid$fsid
                return$fsid.type <- fsid$fsid.type
            }
        }
        
        return(return)
    }) %>% data.table::rbindlist(., fill = TRUE)
    
    # If geometry requested, convert final return to sf object:
    if ("geometry" %in% colnames(resp)) {
        resp <- sf::st_as_sf(resp)
    }
    return(resp)
}
