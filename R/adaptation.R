#' Adaptation projects function
#'
#' This function queries the Adaptation Summary API:
#' @param lookup.type is the lookup type
#' @param loc.type is the type of location used in lookup
#' @param lookup.arg is the location used for the lookup
#' @param detail is a boolean indicating whether detailed adaptaion project information should be returned
#' @param geometry is a boolean indicating whether geometry data should be returned
#' @keywords adaptation
#' @export

adaptation <- function(lookup.type, loc.type, lookup.arg, detail = TRUE, geometry = TRUE) {
    # Retrieve list of adaptation projects for lookup arguments:
    resp <- lapply(lookup.arg, function(l) {
        # Generate location lookup:
        lookup <- location.lookup(lookup = lookup.type, type = loc.type, arg = l)
        
        # Retrieve list of adaptation projects obtained from given location lookup from FSF API:
        temp.resp <- fsf.query("adaptation", "summary", lookup)
        
        # If unsuccessful, return HTTP code:
        if (typeof(temp.resp) == "integer") {
            stop(paste0("FSF API query returned the following HTTP code:", temp.resp))
        }
        
        # Parse adaptation project summary data:
        parsed <- jsonlite::fromJSON(httr::content(temp.resp, "text"), simplifyVector = TRUE)
        
        # Generate fsid data:
        fsid <- data.table::data.table(parsed$fsid)
        names(fsid) <- "fsid"
        fsid$fsid.type <- loc.type
        
        # If summary requested, format summary response:
        if (detail == FALSE) {
            # If detail not requested but geometry requested, throw error
            if (geometry == TRUE) {
                stop("Cannot return geometry data from the summary API. Specify option 'detail == TRUE' to return geometry data.")
            }
            
            # Generate summary adaptaion data:
            if (length(parsed[[2]]) != 0) {
                adaptation.summary <- data.table::data.table(cbind(parsed$adaptation, parsed$properties))
                names(adaptation.summary) <- c("adaptation", "properties")
            } else {
                adaptation.summary <- NULL
            }
            
            # Bind return object:
            return <- cbind(fsid, adaptation.summary)
        } else {
            # If no adaptation projects for queried geometry, null return:
            if (length(parsed[[2]]) == 0) {
                return <- fsid
            } else {
                # Extract adaptation ids from summary API response
                adaptation.id <- parsed$adaptation
                
                # For each adaptation id, retrieve response from detail API:
                adapt.resp <- lapply(adaptation.id, function(a) {
                  # Retrieve response from detail API:
                  temp.resp.adapt <- fsf.query("adaptation", "detail", a)
                  
                  # Parse response from detail API:
                  parsed.adapt <- jsonlite::fromJSON(httr::content(temp.resp.adapt, "text"), simplifyVector = TRUE)
                  
                  # Process non-geometry data:
                  temp.return <- parsed.adapt
                  temp.return[[length(temp.return)]] <- NULL
                  temp.return <- data.frame(t(unlist(temp.return)))
                  
                  # If geometry requested:
                  if (geometry == TRUE) {
                    # Extract geometry data from parsed response:
                    geometry.data <- process.geometry(parsed.adapt)
                    
                    # If retruned geometry data is non-empty:
                    if (class(geometry.data)[1] != "character") {
                      # Bind geometry data to non-geometry data:
                      temp.return <- cbind(temp.return, geometry.data) %>% sf::st_as_sf(.)
                    }
                  }
                  return(temp.return)
                }) %>% data.table::rbindlist(., fill = TRUE)
                return <- adapt.resp
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

