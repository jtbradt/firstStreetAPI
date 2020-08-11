#' Adaptation projects function
#'
#' This function queries the Adaptation Summary API:
#' @param lookup.type is the lookup type
#' @param loc.type is the type of location used in lookup
#' @param lookup.arg is the location used for the lookup
#' @keywords adaptation.projects
#' @export

adaptation.projects <- function(lookup.type, loc.type, lookup.arg, detail = TRUE, geometry = TRUE) {
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
        temp.return <- data.table::data.table(cbind(parsed$fsid, parsed$adaptation, parsed$properties))
        
        # Set to null if no adaptation projects returned:
        if (ncol(temp.return) == 1) {
            temp.return <- NULL
        } else {
            # Set summary field names:
            names(temp.return) <- c("fsid", "adaptation", "properties")
            
            # If detailed data requested, retrieve from FSF API:
            if (detail == TRUE) {
                # Retrieve detailed data on unique adaptation projects:
                detail <- adaptation.detail(temp.return$adaptation, geometry = geometry)
                
                # Add FSID and FSID type fields:
                temp.return <- lapply(detail, function(x) {
                  x$fsid <- l
                  x$fsid.type <- loc.type
                  return(x)
                })
            } else {
                # Add FSID type field:
                temp.return$fsid.type <- loc.type
            }
        }
        return(temp.return)
    })
    
    # If detailed data requested, format and or throw null return error if applicable:
    if (detail == TRUE) {
        # Bind across data types (detail and geometry):
        return <- lapply(1:length(resp[[1]]), function(l) do.call(rbind, lapply(resp, function(x) x[[l]])))
        
        # Stop if no adaptation projects in full query:
        if (length(return[[1]]) == 0) {
            stop("No adaptation projects returned for query. Try a different geography.")
        }
    } else {
        # Name return object:
        return <- do.call(rbind, resp)
        
        # Stop if no adaptation projects in full query:
        if (length(return) == 0) {
            stop("No adaptation projects returned for query. Try a different geography.")
        }
    }
    
    # Return relevant return object:
    return(return)
}

#' Adaptation detail function
#'
#' This function queries the Adaptation Detail API:
#' @param adaptation.ids is a list of adaptation ids
#' @keywords adaptation.detail
#' @export

adaptation.detail <- function(adaptation.ids, geometry = TRUE) {
    # Loop over adaptation.ids argument:
    resp <- lapply(adaptation.ids, function(i) {
        # Retrieve detailed data on adaptaion project from FSF API:
        temp.resp <- fsf.query("adaptation", "detail", i)
        
        if (typeof(temp.resp) == "integer") {
            temp.return <- NULL
        } else {
            # Parse detailed adaptation project data:
            parsed <- jsonlite::fromJSON(httr::content(temp.resp, "text"), simplifyVector = TRUE)
            
            # Replace null return objects with NA:
            parsed <- lapply(parsed, function(x) {
                if (is.null(x)) {
                  x <- NA
                } else {
                  x <- x
                }
            })
            
            # Construct detail.data detail object for export:
            detail <- data.table::data.table(cbind(parsed$adaptationId, parsed$name, tidyr::expand_grid(parsed$type, parsed$scenario), 
                parsed$conveyance, parsed$returnPeriod, t(parsed$serving)))
            data.table::setnames(detail, names(detail)[1:6], new = c("adaptation", "name", "type", "scenario", "conveyance", 
                "return.period"))
            
            # If geometry is specified, parse and format as SF object:
            if (geometry == TRUE) {
                # Extract geometry data from parsed response:
                geometry <- parsed$geometry$polygon$coordinates
                
                # Convert parsed geometry data to list of list of long-lat arrays:
                geometry.list <- lapply(seq(dim(geometry)[1]), function(i) {
                  geometry[i, , , ] <- lapply(seq(dim(geometry)[2]), function(j) {
                    geometry[, j, , ]
                  })
                })
                
                # Convert nested list of long-lat arrays to sf object:
                geometry.sfc <- sf::st_sfc(sf::st_multipolygon(geometry.list)) %>% sf::st_set_crs(., 4326)
                
                # Generate df of adaptation id:
                adaptation.id <- data.frame(i)
                names(adaptation.id) <- "adaptation"
                
                # Bind adaptation id field to features:
                detail.geometry <- cbind(adaptation.id, geometry.sfc) %>% sf::st_as_sf(.)
                
                # List adaptation project detailed data and geographic data objects:
                detail <- list(detail, detail.geometry)
            }
            
            # Return requested detail data from parsed successful response:
            temp.return <- detail
        }
        
        # Return either HTTP code or parsed successful response:
        return(temp.return)
    })
    
    # Bind across data types (detail and geometry):
    return <- lapply(1:length(resp[[1]]), function(l) do.call(rbind, lapply(resp, function(x) x[[l]])))
    return(return)
}
