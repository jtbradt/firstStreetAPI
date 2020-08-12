#' Location function
#'
#' This function queries the location API:
#' @param lookup.type is the lookup type
#' @param loc.type is the type of location used in lookup
#' @param lookup.arg is the location used for the lookup
#' @param detail is a boolean indicating whether detailed adaptaion project information should be returned
#' @param geometry is a boolean indicating whether geometry data should be returned
#' @keywords location
#' @export

location <- function(lookup.type, loc.type, lookup.arg, detail, geometry = FALSE) {
    # Retrieve list of adaptation projects for lookup arguments:
    resp <- lapply(lookup.arg, function(l) {
        # Generate location lookup:
        lookup <- location.lookup(lookup = lookup.type, type = loc.type, arg = l)
        
        # Format separate responses if summary or detail requested:
        if (detail == FALSE) {
            # If detail not requested but geometry requested, throw error
            if (detail == FALSE & geometry == TRUE) {
                stop("Cannot return geometry data from the detail API. Specify option 'detail == TRUE' to return geometry data.")
            }
            
            # Retrieve list of adaptation projects obtained from given location lookup from FSF API:
            temp.resp <- fsf.query("location", "summary", lookup)
            
            # If unsuccessful, return HTTP code:
            if (typeof(temp.resp) == "integer") {
                stop(paste0("FSF API query returned the following HTTP code:", temp.resp))
            }
            
            # Parse adaptation project summary data:
            parsed <- jsonlite::fromJSON(httr::content(temp.resp, "text"), simplifyVector = TRUE)
            
            # Format return object:
            return <- data.table::data.table(t(parsed))
            
            # If non-property type location, unnest property counts:
            if (loc.type != "property") {
                return <- return %>% pull(properties) %>% transpose %>% map_df(unlist) %>% bind_cols(return, .) %>% select(-properties)
            }
        } else {
            # Retrieve list of adaptation projects obtained from given location lookup from FSF API:
            temp.resp <- fsf.query("location", "detail", lookup)
            
            # If unsuccessful, return HTTP code:
            if (typeof(temp.resp) == "integer") {
                stop(paste0("FSF API query returned the following HTTP code:", temp.resp))
            }
            
            # Parse adaptation project summary data:
            parsed <- jsonlite::fromJSON(httr::content(temp.resp, "text"), simplifyVector = TRUE)
            
            # Format non-geometry return object:
            return <- parsed
            return[[length(return)]] <- NULL
            return <- data.frame(t(unlist(return)))
            
            # If geometry requested:
            if (geometry == TRUE) {
                # Extract geometry data from parsed response:
                geometry <- parsed$geometry$polygon$coordinates
                
                if (is.null(geometry)) {
                  warning("Empty geography returned. No geography data available from API for search.")
                } else {
                  # If single array returned, format as list:
                  if (class(geometry) == "array") {
                    # Convert parsed geometry data to list of list of long-lat arrays:
                    geometry <- lapply(seq(dim(geometry)[1]), function(i) {
                      geometry[i, , , ] <- lapply(seq(dim(geometry)[2]), function(j) {
                        geometry[, j, , ]
                      })
                    })
                  }
                  
                  # Convert nested list of long-lat arrays to sf object:
                  geometry.sfc <- sf::st_sfc(sf::st_multipolygon(geometry)) %>% sf::st_set_crs(., 4326)
                  
                  # Bind geometry data to non-geometry data:
                  return <- cbind(return, geometry.sfc) %>% sf::st_as_sf(.)
                }
            }
        }
        
        # Add location type field:
        return$fsid.type <- loc.type
        
        # Return object:
        return(return)
    }) %>% dplyr::bind_rows(.)
}
