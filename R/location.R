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
            if (geometry == TRUE) {
                stop("Cannot return geometry data from the summary API. Specify option 'detail == TRUE' to return geometry data.")
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
                return <- cbind(return[,-"properties"], t(unlist(return$properties)))
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
                geometry.data <- process.geometry(parsed$geometry$polygon)

                if (class(geometry.data)[1] != "character") {
                  # Bind geometry data to non-geometry data:
                  return <- sf::st_as_sf(cbind(return, geometry.data))
                }
            }
        }

        # Add location type field:
        return$fsid.type <- loc.type

        # Return object:
        return(return)
    })

    if (length(lookup.arg) > 1) {
        resp <- dplyr::bind_rows(resp)
    } else {
        resp <- resp[[1]]
    }

    return(resp)
}
