#' Process geometry function
#'
#' This function process geometry returns from the FSF APIs:
#' @param parsed.data is the parsed response from the FSF API
#' @keywords process.geometry
#' @export

process.geometry <- function(parsed.data) {
    # Extract geometry data from parsed response:
    geometry <- parsed$geometry$polygon$coordinates

    if (is.null(geometry)) {
        warning("Empty geography returned. No geography data available from API for search.")
    } else {
        # If single array returned, format as list:
        if (class(geometry) == "array") {
            # Convert parsed geometry data to list of list of long-lat arrays:
            geometry <- lapply(seq(dim(geometry)[2]), function(i) {
                geometry[, i, , ] <- lapply(seq(dim(geometry)[1]), function(j) {
                  geometry[j, , , ]
                })
            })
        }

        # If list returned,
        if (class(geometry) == "list") {
          geometry <- lapply(geometry, function(g){
            if (class(g) == "list") {
              temp <- g
            }
            if (class(g) == "array") {
              temp <- lapply(seq(dim(g)[1]), function(j) {
                  g[j, , ]
              })
            }
            return(temp)
          })
        }

        # Convert nested list of long-lat arrays to sf object:
        geometry.sfc <- sf::st_sfc(sf::st_multipolygon(geometry)) %>% sf::st_set_crs(., 4326)

        # Return sf geometry object:
        return(geometry.sfc)
    }
}


