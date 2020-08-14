#' Tile function
#'
#' This function queries the historic API:
#' @param product is the tileset product
#' @param year is the year of annual risk requested from the probability depth product
#' @param return.period is the return period requested from the probability depth product
#' @param event.id is the event id for the event tile requested from the historic API
#' @param z is the zoom level (min=1, max=18)
#' @param x is the X (latitude) coordinate (Mercator projection)
#' @param y is the Y (longitude) coordinate (Mercator projection)
#' @keywords tile
#' @export

tile <- function(product, year, return.period, event.id, z, x, y) {
    # If product is probability depth:
    if (product == "probability") {
        # Construct probability depth tile query:
        tile.query <- paste0(paste(year, return.period, z, x, y, sep = "/"), ".png")
        
        # Retrieve probability depth tile:
        tile <- fsf.query("tile", "probability/depth", tile.query)
    } else if (product == "historic") {
        # Construct historic event tile query:
        tile.query <- paste0(paste(event.id, z, x, y, sep = "/"), ".png")
        
        # Retrieve historic event tile:
        tile <- fsf.query("tile", "historic/event", tile.query)
    } else {
        stop("Invalid tile product requested.  Please request either 'probability' or 'historic' tile products.")
    }
    return(tile)
}

