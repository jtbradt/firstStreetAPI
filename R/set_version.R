#' Define package environment
#' @name default.api.version
#' @description Sets default API version
#' @details Sets default API version. To change API version, use function set.api.version()
assign("api.version", "v1", envir = pkg.env)

#' Set the First Street Foundation API version
#'
#' This function stores the First Street Foundation  (FSF) API version
#' package's environmental variable
#'
#' @param version is the user's FSF key
#' @export
set.api.version = function(version = "v1") {
    assign("api.version", version, envir = pkg.env)
}
