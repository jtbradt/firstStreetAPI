#' Define package environment
#'
#' \code{pkg.env} is a package environment that contains the variable
#' \code{api.key} with the user's First Street Foundation (FSF) API key
#' @export
pkg.env = new.env()
assign("api.key", NULL, envir = pkg.env)


#' Get the First Street Foundation API key
#'
#' This function returns the user's First Street Foundation (FSF) API key that was defined with
#' \code{set.api.key}.
#'
#' @return the user's api key
#' @export
get.api.key = function() {
    get("api.key", envir = pkg.env)
}


#' Set the First Street Foundation API key
#'
#' This function stores a user's First Street Foundation  (FSF) API key as the
#' package's environmental variable
#'
#' @param key is the user's FSF key
#' @export
set.api.key = function(key) {
    assign("api.key", key, envir = pkg.env)
}
