#' Set the life cycle stage of a function
#'
#' @description The `lifeCycle` function is used to set the life cycle stage of
#'   a function. It is to be used within the body of the function that is being
#'   deprecated or defunct. It is a wrapper around both `.Deprecated` and
#'   `.Defunct` base R functions.
#'
#' @param newfun `character(1)` The name of the function to use instead. It can
#'   be a specific function within another package (e.g., `package::function`)
#'   or a function in the current package (e.g., `function`). If `newfun` is not
#'   specified, the calling function `oldfun` is assumed to be the replacement.
#'
#' @param newpackage `character(1)` If a function is moved to a new package, the
#'   name of the new package can be specified here. This is equivalent to
#'   specifying `newfun = paste0(newpackage, "::", newfun)`.
#'
#' @param package `character(1)` The name of the package where the deprecated
#'   or defunct function resides. It corresponds to the package from where the
#'   `lifeCycle` function is called.
#'
#' @param cycle `character(1)` The life cycle stage of the function. This can be
#'   either `"deprecated"` or `"defunct"`.
#'
#' @param title `character(1)` The Rd name prefix of the documentation page
#'   where deprecated or defunct functions are documented (e.g., "summary" for
#'   "summary-deprecated"). By default, the package name is used.
#'
#' @examples
#' test_fun <- function() {
#'     lifeCycle(newfun = "new_test", package = "BiocBaseUtils")
#' }
#' ## catch warning and convert to message
#' tryCatch(test_fun(), warning = function(w) message(w) )
#'
#' test_fun <- function() {
#'     lifeCycle(
#'         newfun = "new_test", package = "BiocBaseUtils", cycle = "defunct"
#'     )
#' }
#' ## catch error and convert to message
#' tryCatch(test_fun(), error = function(e) message(e))
#' @export
lifeCycle <- function(
    newfun = oldfun, newpackage, package,
    cycle = c("deprecated", "defunct"), title = package
) {
    removal <- missing(newfun) && missing(newpackage)
    oldfun <- as.character(sys.call(sys.parent())[1L])
    cycle <- match.arg(cycle)

    if (!missing(newpackage))
        newfun <- paste0(newpackage, "::", gsub("^.*::", "", newfun))

    msg <- c(
        gettextf("'%s' is %s.\n  ", oldfun, cycle),
        if (!removal) gettextf("Use '%s' instead.\n  ", newfun),
        gettextf("See help('%s').",  paste0(title, "-", cycle))
    )
    cycle_fun <- switch(cycle, deprecated = .Deprecated, defunct = .Defunct)
    arglist <- list(new = newfun, msg = msg, package = package)
    do.call(cycle_fun, arglist)
}
