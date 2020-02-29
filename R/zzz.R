##' @importFrom utils packageDescription

.onAttach <- function(libname, pkgname) {
    pkgVersion <- packageDescription(pkgname, fields = "Version")
    msg <- paste0("Welcome to ", pkgname, " v ", pkgVersion, "!")
    packageStartupMessage(msg)
}
