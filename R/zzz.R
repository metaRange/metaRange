.onAttach <- function(lib, pkg) {
    pkgVersion <- as.character(utils::packageVersion("metaRange"))
    if (!interactive()) {
        msg <- paste0(c("metaRange version: ", pkgVersion))
    } else {
        msg <- paste0(c("Welcome to metaRange version: ", pkgVersion))
    }
    packageStartupMessage(msg)
}

#' @references Fallert, S., Li, L., & Sarmento Cabral, J. (2023).
#' metaRange: Framework to Build Mechanistic and Metabolic Constrained Species Distribution Models.
#' Zenodo. \doi{10.5281/zenodo.10364778}
#' @keywords internal
"_PACKAGE"

# Why this ^? See:
# https://roxygen2.r-lib.org/articles/rd-other.html

# import compiled code
#' @useDynLib metaRange

# Import some functions from the 'Imports' packages
# To suppress warnings about 'All inputs should be used'
#' @importFrom Rcpp evalCpp
#' @importFrom R6 R6Class
#' @importFrom terra writeRaster
#' @importFrom checkmate assert_function
#' @importFrom utils write.csv
NULL
