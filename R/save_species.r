# Copyright (C) 2023, 2024 Stefan Fallert, Lea Li, Juliano Sarmento Cabral
#
# This file is part of metaRange.
#
# metaRange is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, version 3.
#
# metaRange is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with metaRange. If not, see <http://www.gnu.org/licenses/>.

#' Save function
#'
#' Saves the specified traits of a [metaRangeSpecies] object.
#'
#' @param x `<metaRangeSpecies>` [metaRangeSpecies] object.
#' @param traits `<character>` `NULL` or a character vector specifying the trait to save.
#' If `NULL`, all traits are saved.
#' @param prefix `<string>` prefix for the file names or `NULL`.
#' @param path `<string>`path to the directory where the files are saved.
#' @param overwrite `<boolean>` overwrite existing files.
#' @param ... additional arguments passed to [terra::writeRaster].
#' @details The generated file names are of the form
#' `file.path(path, paste0(prefix, species_name, "_", trait_name, ".file_extension"))`.
#' If the trait is in a matrix or raster form, the file extension is `.tif`. Otherwise it is `.csv`.
#' The prefix is optional and mainly useful to add a time step to the file name, in case the trait
#' is saved multiple times during a simulation.
#' @examples
#' sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2))
#' names(sim_env) <- "env_01"
#' test_sim <- metaRangeSimulation$new(source_environment = sim_env)
#' test_sim$add_species("species_01")
#' test_sim$add_traits(
#'     "species_01",
#'     trait_01 = matrix(1, nrow = 2, ncol = 2),
#'     trait_02 = matrix(2, nrow = 2, ncol = 2)
#' )
#'
#' file_prefix <- "This_could_be_a_time_step"
#' directory_name <- tempdir()
#'
#' res_path <- save_species(
#'     test_sim$species_01,
#'     traits = "trait_01",
#'     prefix = file_prefix,
#'     path = directory_name
#' )
#' # the following should be TRUE
#' # but might fail due to floating point errors (that's why we round the values)
#' identical(
#'     round(terra::as.matrix(terra::rast(res_path), wide = TRUE)),
#'     round(test_sim$species_01$traits[["trait_01"]])
#' )
#'
#' # test overwrite
#' res_path2 <- save_species(
#'     test_sim$species_01,
#'     traits = "trait_01",
#'     prefix = file_prefix,
#'     path = directory_name,
#'     overwrite = TRUE
#' )
#' stopifnot(identical(res_path, res_path2))
#'
#' # Saving all traits
#' res_path3 <- save_species(
#'     test_sim$species_01,
#'     prefix = basename(tempfile()),
#'     path = directory_name
#' )
#' res_path3
#' # cleanup
#' unlink(c(res_path, res_path3))
#' stopifnot(all(!file.exists(res_path, res_path3)))
#' @return `<invisible character>` the paths to the saved files.
#' @export
save_species <- function(x, traits = NULL, prefix = NULL, path, overwrite = FALSE, ...) {
    checkmate::assert_class(x, "metaRangeSpecies")
    checkmate::assert_character(traits, null.ok = TRUE, unique = TRUE)
    if (!checkmate::test_string(prefix, null.ok = TRUE)) {
        prefix <- as.character(prefix)
        checkmate::assert_string(prefix, null.ok = TRUE)
    }
    checkmate::assert_flag(overwrite)
    if (is.null(traits)) {
        traits <- names(x[["traits"]])
    }
    return_paths <- c()
    full_path <- c()
    for (att in traits) {
        if (is.null(x$traits[[att]])) {
            warning(att, " is not an trait of species: ", x$name, call. = TRUE)
            next
        }
        if (inherits(x$traits[[att]], "matrix")) {
            dim_m <- dim(x$traits[[att]])[c(1, 2)]
            dim_r <- dim(x$sim$environment$sourceSDS)[c(1, 2)]

            if (!is.null(dim_m) && all(dim_m == dim_r)) {
                r <- terra::rast(
                    x$sim$environment$sourceSDS[[1]],
                    nlyrs = 1,
                    vals = x$traits[[att]]
                )
            } else {
                r <- terra::rast(x$traits[[att]])
            }
            full_path <- file.path(path, paste0(prefix, x$name, "_", att, ".tif"))
            checkmate::assert_path_for_output(full_path, overwrite = overwrite)
            terra::writeRaster(r, full_path, overwrite = overwrite, ...)
        } else if (checkmate::test_atomic(x$traits[[att]])) {
            full_path <- file.path(path, paste0(prefix, x$name, "_", att, ".csv"))
            checkmate::assert_path_for_output(full_path, overwrite = overwrite)
            write.csv(x$traits[[att]], full_path, row.names = FALSE)
        } else {
            warning(
                "Couldn't save trait: ", att, " unknown format.\n",
                "Use: [saveRDS()] to save arbitrary data."
            )
        }
        return_paths <- c(return_paths, full_path)
    }
    return(invisible(return_paths))
}
