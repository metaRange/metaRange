# Copyright (C) 2023 Stefan Fallert, Lea Li, Juliano Sarmento Cabral
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

#' Set verbosity of metaRange simulation
#'
#' Just a wrapper for `options(metaRange.verbose = [0 | 1 | 2])` but documented.
#' If `0`, metaRange functions will print no messages to the console.
#' If `1`, metaRange functions will print some messages to the console.
#' If `2`, metaRange functions will print many messages to the console.
#'
#' @param verbose `<integer>` message verbosity (see description).
#' @examples
#' set_verbosity(0)
#' getOption("metaRange.verbose")
#' @return `<invisible list>` a list with the previous verbosity setting.
#' @export
set_verbosity <- function(verbose) {
    checkmate::assert_int(verbose, lower = 0L, upper = 2L, coerce = TRUE)
    res <- options(metaRange.verbose = verbose)
    return(invisible(res))
}

#' Summary for metaRange simulation
#'
#' Print a summary of the simulation to the console.
#' @param object `<metaRangeSimulation>` The [metaRangeSimulation] object to summarize.
#' @param ... `<any>` ignored.
#' @examples
#' sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2))
#' names(sim_env) <- "env_01"
#' test_sim <- metaRangeSimulation$new(source_environment = sim_env)
#' test_sim$add_species("species_01")
#' summary(test_sim)
#' @return `<invisible NULL>`
#' @export
summary.metaRangeSimulation <- function(object, ...) {
    object$summary()
    return(invisible(NULL))
}

#' Summary for metaRange species
#' @param object `<metaRangeSpecies>` The [metaRangeSpecies] object to summarize.
#' @param ... `<any>` ignored.
#' @examples
#' sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2))
#' names(sim_env) <- "env_01"
#' test_sim <- metaRangeSimulation$new(source_environment = sim_env)
#' test_sim$add_species("species_01")
#' summary(test_sim$species_01)
#' @return `<invisible NULL>`
#' @export
summary.metaRangeSpecies <- function(object, ...) {
    cat("Species name: ", object$name, "\n")
    cat("Species processes: \n")
    print(object$processes)
    cat("Species traits: \n")
    print(object$traits)
    return(invisible(NULL))
}

#' Print traits or globals
#'
#' Print method for species traits and simulation globals.
#' @param x `<metaRangeVariableStorage>` The object to print.
#' @param ... `<any>` ignored.
#' @examples
#' sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2))
#' names(sim_env) <- "env_01"
#' test_sim <- metaRangeSimulation$new(source_environment = sim_env)
#' test_sim$add_species("species_01")
#' test_sim$add_traits(species = "species_01", a = 1)
#' print(test_sim$species_01$traits)
#' test_sim$add_globals(b = 2)
#' print(test_sim$globals)
#' @return `<invisible x>`
#' @export
print.metaRangeVariableStorage <- function(x, ...) {
    if (length(ls(x, sorted = FALSE)) == 0) {
        cat("NULL\n")
    } else {
        print(utils::ls.str(x))
    }
    return(invisible(x))
}

#' Create a simulation
#'
#' Creates a [metaRangeSimulation] object.
#' A convenience wrapper for `metaRangeSimulation$new()`.
#' @param source_environment `<SpatRasterDataset>` created by [terra::sds()] that represents the environment.
#' The individual data sets represent different environmental variables
#' (e.g. temperature or habitat availability) and the different layer of the data sets
#' represent the different timesteps of the simulation.
#' The function [metaRangeSimulation]`$set_time_layer_mapping()` can be used
#' to extend/ shorten the simulation timesteps and set the mapping between each time step and a corresponding
#' environmental layer. This can be used e.g. to repeat the first (few) layer as a burn-in period.
#' The number of layers must be the same for all data sets.
#' @param ID `<string>` optional simulation identification string.
#' Will be set automatically if none is specified.
#' @param seed `<integer>` optional seed for the random number generator.
#' Will be set automatically if none is specified.
#' @examples
#' sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2))
#' names(sim_env) <- "env_01"
#' test_sim <- create_simulation(sim_env)
#' @return A [metaRangeSimulation] object
#' @export
create_simulation <- function(source_environment, ID = NULL, seed = NULL) {
    return(metaRangeSimulation$new(source_environment = source_environment, ID = ID, seed = seed))
}
