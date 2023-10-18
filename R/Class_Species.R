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

#' @title metaRangeSpecies object
#'
#' @description Creates an species object in form of an
#' [R6][R6::R6Class] class that stores and handles all the individual parts
#' that define a species.
#'
#' @return A `<metaRangeSpecies>` object.
#' @export
metaRangeSpecies <- R6::R6Class("metaRangeSpecies",
    cloneable = FALSE,
    lock_objects = FALSE,
    public = list(
        # ---------- public fields -------------
        #' @field name `<string>` name or ID of the species.
        name = NULL,

        #' @field processes `<list>` of `<[metaRangeProcess]es>`.
        #' The processes that describe how the species interacts
        #' with the environment, itself and other species.
        processes = NULL,

        #' @field traits `<list>` the traits of the species.
        traits = NULL,

        #' @field sim `<[metaRangeSimulation]>` a reference to the simulation object that the species is part of.
        #' Useful to access environmental data or data of other species.
        sim = NULL,
        # ---------- initialization -----------

        #' @description Creates a new [metaRangeSpecies] object
        #' @param name `<string>` name or ID of the species.
        #' @param sim `<[metaRangeSimulation]>` A reference to the simulation
        #' object that the species is part of.
        #' @examples
        #' # The following is bad practice, since species should be added to a simulation
        #' # via the add_species method of the simulation object. But for illustration
        #' # purposes:
        #' sim_env <- terra::sds(terra::rast(nrow = 2, ncol = 2))
        #' test_sim <- metaRangeSimulation$new(source_environment = sim_env)
        #' sp <- metaRangeSpecies$new(name = "species_01", sim = test_sim)
        #' sp
        initialize = function(name, sim) {
            checkmate::assert_class(sim, "metaRangeSimulation")
            cond_01 <- is.null(sim$environment)
            cond_02 <- !checkmate::test_class(sim$environment, "metaRangeEnvironment")
            if (cond_01 || cond_02) {
                message("cannot add species to a simulation without environment. Please add environment first.")
                return()
            }
            self$sim <- sim

            checkmate::assert_string(name)
            self$name <- name

            self$processes <- list()
            self$traits <- list()
        },
        # ---------- public methods -----------
        #' @description Prints information about the species to the console
        #' @return `<invisible self>`
        print = function() {
            cat("Species: ", self$name, "\n")
            cat("processes: \n")
            cat(str(self$processes))
            cat("traits: \n")
            cat(str(self$traits))
            return(invisible(self))
        }
    )
)
