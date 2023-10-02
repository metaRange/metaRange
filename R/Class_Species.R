# Author: Stefan Fallert
# Date: 26.02.2023
# License: GPL-3 (See License.md)

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
        #' @field name name or ID of the species.
        name = NULL,

        #' @field processes The processes that describe how the species interacts
        #' with the environment, itself and other species.
        processes = NULL,

        #' @field traits The traits of the species.
        traits = NULL,

        #' @field sim A reference to the [metaRangeSimulation] object that the species is part of.
        #' Usefull to acces evironmental data or data of other species.
        sim = NULL,
        # ---------- initialization -----------

        #' @description Creates a new [metaRangeSpecies] object
        #' @param name `<string>` name or ID of the species.
        #' @param sim `<metaRangeSimulation>` A reference to the [metaRangeSimulation]
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
        #' @return invisible self
        print = function() {
            cat("Species: ", self$name, "\n")
            cat("processes: \n")
            cat(str(self$processes))
            cat("traits: \n")
            cat(str(self$traits))
            return(invisible(self))
        }
    )

    # TODO add private deep_clone method to allow speciation
    # https://r6.r-lib.org/articles/Introduction.html#cloning-objects
)
