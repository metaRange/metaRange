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

#' @title metaRangeEnvironment object
#'
#' @description Creates an [metaRangeEnvironment] object in form of an
#' [R6][R6::R6Class] class that stores and handles the environmental
#' values that influence the species in the simulation.
#'
#' @return An `<metaRangeEnvironment>` object
#' @export
metaRangeEnvironment <- R6::R6Class("metaRangeEnvironment",
    cloneable = FALSE,
    public = list(

        # ---------- 1 public fields -------------

        #' @field sourceSDS A *SpatRasterDataset* created by [terra::sds()]
        #' that holds all the environmental values influencing the simulation.
        #' Note that the individual data sets should be sensibly named as
        #' their names will used throughout the simulation to refer to them.
        sourceSDS = NULL,

        #' @field current an R environment that holds all the
        #' environmental values influencing the present / current time step of the
        #' simulation. These values are copies of the current layers
        #' of the respective individual data sets in the sourceSDS and they are
        #' stored as regular 2D R matrices under the same name given to the
        #' corresponding sub data set in the sourceSDS. These matrices are updated
        #' automatically at the beginning of each time step.
        current = NULL,

        # ---------- 2 initialization -----------

        #' @description Creates a new [metaRangeEnvironment] object.
        #' This is done automatically when a simulation is created. There is no need to
        #' call this as user.
        #' @param sourceSDS `<SpatRasterDataset>` created by [terra::sds()]
        #' that holds all the environmental values influencing the simulation.
        #' Note that the individual data sets should be sensibly named as
        #' their names will used throughout the simulation to refer to them.
        #' @examples
        #' # Note: Only for illustration purposes.
        #' # The environment is automatically created when creating a simulation.
        #' metaRangeEnvironment$new(
        #'      sourceSDS = terra::sds(
        #'          terra::rast(vals = 1, nrow = 2, ncol = 2)
        #'      )
        #' )
        #' @return A `<metaRangeEnvironment>` object
        initialize = function(sourceSDS = NULL) {
            private$set_source_environment(sourceSDS)
            self$current <- new.env()
            lockBinding("sourceSDS", self) # make sourceSDS non-mutable
        },


        # ---------- 3 public methods -----------

        #' @description Set current (active) time step / environment layer.
        #' No reason to call this as user. The current time step is set
        #' automatically by the simulation.
        #' @param layer `<integer>` layer number.
        #' @examples
        #' # Note: Only for illustration purposes.
        #' # The time step is automatically set by the simulation.
        #' sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2, nlyr = 2))
        #' names(sim_env) <- "env_01"
        #' env <- metaRangeEnvironment$new(sourceSDS = sim_env)
        #' env$set_current(layer = 1)
        #' @return `<invisible self>`
        set_current = function(layer) {
            layer <- checkmate::assert_int(
                x = layer,
                lower = 1,
                upper = min(terra::nlyr(self$sourceSDS)),
                coerce = TRUE
            )
            if (layer == private$current_layer) {
                return()
            }
            private$current_layer <- layer
            for (i in names(self$sourceSDS)) {
                self$current[[i]] <-
                    terra::as.matrix(
                        self$sourceSDS[[i]][[layer]],
                        wide = TRUE
                    )
            }
            return(invisible(self))
        },
        #' @description Prints information about the environment to the console
        #' @examples
        #' env <- metaRangeEnvironment$new(
        #'     sourceSDS = terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2, nlyr = 2))
        #' )
        #' env$print()
        #' @return `<invisible self>`
        print = function() {
            cat("Fields: \n")
            cat("$current ==== the environment at the current time step\n")
            cat("classes     : all -> matrix\n")
            cat("number      : ", length(names(self$current)), "\n", sep = "")
            cat("names       : ",
                ifelse(
                    length(names(self$current)) == 0,
                    "[NULL]",
                    paste0(names(self$current), collapse = ", ")),
                "\n", sep = ""
            )
            cat("$sourceSDS == the source raster data of the environment\n")
            show(self$sourceSDS)
            return(invisible(self))
        }
    ),
    private = list(
        # ---------- 1 private fields -------------
        # @field current_layer the current layer
        current_layer = -1L,
        # ---------- 2 private methods -----------
        # @description Set the sds / sourceSDS environment
        # @param sourceSDS `<SpatRasterDataset>` created by [terra::sds()]
        # that holds all the environmental values influencing the simulation.
        # @return `<invisible self>`
        set_source_environment = function(sourceSDS) {
            checkmate::assert_class(x = sourceSDS, classes = "SpatRasterDataset")
            nlayer <- terra::nlyr(sourceSDS)
            checkmate::assert_true(length(sourceSDS) >= 1)
            if (any(nlayer == 0)) stop("sourceSDS must have at least one layer")
            nlayer <- checkmate::assert_integerish(x = nlayer, lower = max(nlayer), upper = min(nlayer), coerce = TRUE)
            self$sourceSDS <- sourceSDS
            if (all(nchar(names(self$sourceSDS)) == 0)) {
                names(self$sourceSDS) <- paste0("env_", seq_len(length(self$sourceSDS)))
            }
            checkmate::assert_names(x = names(self$sourceSDS), type = "strict")
            return(invisible(self))
        }
    )
)
