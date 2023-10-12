# Author: Stefan Fallert
# Date: 26.02.2023
# License: GPL-3 (See License.md)


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
        #' environmental values influencing the present time step of the
        #' simulation as regular 2D R matrices.
        current = new.env(),

        # ---------- 2 initialization -----------

        #' @description Creates a new [metaRangeEnvironment] object.
        #' This is done automatically when a simulation is created. No need to
        #' call this as user.
        #' @param sourceSDS `<SpatRasterDataset>` created by [terra::sds()]
        #' that holds all the environmental values influencing the simulation.
        #' Note that the individual data sets should be sensibly named as
        #' their names will used throughout the simulation to refer to them.
        #' @examples
        #' # Note: Only for illustration purposes.
        #' env <- metaRangeEnvironment$new(sourceSDS = terra::sds(terra::rast(nrow = 2, ncol = 2)))
        #' env
        #' @return An `<metaRangeEnvironment>` object
        initialize = function(sourceSDS = NULL) {
            private$set_source_environment(sourceSDS)
            lockBinding("sourceSDS", self) # make sourceSDS non-mutable
        },


        # ---------- 3 public methods -----------

        #' @description Set current (active) time step / environment.
        #' No reason to call this as user. The current time step is set automatically.
        #' @param layer `<integer>` layer
        #' @examples
        #' # Only for illustration purposes.
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
        # Note that the individual data sets/ raster should be sensibly named as
        # their names will used throughout the simulation to refer to them.
        # @return `<invisible self>`
        set_source_environment = function(sourceSDS) {
            checkmate::assert_class(x = sourceSDS, classes = "SpatRasterDataset")
            nlayer <- terra::nlyr(sourceSDS)
            if (any(nlayer == 0)) stop("sourceSDS must have at least one layer")
            nlayer <- checkmate::assert_integerish(x = nlayer, lower = max(nlayer), upper = min(nlayer), coerce = TRUE)
            self$sourceSDS <- sourceSDS
            return(invisible(self))
        }
    )
)
