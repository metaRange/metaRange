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

#' Plotting function
#'
#' Plots the specified element of a [metaRangeSimulation] object.
#'
#' @param x `<metaRangeSimulation>` [metaRangeSimulation] object.
#' @param obj `<string>` either the string `environment` or the name of a species.
#' @param name `<string>` either the name of an environment of the name of a species trait.
#' @param col `<character>` colors to use. Defaults to `grDevices::hcl.colors()` with
#' `n = 50` and a random palette.
#' @param ... additional arguments passed to [terra::plot] or [base::plot].
#' @examples
#' sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2))
#' names(sim_env) <- "env_01"
#' test_sim <- metaRangeSimulation$new(source_environment = sim_env)
#' test_sim$add_species("species_01")
#' test_sim$add_traits("species_01", trait_01 = matrix(1, nrow = 2, ncol = 2))
#' plot(test_sim, "species_01", "trait_01")
#' @return `<invisible NULL>`.
#' @export
plot.metaRangeSimulation <- function(x, obj, name, col, ...) {
    if (missing(x) || is.null(x)) {
        stop("No 'x' supplied. Nothing to plot", call. = FALSE)
    }
    checkmate::assert_string(obj)
    env_names <- names(x$environment$current)
    species_names <- x$species_names()
    possible_names <- c(env_names, species_names)
    if (obj %in% possible_names) {
        plot(x[[obj]], name, col, ...)
    } else {
        stop("No element named '", obj, "' found in 'x'", call. = FALSE)
    }
    return(invisible(NULL))
}

#' Plotting function
#'
#' Plots the specified current environment of a [metaRangeSimulation] object.
#'
#' @param x `<metaRangeEnvironment>` [metaRangeEnvironment] object.
#' @param env_name `<string>` name of the (sub) environment to plot.
#' @param col `<character>` colors to use. Defaults to `grDevices::hcl.colors()` with
#' `n =50` and a random palette.
#' @param main `<string>` optional title of the plot. Will be labeled automatically when NULL.
#' @param ... additional arguments passed to [terra::plot] or [base::plot].
#' @return `<invisible NULL>`.
#' @examples
#' sim_env <- terra::sds(terra::rast(vals = 1:4, nrow = 2, ncol = 2))
#' names(sim_env) <- "env_01"
#' test_sim <- metaRangeSimulation$new(source_environment = sim_env)
#' test_sim$environment$set_current(1)
#' plot(test_sim$environment, "env_01")
#' @export
plot.metaRangeEnvironment <- function(x, env_name, col, main = NULL, ...) {
    if (missing(x) || is.null(x)) {
        stop("No 'x' supplied. Nothing to plot", call. = FALSE)
    }
    checkmate::assert_string(env_name)
    checkmate::assert_string(main, null.ok = TRUE)
    if (is.null(main)) {
        main <- env_name
    }
    r <- terra::rast(
        x$sourceSDS[[1]],
        nlyrs = 1,
        vals = x$current[[env_name]]
    )
    plot_internal(x = r, col = col, main = main, ...)
    return(invisible(NULL))
}

#' Plotting function
#'
#' Plots the specified trait of a [metaRangeSpecies] object.
#'
#' @param x `<metaRangeSpecies>` [metaRangeSpecies] object.
#' @param trait_name `<string>` name of the trait to plot.
#' @param col `<character>` colors to use. Defaults to `grDevices::hcl.colors()` with
#' `n =50` and a random palette.
#' @param main `<string>` optional title of the plot. Will be labeled automatically when NULL.
#' @param ... additional arguments passed to [terra::plot] or [base::plot].
#' @examples
#' sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2))
#' names(sim_env) <- "env_01"
#' test_sim <- metaRangeSimulation$new(source_environment = sim_env)
#' test_sim$add_species("species_01")
#' test_sim$add_traits("species_01", trait_01 = matrix(1:4, nrow = 2, ncol = 2))
#' plot(test_sim$species_01, "trait_01")
#' @return `<invisible NULL>`.
#' @export
plot.metaRangeSpecies <- function(x, trait_name, col, main = NULL, ...) {
    if (missing(x) || is.null(x)) {
        stop("No 'x' supplied. Nothing to plot", call. = FALSE)
    }
    checkmate::assert_string(trait_name)
    checkmate::assert_string(main, null.ok = TRUE)
    if (is.null(main)) {
        main <- paste0(x$name, ": ", trait_name)
    }
    dim_m <- dim(x$traits[[trait_name]])[c(1, 2)]
    dim_r <- dim(x$sim$environment$sourceSDS)[c(1, 2)]

    if (!is.null(dim_m) && all(dim_m == dim_r)) {
        r <- terra::rast(
            x$sim$environment$sourceSDS[[1]],
            nlyrs = 1,
            vals = x$traits[[trait_name]]
        )
        plot_internal(x = r, col = col, main = main, ...)
    } else {
        plot_internal(x = x$traits[[trait_name]], col = col, main = main, ...)
    }
    return(invisible(NULL))
}

#' internal plotting function
#'
#' Plots parts of a [metaRangeSimulation] object
#
#' @param x thing to plot
#' @param col colors to use. Defaults to `grDevices::hcl.colors()` with
#' `n =50` and a random palette.
#' @param ... if x is a [terra::SpatRaster] these can be arguments
#' passed to [terra::plot], else these are additional
#' graphical arguments passed on to [base::plot]
#' @return `<invisible NULL>`
#' @keywords internal
#' @noRd
plot_internal <- function(x, col, ...) {
    if (missing(x) || is.null(x)) {
        stop("No 'x' supplied. Nothing to plot", call. = FALSE)
    }
    if (missing(col)) {
        col <- grDevices::hcl.colors(50, palette = sample(
            grDevices::hcl.pals()[
                grDevices::hcl.pals() %in% c(
                    "Blue-Red 3", "Vik", "Blue-Yellow",
                    "Blues 3", "Broc", "Earth", "Rocket",
                    "Mako", "Lajolla", "BuPu", "Emrld"
                )
            ],
            1
        ))
    }
    if (inherits(x, "SpatRaster")) {
        terra::plot(x, col = col, ...)
    } else if (inherits(x, "matrix")) {
        terra::plot(terra::rast(x), col = col, ...)
    } else {
        plot(x, col = col, ...)
    }
}
