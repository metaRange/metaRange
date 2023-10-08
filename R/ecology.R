# Author: Stefan Fallert
# Date: 10.05.2023
# License: GPL-3 (See License.md)

#' Normalization constant calculation
#'
#' Calculates the normalization constant for the metabolic scaling
#' based on a known or estimated parameter value under at a reference temperature.
#'
#' @param parameter_value `<numeric>` paramter value at the reference temperature.
#' @param reference_temperature `<numeric>` reference temperature in kelvin (K).
#' @param E `<numeric>` Activation energy in electronvolts (eV).
#' @param k `<numeric>` Boltzmann's constant (eV / K).
#' @param mass `<numeric>`  mean (individual) mass.
#' @param scaling_exponent `<numeric>` allometric scaling exponent of the mass.
#' @details
#' Note the different scaling values for different parameter.
#' The following is a summary from table 4 in Brown, Sibly and Kodric-Brown (2012)
#' (see references).
#'
#' | Parameter  | Scaling exponent | Activation energy |
#' | :------------ | :-----------: | -------------------: |
#' | resource usage | 3/4 | -0.65 |
#' | reproduction, mortality | -1/4 | -0.65 |
#' | carrying capacity | -3/4 | 0.65 |
#' @references
#' Brown, J.H., Gillooly, J.F., Allen, A.P., Savage, V.M. and West, G.B. (2004)
#' Toward a Metabolic Theory of Ecology. *Ecology*, **85** 1771--1789.
#' [doi:10.1890/03-9000](https://doi.org/10.1890/03-9000)
#'
#' Brown, J.H., Sibly, R.M. and Kodric-Brown, A. (2012)
#' Introduction: Metabolism as the Basis for a Theoretical Unification of Ecology.
#' In *Metabolic Ecology* (eds R.M. Sibly, J.H. Brown and A. Kodric-Brown)
#' [doi:10.1002/9781119968535.ch](https://doi.org/10.1002/9781119968535.ch)
#' @examples
#' calculate_normalization_constant(
#'      parameter_value = 1,
#'      scaling_exponent = -1 / 4,
#'      mass = 1,
#'      reference_temperature = 273.15,
#'      E = -0.65
#' )
#' @return The calculated normalization constant.
#' @export
calculate_normalization_constant <- function(
    parameter_value,
    scaling_exponent,
    mass,
    reference_temperature,
    E = NULL,
    k = 8.617333e-05) {
    verbosity <- getOption("metaRange.verbose", default = FALSE)
    if (verbosity && scaling_exponent == -1 / 4 && E > 0) {
        message(
            "Respiratory rates are generally assumed to scale with:\n",
            "exponent ~ [-1/4]\n",
            "activation energy ~ [-0.65] eV.\n",
            "(Feel free to ignore this message if you know what you are doing)."
        )
    }
    if (verbosity && scaling_exponent == -3 / 4 && E < 0) {
        message(
            "Carrying capacitiy is generally assumed to scale with:\n",
            "exponent ~ [-3/4]\n",
            "activation energy ~ [0.65] eV.\n",
            "(Feel free to ignore this message if you know what you are doing)."
        )
    }
    parameter_value / (mass^scaling_exponent * exp(E / (k * reference_temperature)))
}

## Implementation of a dispersal kernel as found in:
#
# Nathan et al. (2012),
# 'Dispersal kernels: review',
# Chapter 15 (Pages 186–210) in Jean Clobert and others (eds),
# Dispersal Ecology and Evolution (Oxford, 2012; online edn, Oxford Academic, 17 Dec. 2013),
# https://doi.org/10.1093/acprof:oso/9780199608898.003.0015
#
# __________
# The chapter can also be found on researchgate:
# Nathan, R., Klein, E., Robledo-Anuncio, J. J., Revilla, E., (2012).
# Dispersal kernels: review. Chapter 15. Dispersal Ecology and Evolution.
# https://www.researchgate.net/publication/284654445_Dispersal_kernels_review_Chapter_15

#' Negative Exponential kernel
#'
#' @param x `<numeric>` distance at which the probability is calculated.
#' @param mean_dispersal_dist `<numeric>` mean dispersal distance (>0)
#' @details
#' The negative exponential kernel is defined as:
#' \deqn{f(x) = \frac{1}{2 \pi a^2} e^{-\frac{x}{a}}}
#' where \eqn{a} is the mean dispersal distance divided by 2.
#' @examples
#' negative_exponential_function(1, 1)
#' @references
#' Nathan, R., Klein, E., Robledo-Arnuncio, J.J. and Revilla, E. (2012)
#' Dispersal kernels: review.
#' in: *Dispersal Ecology and Evolution* pp. 187--210.
#' (eds J. Clobert, M. Baguette, T.G. Benton and J.M. Bullock),
#' Oxford, UK: Oxford Academic, 2013.
#' [doi:10.1093/acprof:oso/9780199608898.003.0015](https://doi.org/10.1093/acprof:oso/9780199608898.003.0015)
#' @return `<numeric>` The probability at distance x.
#' @export
negative_exponential_function <- function(x, mean_dispersal_dist) {
    a <- mean_dispersal_dist / 2
    (1 / (2 * pi * (a^2))) * exp(-x / a)
}

#' Calculate 2D dispersal kernel.
#'
#' Use a user defined function to create a 2D dispersal kernel.
#'
#' @param max_dispersal_dist `<numeric>` maximum dispersal distance.
#' @param kfun `<function>` the kernel function to use. Can be user-defined,
#' in which case it needs to accept (at least) the parameter
#' "x" representing the distance from the source as its input and return a probability.
#' @param ... additional parameters to be passed to the kernel function.
#' @examples
#' calculate_dispersal_kernel(
#'     max_dispersal_dist = 3,
#'     kfun = negative_exponential_function,
#'     mean_dispersal_dist = 1
#' )
#' @return Dispersal kernel with probabilities.
#' @export
calculate_dispersal_kernel <- function(
    max_dispersal_dist,
    kfun,
    ...) {
    size <- 2 * max_dispersal_dist + 1
    dispersal_kernel <- matrix(0, nrow = size, ncol = size)
    midpoint <- max_dispersal_dist + 1
    for (i in 1:size) {
        for (j in 1:size) {
            dispersal_kernel[i, j] <- sqrt(abs(i - midpoint)^2 + abs(j - midpoint)^2)
        }
    }
    dispersal_kernel <- kfun(x = dispersal_kernel, ...)
    dispersal_kernel / sum(dispersal_kernel)
}

#' Dispersal process.
#'
#' Disperse a (abundance) matrix using a dispersal kernel and optional suitability weights.
#' @param dispersal_kernel `<matrix>` dispersal kernel.
#' @param abundance `<matrix>` abundance matrix.
#' @param suitability `<matrix>`  optional suitability matrix `range: 0, 1`.
#' @details
#' The abundance matrix is dispersed using the dispersal kernel.
#' If a suitability matrix is supplied, the individuals will redistribute
#' within the dispersal kernel according to the suitability matrix.
#' This means that individuals will move towards more suitable areas,
#' if they can reach them.
#' @examples
#' n <- 10
#' n2 <- n^2
#' abu <- matrix(1:n2, nrow = n, ncol = n)
#' suitab <- matrix(1, nrow = n, ncol = n)
#' kernel <- calculate_dispersal_kernel(
#'     max_dispersal_dist = 4,
#'     kfun = negative_exponential_function,
#'     mean_dispersal_dist = 1.2
#' )
#' res1 <- dispersal(
#'     dispersal_kernel = kernel,
#'     abundance = abu
#' )
#' res2 <- dispersal(
#'     dispersal_kernel = kernel,
#'     abundance = abu,
#'     suitability = suitab
#' )
#' stopifnot(sum(res1) - sum(res2) < 0.01)
#' # Also, both results should be the same within numirical precision,
#' # since the suitability is 1 everywhere.
#' @return `<matrix>` Dispersed abundance matrix.
#' @export
dispersal <- function(
    dispersal_kernel,
    abundance,
    suitability) {
    if (missing(dispersal_kernel)) {
        stop("No dispersal kernel supplied.")
    }
    if (missing(abundance)) {
        stop("No abundance matrix supplied.")
    }
    if (missing(suitability)) {
        return(dispersal_fixed_undirected(
            dispersal_kernel = dispersal_kernel,
            abundance = abundance
        ))
    } else {
        return(dispersal_fixed_directed(
            dispersal_kernel = dispersal_kernel,
            abundance = abundance,
            suitability = suitability
        ))
    }
}
