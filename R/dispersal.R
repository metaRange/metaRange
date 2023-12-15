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

#' Negative Exponential kernel
#'
#' @param x `<numeric>` distance at which the probability is calculated.
#' @param mean_dispersal_dist `<numeric>` mean dispersal distance (>0)
#' @details
#' The negative exponential kernel is defined as:
#' \deqn{f(x) = \frac{1}{2 \pi a^2} e^{-\frac{x}{a}}}{fx = 1 / (2 * pi * a^2) * exp(-x / a)}
#' where \eqn{a} is the mean dispersal distance divided by 2.
#' @examples
#' negative_exponential_function(1, 1)
#' @references
#' Nathan, R., Klein, E., Robledo-Arnuncio, J.J. and Revilla, E. (2012)
#' Dispersal kernels: review.
#' in: *Dispersal Ecology and Evolution* pp. 187--210.
#' (eds J. Clobert, M. Baguette, T.G. Benton and J.M. Bullock),
#' Oxford, UK: Oxford Academic, 2013.
#' \doi{10.1093/acprof:oso/9780199608898.003.0015}
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
#' in which case it needs to vectorized and accept (at least) the parameter
#' "x" representing the distance from the source as its input and return a
#' vector of the same size as `max_dispersal_dist`.
#' @param normalize `<boolean>` whether to normalize the kernel.
#' @param ... additional parameters to be passed to the kernel function.
#' @examples
#' # a very simple uniform kernel
#' uniform_kernel <- calculate_dispersal_kernel(
#'     max_dispersal_dist = 3,
#'     kfun = function(x) {
#'         x * 0 + 1
#'     }
#' )
#' # same as
#' stopifnot(
#'     uniform_kernel == matrix(1 / 49, nrow = 7, ncol = 7)
#' )
#'
#' # now a negative exponential kernel
#' # not that `mean_dispersal_dist`
#' # is passed to the kernel function.
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
    normalize = TRUE,
    ...) {
    max_dispersal_dist <- checkmate::assert_int(max_dispersal_dist, coerce = TRUE, lower = 1L)
    checkmate::assert_function(kfun, args = c("x"))
    checkmate::assert_flag(normalize)
    size <- 2 * max_dispersal_dist + 1
    dispersal_kernel <- matrix(0, nrow = size, ncol = size)
    midpoint <- max_dispersal_dist + 1
    for (i in 1:size) {
        for (j in 1:size) {
            dispersal_kernel[i, j] <- sqrt(abs(i - midpoint)^2 + abs(j - midpoint)^2)
        }
    }
    dispersal_kernel <- kfun(x = dispersal_kernel, ...)
    if (length(dispersal_kernel) != size^2) {
        stop("Kernel function should return: ", size^2, " values, but returned: ", length(dispersal_kernel), ".")
    }
    dim(dispersal_kernel) <- c(size, size)
    if (normalize) {
        dispersal_kernel <- dispersal_kernel / sum(dispersal_kernel)
    }
    return(dispersal_kernel)
}

#' Dispersal process
#'
#' Disperse a (abundance) matrix using a dispersal kernel and optional weights.
#' @param dispersal_kernel `<numeric matrix>` dispersal kernel.
#' @param abundance `<numeric matrix>` abundance matrix.
#' @param weights `<numeric matrix>`  optional weights in form of a matrix
#' that has the same dimensions as the abundance and a range: between `0, 1`.
#' Should not contain any `NA`.
#' @details
#' The abundance matrix is dispersed using the dispersal kernel.
#' If a matrix of weights is supplied, the individuals will redistribute
#' within the dispersal kernel according to the weights.
#' I.e. individuals will more likely move towards areas with a higher
#' weight, if they are within their dispersal distance.
#' Note that the abundance is modified in place, to optimize performance.
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
#'     weights = suitab
#' )
#' stopifnot(sum(res1) - sum(res2) < 0.01)
#' # Note that the abundance is modified in place, i.e:
#' stopifnot(sum(abu - res2) < 0.01)
#' @return `<numeric matrix>` Dispersed abundance matrix.
#' @export
dispersal <- function(
    dispersal_kernel,
    abundance,
    weights) {
    if (missing(dispersal_kernel)) {
        stop("No dispersal kernel supplied.")
    }
    if (missing(abundance)) {
        stop("No abundance matrix supplied.")
    }
    if (missing(weights)) {
        return(dispersal_fixed_unweighted(
            dispersal_kernel = dispersal_kernel,
            abundance = abundance
        ))
    } else {
        return(dispersal_fixed_weighted(
            dispersal_kernel = dispersal_kernel,
            abundance = abundance,
            weights = weights
        ))
    }
}
