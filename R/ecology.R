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

#' Normalization constant calculation
#'
#' Calculates the normalization constant for the metabolic scaling
#' based on a known or estimated parameter value under at a reference temperature.
#'
#' @param parameter_value `<numeric>` parameter value at the reference temperature.
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
#' \doi{10.1890/03-9000}
#'
#' Brown, J.H., Sibly, R.M. and Kodric-Brown, A. (2012)
#' Introduction: Metabolism as the Basis for a Theoretical Unification of Ecology.
#' In *Metabolic Ecology* (eds R.M. Sibly, J.H. Brown and A. Kodric-Brown)
#' \doi{10.1002/9781119968535.ch}
#' @seealso
#' `metabolic_scaling()`
#' @examples
#' calculate_normalization_constant(
#'     parameter_value = 1,
#'     scaling_exponent = -1 / 4,
#'     mass = 1,
#'     reference_temperature = 273.15,
#'     E = -0.65
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
    checkmate::assert_numeric(parameter_value)
    checkmate::assert_numeric(scaling_exponent, len = 1L)
    checkmate::assert_numeric(mass)
    checkmate::assert_numeric(reference_temperature, len = 1L)
    checkmate::assert_numeric(E, len = 1L)
    checkmate::assert_numeric(k, len = 1L)
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
