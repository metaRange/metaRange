// Copyright (C) 2023, 2024 Stefan Fallert, Lea Li, Juliano Sarmento Cabral
//
// This file is part of metaRange.
//
// metaRange is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation, version 3.
//
// metaRange is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with metaRange. If not, see <http://www.gnu.org/licenses/>.

#include <Rcpp.h>
using namespace Rcpp;

//' Metabolic scaling
//'
//' A function to calculate the metabolic scaling of a parameter, based on the
//' metabolic theory of ecology (Brown et al. 2004).
//'
//' @param normalization_constant `<numeric>` normalization constant.
//' @param scaling_exponent `<numeric>` allometric scaling exponent of the mass.
//' @param mass `<numeric matrix>`  mean (individual) mass.
//' @param temperature `<numeric matrix>` temperature in kelvin (K).
//' @param E `<numeric>` activation energy in electronvolts (eV).
//' @param k `<numeric>` Boltzmann's constant (eV / K).
//' @details
//' ## Equation:
//' The function uses the equation in the form of:
//' \deqn{parameter = normalization\_constant \cdot mass^{scaling\_exponent} \cdot e^{\frac{Activation\_energy}{k \cdot temperature}}}{parameter = normalization_constant * mass^scaling_exponent * e^(Activation_energy/ (k * temperature))}
//'
//' ## Parameter:
//' Note the different scaling values for different parameter.
//' The following is a summary from table 4 in Brown, Sibly and Kodric-Brown (2012)
//' (see references).
//' | Parameter  | Scaling exponent | Activation energy |
//' | :------------ | :-----------: | -------------------: |
//' | resource usage | 3/4 | -0.65 |
//' | reproduction, mortality | -1/4 | -0.65 |
//' | carrying capacity | -3/4 | 0.65 |
//'
//' ## Units:
//'
//' \deqn{1 \ electronvolt = 1.602176634 \cdot 10^{-19} Joule}{1 electronvolt = 1.602176634 * 10^-19 Joule}
//'
//' \deqn{Boltzmann \ constant = 1.380649 \cdot 10^{-23} \frac{Joule}{Kelvin}}{Boltzmann constant = 1.380649 * 10^-23 Joule/Kelvin}
//'
//' \deqn{Boltzmann \ constant \ in \frac{eV}{K} = 8.617333e-05 = \frac{1.380649 \cdot 10^{-23}}{1.602176634 \cdot 10^{-19}}}{Boltzmann constant in eV/K = 8.617333e-05 = (1.380649 * 10^-23) / (1.602176634 * 10^-19)}
//' @references
//' Brown, J.H., Gillooly, J.F., Allen, A.P., Savage, V.M. and West, G.B. (2004)
//' Toward a Metabolic Theory of Ecology. *Ecology*, **85** 1771--1789.
//' \doi{10.1890/03-9000}
//'
//' Brown, J.H., Sibly, R.M. and Kodric-Brown, A. (2012)
//' Introduction: Metabolism as the Basis for a Theoretical Unification of Ecology.
//' In *Metabolic Ecology* (eds R.M. Sibly, J.H. Brown and A. Kodric-Brown)
//' \doi{10.1002/9781119968535.ch}
//' @seealso
//' `calculate_normalization_constant()`
//' @return `<numeric>` The scaled parameter.
//' @examples
//' reproduction_rate <- 0.25
//' E_reproduction_rate <- -0.65
//' estimated_normalization_constant <-
//'     calculate_normalization_constant(
//'         parameter_value = reproduction_rate,
//'         scaling_exponent = -1/4,
//'         mass = 100,
//'         reference_temperature = 273.15 + 10,
//'         E = E_reproduction_rate
//'     )
//' metabolic_scaling(
//'     normalization_constant = estimated_normalization_constant,
//'     scaling_exponent = -1/4,
//'     mass = 100,
//'     temperature = 273.15 + 20,
//'     E = E_reproduction_rate
//' )
//'
//' carrying_capacity <- 100
//' E_carrying_capacity <- 0.65
//' estimated_normalization_constant <-
//'     calculate_normalization_constant(
//'         parameter_value = carrying_capacity,
//'         scaling_exponent = -3/4,
//'         mass = 100,
//'         reference_temperature = 273.15 + 10,
//'         E = E_carrying_capacity
//'     )
//' metabolic_scaling(
//'     normalization_constant = estimated_normalization_constant,
//'     scaling_exponent = -3/4,
//'     mass = 100,
//'     temperature = 273.15 + 20,
//'     E = E_carrying_capacity
//' )
//' @export
// [[Rcpp::export]]
NumericVector metabolic_scaling(
        NumericVector normalization_constant,
        NumericVector scaling_exponent,
        NumericVector mass,
        NumericVector temperature,
        NumericVector E,
        NumericVector k = 8.617333e-05) {
    if ((mass.size() != temperature.size())) {
        stop("The sizes of mass and temperature are not equal.");
    }
    if (normalization_constant.size() != 1) {
        stop("The normalization_constant should be a single value.");
    }
    if (scaling_exponent.size() != 1) {
        stop("The scaling_exponent should be a single value.");
    }
    if (E.size() != 1) {
        stop("Parameter E should be a single value.");
    }
    if (k.size() != 1) {
        stop("Parameter k should be a single value.");
    }

    NumericVector result = normalization_constant[0] *
                           pow(mass, scaling_exponent[0]) *
                           exp((E[0] / (k[0] * temperature)));
    result.attr("dim") = mass.attr("dim");
    return result;
}
