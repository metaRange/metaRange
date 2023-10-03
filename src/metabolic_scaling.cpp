
// metabolic_scaling.cpp: c++ implementation of the metabolic scaling
// as descibed in the metabolic theory of ecology (Brown et al. 2004)
//
// Copyright (C) 2023  Stefan Fallert
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
// along with metaRange.  If not, see <http://www.gnu.org/licenses/>.

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
//' The function uses the formula in the form of:
//' \deqn{parameter = normalization\_constant \cdot mass^{scaling\_exponent} \cdot e^{\frac{E}{k \cdot temperature}}}
//' General notes:
//'
//' Reproduction rate is generally assumed to scale with an exponent of `-1/4`
//' and an activation energy of `-0.65 eV` (important: it's negative).
//'
//' Carrying capacity is generally assumed to scale with an exponent of `-3/4`
//' and an activation energy of `0.65 eV` (important: it's positive).
//' But read: (Brown et. al. 2004; Brown & Sibly, 2012) for an in-depth explanation.
//'
//' Units:
//'
//' 1 electronvolt = 1.602176634 * 10^-19 Joule
//'
//' Boltzmann constant 1.380649 * 10^-23 Joule/Kelvin
//'
//' Boltzmann constant in eV/K = 8.617333e-05 = (1.380649 * 10^-23) / (1.602176634 * 10^-19)
//' @references
//' Brown, J.H., Gillooly, J.F., Allen, A.P., Savage, V.M. & West, G.B. (2004).
//' Toward a Metabolic Theory of Ecology. Ecology, 85, 1771–1789.
//' <doi:10.1890/03-9000>
//'
//' Brown, J.H. and Sibly, R.M. (2012). The Metabolic Theory of Ecology and Its Central Equation.
//' In Metabolic Ecology (eds R.M. Sibly, J.H. Brown and A. Kodric-Brown).
//' <doi:10.1002/9781119968535.ch2>
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
        double normalization_constant,
        double scaling_exponent,
        NumericVector mass,
        NumericVector temperature,
        double E,
        double k = 8.617333e-05) {
    if ((mass.size() != temperature.size())) {
        stop("The sizes of mass and temperature are not equal.");
    }

    NumericVector result = normalization_constant * pow(mass, scaling_exponent) * exp((E / (k * temperature)));
    result.attr("dim") = mass.attr("dim");
    return result;
}