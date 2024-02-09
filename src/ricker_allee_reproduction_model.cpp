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

//' Ricker reproduction model with Allee effects
//'
//' An implementation of the Ricker reproduction model with Allee effects based on
//' (Cabral and Schurr, 2010) with variable overcompensation and an extension to
//' handle negative reproduction rates.
//'
//' @param abundance `<numeric>` vector (or matrix) of abundances.
//' @param reproduction_rate `<numeric>` vector (or matrix) of reproduction rates.
//' @param carrying_capacity `<numeric>` vector (or matrix) of carrying capacities.
//' @param allee_threshold `<numeric>` vector (or matrix) of Allee thresholds.
//' @param overcomp_factor `<numeric>` overcompensation factor (default: 1.0).
//' Higher values lead to stronger overcompensation. Can also be a vector or matrix.
//'
//' @details
//' ## Equations:
//' If \eqn{reproduction\_rate >= 0} (based on: Cabral and Schurr, 2010):
//' \deqn{N_{t+1} = N_t e^{b r \frac{(K - N_t)(N_t - C)}{(K - C)^2})}}{N_t1 = N_t * e^(b * r * ((K - N_t) * (N_t - C) / (K - C)^2))}
//'
//' If \eqn{reproduction\_rate < 0}:
//' \deqn{N_{t+1} = N_t \cdot e^{r}}{N_t1 = N_t * e^(r)}
//'
//' With:
//' * \eqn{N_t} = abundance at time t
//' * \eqn{N_{t+1}} = abundance at time t+1
//' * \eqn{r} = reproduction rate
//' * \eqn{K} = carrying capacity
//' * \eqn{C} = (critical) Allee threshold
//' * \eqn{b} = overcompensation factor
//'
//' Note that:
//'
//' * `abundance` should generally be greater than 0.
//' * `reproduction_rate`, `carrying_capacity` and `allee_threshold` should either all have the same size as the input abundance or all be of length 1.
//' * `carrying_capacity` should be greater than 0. If it is 0 or less, the abundance will be set to 0.
//' * `allee_threshold` should be less than `carrying_capacity`. If it is greater than or equal, the abundance will be set to 0.
//'
//' Important Note:
//' To optimize performance, the functions modifies the abundance in-place.
//' This mean the input abundance will be modified (See Examples).
//' Since the result of this function is usually assigned to the same variable as the input abundance, this is unnoticable in most use cases.
//' Should you wish to keep the input abundance unchanged, you can `rlang::duplicate()` it before passing it to this function.
//'
//' @return `<numeric>` vector (or matrix) of abundances.
//' @examples
//' ricker_allee_reproduction_model(
//'     abundance = 50,
//'     reproduction_rate = 2,
//'     carrying_capacity = 100,
//'     allee_threshold = -100
//' )
//' ricker_allee_reproduction_model(
//'     abundance = 50,
//'     reproduction_rate = 2,
//'     carrying_capacity = 100,
//'     allee_threshold = -100,
//'     overcomp_factor = 4
//' )
//' ricker_allee_reproduction_model(
//'     abundance = matrix(10, 5, 5),
//'     reproduction_rate =  0.25,
//'     carrying_capacity =  100,
//'     allee_threshold =  20
//' )
//' ricker_allee_reproduction_model(
//'     abundance = matrix(10, 5, 5),
//'     reproduction_rate =  matrix(seq(-0.5, 0.5, length.out = 25), 5, 5),
//'     carrying_capacity =  matrix(100, 5, 5),
//'     allee_threshold =  matrix(20, 5, 5)
//' )
//' ricker_allee_reproduction_model(
//'     abundance = matrix(10, 5, 5),
//'     reproduction_rate =  matrix(1, 5, 5),
//'     carrying_capacity =  matrix(100, 5, 5),
//'     allee_threshold =  matrix(seq(0, 100, length.out = 25), 5, 5)
//' )
//' ricker_allee_reproduction_model(
//'     abundance = matrix(10, 5, 5),
//'     reproduction_rate =  matrix(seq(0, -2, length.out = 25), 5, 5),
//'     carrying_capacity =  matrix(100, 5, 5),
//'     allee_threshold =  matrix(20, 5, 5)
//' )
//' # Note that the input abundance is modified in-place
//' abu <- 10
//' res <- ricker_allee_reproduction_model(
//'     abundance = abu,
//'     reproduction_rate = 0.25,
//'     carrying_capacity = 100,
//'     allee_threshold = -100
//' )
//' stopifnot(identical(abu, res))
//' @references
//' Cabral, J.S. and Schurr, F.M. (2010)
//' Estimating demographic models for the range dynamics of plant species.
//' *Global Ecology and Biogeography*, **19**, 85--97.
//' \doi{10.1111/j.1466-8238.2009.00492.x}
//'
//' @export
// [[Rcpp::export]]
NumericVector ricker_allee_reproduction_model(
        NumericVector abundance,
        NumericVector reproduction_rate,
        NumericVector carrying_capacity,
        NumericVector allee_threshold,
        NumericVector overcomp_factor = NumericVector::create(1.0)) {
    const int size = abundance.size();
    // code path 1: all parameters besides abundance are scalars
    if (reproduction_rate.size() == 1 && carrying_capacity.size() == 1 &&
        allee_threshold.size() == 1 && overcomp_factor.size() == 1) {
        if (carrying_capacity[0] <= 0 ||
            allee_threshold[0] >= carrying_capacity[0]) {
            abundance = abundance * 0;
            for (int i = 0; i < size; i++) {
                if (NumericVector::is_na(abundance[i])) {
                    abundance[i] = 0.0;
                }
            }
            return abundance;
        }
        if (reproduction_rate[0] < 0) {
            abundance = abundance * exp(reproduction_rate[0]);
            for (int i = 0; i < size; i++) {
                if (NumericVector::is_na(abundance[i])) {
                    abundance[i] = 0.0;
                }
            }
            return abundance;
        }
        double C_minus_A = carrying_capacity[0] - allee_threshold[0];
        double C_minus_A_squared = pow(C_minus_A, 2);
        for (int i = 0; i < size; i++) {
            if (abundance[i] > 0) {
                abundance[i] = abundance[i] *
                               exp(reproduction_rate[0] * overcomp_factor[0] *
                                   (((carrying_capacity[0] - abundance[i]) *
                                     (abundance[i] - allee_threshold[0])) /
                                    (C_minus_A_squared)));
            }
            if (NumericVector::is_na(abundance[i])) {
                abundance[i] = 0.0;
            }
        }
        return abundance;
    }
    // code path 2: all parameters have the same size (besides overcomp_factor)
    if ((size == reproduction_rate.size()) &&
        (size == carrying_capacity.size()) &&
        (size == allee_threshold.size())) {

        // the following handels the case where overcomp_factor is on its default value
        // without unnecessary resizing and memory allocation and without the need to
        // have a third code path for this case
        int oc_index = 0;
        if (overcomp_factor.size() == 1) {
            oc_index = 0;
        } else if (overcomp_factor.size() == size) {
            oc_index = 1;
        } else {
            stop("The size of overcomp_factor is not equal to 1 or the size of abundance.");
        }

        for (int i = 0; i < size; i++) {
            if (carrying_capacity[i] <= 0 ||
                allee_threshold[i] >= carrying_capacity[i]) {
                abundance[i] = 0.0;
                continue;
            }
            if (reproduction_rate[i] < 0) {
                abundance[i] = abundance[i] * exp(reproduction_rate[i]);
                continue;
            }
            if (abundance[i] > 0) {
                abundance[i] =
                        abundance[i] *
                        exp(reproduction_rate[i] * overcomp_factor[i * oc_index] *
                            (((carrying_capacity[i] - abundance[i]) *
                                (abundance[i] - allee_threshold[i])) /
                                (pow(carrying_capacity[i] - allee_threshold[i],
                                    2))));
            }
            if (NumericVector::is_na(abundance[i])) {
                abundance[i] = 0.0;
            }
        }
        return abundance;
    }
    // if we made it here something went wrong
    stop("The sizes of the inputs must either all be equal to the abundence"
            " or all be of length 1.");
}
