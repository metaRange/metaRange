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

//' Ricker reproduction model
//'
//' An implementation of the Ricker reproduction model (Ricker, 1954) with
//' an extension to handle negative reproduction rates.
//'
//' @param abundance `<numeric>` vector (or matrix) of abundances.
//' @param reproduction_rate `<numeric>` vector (or matrix) of reproduction rates.
//' @param carrying_capacity `<numeric>` vector (or matrix) of carrying capacities.
//'
//' @details
//' ## Equations:
//' If \eqn{reproduction\_rate >= 0} (Ricker, 1954):
//' \deqn{N_{t+1} = N_t e^{r (1 - \frac{N_t}{K})}}{N_t1 = N_t * e^(r * (1 - N_t / K))}
//'
//' If \eqn{reproduction\_rate < 0}:
//' \deqn{N_{t+1} = N_t \cdot e^{r}}{N_t1 = N_ * e^(r)}
//'
//' With:
//' * \eqn{N_t} = abundance at time t
//' * \eqn{N_{t+1}} = abundance at time t+1
//' * \eqn{r} = reproduction rate
//' * \eqn{K} = carrying capacity
//'
//' Note that:
//'
//' * `abundance` should generally be greater than 0.
//' * `reproduction_rate` and  `carrying_capacity` should either both have the same size as the input abundance or both be of length 1.
//' * `carrying_capacity` should generally be greater than 0. If it is 0 or less, the abundance will be set to 0.
//'
//' Important Note:
//' To optimize performance, the functions modifies the abundance in-place.
//' This mean the input abundance will be modified (See Examples).
//' Since the result of this function is usually assigned to the same variable as the input abundance, this is unnoticable in most use cases.
//' Should you wish to keep the input abundance unchanged, you can `rlang::duplicate()` it before passing it to this function.
//'
//' @return `<numeric>` vector (or matrix) of abundances.
//' @examples
//' ricker_reproduction_model(
//'     abundance = 10,
//'     reproduction_rate = 0.25,
//'     carrying_capacity = 100
//' )
//' ricker_reproduction_model(
//'     abundance = matrix(10, 5, 5),
//'     reproduction_rate =  0.25,
//'     carrying_capacity =  100
//' )
//' ricker_reproduction_model(
//'     abundance = matrix(10, 5, 5),
//'     reproduction_rate =  matrix(seq(-0.5, 0.5, length.out = 25), 5, 5),
//'     carrying_capacity =  matrix(100, 5, 5)
//' )
//' ricker_reproduction_model(
//'     abundance = matrix(10, 5, 5),
//'     reproduction_rate =  matrix(seq(0, -2, length.out = 25), 5, 5),
//'     carrying_capacity =  matrix(100, 5, 5)
//' )
//' # Note that the input abundance is modified in-place
//' abu <- 10
//' res <- ricker_reproduction_model(
//'     abundance = abu,
//'     reproduction_rate = 0.25,
//'     carrying_capacity = 100
//' )
//' stopifnot(identical(abu, res))
//' @references
//' Ricker, W.E. (1954) Stock and recruitment.
//' *Journal of the Fisheries Research Board of Canada*, **11**, 559--623.
//' \doi{10.1139/f54-039}
//' @export
// [[Rcpp::export]]
NumericVector ricker_reproduction_model(
        NumericVector abundance,
        NumericVector reproduction_rate,
        NumericVector carrying_capacity) {
    const int size = abundance.size();
    if (reproduction_rate.size() == 1 && carrying_capacity.size() == 1) {
        if (carrying_capacity[0] <= 0) {
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
        for (int i = 0; i < size; i++) {
            if (abundance[i] > 0) {
                abundance[i] = abundance[i] *
                               exp(reproduction_rate[0] *
                                   (1 - abundance[i] / carrying_capacity[0]));
            }
            if (NumericVector::is_na(abundance[i])) {
                abundance[i] = 0.0;
            }
        }
        return abundance;
    } else {
        if ((size != reproduction_rate.size()) ||
            (size != carrying_capacity.size())) {
            stop("The sizes of abundance, reproduction_rate and "
                 "carrying_capacity are not equal.");
        }
        for (int i = 0; i < size; i++) {
            if (carrying_capacity[i] <= 0) {
                abundance[i] = 0.0;
                continue;
            }
            if (reproduction_rate[i] < 0) {
                abundance[i] = abundance[i] * exp(reproduction_rate[i]);
                continue;
            }
            if (abundance[i] > 0) {
                abundance[i] = abundance[i] *
                               exp(reproduction_rate[i] *
                                   (1 - abundance[i] / carrying_capacity[i]));
            }
            if (NumericVector::is_na(abundance[i])) {
                abundance[i] = 0.0;
            }
        }
        return abundance;
    }
}
