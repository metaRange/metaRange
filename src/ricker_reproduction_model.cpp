// Copyright (C) 2023 Stefan Fallert, Lea Li, Juliano Sarmento Cabral
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
//' An implementation of the "classic" Ricker reproduction model (Ricker, 1954).
//'
//' @param abundance `<numeric>` vector (or matrix) of abundances.
//' @param reproduction_rate `<numeric>` vector (or matrix) of reproduction rates.
//' @param carrying_capacity `<numeric>` vector (or matrix) of carrying capacities.
//' @details
//' ## Equation:
//' \deqn{abundance_{t+1} = abundance_t \cdot e^{reproduction\_rate \cdot (1 - \frac{abundance_t}{carrying\_capacity})}}{abundance_t1 = abundance_t0 * e^(reproduction_rate * (1 - abundance_t0 / carrying_capacity))}
//'
//' Note that:
//'
//' * `abundance` should generally be greater than 0.
//' * `reproduction_rate` and  `carrying_capacity` should either both have the same size as the input abundance or both be of lenght 1.
//' * `reproduction_rate` should generally be greater than 0. A negative reproduction rate only works reliably as long as the abundance is below the carrying capacity.
//' * `carrying_capacity` should generally be greater than 0.
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
//'     abundance = matrix(10, 10, 5),
//'     reproduction_rate =  0.25,
//'     carrying_capacity =  100
//' )
//' ricker_reproduction_model(
//'     abundance = matrix(10, 10, 5),
//'     reproduction_rate =  matrix(seq(-0.5, 0.5, length.out = 25), 10, 5),
//'     carrying_capacity =  matrix(100, 10, 5)
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
//' Cabral, J.S. and Schurr, F.M. (2010)
//' Estimating demographic models for the range dynamics of plant species.
//' *Global Ecology and Biogeography*, **19**, 85--97.
//' \doi{10.1111/j.1466-8238.2009.00492.x}
//'
//' Original model:
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
    } else {
        if ((size != reproduction_rate.size()) ||
            (size != carrying_capacity.size())) {
            stop("The sizes of abundance, reproduction_rate and "
                 "carrying_capacity are not equal.");
        }

        for (int i = 0; i < size; i++) {
            if (abundance[i] > 0) {
                abundance[i] = abundance[i] *
                               exp(reproduction_rate[i] *
                                   (1 - abundance[i] / carrying_capacity[i]));
            }
            if (NumericVector::is_na(abundance[i])) {
                abundance[i] = 0.0;
            }
        }
    }
    return abundance;
}
