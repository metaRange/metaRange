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
//' Note that the input should have an equal size and that the input abundance
//' should be positive for the results to make sense.
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
