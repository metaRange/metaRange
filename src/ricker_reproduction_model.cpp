
// ricker_reproduction_model.cpp: c++ implementation of a ricker reproduction model
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

//' Ricker reproduction model c++ version
//'
//' Ricker reproduction model c++ version
//'
//' @param abundance matrix .
//' @param reproduction_rate matrix.
//' @param carrying_capacity matrix.
//' @references Cabral, J.S. & Schurr, F.M. (2010) Estimating demographic
//' models for the range dynamics of plant species.
//' Global Ecology and Biogeography, 19, 85–97.
//' <doi:10.1111/j.1466-8238.2009.00492.x>
//'
//' Original model:
//' Ricker, W.E. (1954) Stock and recruitment.
//' Journal of the Fisheries Research Board of Canada, 11, 559–623.
//' <doi:10.1139/f54-039>
//' @export
// [[Rcpp::export]]
NumericVector ricker_reproduction_model(
        NumericVector abundance,
        NumericVector reproduction_rate,
        NumericVector carrying_capacity) {

    if ((abundance.size() != reproduction_rate.size()) || (abundance.size() != carrying_capacity.size())) {
        stop("The sizes of abundance, reproduction_rate and carrying_capacity are not equal.");
    }

    for (int i = 0; i < abundance.size(); i++) {
        if (abundance[i] > 0) {
            abundance[i] = abundance[i] * exp(reproduction_rate[i] * (1 - abundance[i] / carrying_capacity[i]));
        }
        if (NumericVector::is_na(abundance[i])) {
            abundance[i] = 0.0;
        }
    }
    return abundance;
}