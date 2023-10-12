
// dispersal_fixed_directed.cpp: c++ implementation of an kernel based
// directed dispersal function
//
// Copyright (C) 2023 Stefan Fallert
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
//
// This file incorporates work covered by the following copyright and license:
//
// Copyright (c) 2018-2023 Tyler Morgan-Wall <tylermw@gmail.com>
// License: GPL-3
//
// Explanation:
// This file implements a kernel based dispersal that takes inspiration from
// the R package: "rayimage"
// (Morgan-Wall T (2023). rayimage: Image Processing for Simulated Cameras.
// https://www.rayimage.dev, https://github.com/tylermorganwall/rayimage.)
// and specifically the function "convolution_cpp" in the file:
// https://github.com/tylermorganwall/rayimage/blob/master/src/pointspread.cpp
// It is not a direct copy of the code/ functionality but uses it as a template
// for the convolution, specifically the indexing and use of submatrices.

#include "RcppArmadillo.h"

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;
using namespace arma;



//' Undirected and fixed sized dispersal
//'
//' Dispersal function that uses a fixed sized kernel and isn't influenced by
//' the environmental suitability. The individuals in each cell are redistributed to the
//' surrounding cells according to probability defined in the dispersal kernel.
//' Useful for e.g. wind dispersed plants.
//'
//' @param abundance `<numeric matrix>` Values need to be non-negative.
//' @param dispersal_kernel `<numeric matrix>` Dispersal kernel. Needs to have an odd size.
//' @return `<numeric matrix>` The new abundance matrix.
//' @keywords internal
// [[Rcpp::export]]
arma::mat dispersal_fixed_undirected(
        arma::mat abundance,
        arma::mat dispersal_kernel) {
    if ((dispersal_kernel.n_rows != dispersal_kernel.n_cols)) {
        stop("Dispersal kernel is not quadratic.");
    }
    if ((dispersal_kernel.n_rows % 2) == 0) {
        stop("Dispersal kernel has an even number of rows.");
    }

    // create a matrix to store the intermediate results
    arma::mat offspring(abundance.n_rows, abundance.n_cols, arma::fill::zeros);
    abundance.replace(datum::nan, 0);

    sword nrows = abundance.n_rows;
    sword ncols = abundance.n_cols;

    sword dipersal_kernel_size = dispersal_kernel.n_rows;
    sword half_size = (dipersal_kernel_size - 1) / 2;

    for (sword rowid = 0; rowid < nrows; rowid++) {
        for (sword colid = 0; colid < ncols; colid++) {
            if (abundance(rowid, colid) <= 0) {
                continue;
            }

            sword min_row = rowid - half_size;
            sword max_row = rowid + half_size;
            sword min_col = colid - half_size;
            sword max_col = colid + half_size;

            sword kernel_min_row = 0;
            sword kernel_max_row = dipersal_kernel_size - 1;
            sword kernel_min_col = 0;
            sword kernel_max_col = dipersal_kernel_size - 1;

            if (min_row < 0) {
                kernel_min_row = -min_row;
                min_row = 0;
            }
            if (max_row > nrows - 1) {
                kernel_max_row = kernel_max_row - (max_row - nrows + 1);
                max_row = nrows - 1;
            }
            if (min_col < 0) {
                kernel_min_col = -min_col;
                min_col = 0;
            }
            if (max_col > ncols - 1) {
                kernel_max_col = kernel_max_col - (max_col - ncols + 1);
                max_col = ncols - 1;
            }

            arma::mat current_dispersal_kernel = dispersal_kernel.submat(
                kernel_min_row, kernel_min_col, kernel_max_row, kernel_max_col);

            offspring.submat(min_row, min_col, max_row, max_col) += (abundance(rowid, colid) *
                (current_dispersal_kernel  / accu(current_dispersal_kernel)));

        }
        // check if the user wants to interrupt the function
        // do this here to avoid overhead from checking in each cell
        Rcpp::checkUserInterrupt();
    }
    abundance = offspring;
    return abundance;
}



//' Directed and fixed sized dispersal
//'
//' Dispersal function that uses a fixed sized kernel and directed dispersal towards suitable areas.
//' Suitable for e.g. animals that can sense their surroundings.
//'
//' @param abundance `<numeric matrix>` Values need to be non-negative.
//' @param suitability `<numeric matrix>` Values need to non-missing and between 0 and 1 for the result to make sense.
//' Needs to have same size as abundance.
//' @param dispersal_kernel `<numeric matrix>` Dispersal kernel. Needs to have an odd size.
//' @return `<numeric matrix>` The new abundance matrix.
//' @keywords internal
// [[Rcpp::export]]
arma::mat dispersal_fixed_directed(
        arma::mat abundance,
        arma::mat suitability,
        arma::mat dispersal_kernel) {
    if ((abundance.n_rows != suitability.n_rows) || (abundance.n_cols != suitability.n_cols)) {
        stop("Size of abundance and suitability are not equal.");
    }
    if ((dispersal_kernel.n_rows != dispersal_kernel.n_cols)) {
        stop("Dispersal kernel is not quadratic.");
    }
    if ((dispersal_kernel.n_rows % 2) == 0) {
        stop("Dispersal kernel has an even number of rows.");
    }
    // create a matrix to store the intermediate results
    arma::mat offspring(abundance.n_rows, abundance.n_cols, arma::fill::zeros);
    abundance.replace(datum::nan, 0);

    sword nrows = abundance.n_rows;
    sword ncols = abundance.n_cols;

    sword dipersal_kernel_size = dispersal_kernel.n_rows;
    sword half_size = (dipersal_kernel_size - 1) / 2;

    for (sword rowid = 0; rowid < nrows; rowid++) {
        for (sword colid = 0; colid < ncols; colid++) {
            if (abundance(rowid, colid) <= 0) {
                continue;
            }
            // the following check prevents NaNs due to division by zero
            // from floating point imprecision (accu(small_numers) == 0)
            // TODO: Should this configurable?
            if (suitability(rowid, colid) <= 0) {
                continue;
            }

            sword min_row = rowid - half_size;
            sword max_row = rowid + half_size;
            sword min_col = colid - half_size;
            sword max_col = colid + half_size;

            sword kernel_min_row = 0;
            sword kernel_max_row = dipersal_kernel_size - 1;
            sword kernel_min_col = 0;
            sword kernel_max_col = dipersal_kernel_size - 1;

            if (min_row < 0) {
                kernel_min_row = -min_row;
                min_row = 0;
            }
            if (max_row > nrows - 1) {
                kernel_max_row = kernel_max_row - (max_row - nrows + 1);
                max_row = nrows - 1;
            }
            if (min_col < 0) {
                kernel_min_col = -min_col;
                min_col = 0;
            }
            if (max_col > ncols - 1) {
                kernel_max_col = kernel_max_col - (max_col - ncols + 1);
                max_col = ncols - 1;
            }

            arma::mat current_dispersal_kernel = dispersal_kernel.submat(kernel_min_row, kernel_min_col, kernel_max_row, kernel_max_col);
            arma::mat current_suitability = suitability.submat(min_row, min_col, max_row, max_col);

            offspring.submat(min_row, min_col, max_row, max_col) += (abundance(rowid, colid) *
                ((current_dispersal_kernel % current_suitability)  /
                accu((current_dispersal_kernel % current_suitability))));

        }
        // check if the user wants to interrupt the function
        // do this here to avoid overhead from checking in each cell
        Rcpp::checkUserInterrupt();
    }
    abundance = offspring;
    return abundance;
}
