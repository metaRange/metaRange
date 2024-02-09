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

//' Calculate (estimate) environmental suitability
//'
//' Calculate / estimate the environmental suitability for a given environmental value,
//' based on a beta distribution, using the three "cardinal" values of the species for that environmental niche.
//'
//' @param vmax `<numeric>` upper (i.e. maximum) tolerable value
//' @param vopt `<numeric>` optimal (i.e. preferred) value
//' @param vmin `<numeric>` lower (i.e. minimum) tolerable value
//' @param venv `<numeric>` environmental value for which to calculate the suitability
//' @return `<numeric>` environmental suitability
//' @details The environmental suitability is calculated based on a beta distribution
//' after a formula provided by Yin et al. (1995) and simplified by Yan and Hunt (1999) (see references paragraph)
//' \deqn{suitability = (\frac{V_{max} - V_{env}}{V_{max} - V_{opt}}) * (\frac{V_{env} - V_{min}}{V_{opt} - V_{min}})^{\frac{V_{opt} - V_{min}}{V_{max} - V_{opt}}}}{suitability = ((V_max - V_env) / (V_max - V_opt)) * ((V_env - V_min) / (V_opt - V_min))^((V_opt - V_min) / (V_max - V_opt)))}
//' @note The original formula by Yin et al. was only intended to calculate
//' the relative daily growth rate of plants in relation to temperature. The abstraction to
//' use this to A) calculate a niche suitability; and B) use it on other
//' environmental values than temperature might not be valid. However, the assumption that the
//' environmental suitability for one niche dimension is highest at one optimal value and
//' decreases towards the tolerable minimum and maximum values in a nonlinear fashion seems reasonable.
//' @references
//' Yin, X., Kropff, M.J., McLaren, G., Visperas, R.M., (1995)
//' A nonlinear model for crop development as a function of temperature,
//' *Agricultural and Forest Meteorology*,
//' Volume **77**, Issues 1–2,
//' Pages 1--16,
//' \doi{10.1016/0168-1923(95)02236-Q}
//'
//' Also, see equation 4 in:
//' Weikai Yan, L.A. Hunt, (1999)
//' An Equation for Modelling the Temperature Response of Plants using only the Cardinal Temperatures,
//' *Annals of Botany*,
//' Volume **84**, Issue 5,
//' Pages 607--614,
//' ISSN 0305-7364,
//' \doi{10.1006/anbo.1999.0955}
//' @examples
//' calculate_suitability(
//'     vmax = 30,
//'     vopt = 25,
//'     vmin = 10,
//'     venv = 1:40
//' )
//' calculate_suitability(
//'     vmax = seq(30, 32, length.out = 40),
//'     vopt = seq(20, 23, length.out = 40),
//'     vmin = seq(9, 11, length.out = 40),
//'     venv = 1:40
//' )
//'
//' try(calculate_suitability(
//'     vmax = 1,
//'     vopt = seq(20, 23, length.out = 40),
//'     vmin = seq(9, 11, length.out = 40),
//'     venv = 1:40
//' ))
//' @export
// [[Rcpp::export]]
NumericVector calculate_suitability(
        NumericVector vmax,
        NumericVector vopt,
        NumericVector vmin,
        NumericVector venv) {

    const int max_size = venv.size();
    NumericVector result(max_size);
    result.attr("dim") = venv.attr("dim");

    if (vmax.size() == 1 && vopt.size() == 1 && vmin.size() == 1) {
        const double v_max = vmax[0];
        const double v_opt = vopt[0];
        const double v_min = vmin[0];
        if (v_max < v_opt || v_opt < v_min) {
            stop("Arguments don't meet the following criteria: v_max > vopt > "
                 "vmin");
        }
        const double opt_minus_min = v_opt - v_min;
        const double max_minus_opt = v_max - v_opt;
        const double exponent = opt_minus_min / max_minus_opt;

        for (int i = 0; i < max_size; i++) {
            if (venv[i] < v_min || venv[i] > v_max) {
                result[i] = 0.0;
                continue;
            }
            result[i] = ((v_max - venv[i]) / (max_minus_opt)) *
                        pow(((venv[i] - v_min) / (opt_minus_min)), exponent);
            if (NumericVector::is_na(result[i])) {
                result[i] = 0.0;
            }
        }
        return result;
    }
    if (vmax.size() == vopt.size() &&
        vmax.size() == vmin.size() &&
        vmax.size() == venv.size()) {

        for (int i = 0; i < max_size; i++) {
            if (vmax[i] < vopt[i] || vopt[i] < vmin[i]) {
                stop("Arguments don't meet the following criteria: "
                     "vmax > vopt > vmin");
            }
            if (venv[i] < vmin[i] || venv[i] > vmax[i]) {
                result[i] = 0.0;
                continue;
            }
            result[i] = ((vmax[i] - venv[i]) / (vmax[i] - vopt[i])) *
                        pow(((venv[i] - vmin[i]) / (vopt[i] - vmin[i])),
                            ((vopt[i] - vmin[i]) / (vmax[i] - vopt[i])));
            if (NumericVector::is_na(result[i])) {
                result[i] = 0.0;
            }
        }
        return result;
    }
    stop("The sizes of venv, vmax, vopt and vmin are not equal.");
}
