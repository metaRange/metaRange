# Calculate (estimate) environmental suitability
#
# Calculate / estimate the environmental suitability for a given environmental value,
# based on the three "cardinal" values of the species for that environmental niche.
#
# @param vmax `<numeric>` upper (i.e. maximum) tolerable value.
# @param vopt `<numeric>` optimal (i.e. prefered) value.
# @param vmin `<numeric>` lower (i.e. minimum) tolerable value.
# @param venv `<numeric>` environmental value for which to calculate the suitability.
# @return `<numeric>` environmental suitability between 0 (0% suitable) and 1 (100% suitable).
# @details The environmental suitability is calculated based on a beta distribution
# after a formula provided by Yan & Hunt (1999) (see references paragraph)
# \deqn{suitability = (\frac{V_{max} - V_{env}}{V_{max} - V_{opt}}) * (\frac{V_{env} - V_{min}}{V_{opt} - V_{min}})^{\frac{V_{opt} - V_{min}}{V_{max} - V_{opt}}}}
# @note The original formula by Yan & Hunt was only intended to calculate
# the relative daily growth rate of plants in relation to temperature. The abstraction to
# use this to A) calculate a niche suitability; and B) use it on other
# environmental values than temperature might not be valid. However, the assumption that the
# environmental suitability for one niche dimension is highest at one optimal value and
# decreases towards the tolerable minumum and maximum values seems reasonable.
# @references
# Weikai Yan, L.A. Hunt, (1999)
# An Equation for Modelling the Temperature Response of Plants using only the Cardinal Temperatures,
# Annals of Botany,
# Volume 84, Issue 5,
# Pages 607-614,
# ISSN 0305-7364,
# <doi:10.1006/anbo.1999.0955>
# @export
calculate_suitability_R <- function(vmax, vopt, vmin, venv) {
    res <- ((vmax - venv) / (vmax - vopt)) * ((venv - vmin) / (vopt - vmin))^((vopt - vmin) / (vmax - vopt))
    res[res < 0] <- 0
    res[is.nan(res)] <- 0 # see ?"^" for details
    return(res)
}

tmax <- 30
tmin <- 10
topt <- 25
tenv <- 10:40

expect_true(
    all.equal(
        calculate_suitability_R(tmax, topt, tmin, tenv),
        calculate_suitability(tmax, topt, tmin, tenv)
    ),
    info = "testing that suitability is the same in c++ and R"
)

expect_true(
    all(
        calculate_suitability(tmax, topt, tmin, 9) == 0,
        calculate_suitability(tmax, topt, tmin, 31) == 0
    ),
    info = "testing that suitability is 0 outside of limits"
)

expect_true(calculate_suitability(tmax, topt, tmin, 25) == 1,
    info = "testing that suitability is 1 at optimal value"
)
