# Calculate (estimate) environmental suitability
# Yin, X., Kropff, M.J., McLaren, G., Visperas, R.M., (1995)
# A nonlinear model for crop development as a function of temperature,
# Agricultural and Forest Meteorology,
# Volume 77, Issues 1–2,
# Pages 1-16,
# <doi:10.1016/0168-1923(95)02236-Q>
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
