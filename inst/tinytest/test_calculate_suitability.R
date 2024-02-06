# Calculate (estimate) environmental suitability
# Yin, X., Kropff, M.J., McLaren, G., Visperas, R.M., (1995)
# A nonlinear model for crop development as a function of temperature,
# Agricultural and Forest Meteorology,
# Volume 77, Issues 1–2,
# Pages 1-16,
# \doi{10.1016/0168-1923(95)02236-Q}
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

tmax <- seq(40, 50, length.out = 10)
tmin <- seq(10, 20, length.out = 10)
topt <- seq(25, 35, length.out = 10)
tenv <- seq(10, 40, length.out = 10)
expect_true(
    all.equal(
        calculate_suitability_R(tmax, topt, tmin, tenv),
        calculate_suitability(tmax, topt, tmin, tenv)
    ),
    info = "testing that suitability is the same in c++ and R pt.2"
)

tmax <- seq(40, 50, length.out = 11)
tmin <- seq(10, 20, length.out = 10)
topt <- seq(25, 35, length.out = 10)
tenv <- seq(10, 40, length.out = 10)
expect_error(
    calculate_suitability(tmax, topt, tmin, tenv),
    info = "testing that error when inputs are of different lengths pt.1"
)
tmax <- seq(40, 50, length.out = 10)
tmin <- seq(10, 20, length.out = 11)
topt <- seq(25, 35, length.out = 10)
tenv <- seq(10, 40, length.out = 10)
expect_error(
    calculate_suitability(tmax, topt, tmin, tenv),
    info = "testing that error when inputs are of different lengths pt.2"
)
tmax <- seq(40, 50, length.out = 10)
tmin <- seq(10, 20, length.out = 10)
topt <- seq(25, 35, length.out = 11)
tenv <- seq(10, 40, length.out = 10)
expect_error(
    calculate_suitability(tmax, topt, tmin, tenv),
    info = "testing that error when inputs are of different lengths pt.3"
)
tmax <- seq(40, 50, length.out = 10)
tmin <- seq(10, 20, length.out = 10)
topt <- seq(25, 35, length.out = 10)
tenv <- seq(10, 40, length.out = 11)
expect_error(
    calculate_suitability(tmax, topt, tmin, tenv),
    info = "testing that error when inputs are of different lengths pt.4"
)

tmax <- seq(40, 50, length.out = 4)
tmax[[1]] <- NA
tmin <- seq(10, 20, length.out = 4)
tmin[[2]] <- NA
topt <- seq(25, 35, length.out = 4)
topt[[3]] <- NA
tenv <- seq(10, 40, length.out = 4)
tenv[[4]] <- NA
expect_true(
    all(calculate_suitability(tmax, topt, tmin, tenv) == 0),
    info = "testing that error when inputs contain NAs"
)
