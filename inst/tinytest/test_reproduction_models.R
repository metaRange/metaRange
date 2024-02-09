# Ricker reproduction model
#
# Ricker reproduction model
#
# @param abundance `<numeric>` Abundance.
# @param reproduction_rate `<numeric>` Growth / reproduction rate.
# @param carrying_capacity `<numeric>` Carrying capacity.
# @references Cabral, J.S. and Schurr, F.M. (2010) Estimating demographic
# models for the range dynamics of plant species.
# GlobalEcology and Biogeography, 19, 85–97.
# Original model:
# Ricker, W.E. (1954) Stock and recruitment.
# Journal of the Fisheries Research Board of Canada, 11, 559–623.
# @return \code{N} The number of individuals in the next time step.
ricker_reproduction_model_R <- function(abundance, reproduction_rate, carrying_capacity) {
    res <- abundance * exp(reproduction_rate * (1 - abundance / carrying_capacity))
    res[res < 0] <- 0
    res[is.nan(res)] <- 0
    return(res)
}

tabu <- 1:20
tr <- rep(0.5, 20)
tc <- rep(100, 20)
expect_true(
    all.equal(
        ricker_reproduction_model_R(tabu, tr, tc),
        ricker_reproduction_model(tabu, tr, tc)
    ),
    info = "testing that ricker reproduction is the same in c++ and R"
)

tabu <- 1:20
correct_vec <- rep(0, 20)
tr <- rep(2, 20)
tc <- rep(-10, 20)
ta <- rep(50, 20)
res1 <- ricker_reproduction_model(tabu, tr, tc)
res2 <- ricker_allee_reproduction_model(tabu, tr, tc, ta)
expect_identical(
    res1, correct_vec,
    info = "Test that the result is zero when K < 0 (ricker)"
)
expect_identical(
    res2, correct_vec,
    info = "Test that the result is zero when K < 0 (allee)"
)


tabu <- 1:20
comparison_vec <- 1:20
tr <- rep(-1, 20)
tc <- rep(100, 20)
ta <- rep(50, 20)
res1 <- ricker_reproduction_model(tabu, tr, tc)
res2 <- ricker_allee_reproduction_model(tabu, tr, tc, ta)
expect_true(
    all(res1 < comparison_vec),
    info = "Test negative reproduction rates (ricker)"
)
expect_true(
    all(res2 < comparison_vec),
    info = "Test negative reproduction rates (allee)"
)



ricker_allee_reproduction_model_R <- function(
    abundance,
    reproduction_rate,
    carrying_capacity,
    allee_threshold,
    overcomp_factor = 1) {

    N2 <- abundance *
        exp(reproduction_rate * overcomp_factor *
            (
                ((carrying_capacity - abundance) * (abundance - allee_threshold)) /
                    (carrying_capacity - allee_threshold)^2
            )
        )
    N2[allee_threshold >= carrying_capacity] <- 0
    N2[is.na(N2)] <- 0
    return(N2)
}

tabu <- 1:20
tr <- rep(0.5, 20)
tc <- rep(90, 20)
ta <- seq(-100, 100, length.out = 20)
expect_equal(
    ricker_allee_reproduction_model_R(tabu, tr, tc, ta),
    ricker_allee_reproduction_model(tabu, tr, tc, ta),
    info = "testing that ricker + allee reproduction is the same in c++ and R"
)

tabu <- 1:20
tr <- 0.5
tc <- 90
ta <- 0
expect_equal(
    ricker_allee_reproduction_model_R(tabu, tr, tc, ta),
    ricker_allee_reproduction_model(tabu, tr, tc, ta),
    info = "testing that ricker + allee reproduction is the same in c++ and R pt.2"
)

tabu <- 1:20
tcompare <- 1:20
correct_vec <- 1:20 < 100 & 1:20 > 50
tr <- rep(2, 20)
tc <- rep(100, 20)
ta <- rep(50, 20)
res <- ricker_allee_reproduction_model(tabu, tr, tc, ta) > tcompare
expect_identical(
    res, correct_vec,
    info = "testing the growth rate is only positive when N < K and N > A"
)

tabu <- 1:20
correct_vec <- rep(0, 20)
tr <- rep(2, 20)
tc <- rep(100, 20)
ta <- rep(120, 20)
res <- ricker_allee_reproduction_model(tabu, tr, tc, ta)
expect_identical(
    res, correct_vec,
    info = "Test that the result is zero when A > K"
)

tabu <- 1:20
tr <- 0.5
tc <- 90
ta <- 0
toc <- 0.5
expect_equal(
    ricker_allee_reproduction_model_R(tabu, tr, tc, ta, toc),
    ricker_allee_reproduction_model(tabu, tr, tc, ta, toc),
    info = "Test different scalar values for overcomp_factor"
)

tabu <- 1:20
tr <- rep(0.5, 20)
tc <- rep(90, 20)
ta <- rep(0, 20)
toc <- 0.5
expect_equal(
    ricker_allee_reproduction_model_R(tabu, tr, tc, ta, toc),
    ricker_allee_reproduction_model(tabu, tr, tc, ta, toc),
    info = "Test vector input for overcomp_factor pt.2"
)


tabu <- 1:20
tr <- rep(0.5, 20)
tc <- rep(90, 20)
ta <- rep(0, 20)
toc <- rep(0.5, 20)
expect_equal(
    ricker_allee_reproduction_model_R(tabu, tr, tc, ta, toc),
    ricker_allee_reproduction_model(tabu, tr, tc, ta, toc),
    info = "Test vector input for overcomp_factor"
)

tabu <- 1:20
correct_vec <- rep(0, 20)
tr <- rep(2, 20)
tc <- rep(100, 20)
ta <- rep(0, 20)
toc <- sqrt(tr)
expect_equal(
    ricker_allee_reproduction_model_R(tabu, tr, tc, ta, toc),
    ricker_allee_reproduction_model(tabu, tr, tc, ta, toc),
    info = "Test sqrt(r) as input for overcomp_factor"
)

tabu <- 1:20
correct_vec <- rep(0, 20)
tr <- rep(2, 20)
tc <- rep(100, 20)
ta <- rep(0, 20)
expect_equal(
    ricker_allee_reproduction_model_R(tabu, tr, tc, ta, tr),
    ricker_allee_reproduction_model(tabu, tr, tc, ta, tr),
    info = "Test sqrt(r) as input for overcomp_factor"
)

tabu <- 1:20
tr <- rep(2, 100)
tc <- rep(100, 20)
ta <- rep(0, 20)
toc <- rep(0.5, 20)
expect_error(
    ricker_allee_reproduction_model(tabu, tr, tc, ta, toc),
    info = "Test error if wrong vector size"
)

tabu <- 1:20
tr <- 0.5
tc <- 90
ta <- 0
toc <- rep(0.5, 20)
expect_error(
    ricker_allee_reproduction_model(tabu, tr, tc, ta, toc),
    info = "Test error if wrong vector size pt.2"
)

tabu <- 1:20
tr <- rep(2, 20)
tc <- rep(100, 20)
ta <- rep(0, 20)
toc <- rep(0.5, 21)
expect_error(
    ricker_allee_reproduction_model(tabu, tr, tc, ta, toc),
    info = "Test error if wrong vector size pt.3"
)
tabu <- 1:20
tr <- rep(2, 20)
tc <- rep(100, 20)
ta <- rep(0, 20)
toc <- rep(0.5, 19)
expect_error(
    ricker_allee_reproduction_model(tabu, tr, tc, ta, toc),
    info = "Test error if wrong vector size pt.3"
)
tabu <- 1
tr <- rep(2, 20)
tc <- rep(100, 20)
ta <- rep(0, 20)
toc <- 1
expect_error(
    ricker_allee_reproduction_model(tabu, tr, tc, ta, toc),
    info = "Test error if wrong vector size pt.4"
)
tabu <- 1:20
tr <- 2
tc <- rep(100, 20)
ta <- rep(0, 20)
toc <- rep(1, 20)
expect_error(
    ricker_allee_reproduction_model(tabu, tr, tc, ta, toc),
    info = "Test error if wrong vector size pt.5"
)
tabu <- 1
tr <- 2
tc <- 100
ta <- 0
toc <- c(1, 2)
expect_error(
    ricker_allee_reproduction_model(tabu, tr, tc, ta, toc),
    info = "Test error if wrong vector size pt.5"
)
tabu <- 1
tr <- NA
tc <- 100
ta <- 0
expect_true(
    ricker_allee_reproduction_model(tabu, tr, tc, ta) == 0,
    info = "Test error if wrong vector size pt.5"
)


tabu <- matrix(1:16, ncol = 4, nrow = 4)
tr <- matrix(0.5, ncol = 4, nrow = 4)
tc <- matrix(100, ncol = 4, nrow = 4)
ta <- matrix(0, ncol = 4, nrow = 4)
expect_true(
    all(dim(ricker_allee_reproduction_model(tabu, tr, tc, ta)) == c(4, 4)),
    info = "testing that dimensionality is preserved"
)

tabu <- 1:4
tr <- 0.5
tc <- 90
ta <- 0
expect_true(
    is.null(dim(ricker_allee_reproduction_model(tabu, tr, tc, ta))),
    info = "testing that dimensionality is preserved pt.2"
)

tabu <- matrix(1:16, ncol = 4, nrow = 4)
tr <- matrix(0.5, ncol = 4, nrow = 4)
tc <- matrix(100, ncol = 4, nrow = 4)
expect_true(
    all(dim(ricker_reproduction_model(tabu, tr, tc)) == c(4, 4)),
    info = "testing that dimensionality is preserved pt.3"
)

tabu <- 1:4
tr <- 0.5
tc <- 90
expect_true(
    is.null(dim(ricker_reproduction_model(tabu, tr, tc))),
    info = "testing that dimensionality is preserved pt.4"
)

tabu <- numeric(0)
tr <- numeric(0)
tc <- numeric(0)
res <- ricker_reproduction_model(tabu, tr, tc)
expect_equal(
    res, numeric(0),
    info = "testing that empty vectors return empty vectors"
)
tabu <- numeric(0)
tr <- numeric(0)
tc <- numeric(0)
ta <- numeric(0)
res <- ricker_allee_reproduction_model(tabu, tr, tc, ta)
expect_equal(
    res, numeric(0),
    info = "testing that empty vectors return empty vectors pt.2"
)
