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
# @export
ricker_reproduction_model_R <- function(abundance,
                                        reproduction_rate,
                                        carrying_capacity) {
    res <- abundance * exp(reproduction_rate * (1 - abundance / carrying_capacity))
    res[res < 0] <- 0
    res[is.nan(res)] <- 0
    return(res)
}

tabu <- 1:200
tr <- rep(0.5, 200)
tc <- rep(100, 200)
expect_true(
    all.equal(
        ricker_reproduction_model_R(tabu, tr, tc),
        ricker_reproduction_model(tabu, tr, tc)
    ),
    info = "testing that ricker reproduction is the same in c++ and R"
)



ricker_allee_reproduction_model_R <- function(abundance, reproduction_rate, carrying_capacity, allee_threshold) {
    N2 <- abundance * exp(reproduction_rate * (((carrying_capacity - abundance) * (abundance - allee_threshold)) / (carrying_capacity - allee_threshold)^2))
    N2[allee_threshold >= carrying_capacity] <- 0
    N2[is.na(N2)] <- 0
    return(N2)
}

tabu <- 1:200
tr <- rep(0.5, 200)
tc <- rep(90, 200)
ta <- seq(-100, 100, length.out = 200)
expect_true(
    all.equal(
        ricker_allee_reproduction_model_R(tabu, tr, tc, ta),
        ricker_allee_reproduction_model(tabu, tr, tc, ta)
    ),
    info = "testing that ricker + allee reproduction is the same in c++ and R"
)

tabu <- 1:200
tr <- 0.5
tc <- 90
ta <- 0
expect_true(
    all.equal(
        ricker_allee_reproduction_model_R(tabu, tr, tc, ta),
        ricker_allee_reproduction_model(tabu, tr, tc, ta)
    ),
    info = "testing that ricker + allee reproduction is the same in c++ and R pt.2"
)

tabu <- 1:200
tcompare <- 1:200
correct_vec <- 1:200 < 100 & 1:200 > 50
tr <- rep(2, 200)
tc <- rep(100, 200)
ta <- rep(50, 200)
res <- ricker_allee_reproduction_model(tabu, tr, tc, ta) > tcompare
expect_identical(
    res, correct_vec,
    info = "testing the growth rate is only positive when N < K and N > A"
)

tabu <- 1:200
correct_vec <- rep(0, 200)
tr <- rep(2, 200)
tc <- rep(100, 200)
ta <- rep(120, 200)
res <- ricker_allee_reproduction_model(tabu, tr, tc, ta)
expect_identical(
    res, correct_vec,
    info = "Test that the result is zero when A > K"
)

tabu <- 1:200
correct_vec <- rep(0, 200)
tr <- rep(2, 200)
tc <- rep(-10, 200)
ta <- rep(50, 200)
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


tabu <- 1:200
comparison_vec <- 1:200
tr <- rep(-1, 200)
tc <- rep(100, 200)
ta <- rep(50, 200)
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
