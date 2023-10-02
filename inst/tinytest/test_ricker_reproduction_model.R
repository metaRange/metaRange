# Ricker reproduction model
#
# Ricker reproduction model
#
# @param abundance `<numeric>` Abundance.
# @param reproduction_rate `<numeric>` Growth / reproduction rate.
# @param carrying_capacity `<numeric>` Carrying capacity.
# @references Cabral, J.S. & Schurr, F.M. (2010) Estimating demographic
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
