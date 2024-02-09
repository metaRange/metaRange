sim_env <- terra::sds(terra::rast(vals = 1:8, nrow = 2, ncol = 2, nlyr = 2))
names(sim_env) <- "env_01"
test_sim <- metaRangeSimulation$new(source_environment = sim_env)
expect_equal(
    current = test_sim$environment$current$env_01,
    target = terra::as.matrix(sim_env$env_01[[1]], wide = TRUE)
)
test_sim$add_process(
    species = NULL,
    "global_process",
    \() {},
    1
)
test_sim$begin()
expect_equal(
    current = test_sim$environment$current$env_01,
    target = terra::as.matrix(sim_env$env_01[[2]], wide = TRUE)
)
