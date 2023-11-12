sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2))
names(sim_env) <- "env_01"
test_sim <- metaRangeSimulation$new(source_environment = sim_env)
test_sim$add_species("species_01")
test_sim$add_traits(
    "species_01",
    population_level = FALSE,
    trait_01 = matrix(1, nrow = 2, ncol = 2),
    trait_02 = matrix(2, nrow = 2, ncol = 2),
    trait_03 = c("a", "b")
)

file_prefix <- "A"
directory_name <- tempdir()

res_path <- save_species(
    test_sim$species_01,
    traits = "trait_01",
    prefix = file_prefix,
    path = directory_name
)

expect_equivalent(
    round(terra::as.matrix(terra::rast(res_path), wide = TRUE)),
    round(test_sim$species_01$traits[["trait_01"]]),
    info = "Saving a single trait works."
)

# test overwrite
res_path2 <- save_species(
    test_sim$species_01,
    traits = "trait_01",
    prefix = file_prefix,
    path = directory_name,
    overwrite = TRUE
)
expect_identical(
    res_path,
    res_path2,
    info = "Overwriting works."
)

res_path3 <- save_species(
    test_sim$species_01,
    prefix = basename(tempfile()),
    path = directory_name
)
expect_true(
    all(
        length(res_path3) == length(names(test_sim$species_01$traits)),
        file.exists(res_path3)),
    info = "Saving all traits works."
)

# cleanup
unlink(c(res_path, res_path3))
stopifnot(all(!file.exists(res_path, res_path3)))
