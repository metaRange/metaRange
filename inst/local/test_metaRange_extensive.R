
extract_stats <- function(species) {
    stats <- list()
    for (i in names(species$traits)) {
        if (is.numeric(species$traits[[i]])) {
            stats[[paste0(i, "_min")]] <- min(species$traits[[i]])
            stats[[paste0(i, "_mean")]] <- mean(species$traits[[i]])
            stats[[paste0(i, "_median")]] <- median(species$traits[[i]])
            stats[[paste0(i, "_max")]] <- max(species$traits[[i]])
            stats[[paste0(i, "_len")]] <- length(species$traits[[i]])
        }
    }
    return(unlist(stats))
}

simlength <- 6
temperature <- terra::rast(datasets::volcano * 0.2 - 10 + 273.15)
precipitation <- terra::rast(1000 - datasets::volcano * 4)
habitat <- terra::rast(datasets::volcano / max(datasets::volcano))
temperature <- terra::disagg(temperature, 2)
precipitation <- terra::disagg(precipitation, 2)
habitat <- terra::disagg(habitat, 2)
for (i in 2:simlength) {
    terra::add(temperature) <- terra::disagg(terra::rast(datasets::volcano * 0.2 - 10 + sin(i) * 2 + 273.15), 2)
    terra::add(precipitation) <- terra::disagg(terra::rast(1000 - datasets::volcano * 4 + sin(i) * 100), 2)
    terra::add(habitat) <- terra::disagg(terra::rast(datasets::volcano / max(datasets::volcano)), 2)
}
env <- terra::sds(temperature, precipitation, habitat, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
names(env) <- c("temperature", "precipitation", "habitat")

sim <- create_simulation(env)
sim$add_species("test_species")

# Add traits that define the environmental limits
sim$add_traits(
    species = "test_species",
    population_level = FALSE,
    "suitability" = NA_real_,
    "temperature_maximum" = 30 + 273,
    "temperature_optimum" = 20 + 273,
    "temperature_minimum" = 0 + 273,
    "precipitation_maximum" = 1200,
    "precipitation_optimum" = 800,
    "precipitation_minimum" = 0
)

# Add a process to calculate the suitability
sim$add_process(
    species = "test_species",
    process_name = "calculate_general_suitability",
    process_fun = function() {
        self$traits[["suitability"]] <-
            calculate_suitability(
                vmax = self$traits$temperature_maximum,
                vopt = self$traits$temperature_optimum,
                vmin = self$traits$temperature_minimum,
                venv = self$sim$environment$current[["temperature"]])

        self$traits[["suitability"]] <- self$traits[["suitability"]] *
            calculate_suitability(
                vmax = self$traits$precipitation_maximum,
                vopt = self$traits$precipitation_optimum,
                vmin = self$traits$precipitation_minimum,
                venv = self$sim$environment$current[["precipitation"]])

        self$traits[["suitability"]] <- self$traits[["suitability"]] *
            self$sim$environment$current[["habitat"]]
    },
    execution_priority = 1
)

# Add traits that are used in the reproduction model
sim$add_traits(
    species = "test_species",
    population_level = TRUE,
    "abundance" = 100,
    "reproduction_rate" = 0.5,
    "carrying_capacity" = 1000,
    "mass" = 1
)
sim$add_traits(
    species = "test_species",
    population_level = FALSE,
    "exponent_reproduction_rate" = -1 / 4,
    "exponent_carrying_capacity" = -3 / 4
)
sim$add_traits(
    species = "test_species",
    population_level = FALSE,
    "reproduction_rate_mte_constant" = calculate_normalization_constant(
        parameter_value = sim$test_species$traits[["reproduction_rate"]][[1]],
        scaling_exponent = sim$test_species$traits[["exponent_reproduction_rate"]],
        mass = sim$test_species$traits[["mass"]][[1]],
        reference_temperature = sim$test_species$traits[["temperature_optimum"]],
        E = -0.65,
        k = 8.617333e-05),
    "carrying_capacity_mte_constant" = calculate_normalization_constant(
        parameter_value = sim$test_species$traits[["carrying_capacity"]][[1]],
        scaling_exponent = sim$test_species$traits[["exponent_carrying_capacity"]],
        mass = sim$test_species$traits[["mass"]][[1]],
        reference_temperature = sim$test_species$traits[["temperature_optimum"]],
        E = 0.65,
        k = 8.617333e-05)
)

# Add a process to apply the metabolic theory of ecology
sim$add_process(
    species = "test_species",
    process_name = "mte",
    process_fun = function() {
        self$traits[["reproduction_rate"]] <-
            metabolic_scaling(
                    normalization_constant = self$traits[["reproduction_rate_mte_constant"]],
                    scaling_exponent = self$traits[["exponent_reproduction_rate"]],
                    mass = self$traits[["mass"]],
                    temperature = self$sim$environment$current[["temperature"]],
                    E = -0.65,
                    k = 8.617333e-05)

        self$traits[["carrying_capacity"]] <-
            metabolic_scaling(
                    normalization_constant = self$traits[["carrying_capacity_mte_constant"]],
                    scaling_exponent = self$traits[["exponent_carrying_capacity"]],
                    mass = self$traits[["mass"]],
                    temperature = self$sim$environment$current[["temperature"]],
                    E = 0.65,
                    k = 8.617333e-05)
    },
    execution_priority = 2
)

# Add a process to calculate the reproduction
sim$add_process(
    species = "test_species",
    process_name = "reproduction",
    process_fun = function() {
        self$traits[["abundance"]] <-
            ricker_reproduction_model(
                self$traits[["abundance"]],
                self$traits[["reproduction_rate"]] * self$traits[["suitability"]],
                self$traits[["carrying_capacity"]] * self$traits[["suitability"]]
            )
    },
    execution_priority = 3
)
sim$add_traits(
    species = "test_species",
    population_level = TRUE,
    "offspring" = 0
)
sim$add_traits(
    species = "test_species",
    population_level = FALSE,
    "dispersal_kernel" = calculate_dispersal_kernel(
        max_dispersal_dist = 5,
        kfun = negative_exponential_function,
        mean_dispersal_dist = 1.5)
)

# Add a process to calculate the dispersal
sim$add_process(
    species = "test_species",
    process_name = "dispersal_process",
    process_fun = function() {
        self$traits[["abundance"]] <- dispersal(
            abundance = self$traits[["abundance"]],
            weights = self$traits[["suitability"]],
            dispersal_kernel = self$traits[["dispersal_kernel"]])
    },
    execution_priority = 4
)




sim$begin()
res <- extract_stats(sim[["test_species"]])
state <- c(
    suitability_min = 0.0418143119708298, suitability_mean = 0.272281332716347,
    suitability_median = 0.281152626551794, suitability_max = 0.360250824994944,
    suitability_len = 21228, temperature_maximum_min = 303, temperature_maximum_mean = 303,
    temperature_maximum_median = 303, temperature_maximum_max = 303,
    temperature_maximum_len = 1, temperature_optimum_min = 293, temperature_optimum_mean = 293,
    temperature_optimum_median = 293, temperature_optimum_max = 293,
    temperature_optimum_len = 1, temperature_minimum_min = 273, temperature_minimum_mean = 273,
    temperature_minimum_median = 273, temperature_minimum_max = 273,
    temperature_minimum_len = 1, precipitation_maximum_min = 1200,
    precipitation_maximum_mean = 1200, precipitation_maximum_median = 1200,
    precipitation_maximum_max = 1200, precipitation_maximum_len = 1,
    precipitation_optimum_min = 800, precipitation_optimum_mean = 800,
    precipitation_optimum_median = 800, precipitation_optimum_max = 800,
    precipitation_optimum_len = 1, precipitation_minimum_min = 0,
    precipitation_minimum_mean = 0, precipitation_minimum_median = 0,
    precipitation_minimum_max = 0, precipitation_minimum_len = 1,
    abundance_min = 6.07158209047509, abundance_mean = 151.646712296458,
    abundance_median = 150.776385795058, abundance_max = 211.231750564186,
    abundance_len = 21228, reproduction_rate_min = 0.17287064803534,
    reproduction_rate_mean = 0.37618599140601, reproduction_rate_median = 0.302530608297634,
    reproduction_rate_max = 1.04101346519378, reproduction_rate_len = 21228,
    carrying_capacity_min = 480.301184103248, carrying_capacity_mean = 1642.34058129101,
    carrying_capacity_median = 1652.72533187152, carrying_capacity_max = 2892.33600777493,
    carrying_capacity_len = 21228, mass_min = 1, mass_mean = 1, mass_median = 1,
    mass_max = 1, mass_len = 21228, exponent_reproduction_rate_min = -0.25,
    exponent_reproduction_rate_mean = -0.25, exponent_reproduction_rate_median = -0.25,
    exponent_reproduction_rate_max = -0.25, exponent_reproduction_rate_len = 1,
    exponent_carrying_capacity_min = -0.75, exponent_carrying_capacity_mean = -0.75,
    exponent_carrying_capacity_median = -0.75, exponent_carrying_capacity_max = -0.75,
    exponent_carrying_capacity_len = 1, reproduction_rate_mte_constant_min = 75747034725.6295,
    reproduction_rate_mte_constant_mean = 75747034725.6295, reproduction_rate_mte_constant_median = 75747034725.6295,
    reproduction_rate_mte_constant_max = 75747034725.6295, reproduction_rate_mte_constant_len = 1,
    carrying_capacity_mte_constant_min = 6.60091846249952e-09, carrying_capacity_mte_constant_mean = 6.60091846249952e-09,
    carrying_capacity_mte_constant_median = 6.60091846249952e-09,
    carrying_capacity_mte_constant_max = 6.60091846249952e-09, carrying_capacity_mte_constant_len = 1,
    offspring_min = 0, offspring_mean = 0, offspring_median = 0,
    offspring_max = 0, offspring_len = 21228, dispersal_kernel_min = 2.10663944345672e-05,
    dispersal_kernel_mean = 0.00826446280991736, dispersal_kernel_median = 0.000914991939840905,
    dispersal_kernel_max = 0.261913481732312, dispersal_kernel_len = 121
)
expect_true(base::all.equal(res[order(names(res))], state[order(names(state))], scale = 1, tolerance = 1e-05))

# test_saving <- function(sim) {
#     on.exit(unlink(save_paths, recursive = TRUE))
#     save_path_1 <- save_species(
#         x = sim[["test_species"]],
#         traits = c("abundance"),
#         prefix = "01_",
#         path = tempdir(),
#         wopt = list(datatype = "FLT8S"), # needed in testing because of numerical inaccuracies
#         overwrite = TRUE)

#     res1 <- terra::all.equal(
#         terra::rast(terra::as.matrix(terra::rast(save_path_1), wide = TRUE)),
#         terra::rast(sim[["test_species"]]$traits$abundance))

#     if (!checkmate::test_true(res1)) {
#         warning("Saved raster is not equal to the expected raster: ", res1, call. = FALSE)
#         res1 <- FALSE
#     }

#     save_path_2 <- save_species(
#         x = sim[["test_species"]],
#         traits = c("temperature_maximum"),
#         prefix = "02_",
#         path = tempdir(),
#         overwrite = TRUE)
#     reload <- as.numeric(read.csv(file.path(save_path_2)))
#     res2 <- reload == sim[["test_species"]]$traits$temperature_maximum

#     save_path_3 <- save_species(
#         x = sim[["test_species"]],
#         prefix = "03_",
#         path = tempdir(),
#         overwrite = TRUE)
#     res3 <- TRUE
#     for (path in save_path_3) {
#         res3 <- res3 && file.exists(path)
#     }



#     save_paths <- c(save_path_1, save_path_2, save_path_3)
#     final_res <- all(res1, res2, res3)
#     return(list("res" = final_res, "paths" = save_paths))
# }
# out <- test_saving(sim)
# expect_true(out$res)

# expect_false(any(file.exists(out$paths)))
