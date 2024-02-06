sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2, nlyr = 3))
sim <- metaRangeSimulation$new(source_environment = sim_env)
sim$add_globals("store" = c(0, 0, 0, 0, 0))
sim$add_species("species_1")
sim$add_process("species_1", "species_process_1",
    function() {
        self$sim$globals$store[[self$sim$queue$get_current_index()]] <-
            self$sim$queue$get_current_index()
    }, 1)
sim$add_process("species_1", "species_process_A",
    function() {
        return(invisible(TRUE))
    }, 1)
sim$add_process(species = NULL, "global_process_3",
    function() {
        self$globals$store[[self$queue$get_current_index()]] <-
            self$queue$get_current_index()
    }, 3)
sim$add_process(species = NULL, "global_process_2",
    function() {
        self$globals$store[[self$queue$get_current_index()]] <-
            self$queue$get_current_index()
    }, 2)
sim$add_process("species_1", "species_process_4",
    function() {
        self$sim$globals$store[[self$sim$queue$get_current_index()]] <-
            self$sim$queue$get_current_index()
    }, 4)
sim$add_process(species = NULL, "global_process_5",
    function() {
        self$exit()
    }, 5)
sim$begin()

expect_identical(
    sim$globals$store,
    c(1, 0, 3, 4, 5),
    info = "Testing that processes are executed in the correct order"
)
