# metaRange 1.2.0

* Expand and rewrite vignettes (how to save a time series of traits).
* Prettier time series plot.
* Make some internals read-only (time_layer_mapping & number_time_steps)
    and introduce getters to retrieve these values.
* Finally fix the formatting of the temaining time estimate.
* When saving a species: check if its possible to convert the prefix to a string instead of directly throwing an error.


# metaRange 1.1.4

* Refactoring & more tests.
* Make sure dimensionality is preserved in the c++ functions.
* Improve the display of the remaining time estimate during the simulation.
* Update vignettes and docs.

# metaRange 1.1.3

* Fix bug in the initialization of the "current" environment which would
    always point to the same memory address, instead of a new one for each
    object / simulation.
* Improve print method for the simulation environment.

# metaRange 1.1.2

* Set the current environment as soon as the simulation is initialized.
* Subsequently: make sure that the environment actually has values, as
    otherwise `terra::as.matrix` fails. Also update examples because of that.

# metaRange 1.1.1

* Safeguard the dispersal function for NA in the inputs.

# metaRange 1.1.0

* update Ricker reproduction model. Add handling of negative reproduction rates,
    improve handling of boundary conditions. Add version with Allee effects.

# metaRange 1.0.3

* fix bug where traits could be added as references, which in turn could
    lead to unpredictable results in combination with the cpp functions.

# metaRange 1.0.2

* allow multiple species to be added in add_species()
* improve information printing
* automatically generate names for the environment sub-datasets if none are set.
    Error if the supplied names are not suitable.

# metaRange 1.0.1

* fix out-of-bounds access in calculate_suitability()
    found by CRAN's ASAN / valgrind checks
* fix warning for recycled values in example

# metaRange 1.0.0

* first release
