# metaRange 1.0.3

* fix bug where traits could be added as references, which in turn could
    lead to unpredicted results in combination with the cpp functions.

# metaRange 1.0.2

* allow multiple species to be added in add_species()
* improve information printing
* automatically generate names for the environment subdatasets if none are set.
    Error if the supplied names are not suitable.

# metaRange 1.0.1

* fix out-of-bounds access in calculate_suitability()
    found by CRAN's ASAN / valgrind checks
* fix warning for recycled values in example

# metaRange 1.0.0

* first release
