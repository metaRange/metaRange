[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/metaRange/metaRange/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/metaRange/metaRange/actions)


# metaRange
![](man/figures/logo.png)

metaRange is a collection of functions that allow you to build an process based species distribution model that can include (basically) arbitrary number of environmental factors, processes, species and species interactions. The common denominator for all models build with metaRange is that they are grid and population (i.e. not individual) based.

_____________________________________________

## For users:

Install with:
```
# CRAN / stable version
# install.packages("metaRange")
# Note: not yet on CRAN

# github / development version
devtools::install_github("metaRange/metaRange")
```

# Overview
Every simulated species consists of a set of `processes` or functions that describe it's relationship with time, itself, the abiotic environment and other species. Additional to the processes the species is described by `traits`, which can be any type of data that can be the basis for or changed by the processes. Each species consists of populations, each of which occupies one grid cell of the landscape. The processes of all the species are then executed in each timestep, for each population in the landscape, based on a user defined priority (i.e. the user can choose which process of which species should be executed at what time in the sequence/ simulation time step)

A simple example would be a species with the process `reproduction` that describes how the species reproduces during one simulation time step. This process could for example depend on: the temperature of the environment, the presence of other species (e.g. as food source), the current population density of the species and the reproduction rate of the species. This process would therefore most likely influence an `abundance` trait of the species (in form of a grid/ raster with an abundance value for each population). Note that neither `reproduction` nor `abundance` are automatically created but have to be defined by the user.

Note that this model is build using the [R6](https://r6.r-lib.org/) system, which means it feature a syntax that is slightly different than most base R functions. If one is unfamiliar with "classic" object-oriented programming, it might be advisable to read the short intro of the [R6 package](https://r6.r-lib.org/articles/Introduction.html) or even the chapter in [Hadley Wickham's "Advanced R"](https://adv-r.hadley.nz/r6.html).

_____________________________________________

## For developers interested in changing the package itself:

Note that this package includes `C++` code. To build it, you need a functioning compiler toolchain ([RTools](https://cran.r-project.org/bin/windows/Rtools/index.html) on windows).

A quick workflow to make sure nothing breaks when making changes:
```
library(here)
library(terra)
styler::style_pkg(
    scope = "line_breaks",
    strict = TRUE,
    indent_by = 4L
)
Rcpp::compileAttributes()
roxygen2::roxygenize('.', roclets=c('rd', 'collate', 'namespace'), clean = TRUE)
# or: devtools::document()
library(tinytest)
devtools::load_all()

# Enable extensive reporting from metaRange functions when testing
set_verbosity(2)

# quick tests; suitable for CRAN and the CI
tinytest::test_all()

# extensive tests; suitable for local testing
tinytest::test_all(testdir = "inst/local")

# and lastly the R CMD check
devtools::check()
```
_____________________________________________
Copyright (C) 2023  metaRange authors
License: GPL-3