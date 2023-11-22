[![R-CMD-check](https://github.com/metaRange/metaRange/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/metaRange/metaRange/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

# metaRange
![](man/figures/logo.png)

The R package **`metaRange`** is a framework that allows you to build process based species distribution models that can include a (basically) arbitrary number of environmental factors, processes, species and species interactions.
The common denominator for all models build with metaRange is that they are grid (i.e. raster) and population (i.e. not individual) based.


## Installation:
Install the **stable version** from CRAN with:
```
install.packages("metaRange")
```

Or the **development version** from github with:
```
devtools::install_github("metaRange/metaRange")
```

## Who is this package for?
Ecologists and programmers that are interested in studying and predicting species distributions and interactions under the influence of a changing environment on an intermediate time scale (years to hundreds of years).

## Prerequisites / where to get help
To give users with different experience levels an easy introduction to metaRange, it is accompanied by [vignettes / articles](https://metarange.github.io/metaRange/articles/index.html) that give in-depth explanations and examples on how to setup different types of simulations.
That being said, a basic knowledge of general R programming and "classic" object-oriented programming (OOP) concepts as well as some familiarity with geographic (raster) data processing and species distribution models (SDM) might be beneficial.
In case one is unfamiliar with any of these topics here are some reading recommendations:

| Topic  | Recommended introduction |
| ------------- | ------------- |
|  general R programming  | [Hadley Wickham's "Advanced R"](https://adv-r.hadley.nz/r6.html)  |
| OOP / [R6 package](https://r6.r-lib.org/articles/Introduction.html)  | ["Advanced R" Chapter "R6"](https://adv-r.hadley.nz/r6.html) |
| terra & geodata handling | [rspatial.org](https://rspatial.org/spatial/) |
| SDMs | Dormann et al. 2012 (Ref. 5) |



# Feature overview

## Environment
The package supports an unlimited number and type of environmental variables as for example climate data, land-use data, habitat suitability maps or any other kind of data that may influence the species in the simulation.
Environment variables can be supplied as raster data (specifically as a [`SpatRasterDataset`](https://rspatial.github.io/terra/reference/sds.html) (SDS) created by the [terra](https://rspatial.github.io/terra/index.html) package) (Hijmans 2023).
The layer in the SDS correspond to / represent the different time steps of the simulation which enables simulations with varying environmental conditions.

## Species
A simulation can contain multiple species, as well as interactions between them.
Every simulated species (i.e. each species object) consists of a set of `processes` that describe it's relationship with time, itself, the abiotic environment and other species and `traits`, which can be any type of data that can be accessed or changed by the processes.

Species have implied populations, each of which inhabit one grid cell of the landscape.
On a computational level this means that the `traits` of a species will in most cases be stored in an matrix with the same size as the landscape it is simulated in, where each value in the matrix represents the trait value of a population.

## Flexibility / dynamic process execution
The processes of all the species are executed in each time step, based on a user defined priority (i.e. the user can choose which process of which species should be executed at what time within a time step).
This gives enormous flexibility in the type of simulations the user wants to perform.
One can simulate and study single species [range dynamics](https://metarange.github.io/metaRange/articles/metaRange.html), [species interactions](https://metarange.github.io/metaRange/articles/species_interactions.html), [age-structured populations](https://metarange.github.io/metaRange/articles/age-structured-populations.html), [invasion dynamics](https://metarange.github.io/metaRange/articles/advanced_setup.html#interaction-with-the-priority-queue), climate and land-use change or a combination of any of those factors on a population / meta-population level.

## Ecological functions
To provide the user a quick start into building models with metaRange, it includes common ecological functions that can be used to model a variety of species such as [kernel-based dispersal](https://metarange.github.io/metaRange/reference/dispersal.html), a [negative exponential function](https://metarange.github.io/metaRange/reference/negative_exponential_function.html) to calculate such a kernel, [environmental suitability estimation](https://metarange.github.io/metaRange/reference/calculate_suitability.html) based on cardinal values, a [reproduction model](https://metarange.github.io/metaRange/reference/ricker_reproduction_model.html) in form of an Ricker model, as well as [metabolic scaling](https://metarange.github.io/metaRange/reference/metabolic_scaling.html) of multiple parameter based on the bodymass and temperature.

## Performance
The performance critical parts of the package have been implemented in `C++` with the help of the [`Rcpp`](https://www.rcpp.org/) (Eddelbuettel & Balamuta 2018) and [`RcppArmadillo`](https://github.com/RcppCore/RcppArmadillo) (Eddelbuettel & Sanderson 2014) packages.
The package can handle simulations of a large spatial (millions of grid cells) and temporal extent (> hundreds of years / time steps) on regular consumer grade hardware.

# Acknowledgments
Thanks to  [mikefc / coolbutuseless](https://github.com/coolbutuseless) for the tutorial on ["Modifying R6 objects after creation"](https://coolbutuseless.github.io/2021/02/19/modifying-r6-objects-after-creation/) and to [Tyler Morgan-Wall](https://github.com/tylermorganwall) for the R package [rayimage](https://github.com/tylermorganwall/rayimage) which gave a good example of a *Rcpp* implementation of an image convolution function.

# References

(1) Eddelbuettel D, Balamuta J (2018). "Extending R with C++: A Brief
Introduction to Rcpp." _The American Statistician_, *72*(1), 28-36.
doi:10.1080/00031305.2017.1375990

(2)Eddelbuettel D, Sanderson C (2014). "RcppArmadillo: Accelerating R
with high-performance C++ linear algebra." _Computational Statistics
and Data Analysis_, *71*, 1054-1063.
doi:10.1016/j.csda.2013.02.005

(3) Chang W (2021). _R6: Encapsulated Classes with Reference Semantics_.
R package version 2.5.1, <https://CRAN.R-project.org/package=R6>.

(4) Hijmans R (2023). _terra: Spatial Data Analysis_. R package version
1.7-46, <https://CRAN.R-project.org/package=terra>.

(5) Dormann, C.F., Schymanski, S.J., Cabral, J., Chuine, I., Graham, C., Hartig, F., Kearney, M., Morin, X., Römermann, C., Schröder, B. and Singer, A. (2012), Correlation and process in species distribution models: bridging a dichotomy. *Journal of Biogeography*, **39**: 2119--2131. doi:10.1111/j.1365-2699.2011.02659.x