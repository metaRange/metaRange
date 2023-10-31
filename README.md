[![Name](https://img.shields.io/badge/meta-Range-blue.svg)](https://metarange.github.io/metaRange/)
[![R-CMD-check](https://github.com/metaRange/metaRange/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/metaRange/metaRange/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

# metaRange
![](man/figures/logo.png)

The R package **`metaRange`** is a framework that allows you to build process based species distribution model that can include a (basically) arbitrary number of environmental factors, processes, species and species interactions. The common denominator for all models build with metaRange is that they are grid (i.e. raster) and population (i.e. not individual) based.

_____________________________________________

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
To allow users of different experience levels an easy introduction to metaRange, it is accompanied by vignettes that give in-depth explanations and examples on how to setup different types of simulations.
That being said, a basic knowledge of general R programming and "classic" object-oriented programming concepts might be beneficial. In case one is unfamiliar with the [R6 package](https://r6.r-lib.org/articles/Introduction.html) , it is advisable to read the short intro about it in [Hadley Wickham's "Advanced R"](https://adv-r.hadley.nz/r6.html).

# Overview

## Environment
Environment variables can be supplied as raster data (specifically as a [`SpatRasterDataset`](https://rspatial.github.io/terra/reference/sds.html) (SDS) created by the [terra](https://rspatial.github.io/terra/index.html) package).
There is no hard limit on the number of environmental variables that can be included in a simulation and there is also no limit on the type of these variables. One can use climate data, land-use data, habitat suitability maps or any other kind of data that may influence the species in the simulation.
The number of layer in the SDS correspond to the different time steps of the simulation. This means that each consecutive layer represents the environmental conditions of one time step. Note: Experienced users have the ability to define more complex mappings between layer and time steps.

## Species
Every simulated species (i.e. each species object) consists of a set of `processes` or functions that describe it's relationship with time, itself, the abiotic environment and other species. Additional to the processes the species is described by `traits`, which can be any type of data that can be accessed or changed by the processes. Species have implied populations, each of which inhabit one grid cell of the landscape. On a computational level this means that the `traits` of a species will in most cases be stored in an matrix with the same size as the landscape it is simulated in, where each value in the matrix represents the trait value of a population.

A simple example would be a species with the process `reproduction` that describes how the species reproduces during one simulation time step. This process could for example depend on: the temperature of the environment, the presence of other species (e.g. as food source), the current population density of the species and the reproduction rate of the species.

Summarized: A species consists of `processes` (functions) and `traits` (data). Processes can access trait and environmental data and change trait data of themselves or other species.

The processes of all the species are executed in each time step, based on a user defined priority (i.e. the user can choose which process of which species should be executed at what time within a time step). This gives enormous flexibility in the type of simulations the user wants to perform. One can simulate and study single species range dynamics, species interactions, age-structured populations, invasion dynamics, climate and land-use change or a combination of any of those factors on a population / meta-population level.
## Ecological functions
To provide the user a quick start into building models with metaRange, it includes a variety of common ecological functions such as negative exponential, kernel-based dispersal (see Nathan et al. 2012), calculation of the environmental suitability based on cardinal values (Yin et al. (1995), simplified by Yan and Hunt (1999) see eq: 4), reproduction in form of an Ricker model (see Ricker (1954) and Cabral and Schurr (2010)),
as well as metabolic scaling based on the metabolic theory of ecology (see Brown et al. (2004) and Brown, Sibly and Kodric-Brown (2012)).

## Performance
The performance critical parts of the package have been implemented in `C++` with the help of the [`Rcpp`](https://www.rcpp.org/) and [`RcppArmadillo`](https://github.com/RcppCore/RcppArmadillo) packages. The package can handle simulations of a large spatial (millions of grid cells) and temporal extent (> hundreds of years / time steps) on regular consumer grade hardware.
_____________________________________________

## Acknowledgements
Thanks to  [mikefc / coolbutuseless](https://github.com/coolbutuseless) for the tutorial on ["Modifying R6 objects after creation"](https://coolbutuseless.github.io/2021/02/19/modifying-r6-objects-after-creation/) and to [Tyler Morgan-Wall](https://github.com/tylermorganwall) for the R package [rayimage](https://github.com/tylermorganwall/rayimage) which gave a good example of a *Rcpp* implementation of an image convolution function.

## References

Eddelbuettel D, Balamuta J (2018). "Extending R with C++: A Brief
Introduction to Rcpp." _The American Statistician_, *72*(1), 28-36.
doi:10.1080/00031305.2017.1375990

Eddelbuettel D, Sanderson C (2014). "RcppArmadillo: Accelerating R
with high-performance C++ linear algebra." _Computational Statistics
and Data Analysis_, *71*, 1054-1063. doi:10.1016/j.csda.2013.02.005

Chang W (2021). _R6: Encapsulated Classes with Reference Semantics_.
R package version 2.5.1, <https://CRAN.R-project.org/package=R6>.

Hijmans R (2023). _terra: Spatial Data Analysis_. R package version
1.7-46, <https://CRAN.R-project.org/package=terra>.

Yin, X., Kropff, M.J., McLaren, G., Visperas, R.M., (1995) A nonlinear model for crop development as a function of temperature, Agricultural and Forest Meteorology, Volume 77, Issues 1–2, Pages 1--16, doi:10.1016/0168-1923(95)02236-Q

Weikai Yan, L.A. Hunt, (1999) An Equation for Modelling the Temperature Response of Plants using only the Cardinal Temperatures, Annals of Botany, Volume 84, Issue 5, Pages 607--614, ISSN 0305-7364, doi:10.1006/anbo.1999.0955

Brown, J.H., Gillooly, J.F., Allen, A.P., Savage, V.M. and West, G.B. (2004) Toward a Metabolic Theory of Ecology. Ecology, 85 1771--1789. doi:10.1890/03-9000

Brown, J.H., Sibly, R.M. and Kodric-Brown, A. (2012) Introduction: Metabolism as the Basis for a Theoretical Unification of Ecology. In Metabolic Ecology (eds R.M. Sibly, J.H. Brown and A. Kodric-Brown) doi:10.1002/9781119968535.ch

Cabral, J.S. and Schurr, F.M. (2010) Estimating demographic models for the range dynamics of plant species. Global Ecology and Biogeography, 19, 85--97. doi:10.1111/j.1466-8238.2009.00492.x

Ricker, W.E. (1954) Stock and recruitment. Journal of the Fisheries Research Board of Canada, 11, 559--623. doi:10.1139/f54-039

Nathan, R., Klein, E., Robledo-Arnuncio, J.J. and Revilla, E. (2012) Dispersal kernels: review. in: Dispersal Ecology and Evolution pp. 187--210. (eds J. Clobert, M. Baguette, T.G. Benton and J.M. Bullock), Oxford, UK: Oxford Academic, 2013. doi:10.1093/acprof:oso/9780199608898.003.0015