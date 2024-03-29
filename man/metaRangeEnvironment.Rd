% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_Environment.R
\name{metaRangeEnvironment}
\alias{metaRangeEnvironment}
\title{metaRangeEnvironment object}
\value{
An \verb{<metaRangeEnvironment>} object
}
\description{
Creates an \link{metaRangeEnvironment} object in form of an
\link[R6:R6Class]{R6} class that stores and handles the environmental
values that influence the species in the simulation.
}
\examples{

## ------------------------------------------------
## Method `metaRangeEnvironment$new`
## ------------------------------------------------

# Note: Only for illustration purposes.
# The environment is automatically created when creating a simulation.
metaRangeEnvironment$new(
     sourceSDS = terra::sds(
         terra::rast(vals = 1, nrow = 2, ncol = 2)
     )
)

## ------------------------------------------------
## Method `metaRangeEnvironment$set_current`
## ------------------------------------------------

# Note: Only for illustration purposes.
# The time step is automatically set by the simulation.
sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2, nlyr = 2))
names(sim_env) <- "env_01"
env <- metaRangeEnvironment$new(sourceSDS = sim_env)
env$set_current(layer = 1)

## ------------------------------------------------
## Method `metaRangeEnvironment$print`
## ------------------------------------------------

env <- metaRangeEnvironment$new(
    sourceSDS = terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2, nlyr = 2))
)
env$print()
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{sourceSDS}}{A \emph{SpatRasterDataset} created by \code{\link[terra:sds]{terra::sds()}}
that holds all the environmental values influencing the simulation.
Note that the individual data sets should be sensibly named as
their names will used throughout the simulation to refer to them.}

\item{\code{current}}{an R environment that holds all the
environmental values influencing the present / current time step of the
simulation. These values are copies of the current layers
of the respective individual data sets in the sourceSDS and they are
stored as regular 2D R matrices under the same name given to the
corresponding sub data set in the sourceSDS. These matrices are updated
automatically at the beginning of each time step.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-metaRangeEnvironment-new}{\code{metaRangeEnvironment$new()}}
\item \href{#method-metaRangeEnvironment-set_current}{\code{metaRangeEnvironment$set_current()}}
\item \href{#method-metaRangeEnvironment-print}{\code{metaRangeEnvironment$print()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-metaRangeEnvironment-new"></a>}}
\if{latex}{\out{\hypertarget{method-metaRangeEnvironment-new}{}}}
\subsection{Method \code{new()}}{
Creates a new \link{metaRangeEnvironment} object.
This is done automatically when a simulation is created. There is no need to
call this as user.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{metaRangeEnvironment$new(sourceSDS = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sourceSDS}}{\verb{<SpatRasterDataset>} created by \code{\link[terra:sds]{terra::sds()}}
that holds all the environmental values influencing the simulation.
Note that the individual data sets should be sensibly named as
their names will used throughout the simulation to refer to them.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \verb{<metaRangeEnvironment>} object
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# Note: Only for illustration purposes.
# The environment is automatically created when creating a simulation.
metaRangeEnvironment$new(
     sourceSDS = terra::sds(
         terra::rast(vals = 1, nrow = 2, ncol = 2)
     )
)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-metaRangeEnvironment-set_current"></a>}}
\if{latex}{\out{\hypertarget{method-metaRangeEnvironment-set_current}{}}}
\subsection{Method \code{set_current()}}{
Set current (active) time step / environment layer.
No reason to call this as user. The current time step is set
automatically by the simulation.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{metaRangeEnvironment$set_current(layer)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{layer}}{\verb{<integer>} layer number.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\verb{<invisible self>}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{# Note: Only for illustration purposes.
# The time step is automatically set by the simulation.
sim_env <- terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2, nlyr = 2))
names(sim_env) <- "env_01"
env <- metaRangeEnvironment$new(sourceSDS = sim_env)
env$set_current(layer = 1)
}
\if{html}{\out{</div>}}

}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-metaRangeEnvironment-print"></a>}}
\if{latex}{\out{\hypertarget{method-metaRangeEnvironment-print}{}}}
\subsection{Method \code{print()}}{
Prints information about the environment to the console
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{metaRangeEnvironment$print()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
\verb{<invisible self>}
}
\subsection{Examples}{
\if{html}{\out{<div class="r example copy">}}
\preformatted{env <- metaRangeEnvironment$new(
    sourceSDS = terra::sds(terra::rast(vals = 1, nrow = 2, ncol = 2, nlyr = 2))
)
env$print()
}
\if{html}{\out{</div>}}

}

}
}
