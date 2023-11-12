# Contributing

## General advice
Be nice.

## Bug reports
Please provide a minimal reproducible example if applicable and depending on the severity of the bug (typo vs crash) also your `sessionInfo()`.

## Notes for developers interested in contributing:

Note that this package includes `C++` code. To build it, you need a functioning compiler toolchain ([RTools](https://cran.r-project.org/bin/windows/Rtools/index.html) on windows).

### Code style
Functions, parameter and variables are in `snake_case`, classes are in `CamelCase`.
Note the `.lintr` file for `R` code and `.clang-format` file for the `c++` code.
Also, the very helpful command:
```
styler::style_pkg(
    scope = "line_breaks",
    strict = TRUE,
    indent_by = 4L
)
```

A quick workflow to make sure nothing breaks when making changes:
```
library(terra)
Rcpp::compileAttributes()
devtools::document()
library(tinytest)
devtools::load_all()

# Enable extensive reporting from metaRange functions when testing
set_verbosity(2)

# quick tests; suitable for CRAN and the CI
tinytest::test_all()

# A longer a bit more extensive test; suitable for local testing
tinytest::test_all(testdir = "inst/local")

# and lastly the R CMD check
devtools::check()
```

## Feature requests
Open an issue with the desired functionality to discuss.

## Pull requests
Please create an issue first.
After that, make sure that CRAN check passes, adhere to the general code style and create tests if you add new functionality.

## Scientific collaboration
Always welcome (but the ability to do so may depend on the number of current ongoing projects), be sure to reach out via mail or in an issue.