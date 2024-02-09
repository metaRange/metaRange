# Contributing

## General advice
Be nice.

## Bug reports
Please provide a minimal reproducible example if applicable and depending on the severity of the bug (typo vs crash) also your `sessionInfo()`.

## Feature requests
Open an issue with the desired functionality to discuss.

## Pull requests
Please create an issue first.
After that, make sure that CRAN check passes, adhere to the general code style and create tests if you add new functionality.

## Scientific collaboration
Always welcome (but the ability to do so may depend on the number of current ongoing projects), be sure to reach out via mail or in an issue.

## Notes for developers

Note that this package includes `C++` code. To build it, you need a functioning compiler toolchain ([RTools](https://cran.r-project.org/bin/windows/Rtools/index.html) on windows).

### Code style
Functions, parameter and variables are in `snake_case`, classes are in `CamelCase`.
Note the `.lintr` file for `R` code and `.clang-format` file for the `c++` code.
Also, the very helpful command:
```R
styler::style_pkg(
    scope = "line_breaks",
    strict = TRUE,
    indent_by = 4L
)
```

### Make sure nothing breaks when making changes
```R
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

# test that examples work without problems
options(warn = 2)
devtools::run_examples()

# and lastly the R CMD check
devtools::check()
```

### Compiled code troubleshooting
`devtools::document()` and `devtools::load_all()` don't perform a standard install (they set special compile flags) and may cause the intermediate dlls to crash on error in some environments (specifically vscode). If there are any problems, try `pkgbuild::clean_dll()` and build again or test with an installed version via `devtools::install()`.

### Vignettes

Build a vignette for testing purposes (don't forget to delete the html afterwards).
```R
devtools::build_rmd("vignettes/[vignette_name].Rmd")
# or if this ^ doesn't work:
rmarkdown::render("vignettes/[vignette_name].Rmd")
```

Calculate test coverage:
```R
covr::package_coverage(type = "all")
```


### Docs / pkgdown
Build the site.
```R
pkgdown::build_site(seed = 3)
```
Check the html files manually or start a local server to preview the site with working js (search ect.).
```R
servr::httw("docs")
```

### Release process
1. Run extensive checks:
```R
devtools::check(
    incoming = TRUE,
    remote = TRUE
)
```

2. Update News and Description.

3. If it has been a year since the last (CRAN) release, update the copyright header (add current year).

4. Make sure the links are ok.
```R
urlchecker::url_check()
```

5. Check the spelling.
```R
spelling::spell_check_package()
```

6. Make sure to keep in mind:
https://github.com/DavisVaughan/extrachecks


7. Test the package on r-hub with valgrind / sanitizers for potential memory issues.
https://devtools.r-lib.org/reference/check_rhub.html

8. Update cran-comments

9. Follow:
https://r-pkgs.org/release.html#sec-release-process
I.e:
```R
devtools::submit_cran()
usethis::use_github_release()
```
