
<!-- README.md is generated from README.Rmd. Please edit that file -->

# envirotyping

<!-- badges: start -->

<!-- badges: end -->

The `envirotyping` package includes useful functions for gathering and
manipulating environmental data (weather, climate, soil data, etc.) for
use in quantitative genetic or plant breeding modeling and predictions.
You can find examples of functions in this package here:
<https://pages.github.umn.edu/IAA/envirotyping/>

## Installation

To install this package, you first need to install a few dependencies
that are only available on GitHub or have been archived by CRAN. First,
install the `rhwsd` package via:

``` r
devtools::install_github("stineb/rhwsd")
```

Next, install the `APSIM` package, which has been archived on CRAN:

``` r
install.packages("https://cran.r-project.org/src/contrib/Archive/APSIM/APSIM_0.9.3.tar.gz", repos = NULL)
```

Finally, use `devtools` to install `envirotyping`:

``` r
# install.packages("devtools")
devtools::install_github("IAA/envirotyping")
```

## Examples

See the vignette [`envirotyping`
Workflow](https://pages.github.umn.edu/IAA/envirotyping/articles/using_envirotyping.html)
for specific examples.
