
<!-- README.md is generated from README.Rmd. Please edit that file -->

# symbootpkg

<!-- badges: start -->

[![R-CMD-check](https://github.com/blessingofori-atta/PHS7045-Midterm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/blessingofori-atta/PHS7045-Midterm/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of symbootpkg is to provide flexible tools to compute
bootstrap-t, equal-tailed, and symmetric confidence intervals for
statistical analysis. It incorporates methods for plugin and nested
bootstrap standard error estimations, with options for parallelization
to improve efficiency.

## Features

- **Bootstrap-t Confidence Intervals**: Compute confidence intervals
  using the bootstrap-t method with options for plugin and nested
  standard error estimates.
- **Equal-Tailed and Symmetric Intervals**: Includes methods for both
  equal-tailed and symmetric bootstrap confidence intervals.
- **Flexible Standard Error Functions**: Supports user-specified `sdfun`
  functions or nested bootstrap for standard error estimation.
- **Parallel Processing**: Optional parallelization to leverage multiple
  cores for faster computation.

## Installation

You can install the development version of symbootpkg from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("blessingofori-atta/symbootpkg")
```

## Usage

### Basic Example

Here’s a basic example of how to use the bootstrap and bootstrap_t
functions:

``` r
library(symbootpkg)
set.seed(123)

# Define a statistic function
beta <- function(dat) {
  mod <- lm(Sepal.Length ~ Sepal.Width, data = dat)
  bb <- mod$coefficients[2]
  return(bb)
}

# Perform bootstrap resampling
boot_obj <- bootstrap(
  data = iris,
  statistic = beta,
  nboot = 1000,
  return_samples = TRUE
)

# Compute symmetic bootstrap-t confidence intervals
ci_result <- bootstrap_t(
  boot_obj = boot_obj,
  Bsd = 25,
  method = "bootsym-nested",
  alpha = 0.05
)

# Print results
print(ci_result)
#> 
#> Bootstrap-t Results
#> =====================
#> Method:          bootsym-nested 
#> Alpha:           0.05 
#> Theta (Observed): -0.2233611 
#> Standard Error:  0.1422654 
#> Bias:            -0.003427493 
#> Confidence Interval:
#>   Lower:  -0.519971 
#>   Upper:  0.07324885
```

### Parallel Processing Example

You can enable parallel processing to speed up the computation:

``` r
ci_result_parallel <- bootstrap_t(
  boot_obj = boot_obj,
  Bsd = 25,
  method = "bootsym-nested",
  parallel = TRUE,
  cores = 2,
  alpha = 0.05
)

# Print results
print(ci_result_parallel)
#> 
#> Bootstrap-t Results
#> =====================
#> Method:          bootsym-nested 
#> Alpha:           0.05 
#> Theta (Observed): -0.2233611 
#> Standard Error:  0.1422654 
#> Bias:            -0.003427493 
#> Confidence Interval:
#>   Lower:  -0.5131957 
#>   Upper:  0.06647356
```

## Contribution

Contributions are welcome! Please open an issues or submit a pull
request to improve the package.

## License

This package is licensed under the MIT License. See the
[LICENSE](LICENSE) file for details.

## References

1.  Efron, B., & Tibshirani, R. J. (1993). An introduction to the
    bootstrap. New York:Chapman & Hall.
2.  Hall, P. (1988). On Symmetric Bootstrap Confidence Intervals.
    Journal of the Royal Statistical Society. Series B (Methodological),
    50(1), 35–45. <http://www.jstor.org/stable/2345806>.
3.  Elias, C. J. (2015). Percentile and percentile- t bootstrap
    confidence intervals: A practical comparison. Journal of Econometric
    Methods, 4(1), 153–161. <doi:10.1515/jem-2013-0015>
4.  <http://users.stat.umn.edu/~helwig/notes/npboot-notes.html>

------------------------------------------------------------------------
