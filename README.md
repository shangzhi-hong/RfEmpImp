
<!-- README.md is generated from README.Rmd. Please edit that file -->

# missForestFast

`missForestFast` is a project for a case study for improving an existing
random-forst-based imputation method,
[`missForest`](https://CRAN.R-project.org/package=missForest). The
proposed R package added the ability to keep intermediate results for
further study on random-forest-based iterative imputation method.

## Accleration using parallel computation

[`missForestFast`](https://github.com/shangzhi-hong/missForestFast)
provides user with the accelerated version of missing data imputation
powered by [`ranger`](https://CRAN.R-project.org/package=ranger) and
[`randomForestSRC`](https://CRAN.R-project.org/package=randomForestSRC)

## Installation for development version from GitHub

Currently the `missForestFast` package is only available as source files
on GitHub, any interested user can install and test the `missForestFast`
package by running the following code in R:

``` r
if(!"remotes" %in% installed.packages()) install.packages("remotes")
remotes::install_github("shangzhi-hong/missForestFast")
```

Run example:

``` r
library(missForestFast)
data(iris)
set.seed(202003)
iris.mis <- prodNA(iris, noNA = 0.5)
impRanger <- missForestRanger(iris.mis, xtrue = iris, keepAll = TRUE)
impSrc <- missForestSrc(iris.mis, xtrue = iris, keepAll = TRUE)
```

## The auto-stop criterion

The auto-stop criterion used by original missForest algorithm may be
misleading. The original missForest algorithm stops the imputation once
the out-of-bag error increases, however, the increase can be
instantaneous. As a fact, the symbolic auto-stop criterion may just be a
random stop from the continuous fluctuations of the out-of-bag error
estimates.

Example:

``` r
library(missForestFast)
data(iris)
set.seed(202003)
iris.mis <- prodNA(iris, noNA = 0.25)
targetIter <- 100
impRanger <- missForestRanger(iris.mis, xtrue = iris, maxiter = targetIter, keepAll = TRUE, forceIter = TRUE)
# Out-of-bag error (for auto-stop)
print(impRanger[["oobErrAll"]])
# Error from true data
print(impRanger[["errAll"]])
```

<img src="man/figures/README-PlotIterError-1.png" width="50%" style="display: block; margin: auto;" /><img src="man/figures/README-PlotIterError-2.png" width="50%" style="display: block; margin: auto;" />

## The initialization of imputation

In our opinion, the missForest algorithm should be recognized as a
special case of MICE (Multivariate Imputation using Chained Equations),
using predicted means as a replacement of samples from conditional
distributions. Use of variable mean (for continuous variable) and most
frequent category (for categorical variable) may not be the best choice.

Example:

``` r
library(missForestFast)
data(iris)
set.seed(202003)
iris.mis <- prodNA(iris, noNA = 0.25)
targetIter <- 100
impRangerMean <- missForestRanger(iris.mis, xtrue = iris, maxiter = targetIter, keepAll = TRUE, forceIter = TRUE, randInit = FALSE)
# Out-of-bag error with original initialization
print(impRangerMean[["oobErrAll"]])
# Error from true data with original initialization
print(impRangerMean[["errAll"]])
impRangerRand <- missForestRanger(iris.mis, xtrue = iris, maxiter = targetIter, keepAll = TRUE, forceIter = TRUE, randInit = TRUE)
# Out-of-bag error with random initialization
print(impRangerRand[["oobErrAll"]])
# Error from true data with random initialization
print(impRangerRand[["errAll"]])
```

<img src="man/figures/README-Init-1.png" width="50%" style="display: block; margin: auto;" /><img src="man/figures/README-Init-2.png" width="50%" style="display: block; margin: auto;" />

## Number of trees

Even a small number of trees in the random forest model can lead to
valid results, but the results can have unstable out-of-bag errors.

Example:

``` r
library(missForestFast)
data(iris)
set.seed(2020)
iris.mis <- prodNA(iris, noNA = 0.25)
targetIter <- 100
impRangerMean <- missForestRanger(iris.mis, xtrue = iris, maxiter = targetIter, keepAll = TRUE, forceIter = TRUE, randInit = FALSE, ntree = 100)
# Out-of-bag error with 100 trees
print(impRangerMean[["oobErrAll"]])
# Error from true data with 100 trees
print(impRangerMean[["errAll"]])
impRangerRand <- missForestRanger(iris.mis, xtrue = iris, maxiter = targetIter, keepAll = TRUE, forceIter = TRUE, randInit = FALSE, ntree = 10)
# Out-of-bag error with 10 trees
print(impRangerRand[["oobErrAll"]])
# Error from true data with 10 trees
print(impRangerRand[["errAll"]])
```

<img src="man/figures/README-TreeNum-1.png" width="50%" style="display: block; margin: auto;" /><img src="man/figures/README-TreeNum-2.png" width="50%" style="display: block; margin: auto;" />

## Order of variables

By default, missForest algorithm does imputation by the order of
missingness in the variables. But the order of variables for imputation
can be irrelevant to the final results, even the out-of-bag error can
appear higher, but the error from true data shows the contrary

Example:

``` r
library(missForestFast)
data(iris)
set.seed(2020)
iris.mis <- prodNA(iris, noNA = 0.25)
targetIter <- 100
impRangerDefault <- missForestRanger(iris.mis, xtrue = iris, maxiter = targetIter, keepAll = TRUE, forceIter = TRUE, randInit = FALSE, ntree = 100, varOrderSeq = FALSE)
# Out-of-bag error with default order
print(impRangerDefault[["oobErrAll"]])
# Error from true data with default order
print(impRangerDefault[["errAll"]])
impRangerSequential <- missForestRanger(iris.mis, xtrue = iris, maxiter = targetIter, keepAll = TRUE, forceIter = TRUE, randInit = FALSE, ntree = 100, varOrderSeq = TRUE)
# Out-of-bag error with sequential order
print(impRangerSequential[["oobErrAll"]])
# Error from true data with sequential order
print(impRangerSequential[["errAll"]])
```

<img src="man/figures/README-VarOrder-1.png" width="50%" style="display: block; margin: auto;" /><img src="man/figures/README-VarOrder-2.png" width="50%" style="display: block; margin: auto;" />
