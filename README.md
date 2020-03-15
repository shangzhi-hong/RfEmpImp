
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RfEmpImp

## Random-forest-based Multiple Imputation Evolved

This is the repository for R package `RfEmpImp`. The R package
`RfEmpImp` is for multiple imputation based on the empirical
distribution of random forest’s out-of-bag prediction errors. This R
package is an implementation for the `RfEmpImp` algorithm using the
[`mice`](https://CRAN.R-project.org/package=mice) framework.  
For continuous variables, the empirical distribution of random forest’s
out-of-bag prediction errors is used to construct the conditional
distribution of the variable under imputation.  
For categorical variables, the probability machine theory is used, and
the prediction of categories are based on the predicted probabilities of
each observation.

## Installation

This R package is already submitted to CRAN, but it may take a while
before the package being approved.  
Interested users can install the package from Github:

``` r
if(!"remotes" %in% installed.packages()) install.packages("remotes")
remotes::install_github("shangzhi-hong/RfEmpImp")
```

Usage example:

``` r
library(RfEmpImp)
impObj <- mice(nhanes, method = "rfempimp", m = 10, max.iter = 10)
```

## Support for parallel computation

The model building for random forest is accelerated using parallel
computation powered by
[`ranger`](https://CRAN.R-project.org/package=ranger), which provide
support for multi-thread computation using native C++. In our
simulations, parallel computation can provide many-fold performance
boost for multiple imputation process.

## References

1.  Zhang, Haozhe, et al. “Random forest prediction intervals.” The
    American Statistician (2019): 1-15.
2.  Wright, Marvin N., and Andreas Ziegler. “ranger: A Fast
    Implementation of Random Forests for High Dimensional Data in C++
    and R.” Journal of Statistical Software 77.i01 (2017).
3.  Shah, Anoop D., et al. “Comparison of random forest and parametric
    imputation models for imputing missing data using MICE: a CALIBER
    study.” American journal of epidemiology 179.6 (2014): 764-774.
4.  Malley, James D., et al. “Probability machines.” Methods of
    information in medicine 51.01 (2012): 74-81.
