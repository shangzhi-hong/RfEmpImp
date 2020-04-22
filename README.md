
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RfEmpImp

## Random-forest-based multiple imputation evolved

This is the repository for R package `RfEmpImp`.  
This R package is an implementation for the `RfEmp` and `RfNode`
algorithms using the [`mice`](https://CRAN.R-project.org/package=mice)
framework, for both newly proposed and improved algorithms for
random-forest-based multiple imputation of missing data.

## RfEmp imputation

For continuous variables, the empirical distribution of random forest’s
out-of-bag prediction errors is used to construct the conditional
distribution of the variable under imputation.  
For categorical variables, the probability machine theory is used, and
the prediction of categories are based on the predicted probabilities of
each observation.  
With version `2.0.0`, the names of parameters were further simplified,
please refer to the documentation for details.  
Examples:

``` r
library(RfEmpImp)
impObjEmp <- mice(nhanes, method = "rfemp", m = 10, max.iter = 10, maxcor = 1.0)
```

## RfNode imputation

For both continuous variables, the observations under the predicting
nodes of random forest are used as candidates for imputation. Two
methods are now available for the `RfNode` algorithm:  
`RfNode.Cond` uses the conditional distribution formed by the prediction
nodes, i.e. the weight changes of observations caused by the
bootstrapping of random forest are considered, and uses “in-bag”
observations only.  
`RfNode.Prox` uses the concepts of proximity matrices of random forests,
and observations fall under the same predicting nodes are used as
candidates for imputation.  
Examples:

``` r
library(RfEmpImp)
impObjCond <- mice(nhanes, method = "rfnode.cond", m = 10, max.iter = 10, maxcor = 1.0)
impObjProx <- mice(nhanes, method = "rfnode.prox", m = 10, max.iter = 10, maxcor = 1.0)
```

## Installation

This R package is already submitted to CRAN, but it may take a while
before the package being approved.  
Interested users can install the package from Github:

``` r
if(!"remotes" %in% installed.packages()) install.packages("remotes")
remotes::install_github("shangzhi-hong/RfEmpImp")
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
4.  Doove, Lisa L., Stef Van Buuren, and Elise Dusseldorp. “Recursive
    partitioning for missing data imputation in the presence of
    interaction effects.” Computational Statistics & Data Analysis 72
    (2014): 92-104.
5.  Malley, James D., et al. “Probability machines.” Methods of
    information in medicine 51.01 (2012): 74-81.
