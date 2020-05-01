
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RfEmpImp

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)  
An R package for random-forest-empowered imputation of missing Data

## Random-forest-based multiple imputation evolved

This is the repository for R package `RfEmpImp`, for multiple imputation
using random forests (RF).  
This R package is an implementation for the `RfPred` and `RfNode`
algorithms and currently operates under the multiple imputation
computation framework
[`mice`](https://CRAN.R-project.org/package=mice).  
The R package contains both newly proposed and improved existing
algorithms for random-forest-based multiple imputation of missing
data.  
For details of the newly proposed algorithms, please refer to:
[arXiv:2004.14823](https://arxiv.org/abs/2004.14823) (further updates
pending).

## Installation

This R package is already submitted to CRAN, but it may take a while
before the package being approved.  
Currently, interested users can install the package from GitHub:

``` r
# Install
if(!"remotes" %in% installed.packages()) install.packages("remotes")
remotes::install_github("shangzhi-hong/RfEmpImp")
# Attach
library(RfEmpImp)
```

## Imputation based on RF predictions

For continuous variables, in `RfPred.Emp` method, the empirical
distribution of random forest’s out-of-bag prediction errors is used to
construct the conditional distributions of the variable under
imputation, providing conditional distributions with better quality.

``` r
impEmp <- mice(nhanes, method = "rfpred.emp", m = 10, max.iter = 10, maxcor = 1.0)
```

Also, in `RfPred.Norm` method, normality was assumed for RF prediction
errors, as proposed by Shah *et al.*

``` r
impNorm <- mice(nhanes, method = "rfpred.norm", m = 10, max.iter = 10, maxcor = 1.0)
```

For categorical variables, in `RfPred.Cate` method, the probability
machine theory is used, and the predictions of missing categories are
based on the predicted probabilities for each missing observation.

``` r
impCate <- mice(nhanes, method = "rfpred.cate", m = 10, max.iter = 10, maxcor = 1.0)
```

With version `2.0.0`, the names of parameters were further simplified,
please refer to the documentation for details.  
For datasets with mixed types of variables, `RfEmp` method is a short
cut for using `RfPred.Emp` for continuous variables and `RfPred.Cate`
for categorical variables (of type `logical` or `factor`). Example:

``` r
impMixed <- mice(nhanes, method = "rfemp", m = 10, max.iter = 10, maxcor = 1.0)
```

## Imputation based on RF nodes

For both continuous variables, the observations under the predicting
nodes of random forest are used as candidates for imputation.  
Two methods are now available for the `RfNode` algorithm:  
`RfNode.Cond` uses the conditional distribution formed by the prediction
nodes, i.e. the weight changes of observations caused by the
bootstrapping of random forest are considered, and uses “in-bag”
observations only.  
Example:

``` r
impCond <- mice(nhanes, method = "rfnode.cond", m = 10, max.iter = 10, maxcor = 1.0)
```

`RfNode.Prox` uses the concepts of proximity matrices of random forests,
and observations fall under the same predicting nodes are used as
candidates for imputation.  
Example:

``` r
impProx <- mice(nhanes, method = "rfnode.prox", m = 10, max.iter = 10, maxcor = 1.0)
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
