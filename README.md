
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RfEmpImp

## Random-forest-based Multiple Imputation using Empirical Out-of-bag Prediction Error Distribtuion

Example:

``` r
library(RfEmpImp)
impObj <- mice(nhanes, method = "rfempimp", m = 10, max.iter = 10)
```
