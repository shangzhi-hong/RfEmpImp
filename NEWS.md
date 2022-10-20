## RfEmpImp 2.1.8
* Documentation fixes for CRAN checks

## RfEmpImp 2.1.6
* Update documentations

## RfEmpImp 2.1.5
* Add new wrapper function to recover dependency on `ranger (>= 0.12.1)` and
to fix parameter passing
* New independent `mice.impute.rfpred.norm` function, and thus simplified
`mice.impute.rfpred.emp` function
* Bypass collinearity and constant checks in `mice` for `imp.*` functions
* Add support for imputation using samples of classification trees predictions
for prediction-based imputation (RfPred-Cate)
* Add new function `query.rf.pred.idx` for querying observation IDs associated
with the terminal nodes
* Add new function `query.rf.pred.val` for querying observed values associated
with the terminal nodes
* Add new function `conv.factor` for converting variables
* Update dependency for `mice (>= 3.9.0)` to avoid collinearity checks during
imputation
* Update documentations

## RfEmpImp 2.0.3
* Update documentation for initial CRAN release
* Add logo file

## RfEmpImp 2.0.0
* Add support for imputation using predicting nodes of random forests

## RfEmpImp 1.0.1
* First public release on GitHub
* Unit tests ready
