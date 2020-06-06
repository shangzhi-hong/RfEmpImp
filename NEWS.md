## RfEmpImp 2.1.3
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
* Update dependency for `ranger (>= 0.12.3)` to fix parameter passing
* Update documentations

## RfEmpImp 2.0.3
* Update documentation for initial CRAN release
* Add logo file

## RfEmpImp 2.0.0
* Add support for imputation using predicting nodes of random forests

## RfEmpImp 1.0.1
* First public release on GitHub
* Unit tests ready
