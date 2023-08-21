modelPredictions <- function(test, models){

  test <- test[,-1]

  ypred.lm <- stats::predict(models$LinearModel, newdata = test)

  ypred.lasso <- stats::predict(models$LassoModel, newdata = test)

  ypred.pls <- stats::predict(models$PLSModel, newdata = test)

  ypred.xgb <- stats::predict(models$XGBModel, newdata = test)

  outlist <- list(
    Linear = ypred.lm,
    PLS = ypred.pls,
    Lasso = ypred.lasso,
    XGB = ypred.xgb
  )
  outlist
}
