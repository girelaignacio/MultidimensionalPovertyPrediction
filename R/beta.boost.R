beta.boost <- function(X, y) {
  data <- cbind.data.frame(X,y)
  modelframe <- model.frame(y ~ . ,data)
  betaboost <- mboost::glmboost(y~., data, family = betaboost::BetaReg())
  betaboost
}
