nonmetric.pls <- function(y, x, dimensions, scale = FALSE){
  col.region_ <- grep("^region_", colnames(x))
  col.time_ <- grep("^time_", colnames(x))
  col.out <- c(col.region_,col.time_)
  X <- x[,- col.out]
  X_nm <- x[, col.out]
  W <- chemometrics::pls1_nipals(X, y, a = dimensions, scale = scale)$W
  scores <- as.matrix(X) %*% W
  colnames(scores) <- paste("score", 1:dimensions, sep = "")
  data <- as.data.frame(cbind(scores,X_nm))
  colnames.pred <- colnames(data)
  data$y <- y
  model <- stats::as.formula(paste0("y ~ ", paste0(colnames.pred, collapse = "+")))
  #modelframe <- stats::model.frame(y ~ . , data, na.action = "na.exclude")
  model <- betareg::betareg(model, data)
  #model <- betareg::betareg(y ~ . , data, na.action = "na.exclude")
  model$projection <- W
  model
}
