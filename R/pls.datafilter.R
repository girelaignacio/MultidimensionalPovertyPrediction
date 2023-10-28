# pls_chemometrics

pls.datafilter <- function(y, x, dimensions, scale = FALSE){
  col.region_ <- grep("^region_", colnames(x))
  col.time_ <- grep("^time_", colnames(x))
  col.out <- c(col.region_,col.time_)
  X <- x[,- col.out]
  X_nm <- x[, col.out]
  W <- chemometrics::pls1_nipals(X, y, a = dimensions, scale = scale)$W
  out <- list(W=W, X = X, X_nm = X_nm)
}
