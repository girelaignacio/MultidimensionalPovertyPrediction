# pls_chemometrics

pls.datafilter <- function(y, x, dimensions, scale = FALSE){
  non_continuous <- grep("^region_", colnames(X))
  X <- x[,- non_continuous]
  X_nm <- x[, non_continuous]
  W <- chemometrics::pls1_nipals(X, y, a = dimensions, scale = scale)$W
  out <- list(W=W, X = X, X_nm = X_nm)
}
