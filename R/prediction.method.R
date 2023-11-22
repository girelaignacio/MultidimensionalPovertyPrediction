#' @export
prediction.method <- function(model){
  switch(model,
         #### Non-metric PLS with beta regression ####
         pls.beta = {
           list(label = "PLS-lm",
                library = c("chemometrics", "stats"),
                type = "Regression",
                ## Tune over both parameters at the same time
                parameters = data.frame(parameter = c('ncomp'),
                                        class = c("numeric"),
                                        label = c('#Components')),
                grid = function(x, y, len = NULL, search = "grid") {
                  if(search == "grid") {
                    grid <- expand.grid(ncomp = seq(1, min(ncol(x) - 1, len), by = 1))
                  } else {
                    grid <- expand.grid(ncomp = sample(1:ncol(x), size = len))
                  }
                  grid
                },
                loop = NULL,
                fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                  ## First fit the pls model, generate the training set scores,
                  ## then attach what is needed to the model object to
                  ## be used later

                  model <- nonmetric.pls(y, x, dimensions = param$ncomp, scale = FALSE)
                  model
                },
                predict = function(modelFit, newdata, submodels = NULL) {
                  ## Now apply the same scaling to the new samples
                  col.region_ <- grep("^region_", colnames(newdata))
                  col.time_ <- grep("^time_", colnames(newdata))
                  col.out <- c(col.region_, col.time_)
                  X <- newdata[, - col.out]
                  scores <- as.matrix(X) %*% modelFit$projection
                  colnames(scores) <- paste("score", 1:ncol(scores), sep = "")
                  scores <- as.data.frame(scores)
                  X_nm <- newdata[, col.out]

                  newdata <- cbind(X_nm,scores)
                  ## Predict the linear model
                  stats::predict(modelFit, newdata)
                },
                prob = NULL,
                varImp = NULL,
                predictors = function(x, ...) rownames(x$projection),
                levels = function(x) x$obsLevels,
                sort = function(x) x[order(x[,1]),]
           )
         },
         #### Non-metric PLS ####
         pls.lm = {
           list(label = "PLS-lm",
                library = c("chemometrics", "stats"),
                type = "Regression",
                ## Tune over both parameters at the same time
                parameters = data.frame(parameter = c('ncomp'),
                                        class = c("numeric"),
                                        label = c('#Components')),
                grid = function(x, y, len = NULL, search = "grid") {
                  if(search == "grid") {
                    grid <- expand.grid(ncomp = seq(1, min(ncol(x) - 1, len), by = 1))
                  } else {
                    grid <- expand.grid(ncomp = sample(1:ncol(x), size = len))
                  }
                  grid
                },
                loop = NULL,
                fit = function(x, y, wts, param, lev, last, classProbs, ...) {
                  ## First fit the pls model, generate the training set scores,
                  ## then attach what is needed to the model object to
                  ## be used later

                  model <- nonmetric.beta.pls(y, x, dimensions = param$ncomp, scale = FALSE)
                  model
                },
                predict = function(modelFit, newdata, submodels = NULL) {
                  ## Now apply the same scaling to the new samples
                  col.region_ <- grep("^region_", colnames(newdata))
                  col.time_ <- grep("^time_", colnames(newdata))
                  col.out <- c(col.region_, col.time_)
                  X <- newdata[, - col.out]
                  scores <- as.matrix(X) %*% modelFit$projection
                  colnames(scores) <- paste("score", 1:ncol(scores), sep = "")
                  scores <- as.data.frame(scores)
                  X_nm <- newdata[, col.out]

                  newdata <- cbind(X_nm,scores)
                  ## Predict the linear model
                  stats::predict(modelFit, newdata)
                },
                prob = NULL,
                varImp = NULL,
                predictors = function(x, ...) rownames(x$projection),
                levels = function(x) x$obsLevels,
                sort = function(x) x[order(x[,1]),]
           )
         }
         )
}
