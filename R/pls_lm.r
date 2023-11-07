  pls.lm <- list(label = "PLS-lm",
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

                 # x : metric data
                 # x_nm : non-metric data
                 PLS <- pls.datafilter(y, x, dimensions = param$ncomp, scale = FALSE)
                 X <- PLS$X
                 X_nm <- PLS$X_nm
                 loadings <- PLS$W
                 scores <- as.matrix(X) %*% loadings
                 colnames(scores) <- paste("score", 1:param$ncomp, sep = "")
                 data <- scores
                 data <- as.data.frame(cbind(scores,X_nm))
                 colnames.pred <- colnames(data)
                 data$y <- y
                 model.formula <- stats::as.formula(paste0("y ~ ", paste0(colnames.pred,collapse = " + "),sep = ""))
                 model <- stats::lm(model.formula, data = data, na.action = "na.exclude")
                 model$projection <- loadings
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

                 #X_nm <- newdata[, grep("^region_",colnames(newdata))]
                 X_nm <- newdata[, col.out]

                 newdata <- cbind(X_nm,scores)
                 ## Predict the linear model
                 stats::predict(modelFit, newdata)
               },
               prob = NULL,
               varImp = NULL,
               predictors = function(x, ...) rownames(x$projection),
               levels = function(x) x$obsLevels,
               sort = function(x) x[order(x[,1]),])

