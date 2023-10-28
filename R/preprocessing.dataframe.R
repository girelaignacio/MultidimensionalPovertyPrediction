#' @export
preprocessing.dataframe <- function(which.data, target, time.format = c("trend","dummy")){
  ## Select the data.frame with `which.data`
  data <- select_dataframes(which.data)

  ## Preserve target variable (MPI, H, or A) with `target`
  y <- data[,target]
  ## Preserve predictors
  X <- data[,-c(1:8)]
  ## Create time format as dummy variabels (one-hot encoding) or as a trend
  if (time.format == "dummy"){
    time_ <- factor(data$Year)
    time_ <- model.matrix( ~ time_)[,-1]

  } else {
    t <- as.numeric(as.character(data$Year))
    time_ <- t - min(df$Year)
  }

  ## Set regions as dummy variables (one-hot encoding)
  region_ <-  factor(data$Region)
  R <- model.matrix( ~ region_)[,-1]
  colnames(R) <- gsub(pattern = "&", x =  colnames(R), replacement = "and")
  colnames(R) <- gsub(pattern = "-", x =  colnames(R), replacement = "")
  colnames(R) <- gsub(pattern = " ", x =  colnames(R), replacement = "")

  ## Concatenate matrices to create the ultimate dataframe
  data <- as.data.frame(cbind(y,time_,R,X))
    colnames(data)[1] <- target

  return(data)
}
