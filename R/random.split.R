random.split <- function(df, train_size){
  ids <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(train_size,1-train_size))
  train <- df[ids,]
  test <- df[!ids,]
  return(list(dataTrain = train, dataTest = test))
}
