# select_dataframes.R

select.data <- function(x){
  if(x == 1){data <- MultidimensionalPovertyPrediction::data1}
  else if(x == 2){data <- MultidimensionalPovertyPrediction::data2}
  else if(x == 5){data <- MultidimensionalPovertyPrediction::data5}
  else if(x == 8){data <- MultidimensionalPovertyPrediction::data8}
  else if(x == 9){data <- MultidimensionalPovertyPrediction::data9}
  else if(x == 10){data <- MultidimensionalPovertyPrediction::data10}
  else if(x == 13){data <- MultidimensionalPovertyPrediction::data13}
  else if(x == 15){data <- MultidimensionalPovertyPrediction::data15}
  else{data <- MultidimensionalPovertyPrediction::webscrapped_data0}
  data
}
