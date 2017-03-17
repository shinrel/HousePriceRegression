data <- read.csv('train.csv', sep = ',', header = TRUE)

check_missing_values <- function(dataSet)
{
  for (i in seq(1,ncol(dataSet))) {
    num_NA = sum(is.na(dataSet[, i]))
    is_numeric = is.numeric(dataSet[,i]) 
    if (num_NA > 0) {
      print (paste0('column ', names(dataSet)[i],' has #missing values: ', num_NA, ' is numeric: ',is_numeric ))
    }
  }
}
deal_missing_values <- function(dataSet)
{
  for(i in 1:ncol(dataSet)){
    if (is.numeric(dataSet[,i])) {
      dataSet[is.na(dataSet[,i]), i] <- round(mean(dataSet[,i], na.rm = TRUE),0)
    }
  }
  
  #replace missing value for categorial predictors here.
  for(i in 1:ncol(dataSet)){
    lapply(dataSet[,i], function(x) x[is.na(x)] <- 0)
  }
    
  
  
  
  
  return(dataSet)
} 
deal_outliers <- function(dataSet)
{
  
}
do_variable_selection <- function(dataSet)
{
  
}

#missing values: categorical + numeric values. Numeric values can be replaced by mean, median for ease.
#outliers: do later.
#do regression: variable selection + linear regression + xgboost + random forest
# check the residuals
# Mine: regression part + transformation 
check_missing_values(data)
data2 <- deal_missing_values(data)
check_missing_values(data2)
