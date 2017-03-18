require(dplyr)
library(stats)
library(Metrics)
HP.train <- read.csv('train.csv', sep = ',', header = TRUE)
HP.test <- read.csv('test.csv', sep = ',', header = TRUE)
HP.train$Id <- NULL
HP.test.Id <- HP.test$Id
HP.test$Id <- NULL
HP.test$SalePrice <- replicate(nrow(HP.test), -1)
ntrain <- nrow(HP.train)
ntest <- nrow(HP.test)
HP.all <- rbind(HP.train, HP.test)
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
check_categorical_variables <- function(dataSet)
{
  print('************************************************')
  print('Listing all variables with categorical values:')
  count <- 0
  for (i in 1:ncol(dataSet)) {
    if (!is.numeric(dataSet[,i])) {
      count <- count + 1
      factor_names <- paste(levels(dataSet[,i]), sep=" ")
      print (paste0(names(dataSet)[i], ' is categorical variable with number of factors: '))
      print (factor_names)
    }
  }
  print (paste0('Total categorical variables:' , count))
  print('************************************************')
}

deal_missing_values <- function(dataSet, ntrain = 1460, type = 1)
{
  # type is the replace method for numeric values. 
  #if type = 1 --> using mean to replace.
  #if type = 2 --> using median to replace.
  dataSet[] <- lapply(dataSet, function(x){
    # check if variables have a factor:
    if(!is.factor(x)) {
      #replace NA by mean
      if (type == 1) x[is.na(x)] <- mean(x[1:1460], na.rm = TRUE) 
      else if (type == 2) if (type == 1) x[is.na(x)] <- median(x[1:1460], na.rm = TRUE) 
    }
    else {
      # otherwise include NAs into factor levels and change factor levels:
      x <- factor(x, exclude=NULL)
      levels(x)[is.na(levels(x))] <- "Missing"
    }
    return(x)
  })
  
  return(dataSet)
}

#
# encoding categorical variables. If a variable has 3 levels --> we need 2 bit to encode
#
encode_categorical_variables <- function(dataSet, ntrain=1460)
{
  dataSetTemp <- dataSet
  for (i in 1:ncol(dataSet)){
    if (is.factor(dataSet[,i])) {
      col_name = names(dataSet)[i]
      t <- model.matrix(~ factor(dataSet[,i]) - 1)
      dataSetTemp <- cbind(t[,2:ncol(t)], dataSetTemp)
      new_col_names <- paste(col_name, "_bit", seq(1, ncol(t) - 1))
      names(dataSetTemp)[1:ncol(t) - 1] <- new_col_names
    }
  }
  removed <- c()
  for (i in 1:ncol(dataSetTemp)) {
    if (is.factor(dataSetTemp[,i])) removed <- c(removed, i)
  }
  dataSetTemp <- dataSetTemp[, -removed]
  return(dataSetTemp)
}

deal_outliers <- function(dataSet)
{
  
}

do_variable_selection <- function(dataSet)
{
  #must return a dataframe in which important variables are kept.
    
}

build_model <- function(train)
{
  fit <- lm(SalePrice ~., train)
  fit$rsquared <- summary(fit)$r.squared
  fit$adj.rsquared <- summary(fit)$adj.r.squared
  fit$predicted <- fit$fitted.values
  fit$actual <- train$SalePrice
  fit$rmse <- rmse(actual = fit$actual, predicted  = fit$predicted)
  return(fit)
}

evaluate_model <- function(model, test)
{
  eval.predicted <- predict(model, data=test)
  eval.actual <- test$SalePrice
  eval.RMSE <- rmse(actual = eval.actual, predicted = eval.predicted)
  
}


#missing values: categorical + numeric values. Numeric values can be replaced by mean, median for ease.
#outliers: do later.
#do regression: variable selection + linear regression + xgboost + random forest
# check the residuals
# Mine: regression part + transformation 

####################### 1. Dealing with missing values for training and testing data #################################################
check_missing_values(HP.all)
HP.all <- deal_missing_values(HP.all, ntrain)
print ('check missing values of the dataframe after replacing missing values. Should print blank')
check_missing_values(HP.all)

#************************************************************************************************************************************#
####################### 2. Encoding categorical variables ############################################################################
check_categorical_variables(HP.train)
# HP.train <- encode_categorical_variables(HP.train)
HP.all <- encode_categorical_variables(HP.all)
print ('check categorical values of the dataframe after encoding. Should print blank')
check_categorical_variables(HP.all)
#************************************************************************************************************************************#
HP.train = HP.all[1:ntrain,]
HP.test = HP.all[(ntrain+1) : nrow(HP.all),]

HP.train.lr_model <- build_model(HP.train)
HP.test$SalePrice <- predict(HP.train.lr_model, HP.test)

submission <- data.frame(Id <- HP.test.Id, SalePrice <- HP.test$SalePrice)
names(submission) <- c('Id', 'SalePrice')
write.csv(file = 'submission.csv', x = submission, row.names = FALSE, col.names = TRUE)
