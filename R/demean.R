#' Mean-Center the data
#' 
#' Mean Centers the input and output matrices. 
#' 
#' @param data an object of class \code{idframe}
#' 
#' @return 
#' A list containing the following elements
#' 
#' \tabular{ll}{
#'    \code{fitted.values} \tab \code{idframe} object with mean-centered variables \cr
#'    \code{output.mean} \tab \code{vector} containing means for each output variable \cr
#'    \code{input.mean} \tab \code{vector} containing trend fits for each input variable
#'  }
#' 
#' @examples
#' data(cstr)
#' fit.mean <- demean(cstr)
#' cstr_detrend <- predict(fit.mean) 
#' 
#' @seealso \code{\link{predict.demean}}, \code{\link[stats]{colMeans}}
#' @export
demean <- function(data){
  
  data_demean <- data 
  output.mean <- colMeans(data$output)
  input.mean <- colMeans(data$input)
  
  
  data_demean$output <- data$output - output.mean
  data_demean$input <- data$input - input.mean
  
  est <- list(fitted.values=data_detrend,output.mean = output.mean,
              input.mean = input.mean)
  
  class(est) <- "demean"
  return(est)
}

#' Predict the centered values
#' 
#' Center an \code{idframe} object based on the training center means
#' 
#' @param object an object of class \code{idframe}
#' @param newdata An optional idframe object in which to look for variables with which
#' to predict. If ommited, the original idframe object is used
#' 
#' @return an \code{idframe} object
#' 
#' @examples
#' ## Examples for train and test sets
#' data(cstr)
#' splitList <- dataPartition(cstr,p=0.6)
#' train <- splitList$estimation # training set 
#' test <- splitList$validation # testing set
#' fit.mean <- demean(train)
#' train_demean <- predict(fit.mean)
#' test_demean <- predict(fit,newdata=test)  
#' @export
predict.demean <- function(object,newdata=NULL,...){
  
  if(is.null(newdata)){
    data <- fitted(object)
  } else{
    data <- newdata
    data$output <- data$output - object$output.mean
    data$input <- data$input - object$input.mean
  }
  return(data)
}
