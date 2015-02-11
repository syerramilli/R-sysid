#' Remove linear trends
#' 
#' Removes the linear function from the input and output matrices. 
#' 
#' @param data an object of class \code{idframe}
#' 
#' @return 
#' A list containing the following elements
#' 
#' \tabular{ll}{
#'    \code{fitted.values} \tab \code{idframe} object with detrended variables
#'    \code{output.trend} \tab \code{list} containing trend fits for each output variable \cr
#'    \code{input.trend} \tab \code{list} containing trend fits for each input variable
#'  }
#' 
#' @examples
#' data(cstr)
#' fit <- detrend(cstr)
#' cstr_detrend <- predict(fit) 
#' 
#' @seealso \code{\link{predict.detrend.idframe}}, \code{\link[stats]{lm}}
#' @export
detrend.idframe <- function(data){
  
  data_detrend <- data 
    
  output_trend <- lapply(data$output,trend.fit)
  out <- detrend.predict(output_trend,data$output)
  
  input_trend <- lapply(data$input,trend.fit)
  input <- detrend.predict(input_trend,data$input)
  
  data_detrend$output <- out;data_detrend$input <- input
    
  est <- list(fitted.values=data_detrend,output.trend = output_trend,
              input.trend = input_trend)
  
  class(est) <- "detrend.idframe"
  return(est)
}

#' Predict method for trend fits on idframe objects
#' 
#' Detrended \code{idframe} object based on linear trend fit
#' 
#' @param object an object of class \code{idframe}
#' @param newdata An optional idframe object in whic to look for variables with which
#' to predict. If ommited, the original detrended idframe object is used
#' 
#' @return an \code{idframe} object
#' 
#' @examples
#' ## Examples for train and test sets
#' data(cstr)
#' splitList <- dataPartition(cstr,p=0.6)
#' train <- splitList$estimation # training set 
#' test <- splitList$validation # testing set
#' fit <- detrend(trend)
#' train_detrend <- predict(fit)
#' test_detrend <- predict(fit,newdata=test)  
#' @export
predict.detrend.idframe <- function(object,newdata=NULL,...){
  
  if(is.null(newdata)){
    data <- fitted(object)
  } else{
     data <- newdata
     out <- detrend.predict(object$output.trend,data$output)
     input <- detrend.predict(object$input.trend,data$input)
     data$output <- out;data$input <- input
  }
  return(data)
}

detrend.predict <- function(object,data){
  pred_list <- lapply(X=object,FUN=predict,newdata=data)
  pred <- data.frame(matrix(unlist(pred_list),ncol=ncol(data),byrow=T))
  colnames(pred) <- colnames(data)
  return(pred)
}

trend.fit <- function(x){
  fit_self <- lm(x~time(x))
  fit_self
}