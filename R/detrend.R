#' Remove linear trends
#' 
#' Removes the linear function from the input and output matrices. 
#' 
#' @param data an object of class \code{idframe}
#' 
#' @examples
#' data(cstr)
#' fit <- detrend(cstr)
#' cstr_detrend <- predict(fit)
#' 
#' ## Examples for train and test sets
#' data(cstr)
#' splitList <- dataPartition(cstr,p=0.6)
#' train <- splitList$estimation # training set 
#' test <- splitList$validation # testing set
#' fit <- detrend(trend)
#' train_detrend <- predict(fit)
#' test_detrend <- predict(fit,newdata=test)   
#' 
#' @seealso \code{\link[stats]{lm}}
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
#' 
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