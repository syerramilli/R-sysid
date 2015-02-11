#' Remove linear trends
#' 
#' Removes the mean value or (piecewise) linear function from the 
#' input and output matrices
#'  
#' @export
detrend.idframe <- function(data,tt="linear",bp=c()){
  require(pracma)
  
  output0 <- as.data.frame(detrend(as.matrix(data$output),tt=tt,bp=bp))
  input0 <- as.data.frame(detrend(as.matrix(data$input),tt=tt,bp=bp))
  
  data0 <- data; data0$output <- output0; data0$input <- input0
  output_d <- data$output - output0
  input_d <- data$input - input0
  
  est <- list(fitted.values= data0,out.diff = output_d,inp.diff=input_d,
              raw.values = data)
  
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
     data$output <- newdata$output - object$out.diff
     data$input <- newdata$input - object$inp.diff
  }
  return(data)
}