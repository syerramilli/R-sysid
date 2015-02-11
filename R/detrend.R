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
  
  est <- list(fitted.values= data0,out.diff = output_d,inp.diff=input_d)
  
  class(est) <- "detrend.idframe"
  return(est)
}