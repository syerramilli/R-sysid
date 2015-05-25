#' Estimate Impulse Response Models
#' 
#' \code{impulseest} is used to estimate impulse response models in the
#' given data
#' 
#' @param data an object of class \code{idframe}
#' @param lags The number of lags upto which the estimate is to be
#' calculated. (Default:\code{30})
#' @param delay The transport delay 
#' 
#' @seealso \code{\link{plot.impulseest}}, \code{\link{step}}
#' @export
impulseest <- function(data,lags=30){
  
  N <- dim(data$output)[1]
  ind <- (lags+1):N
  
  z_reg <- function(i) data$input[i:(i-lags),]
  Z <- t(sapply(ind,z_reg))
  Y <- data$output[ind,]
  
  fit <- lm(Y~Z-1)
  
  out <- list(coefficients=coef(fit),residuals=resid(fit),
              lags=0:lags)
  class(out) <- "impulseest"
  return(out)
}

#' Impulse Response Plots
#' 
#' Plots the estimated IR Coefficients
#' 
#' @param model an object of class \code{impulseest}
#' 
#' @seealso \code{\link{impulseest}},\code{\link{step}}
#' @export
plot.impulseest <- function(model){
  plot(model$lags,coef(model),type="h");abline(h=0)
}


#' Step Response Plots
#' 
#' Plots the step response of a system, given the IR model
#' 
#' @param model an object of class \code{impulseest}
#' 
#' @seealso \code{\link{impulseest}},\code{\link{impulse}}
#' @export 
step <- function(model){
  
}
