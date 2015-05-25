#' Estimate Impulse Response Models
#' 
#' \code{impulseest} is used to estimate impulse response models in the
#' given data
#' 
#' @param data an object of class \code{idframe}
#' @param lags The number of lags upto which the estimate is to be
#' calculated. (Default:\code{30})
#' @param conf The confidence interval 
#' 
#' @seealso \code{\link{impulse}}, \code{\link{step}}
#' @export
impulseest <- function(data,lags=30){
 
  z_reg <- function(i) data$input[i:(i-lags)]
  Z <- t(sapply())
}

#' Impulse Response Plots
#' 
#' Plots the estimated IR Coefficients
#' 
#' @param model an object of class \code{impulseest}
#' 
#' @seealso \code{\link{impulseest}},\code{\link{step}}
#' @export
impulse <- function(model){
  
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
