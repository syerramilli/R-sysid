#' Estimate Impulse Response Models
#' 
#' \code{impulseest} is used to estimate impulse response models in the
#' given data
#' 
#' @param data an object of class \code{idframe}
#' @param M Order of the FIR Model (Default:\code{30})
#' @param K Transport delay in the estimated impulse response
#' 
#' @seealso \code{\link{plot.impulseest}}, \code{\link{step}}
#' @export
impulseest <- function(data,M=30,K){
  
  N <- dim(data$output)[1]
  ind <- (M+1):N
  
  z_reg <- function(i) data$input[i:(i-M),]
  Z <- t(sapply(ind,z_reg))
  Y <- data$output[ind,]
  
  fit <- lm(Y~Z-1)
  
  out <- list(coefficients=coef(fit),residuals=resid(fit),
              lags=0:M)
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
