#' Estimate Impulse Response Models
#' 
#' \code{impulseest} is used to estimate impulse response models given data
#' 
#' @param data an object of class \code{idframe}
#' @param lags The number of lags upto which the estimate is to be
#' calculated. (Default:\code{30})
#' @param conf The confidence interval 
#' 
#' @details
#' 
#' This function extends the \code{\link[vars]{irf}} function, which estimates the 
#' impulse response coefficients given a VAR model for the data. The VAR model is fit 
#' using the \code{\link[vars]{VAR}} function.
#' 
#' @return An object of class \code{impulseest} containing the following elements
#' \tabular{ll}{
#'   \code{coefs} \tab \code{list} containing the impulse response coefficients \cr
#'   \code{lower} \tab \code{list} containing the lower confidence bounds of the 
#'   impulse response coefficients \cr
#'   \code{upper} \tab \code{list} containing the upper confidence bounds of the 
#'   impulse response coefficients \cr
#' }
#' 
#' @seealso \code{\link[vars]{irf}}, \code{\link[vars]{VAR}}, \code{\link{impulse}} ,
#' \code{\link{step}}
#' @export
impulseest <- function(data,lags=30,conf=0.95){
  require(vars)
  Z <- cbind(data$output,data$input)
  
  fit.var <- VAR(Z,p=10)
  ir <- irf(fit.var,impulse=colnames(data$input),response=colnames(data$output),
            n.ahead = lags,ci=conf)
  
  out <- ir
  class(out) <- "impulseest"
  return(out)
}

# Impulse Response Plots
impulse <- function(model){
  
}


# Step Response Plots
step <- function(model){
  
}
