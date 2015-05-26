#' Estimate Impulse Response Models
#' 
#' \code{impulseest} is used to estimate impulse response models in the
#' given data
#' 
#' @param data an object of class \code{idframe}
#' @param M Order of the FIR Model (Default:\code{30})
#' @param K Transport delay in the estimated impulse response 
#' (Default:\code{0})
#' 
#' @seealso \code{\link{plot.impulseest}}, \code{\link{step}}
#' @export
impulseest <- function(data,M=30,K=0){
  
  N <- dim(data$output)[1]
  ind <- (M+K+1):N
  
  z_reg <- function(i) data$input[(i-K):(i-M-K),]
  Z <- t(sapply(ind,z_reg))
  Y <- data$output[ind,]
  
  # Fit Linear Model and find standard errors
  fit <- lm(Y~Z-1)
  #df <- nrow(Z)-ncol(Z);sigma2 <- resid(fit)^2/df
  #vcov <- sigma2 * inv(t(Z)*Z)
  #se <- sqrt(diag(vcov))
  
  
  out <- list(coefficients=coef(fit),residuals=resid(fit),lags=K:(M+K),
              x=colnames(data$input),y=colnames(data$output))
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
  title <- paste("Impulse Response \n From",model$x,"to",model$y)
  plot(model$lags,coef(model),type="h",xlab="Lag",ylab= model$y,
       main = title)
  abline(h=0)
}


#' Step Response Plots
#' 
#' Plots the step response of a system, given the IR model
#' 
#' @param model an object of class \code{impulseest}
#' 
#' @seealso \code{\link{impulseest}}
#' @export 
step <- function(model){
  title <- paste("Step Response \n From",model$x,"to",model$y)
  stepResp <- cumsum(coef(model))
  plot(model$lags,stepResp,type="h",xlab="Lag",ylab= model$y,
       main = title)
  abline(h=0)
}
