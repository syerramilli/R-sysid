#' Estimate Impulse Response Models
#' 
#' \code{impulseest} is used to estimate impulse response models in the
#' given data
#' 
#' @param data an object of class \code{idframe}
#' @param M Order of the FIR Model (Default:\code{30})
#' @param K Transport delay in the estimated impulse response 
#' (Default:\code{0})
#' @param regul Parameter indicating whether regularization should be 
#' used. (Default:\code{FALSE})
#' @param lambda The value of the regularization parameter. Valid only if
#' \code{regul=TRUE}. (Default:\code{1})
#' 
#' @seealso \code{\link{step}}
#' 
#' @examples
#' uk <- rnorm(1000,1)
#' yk <- filter (uk,c(0.9,-0.4),method="recursive") + rnorm(1000,1)
#' data <- idframe(output=data.frame(yk),input=data.frame(uk))
#' fit <- impulseest(data)
#' plot(fit)
#' 
#' @export
impulseest <- function(data,M=30,K=0,regul=F,lambda=1){
  
  N <- dim(data$output)[1]
  ind <- (M+K+1):N
  
  z_reg <- function(i) data$input[(i-K):(i-M-K),]
  Z <- t(sapply(ind,z_reg))
  Y <- data$output[ind,]
  
  # Dealing with Regularization
  if(regul==F){
    lambda = 0
  }
  
  # Fit Linear Model and find standard errors
  fit <- lm(Y~Z-1)
  df <- nrow(Z)-ncol(Z);sigma2 <- sum(resid(fit)^2)/df
  vcov <- sigma2 * solve(t(Z)%*%Z)
  se <- sqrt(diag(vcov))
  
  
  out <- list(coefficients=coef(fit),residuals=resid(fit),lags=K:(M+K),
              x=colnames(data$input),y=colnames(data$output),se = se)
  class(out) <- "impulseest"
  return(out)
}

#' Impulse Response Plots
#' 
#' Plots the estimated IR Coefficients
#' 
#' @param model an object of class \code{impulseest}
#' @param sig Significance Limits (Default: \code{0.975})
#' 
#' @seealso \code{\link{impulseest}},\code{\link{step}}
#' @export
plot.impulseest <- function(model,sig=0.975){
  lim <- model$se*qnorm(0.975)
  
  ylim <- c(min(coef(model)),max(coef(model)))
  
  title <- paste("Impulse Response \n From",model$x,"to",model$y)
  plot(model$lags,coef(model),type="h",xlab="Lag",ylab= model$y,
       main = title)
  abline(h=0);points(x=model$lags,y=lim,col="blue",lty=2,type="l")
  points(x=model$lags,y=-lim,col="blue",lty=2,type="l")
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
  plot(model$lags,stepResp,type="s",xlab="Lag",ylab= model$y,
       main = title)
  abline(h=0)
}

#' Estimate frequency response with fixed frequency resolution using 
#' spectral analysis
#' 
spa <- function(data,WinSize=NULL){
  
}

#' Estimate empirical transfer function
#' 
etfe <- function(data){
  
}
