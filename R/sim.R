#' @export
sim <- function(model,input,sigma=0) UseMethod("sim")

#' @export
sim.default <- function(model,input,sigma=0){
  print("The sim method is not developed for the current class of the object")
}

#' Simulate from an ARX Model
#' 
#' Simulate the response of an ARX system, given the input
#' 
#' @param model an object of class \code{arx} containing the coefficients
#' @param input a vector/matrix containing the input
#' @param sigma standard deviation of the innovations (Default= \code{0})
#' 
#' @return
#' a vector containing the output
#' 
#' @details
#' The routine is currently built only for SISO systems. Future Versions will
#' include support for MIMO systems
#' 
#' @seealso 
#' \code{\link{arx}} for defining ARX models
#' 
#' @examples
#' u <- rnorm(100,1)
#' model <- arx(A=c(1,-1.5,0.7),B=c(0.8,-0.25))
#' y <- sim(model,u,sigma=3)
#'  
#' @export
sim.arx <- function(model,input,sigma=0){
  na <- length(model$A) - 1; nk <- model$ioDelay; 
  nb <- length(model$B) - nk; nb1 <- nb+nk
  n <- max(na,nb1)
  coef <- matrix(c(model$A[-1],model$B),nrow=na+nb1) 
  
  y <- rep(0,length(input)+n)
  u <- c(rep(0,n),input)
  # padLeftZeros <- function(x) c(rep(0,n),x)
  # u <- apply(input,2,padLeftZeros)
  
  for(i in n+1:length(input)){
    reg <- matrix(c(-(y[i-1:na]),u[i-nk:nb1]),ncol=na+nb1)
    y[i] <- reg%*%coef + rnorm(1,sd = sigma)
  }
  return(y[n+1:length(input)])
}