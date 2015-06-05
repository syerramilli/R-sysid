#' @export
sim <- function(model,input) UseMethod("sim")

#' @export
sim.default <- function(model,input){
  return(NULL)
}

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