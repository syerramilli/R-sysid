#' @export
sim <- function(x) useMethod("sim")

#' @export
sim.arx <- function(model,input,sigma=0){
  na <- length(model$A) - 1; nk <- model$ioDelay; 
  nb <- length(model$B) - nk
  n <- max(na,nb+nk)
  
  y <- matrix(rep(0,length(input)+n),nrow=n)
  u <- matrix(c(rep(0,n),input),nrow=n)
  # padLeftZeros <- function(x) c(rep(0,n),x)
  # u <- apply(input,2,padLeftZeros)
  
  for(i in n+1:length(input)){
    reg <- cbind(-y[i-1:na,],u[i-nk:nb1,])
    y[i] <- reg%*%coef + rnorm(1,sd = sigma)
  }
  return(y[n+1:length(input),,drop=F])
}