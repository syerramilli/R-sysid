#' Simulate dynamic system
#' 
#' Simulate the response of a system given the input
#' 
#' @param model the system model to simulate
#' @param input a vector/matrix containing the input
#' @param sigma standard deviation of the innovations (Default= \code{0})
#' @param seed integer indicating the seed value of the random number generator
#' 
#' @return
#' a vector containing the output
#' 
#' @details
#' The routine is currently built only for SISO systems. Future Versions will
#' include support for MIMO systems. Current support 
#' 
#' @seealso 
#' \code{\link{sim.idpoly}} for simulating polynomial models
#' 
#' @export
sim <- function(model,input,sigma=0,seed=NULL) UseMethod("sim")

#' @export
sim.default <- function(model,input,sigma=0,seed=NULL){
  print("The sim method is not developed for the current class of the object")
}

#' Simulate from a Polynomial Model
#' 
#' Simulate the response of a system governed by a polynomial model
#' , given the input
#' 
#' @param model an object of class \code{idpoly} containing the coefficients
#' @param input a vector/matrix containing the input
#' @param sigma standard deviation of the innovations (Default= \code{0})
#' @param seed integer indicating the seed value of the random number generator
#' 
#' @return
#' a vector containing the output
#' 
#' @details
#' The routine is currently built only for SISO systems. Future Versions will
#' include support for MIMO systems
#' 
#' @seealso 
#' \code{\link{idpoly}} for defining polynomial models
#' 
#' @examples
#' # ARX Model
#' u <- rnorm(200,sd=1)
#' model <- idpoly(A=c(1,-1.5,0.7),B=c(0.8,-0.25),ioDelay=1)
#' y <- sim(model,u,sigma=0.1)
#' 
#' @export
sim.idpoly <- function(model,input,sigma=0,seed=NULL){
  if(model$type=="arx"){
    sim_arx(model,input,sigma,seed)
  } else{
    require(signal);require(polynom)
    
    n <- length(input)[1]
    if(!is.null(seed)) set.seed(seed)
    ek <- rnorm(n,sd=sigma)
    den1 <- as.numeric(polynomial(model$A)*polynomial(model$D))
    filt1 <- Arma(b=model$C,a=den1)
    vk <- signal::filter(filt1,ek)
    
    B <- c(rep(0,model$ioDelay),model$B)
    den2 <- as.numeric(polynomial(model$A)*polynomial(model$F1))
    filt2 <- Arma(b=B,a=den2)
    ufk <- signal::filter(filt2,input)
    
    yk <- as.numeric(ufk) + as.numeric(vk)
    
    return(as.numeric(yk)) 
  }
}

sim_arx <- function(model,input,sigma=0,seed=NULL){
  na <- length(model$A) - 1; nk <- model$ioDelay; 
  nb <- length(model$B) - 1; nb1 <- nb+nk
  n <- max(na,nb1)
  coef <- matrix(c(model$A[-1],model$B),nrow=na+(nb+1))
  
  if(class(input)=="idframe"){
    uk <- input$input[,1,drop=T]
  } else if(class(input) %in% c("matrix","data.frame")){
    uk <- input[,1,drop=T]
  } else if(is.numeric(input)){
    uk <- input
  }
  
  y <- rep(0,length(input)+n)
  u <- c(rep(0,n),uk)
  
  if(!is.null(seed)) set.seed(seed)
  
  ek <- rnorm(length(uk),sd=sigma)
  # padLeftZeros <- function(x) c(rep(0,n),x)
  # u <- apply(input,2,padLeftZeros)
  
  for(i in n+1:length(uk)){
    if(nk==0) v <- u[i-0:nb] else v <- u[i-nk:nb1]
    reg <- matrix(c(-(y[i-1:na]),v),ncol=na+(nb+1))
    y[i] <- reg%*%coef + ek[i-n]
  }
  return(y[n+1:length(uk)])
}