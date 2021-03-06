#' ARX model estimation using instrumental variable method
#' 
#' Estimates an ARX model of the specified order from input-output data using
#' the instrument variable method. If arbitrary instruments are not supplied 
#' by the user, the instruments are generated using the arx routine
#' 
#' @param z an idframe object containing the data
#' @param order Specification of the orders: the three integer components 
#' (na,nb,nk) are the order of polynolnomial A, (order of polynomial B + 1) 
#' and the input-output delay
#' @param x instrument variable matrix. x must be of the same size as the output 
#' data. (Default: \code{NULL})
#' 
#' @return
#' An object of class \code{estpoly} containing the following elements:
#'  \item{sys}{an \code{idpoly} object containing the 
#'    fitted ARX coefficients}
#'  \item{fitted.values}{the predicted response}
#'  \item{residuals}{the residuals}
#'  \item{input}{the input data used}
#'  \item{call}{the matched call}
#'  \item{stats}{A list containing the following fields: \cr
#'      \code{vcov} - the covariance matrix of the fitted coefficients \cr
#'      \code{sigma} - the standard deviation of the innovations\cr
#'      \code{df} - the residual degrees of freedom}
#' 
#' @references
#' Arun K. Tangirala (2015), \emph{Principles of System Identification: 
#' Theory and Practice}, CRC Press, Boca Raton. Sections 21.7.1, 21.7.2
#' 
#' Lennart Ljung (1999), \emph{System Identification: Theory for the User}, 
#' 2nd Edition, Prentice Hall, New York. Section 7.6
#' 
#' @examples
#' data(arxsim)
#' mod_iv <- iv(arxsim,c(2,1,1))
#' 
#' @seealso \code{\link{arx}}, \code{\link{iv4}}
#' 
#' @export
iv <- function(z,order=c(0,1,0),x=NULL){
  if(is.null(x)){
    # Initial Guess using ARX
    mod_arx <- arx(z,order)
    x <- matrix(sim(mod_arx$sys,inputData(z)))
  }
  
  ivcompute(z,x,order)
}

#' @import signal
ivcompute <- function(z,x,order,filt=NULL){
  y <- outputData(z); u <- inputData(z); N <- dim(y)[1]
  na <- order[1];nb <- order[2]; nk <- order[3]
  
  nb1 <- nb+nk-1 ; n <- max(na,nb1); df <- N-na-nb
  yout <- apply(y,2,padZeros,n=n);
  xout <- apply(x,2,padZeros,n=n);
  uout <- apply(u,2,padZeros,n=n);
  
  # Regressors
  reg <- function(i) {
    if(nk==0) v <- i-0:(nb-1) else v <- i-nk:nb1
    c(-yout[i-1:na,,drop=T],uout[v,,drop=T])
  }
  phi <- t(sapply(n+1:(N+n),reg))
  Y <- yout[n+1:(N+n),,drop=F]
  
  # Generating IVs
  ivx <- function(i) {
    if(nk==0) v <- i-0:(nb-1) else v <- i-nk:nb1
    c(-xout[i-1:na,,drop=T],uout[v,,drop=T])
  }
  psi <- t(sapply(n+1:(N+n),ivx))
  
  l <- list(phi,psi,Y)
  if(!is.null(filt)){
    appfilt <- function(x) apply(x,2,signal::filter,filt=filt)
    l <- lapply(l, appfilt)
  }
  phif <- l[[1]]; psif <- l[[2]]; Yf <- l[[3]]
  
  # Estimator
  lhs <- t(psif)%*%phif; lhsinv <- solve(lhs)
  theta <- lhsinv%*%t(psif)%*%Yf
  
  # Residuals
  ypred <- (phi%*%theta)[1:N,,drop=F] 
  e <- y-ypred
  sigma2 <- norm(e,"2")^2/df
  vcov <- sigma2*solve(t(phi)%*%phi)
  
  model <- idpoly(A = c(1,theta[1:na]),B = theta[na+1:nb],ioDelay = nk,
                  Ts=deltat(z),noiseVar = sqrt(sigma2),unit=unit)
  
  estpoly(sys = model,
          stats=list(vcov = vcov, sigma = sqrt(sigma2),df = df),
          fitted.values=ypred,residuals=e,call=match.call(),input=u)
}

#' ARX model estimation using four-stage instrumental variable method
#' 
#' Estimates an ARX model of the specified order from input-output data using
#' the instrument variable method. The estimation algorithm is insensitive to 
#' the color of the noise term.
#' 
#' @param z an idframe object containing the data
#' @param order Specification of the orders: the three integer components 
#' (na,nb,nk) are the order of polynolnomial A, (order of polynomial B + 1) 
#' and the input-output delay
#' 
#' @details 
#' Estimation is performed in 4 stages. The first stage uses the arx function. The resulting model generates the 
#' instruments for a second-stage IV estimate. The residuals obtained from this model are modeled using a sufficently
#' high-order AR model. At the fourth stage, the input-output data is filtered through this AR model and then subjected 
#' to the IV function with the same instrument filters as in the second stage.
#' 
#' @return
#' An object of class \code{estpoly} containing the following elements:
#'  \item{sys}{an \code{idpoly} object containing the 
#'    fitted ARX coefficients}
#'  \item{fitted.values}{the predicted response}
#'  \item{residuals}{the residuals}
#'  \item{input}{the input data used}
#'  \item{call}{the matched call}
#'  \item{stats}{A list containing the following fields: \cr
#'      \code{vcov} - the covariance matrix of the fitted coefficients \cr
#'      \code{sigma} - the standard deviation of the innovations\cr
#'      \code{df} - the residual degrees of freedom}
#' 
#' @references
#' Lennart Ljung (1999), \emph{System Identification: Theory for the User}, 
#' 2nd Edition, Prentice Hall, New York. Section 15.3
#' 
#' @examples
#' mod_dgp <- idpoly(A=c(1,-0.5),B=c(0.6,-.2),C=c(1,0.6),ioDelay = 2,noiseVar = 0.1)
#' u <- idinput(400,"prbs")
#' y <- sim(mod_dgp,u,addNoise=TRUE)
#' z <- idframe(y,u)
#' mod_iv4 <- iv4(z,c(1,2,2))
#' 
#' @seealso \code{\link{arx}}, \code{\link{iv4}}
#' @export
iv4 <- function(z,order=c(0,1,0)){
  na <- order[1]; nb <- order[2]
  # Steps 1-2
  mod_iv <- iv(z,order)
  
  # Step 3
  w <- resid(mod_iv)
  mod_ar <- ar(w,aic = F,order.max =na+nb)
  Lhat <- signal::Arma(b=c(1,-mod_ar$ar),a=1)
  
  # Step 4
  x2 <- matrix(sim(mod_iv$sys,inputData(z)))
  ivcompute(z,x2,order,Lhat)
}