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
#' @details
#' SISO ARX models are of the form 
#' \deqn{
#'    y[k] + a_1 y[k-1] + \ldots + a_{na} y[k-na] = b_{nk} u[k-nk] + 
#'    \ldots + b_{nk+nb} u[k-nk-nb] + e[k] 
#' }
#' The function estimates the coefficients using linear least squares (with
#' regularization).
#' \cr
#' The data is expected to have no offsets or trends. They can be removed 
#' using the \code{\link{detrend}} function. 
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
#' mod_iv <- iv(z,c(2,1,1))
#' 
#' @seealso arx
#' 
#' @export
iv <- function(z,order=c(0,1,0),x=NULL){
  y <- outputData(z); u <- inputData(z); N <- dim(y)[1]
  na <- order[1];nb <- order[2]; nk <- order[3]
  
  if(is.null(x)){
    # Initial Guess using ARX
    mod_arx <- arx(z,order)
    x <- matrix(sim(mod_arx$sys,u))
  }
  
  ivcompute(y,u,x,na,nb,nk,n,N,z$unit)
}

ivcompute <- function(y,u,x,na,nb,nk,n,N,unit){
  nb1 <- nb+nk-1 ; n <- max(na,nb1); df <- N-na-nb
  padZeros <- function(x,n) c(rep(0,n),x,rep(0,n))
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
  
  # Estimator
  lhs <- t(psi)%*%phi; lhsinv <- solve(lhs)
  theta <- lhsinv%*%t(psi)%*%Y
  
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

#' @export
iv4 <- function(z,order=c(0,1,0)){
  y <- outputData(z); u <- inputData(z); N <- dim(y)[1]
  na <- order[1];nb <- order[2]; nk <- order[3]
  nb1 <- nb+nk-1 ; n <- max(na,nb1); df <- N-na-nb
  
  # Steps 1-2
  mod_iv1 <- iv(z,order)
  A <- signal::Ma(mod_iv1$sys$A)
  B <- signal::Ma(c(rep(0,nk),mod_iv1$sys$B))
  
  # Step 3 (AR Modeling)
  w <- matrix(as.numeric(signal::filter(A,y)) - 
                as.numeric(signal::filter(B,u)))
  mod_ar <- ar(w,aic = F,order=na+nb)
  Lhat <- signal::Ma(c(1,-mod_ar$ar))
  
  # Step 4
  G2 <- signal::Arma(as.numeric(B),as.numeric(A))
  x2 <- matrix(sim(mod_iv1$sys,u))
  
  Lf <- function(x,L) matrix(as.numeric(signal::filter(L,x)))
  filtered <- lapply(list(y,u,x2),Lf,L=Lhat)
  yf <- filtered[[1]]; uf<- filtered[[2]]; xf <- filtered[[3]]
  
  ivcompute(yf,uf,xf,na,nb,nk,n,N,z$unit)
}