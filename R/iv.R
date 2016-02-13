#' @export
iv <- function(z,order=c(0,1,0)){
  y <- outputData(z); u <- inputData(z); N <- dim(y)[1]
  na <- order[1];nb <- order[2]; nk <- order[3]
  nb1 <- nb+nk-1 ; n <- max(na,nb1); df <- N-na-nb
  
  # Initial Guess using ARX
  mod_arx <- arx(z,order)
  x <- matrix(sim(mod_arx$sys,u,sigma=0))
  
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
  
  model <- idpoly(A = c(1,theta[1:na]),B = theta[na+1:nb],
                  ioDelay = nk,Ts=deltat(z))
  
  estpoly(sys = model,
          stats=list(vcov = vcov, sigma = sqrt(sigma2),df = df),
          fitted.values=ypred,residuals=e,call=match.call(),input=u)
}