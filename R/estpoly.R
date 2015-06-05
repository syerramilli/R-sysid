estARX <- function(data,order=c(0,1,0)){
  y <- as.matrix(data$output)
  u <- as.matrix(data$input); N <- dim(y)[1]
  na <- order[1];nb <- order[2]; nk <- order[3]
  nb1 <- nb+nk; n <- max(na,nb1); df <- N - n
  
  padZeros <- function(x,n) c(rep(0,n),x,rep(0,n))
  yout <- apply(y,2,padZeros,n=n);
  uout <- apply(u,2,padZeros,n=n);
  
  reg <- function(i) cbind(-yout[i-1:na,],uout[i-nk:nb1])
  X <- t(sapply(n+1:(N+n),reg))
  Y <- yout[n+1:(N+n),,drop=F]
  
  qx <- qr(X); coef <- qr.solve(qx,Y)
  sigma2 <- sum((Y-X%*%coef)^2)/df
  
  vcov <- sigma2 * chol2inv(qx$qr)
  colnames(vcov) <- rownames(vcov) <- colnames(X)
  
  list(coefficients = coef,
       vcov = vcov,
       sigma = sqrt(sigma2),
       df = df)
}