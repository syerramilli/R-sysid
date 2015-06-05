#' Estimate ARX Models
#' 
#' @export
estARX <- function(data,order=c(0,1,0)){
  y <- as.matrix(data$output)
  u <- as.matrix(data$input); N <- dim(y)[1]
  na <- order[1];nb <- order[2]; nk <- order[3]
  nb1 <- nb+nk; n <- max(na,nb1); df <- N - na - nb -nk
  
  padZeros <- function(x,n) c(rep(0,n),x,rep(0,n))
  yout <- apply(y,2,padZeros,n=n);
  uout <- apply(u,2,padZeros,n=n);
  
  reg <- function(i) cbind(-yout[i-1:na,],uout[i-nk:nb1])
  X <- t(sapply(n+1:(N+n),reg))
  Y <- yout[n+1:(N+n),,drop=F]
  
  qx <- qr(X); coef <- qr.solve(qx,Y)
  sigma2 <- sum((Y-X%*%coef)^2)/df
  
  vcov <- sigma2 * chol2inv(qx$qr)
  
  model <- arx(A = c(1,coef[1:na]),B = coef[na+1:nb1],ioDelay = nk)
  
  est <- list(coefficients = model,vcov = vcov, sigma = sqrt(sigma2),
              df = df,fitted.values=(X%*%coef)[1:N,],
              residuals=(Y-X%*%coef)[1:N,],call=match.call())
  class(est) <- "estARX"
  est
}

#' @export
summary.estARX <- function(object)
{
  coefs <- c(coef(object)$A[-1],coef(object)$B)
  se <- sqrt(diag(object$vcov))
  tval <- coefs / se
  TAB <- cbind(Estimate = coefs,
               StdErr = se,
               t.value = tval,
               p.value = 2*pt(-abs(tval), df=object$df))
  na <- length(object$A) - 1; nk <- object$ioDelay; 
  nb <- length(object$B) - nk
  
  res <- list(call=object$call,coefficients=TAB,model=coef(object))
  class(res) <- "summary.estARX"
  res
}