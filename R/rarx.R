#' @export
rarx <- function(x,order=c(1,1,1),lambda=0.95){
  y <- outputData(x); u <- inputData(x)
  N <- dim(y)[1]
  na <- order[1];nb <- order[2]; nk <- order[3]
  nb1 <- nb+nk-1 ; n <- max(na,nb1); df <- N-na-nb
  
  yout <- apply(y,2,padZeros,n=n)
  uout <- apply(u,2,padZeros,n=n)
  
  uindex <- nk:nb1
  if(na!=0) yindex <- 1:na
  
  reg <- function(i) {
    # regressor
    temp <- numeric(0)
    if(na!=0) temp <- c(temp,-yout[i-yindex,])
    phi <- c(temp,uout[i-uindex,])
    phi
  }
  
  # R0 <- reg(n+1)%*%t(reg(n+1))
  # Plast <- solve(R0)
  Plast <- 10^4*diag(na+nb)
  theta <- matrix(0,N+1,na+nb)
  yhat <- y
  
  for(i in 1:N){
    temp <- reg(n+i)
    yhat[i,] <- t(temp)%*%t(theta[i,,drop=FALSE])
    eps_i <- y[i,,drop=FALSE] - yhat[i,,drop=FALSE]
    kappa_i <- Plast%*%temp/(lambda+t(temp)%*%Plast%*%temp)[1]
    theta[i+1,] <- t(t(theta[i,,drop=F])+eps_i[1]*kappa_i)
    Plast <- (diag(na+nb)-kappa_i%*%t(temp))%*%Plast/lambda
  }
  list(theta=theta,yhat=yhat,P=Plast)
}