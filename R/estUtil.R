levbmqdt <- function(y,u,n,reg,theta0,opt=optimOptions()){
  
  # Optimization Parameters
  tol <- opt$tol; maxIter <- opt$maxIter
  d <- opt$adv$LMinit; mu <- opt$adv$LMstep
  
  N <- dim(y,1)
  
  # Initialize Algorithm
  i <- 0
  eout <- matrix(rep(0,N+2*n))
  X <- t(sapply(n+1:(N+n),reg))
  Y <- yout[n+1:(N+n),,drop=F]
  e <- Y-X%*%theta0
  sumsq0 <- sum(e^2)
  # parameter indicating whether to update gradient in the iteration
  update <- 1 
  
  repeat{
    if(update ==1){
      i=i+1
      # Update gradient
      eout <- matrix(c(rep(0,n),e[,]))
      X <- t(sapply(n+1:(N+n),reg))
      filt1 <- Arma(b=1,a=c(1,theta0[(na+nb+1:nc)]))
      grad <- apply(X,2,filter,filt=filt1) 
    }
    
    # Update Parameters
    H <- 1/N*(t(grad)%*%grad) + d*diag(na+nb+nc)
    Hinv <- solve(H)
    theta <- theta0 + 1/N*Hinv%*%t(grad)%*%e
    
    # Update residuals
    e <- Y-X%*%theta
    sumsq <- sum(e^2)
    
    # If sum square error with the updated parameters is less than the 
    # previous one, the updated parameters become the current parameters
    # and the damping factor is reduced by a factor of 10
    if(sumsq<sumsq0){
      d <- d/mu
      theta0 <- theta
      sumsq0 <- sumsq
      update <- 1
    } else{ # increase damping factor
      d <- d*v
      update <- 0
    }
    
    sumSqRatio <- abs(sumsq0-sumsq)/sumsq0*100
    #     print(sumsq);print(sumSqRatio)
    
    if(sumSqRatio < tol || i > maxIter)
      break
  }
  
}

#' @export
optimOptions <- function(tol=0.01,maxIter=50,LMinit=0.1,LMstep=2){
  return(list(tol=tol,maxIter= maxIter, adv= list(LMinit=LMinit,
                                                  LMstep=LMstep)))
}