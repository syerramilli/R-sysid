# Implementation of the Levenberg Marquardt Algorithm
levbmqdt <- function(...,obj,theta0,N,opt){
  dots <- list(...)
  
  # Optimization Parameters
  tol <- opt$tol; maxIter <- opt$maxIter
  d <- opt$adv$LMinit; mu <- opt$adv$LMstep
  
  df <- N - dim(theta0)[1]
  
  # Initialize Algorithm
  i <- 0
  l <- obj(theta=theta0,e=NULL,dots)
  e <- l$Y-l$X%*%theta0
  sumsq0 <- sum(e^2)
  # parameter indicating whether to update gradient in the iteration
  update <- 1 
  # variable to count the number of times objective function is called
  countObj <- 0
  
  repeat{
    i=i+1
    if(update ==1){
      countObj <- countObj+1
      # Update gradient
      l <- obj(theta0,e,dots)
    }
    
    # Update Parameters
    H <- t(l$grad)%*%l$grad + d*diag(dim(theta0)[1])
    Hinv <- solve(H)
    theta <- theta0 + Hinv%*%t(l$grad)%*%e
    
    # Update residuals
    e <- l$Y-l$X%*%theta
    sumsq <- sum(e^2)
    sumSqRatio <- (sumsq0-sumsq)/sumsq0
    
    # If sum square error with the updated parameters is less than the 
    # previous one, the updated parameters become the current parameters
    # and the damping coefficient is reduced by a factor of mu
    if(sumSqRatio > 0){
      d <- d/mu
      theta0 <- theta
      sumsq0 <- sumsq
      update <- 1
    } else{ # increase damping coefficient by a factor of mu
      d <- d*mu
      update <- 0
    }
    
    if((abs(sumSqRatio) < tol) || (i == maxIter)){
      break
      
    }
  }
  
  if(abs(sumSqRatio) < tol){
    WhyStop <- "Tolerance"
  } else{
    WhyStop <- "Maximum Iteration Limit"
  }
  
  e <- e[1:N,]
  sigma2 <- sum(e^2)/df
  vcov <- Hinv
  
  list(params=theta,residuals=e,vcov=vcov,sigma = sqrt(sigma2),
       termination=list(WhyStop=WhyStop,iter=i,FcnCount = countObj))
}

#' Create optimization options
#' 
#' Specify optimization options that are to be passed to the 
#' numerical estimation routines
#' 
#' @param tol Minimum ratio of the improvement to the current loss 
#' function. Iterations stop if this ratio goes below the tolerance
#' limit (Default: \code{1e-5})
#' @param maxIter Maximum number of iterations to be performed
#' @param LMinit Starting value of search-direction length 
#' in the Levenberg-Marquardt method.
#' @param LMstep Size of the Levenberg-Marquardt step
#' 
#' @export
optimOptions <- function(tol=1e-5,maxIter=20,LMinit=100,LMstep=8){
  return(list(tol=tol,maxIter= maxIter, adv= list(LMinit=LMinit,
                                                  LMstep=LMstep)))
}

#' Parameter covariance of the identified model
#' 
#' Obtain the parameter covariance matrix of the linear, identified 
#' parametric model
#' 
#' @param sys a linear, identified parametric model
#' 
#' @export
getcov <- function(sys){
  sys$stats$vcov
}

armaxGrad <- function(theta,e,dots){
  y <- dots[[1]]; u <- dots[[2]]; order <- dots[[3]];
  na <- order[1];nb <- order[2]; nc <- order[3]; nk <- order[4]
  nb1 <- nb+nk-1 ; n <- max(na,nb1,nc)
  
  N <- dim(y)[1]-2*n
  
  if(is.null(e)){
    eout <- matrix(rep(0,N+2*n))
  } else{
    eout <- matrix(c(rep(0,n),e[,]))
  }
  
  reg <- function(i) {
    if(nk==0) v <- i-0:(nb-1) else v <- i-nk:nb1
    matrix(c(-y[i-1:na,],u[v,],eout[i-1:nc,]))
  }
  
  X <- t(sapply(n+1:(N+n),reg))
  Y <- y[n+1:(N+n),,drop=F]
  l <- list(X=X,Y=Y)
  
  if(!is.null(e)){
    filt1 <- signal::Arma(b=1,a=c(1,theta[(na+nb+1:nc)]))
    grad <- apply(X,2,signal::filter,filt=filt1) 
    l$grad <- grad
  }
  
  return(l)
}

oeGrad <- function(theta,e,dots){
  y <- dots[[1]]; uout <- dots[[2]]; order <- dots[[3]];
  nb <- order[1];nf <- order[2]; nk <- order[3];
  nb1 <- nb+nk-1 ; n <- max(nb1,nf)
  N <- dim(y)[1]
  
  if(is.null(e)){
    iv <- dots[[4]]
  } else{
    iv <- y-e
  }
  eout <- matrix(c(rep(0,n),iv[,]))

  reg <- function(i) {
    if(nk==0) v <- i-0:(nb-1) else v <- i-nk:nb1
    matrix(c(uout[v,],-eout[i-1:nf,]))
  }
  
  X <- t(sapply(n+1:N,reg))
  l <- list(X=X,Y=y)
  
  if(!is.null(e)){
    filt1 <- signal::Arma(b=1,a=c(1,theta[nb+1:nf,]))
    grad <- apply(X,2,signal::filter,filt=filt1)
    l$grad <- grad
  }
  
  return(l)
}

bjGrad <- function(theta,e,dots){
  y <- dots[[1]]; uout <- dots[[2]]; order <- dots[[3]];
  nb <- order[1];nc <- order[2]; nd <- order[3];
  nf <- order[4]; nk <- order[5];
  nb1 <- nb+nk-1 ; n <- max(nb1,nc,nd,nf);
  N <- dim(y)[1]
  
  if(is.null(e)){
    zeta <- dots[[4]]
    w <- y-zeta
    e <- dots[[5]]
  } else{
    filt_ts <- signal::Arma(b=c(1,theta[nb+1:nc]),
                            a=c(1,theta[nb+nc+1:nd]))
    w <- signal::filter(filt_ts,e)
    zeta <- y-w
  }
  zetaout <- matrix(c(rep(0,n),zeta[,]))
  wout <- matrix(c(rep(0,n),w[,]))
  eout <- matrix(c(rep(0,n)),e[,])
  
  reg <- function(i) {
    if(nk==0) v <- i-0:(nb-1) else v <- i-nk:nb1
    matrix(c(uout[v,],eout[i-1:nc,],wout[i-1:nd,],-zetaout[i-1:nf,]))
  }
  
  X <- t(sapply(n+1:N,reg))
  l <- list(X=X,Y=y)
  
  if(!is.null(e)){
    filt1 <- signal::Arma(b=c(1,theta[nb+nc+1:nd]),
                          a=c(1,theta[nb+1:nc]))
    grad <- apply(X,2,signal::filter,filt=filt1)
    l$grad <- grad
  }
  
  return(l)
}