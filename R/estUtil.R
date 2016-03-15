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
  e <- l$e; grad <- l$grad
  sumsq0 <- sum(l$fn^2)/df
  
  # variable to count the number of times objective function is called
  countObj <- 0
  sumSqDiff <- 9E-3*sumsq0
  
  repeat{
    i=i+1
    
    g <- 1/df*t(grad)%*%e
    termPar <- norm(g,"2")
    
    repeat{
      # Update Parameters
      H <- 1/df*t(grad)%*%grad + d*diag(dim(theta0)[1])
      Hinv <- solve(H);
      
      theta <- theta0 + Hinv%*%g
      
      # Evaulate sum square error
      l <- obj(theta,e,dots)
      sumsq <- sum(l$fn^2)/df
      sumSqDiff <- sumsq0-sumsq
      countObj <- countObj + 1
      
      if(termPar < tol) break
      # no major improvement
      if(abs(sumSqDiff) < 0.01*sumsq0) break
      
      # If sum square error with the updated parameters is less than the 
      # previous one, the updated parameters become the current parameters
      # and the damping coefficient is reduced by a factor of mu
      if(sumSqDiff > 0){
        d <- d/mu
        theta0 <- theta
        sumsq0 <- sumsq
        e <- l$fn; grad <- l$grad
        break
      } else{ # increase damping coefficient by a factor of mu
        d <- d*mu
      } 
    }
    if(termPar < tol) {
      WhyStop <- "Tolerance"
      break
    }
    if(abs(sumSqDiff) < 0.01*sumsq0){
      WhyStop <- "No significant change"
      break
    } 
    if(i == maxIter){
      WhyStop <- "Maximum Iteration Limit"
      break
    } 
  }
  # theta <- theta0
  e <- l$fn
  sigma2 <- sum(e^2)/df
  vcov <- 1/df*Hinv*sigma2
  
  list(params=theta,residuals=e,vcov=vcov,sigma = sqrt(sigma2),
       termination=list(WhyStop=WhyStop,iter=i,FcnCount = countObj,
                        CostFcn=sumsq0))
}

#' Create optimization options
#' 
#' Specify optimization options that are to be passed to the 
#' numerical estimation routines
#' 
#' @param tol Minimum 2-norm of the gradient (Default: \code{1e-2})
#' @param maxIter Maximum number of iterations to be performed
#' @param LMinit Starting value of search-direction length 
#' in the Levenberg-Marquardt method (Default: \code{0.01})
#' @param LMstep Size of the Levenberg-Marquardt step (Default: \code{2})
#' @param display Argument whether to display iteration details or not
#' (Default: \code{"off"})
#' 
#' @export
optimOptions <- function(tol=1e-2,maxIter=20,LMinit=0.01,LMstep=2,
                         display=c("off","on")[1]){
  return(list(tol=tol,maxIter= maxIter, 
              adv= list(LMinit=LMinit,LMstep=LMstep),display=display))
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
  nb1 <- nb+nk-1 ; n <- max(na,nb1,nc);N <- dim(y)[1]
  
  l <- list()
  if(is.null(e)){
    e <- dots[[4]]; l$e <- e
  } 
  
  yout <- apply(y,2,padZeros,n=n)
  uout <- apply(u,2,padZeros,n=n)
  eout <- apply(e,2,padZeros,n=n)
  
  reg <- function(i) {
    if(nk==0) v <- i-0:(nb-1) else v <- i-nk:nb1
    matrix(c(-yout[i-1:na,],uout[v,],eout[i-1:nc,]))
  }
  
  X <- t(sapply(n+1:(N+n),reg))
  Y <- yout[n+1:(N+n),,drop=F]
  fn <- Y-X%*%theta
  
  # Compute Gradient
  filt1 <- signal::Arma(b=1,a=c(1,theta[(na+nb+1:nc)]))
  grad <- apply(X,2,signal::filter,filt=filt1) 
  
  l$grad <- grad[1:N,,drop=F];l$fn <- fn[1:N,,drop=F]
  return(l)
}

oeGrad <- function(theta,e,dots){
  y <- dots[[1]]; u <- dots[[2]]; order <- dots[[3]];
  nb <- order[1];nf <- order[2]; nk <- order[3];
  nb1 <- nb+nk-1 ; n <- max(nb1,nf)
  N <- dim(y)[1]
  l <- list()
  if(is.null(e)){
    iv <- dots[[4]]
    fn <- y-iv; l$e <- fn
  } else{
    iv <- y-e
  }
  uout <- apply(u,2,leftPadZeros,n=n)
  ivout <- apply(iv,2,leftPadZeros,n=n)
  
  reg <- function(i) {
    if(nk==0) v <- i-0:(nb-1) else v <- i-nk:nb1
    matrix(c(uout[v,],-ivout[i-1:nf,]))
  }
  # Compute new regressor matrix and residuals
  X <- t(sapply(n+1:N,reg))
  fn <- y-X%*%theta
  
  # Compute gradient
  filt1 <- signal::Arma(b=1,a=c(1,theta[nb+1:nf,]))
  grad <- apply(X,2,signal::filter,filt=filt1)
  
  l$fn <- fn; l$grad<-grad
  return(l)
}

bjGrad <- function(theta,e,dots){
  y <- dots[[1]]; u <- dots[[2]]; order <- dots[[3]];
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
    w <- matrix(signal::filter(filt_ts,e))
    zeta <- y-w
  }
  
  uout <- apply(u,2,leftPadZeros,n=n)
  zetaout <- apply(zeta,2,leftPadZeros,n=n)
  eout <- apply(e,2,leftPadZeros,n=n)
  wout <- apply(w,2,leftPadZeros,n=n)
  
  reg <- function(i) {
    if(nk==0) v <- i-0:(nb-1) else v <- i-nk:nb1
    ereg <- if(nc==0) NULL else eout[i-1:nc,]
    matrix(c(uout[v,],ereg,wout[i-1:nd,],-zetaout[i-1:nf,]))
  }
  
  # Compute new regressor matrix and residuals
  X <- t(sapply(n+1:N,reg))
  fn <- y-X%*%theta
  
  # Computing gradient
  C_params <- if(nc==0) NULL else theta[nb+1:nc] 
  den <- as.numeric(polynom::polynomial(c(1,C_params))*
                        polynom::polynomial(c(1,theta[nb+nc+nd+1:nf])))
  filt1 <- signal::Arma(b=c(1,theta[nb+nc+1:nd]),
                          a=den)
  grad <- apply(X,2,signal::filter,filt=filt1)
  
  l$fn <- fn; l$grad <- grad
  return(l)
}

checkInitSys <- function(init_sys){
  z <- strsplit(toString(sys.call(which=-1)),split = ",")[[1]][1]
  if(init_sys$type!=z){
    errMes <- paste("An idpoly model of",toupper(z),"structure expected for the",z,"command.")
    stop(errMes)
  }
}

leftPadZeros <- function(x,n) c(rep(0,n),x)
padZeros <- function(x,n) c(rep(0,n),x,rep(0,n))