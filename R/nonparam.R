#' Estimate Impulse Response Coefficients
#' 
#' \code{impulseest} is used to estimate impulse response coefficients from 
#' the data
#' 
#' @param x an object of class \code{idframe}
#' @param M Order of the FIR Model (Default:\code{30})
#' @param K Transport delay in the estimated impulse response 
#' (Default:NULL)
#' @param regul Parameter indicating whether regularization should be 
#' used. (Default:\code{FALSE})
#' @param lambda The value of the regularization parameter. Valid only if
#' \code{regul=TRUE}. (Default:\code{1})
#' 
#' @details
#' The IR Coefficients are estimated using linear least squares. Future 
#' Versions will provide support for multivariate data.
#' 
#' @references
#' Arun K. Tangirala (2015), \emph{Principles of System Identification: 
#' Theory and Practice}, CRC Press, Boca Raton. Sections 17.4.11 and 20.2
#' 
#' @seealso \code{\link{step}}
#' 
#' @examples
#' uk <- rnorm(1000,1)
#' yk <- filter (uk,c(0.9,-0.4),method="recursive") + rnorm(1000,1)
#' data <- idframe(output=data.frame(yk),input=data.frame(uk))
#' fit <- impulseest(data)
#' impulseplot(fit)
#' 
#' @export
impulseest <- function(x,M=30,K=NULL,regul=F,lambda=1){
  
  N <- dim(x$output)[1]
  if(is.null(K))
    K <- rep(0,nInputSeries(x)*nOutputSeries(x))
  
  out <- rep(list(0),length(K))
  
  for(i in seq(nOutputSeries(x))){
    for(j in seq(nInputSeries(x))){
      index <- (i-1)*nInputSeries(x)+j 
      out[[index]] <- impulsechannel(outputData(x)[,i,drop=F],
                                     inputData(x)[,j,drop=F],N,M,
                                     K[index],regul,lambda)
    }
  }
  out$ninputs <- nInputSeries(x)
  out$noutputs <- nOutputSeries(x)
  class(out) <- "impulseest"
  return(out)
}

impulsechannel <- function(y,u,N,M,K=0,regul=F,lambda=1){
  ind <- (M+K+1):N 
  z_reg <- function(i) u[(i-K):(i-M-K),]
  Z <- t(sapply(ind,z_reg))
  Y <- y[ind,]
  
  # Dealing with Regularization
  if(regul==F){
    # Fit Linear Model and find standard errors
    fit <- lm(Y~Z-1)
    coefficients <- coef(fit); residuals <- resid(fit)
  } else{
    inner <- t(Z)%*%Z + lambda*diag(dim(Z)[2])
    pinv <- solve(inner)%*% t(Z)
    coefficients <- pinv%*%Y
    residuals <- Y - Z%*%coefficients
  }
  df <- nrow(Z)-ncol(Z);sigma2 <- sum(residuals^2)/df
  vcov <- sigma2 * solve(t(Z)%*%Z)
  se <- sqrt(diag(vcov))
  
  out <- list(coefficients=coefficients,residuals=residuals,lags=K:(M+K),
              x=colnames(u),y=colnames(y),se = se)
  out
}

#' Impulse Response Plots
#' 
#' Plots the estimated IR coefficients along with the significance limits
#' at each lag. 
#' 
#' @param model an object of class \code{impulseest}
#' @param sd standard deviation of the confidence region (Default: \code{2})
#' 
#' @seealso \code{\link{impulseest}},\code{\link{step}}
#' @import ggplot2
#' 
#' @export
impulseplot <- function(model,sd=2){
  plotseq <- seq(model$noutputs*model$ninputs)
  g <- vector("list",model$nin*model$nout)
  
  for(i in plotseq){
    z <- model[[i]]
    lim <- z$se*sd
    yindex <- (i-1)%/%model$nin + 1;uindex <- i-model$nin*(yindex-1)
    df <- data.frame(x=z$lags,y=coef(z),lim=lim)
    g[[i]] <- with(df,ggplot(df,aes(x,y))+ 
      geom_segment(aes(xend=x,yend=0))+geom_hline(yintercept = 0) + 
      geom_point(size=2) + ggtitle(paste("From",z$x,"to",z$y))+
      geom_line(aes(y=lim),linetype="dashed",colour="steelblue") +
      geom_line(aes(y=-lim),linetype="dashed",colour="steelblue") +
      ggplot2::theme_bw(14) + ylab(ifelse(uindex==1,"IR Coefficients","")) +
      xlab(ifelse(yindex==model$nout,"Lags","")) + 
      theme(axis.title=element_text(size=12,color = "black",face = "plain"),
            title=element_text(size=9,color = "black",face="bold")) +
      scale_x_continuous(expand = c(0.01,0.01)))
  }
  multiplot(plotlist=g,layout=plotseq)
}


#' Step Response Plots
#' 
#' Plots the step response of a system, given the IR model
#' 
#' @param model an object of class \code{impulseest}
#' 
#' @seealso \code{\link{impulseest}}
#' 
#' @examples
#' uk <- rnorm(1000,1)
#' yk <- filter (uk,c(0.9,-0.4),method="recursive") + rnorm(1000,1)
#' data <- idframe(output=data.frame(yk),input=data.frame(uk))
#' fit <- impulseest(data)
#' step(fit) 
#' 
#' @import ggplot2
#' @export 
step <- function(model){
  plotseq <- seq(model$noutputs*model$ninputs)
  g <- vector("list",model$nin*model$nout)
  
  for(i in plotseq){
    z <- model[[i]]
    stepResp <- cumsum(coef(z))
    yindex <- (i-1)%/%model$nin + 1;uindex <- i-model$nin*(yindex-1)
    df <- data.frame(x=z$lags,y=stepResp)
    g[[i]] <- with(df,ggplot(df,aes(x,y))+ 
      geom_step() + ggtitle(paste("From",z$x,"to",z$y)) +
      theme_bw(14) + ylab(ifelse(uindex==1,"Step Response","")) +
      xlab(ifelse(yindex==model$nout,"Lags","")) + 
      theme(axis.title=element_text(size=12,color = "black",face = "plain"),
            title=element_text(size=9,,color = "black",face="bold")))
  }
  multiplot(plotlist=g,layout=plotseq)
}

#' Estimate frequency response 
#' 
#' Estimates frequency response and noise spectrum from data with 
#' fixed resolution using spectral analysis
#' 
#' @param x an \code{idframe} object
#' @param winsize lag size of the Hanning window (Default: \code{min
#' (length(x)/10,30)})
#' @param freq frequency points at which the response is evaluated
#' (Default: \code{seq(1,128)/128*pi/Ts})
#' 
#' @return
#' an \code{idfrd} object containing the estimated frequency response 
#' and the noise spectrum
#' 
#' @references
#' Arun K. Tangirala (2015), \emph{Principles of System Identification: 
#' Theory and Practice}, CRC Press, Boca Raton. Sections 16.5 and 20.4
#' 
#' @examples
#' data(arxsim)
#' frf <- spa(arxsim)
#' 
#' @import signal
#' @export
spa <- function(x,winsize=NULL,freq=NULL){
  N <- dim(x$out)[1]
  nout <- nOutputSeries(x); nin <- nInputSeries(x)
  
  if(is.null(winsize)) winsize <- min(N/10,30)
  if(is.null(freq)) freq <- (1:128)/128*pi/deltat(x)
  M <- winsize
  
  Ryu <- mult_ccf(x$out,x$input,lag.max = M)
  Ruu <- mult_ccf(x$input,x$input,lag.max=M)
  Ryy <- mult_ccf(x$out,x$out,lag.max = M)
  
  cov2spec <- function(omega,R,M){
    seq1 <- exp(-1i*(-M:M)*omega)
    sum(R*signal::hanning(2*M+1)*seq1)
  }
  
  G <- array(0,c(nout,nin,length(freq)))
  spec <- array(0,c(nout,1,length(freq)))
  for(i in 1:nout){
    phi_y <- sapply(freq,cov2spec,Ryy[i,i,],M)
    temp <- phi_y
    for(j in 1:nin){
      phi_yu <- sapply(freq,cov2spec,Ryu[i,j,],M)
      phi_u <- sapply(freq,cov2spec,Ruu[j,j,],M)
      G[i,j,] <- phi_yu/phi_u
      temp <- temp - phi_yu*Conj(phi_yu)/phi_u
    }
    spec[i,1,] <- temp
  }
  out <- idfrd(G,matrix(freq),deltat(x),spec)
  return(out)
}

mult_ccf <- function(X,Y=NULL,lag.max=30){
  N <- dim(X)[1]; nx <- dim(X)[2]
  ny <- ifelse(is.null(Y),nx,dim(Y)[2])
  
  ccvfij <- function(i,j,lag=30) ccf(X[,i],Y[,j],plot=F,lag.max =lag,
                                     type="covariance")
  Xindex <- matrix(sapply(1:nx,rep,nx),ncol=1)[,1]
  temp <- mapply(ccvfij,i=Xindex,j=rep(1:ny,ny),
                 MoreArgs = list(lag=lag.max))
  
  ccfextract <- function(i,l) l[,i]$acf
  temp2 <- t(sapply(1:(nx*ny),ccfextract,l=temp))
  dim(temp2) <- c(nx,ny,2*lag.max+1)
  return(temp2)
}

#' Estimate empirical transfer function
#' 
#' Estimates the emperical transfer function from the data by taking the 
#' ratio of the fourier transforms of the output and the input variables
#' 
#' @param data an object of class \code{idframe}
#' @param n frequency spacing (Default: \code{128})
#' 
#' @return
#' an \code{idfrd} object containing the estimated frequency response
#' 
#' @references
#' Arun K. Tangirala (2015), \emph{Principles of System Identification: 
#' Theory and Practice}, CRC Press, Boca Raton. Sections 5.3 and 20.4.2
#' 
#' @seealso \code{\link[stats]{fft}}
#' 
#' @examples
#' data(arxsim)
#' frf <- etfe(arxsim)
#' 
#' @export
etfe <- function(data,n=128){
  y <- data$output
  u <- data$input
  N <- dim(data$output)[1]
  if(N < n){
    n=N
  }
  v=seq(1,N,length.out = n)
  temp <- cbind(data$output[v,],data$input[v,])
  tempfft <- mvfft(temp)/dim(temp)[1]
  G <- comdiv(tempfft[,1],tempfft[,2])
  resp = G[1:ceiling(length(G)/2)]
  frequency <- matrix(seq( 1 , ceiling(n/2) ) * pi / floor(n/2) / deltat(data))
  out <- idfrd(respData = resp,freq=frequency,Ts=data$Ts)
  return(out)
}
comdiv <- function(z1,z2){
  mag1 <- Mod(z1);mag2 <- Mod(z2)
  phi1 <- Arg(z1); phi2 <- Arg(z2)
  
  complex(modulus=mag1/mag2,argument=signal::unwrap(phi1-phi2))
}