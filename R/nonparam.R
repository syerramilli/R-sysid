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
#' plot(fit)
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
#' @param sig Significance Limits (Default: \code{0.975})
#' 
#' @seealso \code{\link{impulseest}},\code{\link{step}}
#' @export
plot.impulseest <- function(model,sig=0.975){
  par(mfrow=c(model$noutputs,model$ninputs))
  
  impulseplot <- function(model,sig){
    lim <- model$se*qnorm(sig)
    
    ylim <- c(min(coef(model)),max(coef(model)))
    
    title <- paste("Impulse Response \n From",model$x,"to",model$y)
    plot(model$lags,coef(model),type="h",xlab="Lag",ylab= "IR Coefficient",
         main = title)
    abline(h=0);points(x=model$lags,y=lim,col="blue",lty=2,type="l")
    points(x=model$lags,y=-lim,col="blue",lty=2,type="l")
  }
  
  l <- model[seq(model$noutputs*model$ninputs)]
  p <- lapply(l,impulseplot,sig=sig)
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
#' @export 
step <- function(model){
  par(mfrow=c(model$noutputs,model$ninputs))
  
  stepplot <- function(model){
    title <- paste("Step Response \n From",model$x,"to",model$y)
    stepResp <- cumsum(coef(model))
    plot(model$lags,stepResp,type="s",xlab="Lag",ylab= model$y,
         main = title)
    abline(h=0)
  }
  l <- model[seq(model$noutputs*model$ninputs)]
  p <- lapply(l,stepplot)
}

#' Estimate frequency response 
#' 
#' Estimates Frequency Response with fixed frequency resolution using 
#' spectral analysis
#' 
#' @param data an \code{idframe} object
#' @param npad an integer representing the total length of each time series 
#' to analyze after padding with zeros. This argument allows the user to 
#' control the spectral resolution of the SDF estimates: the normalized 
#' frequency interval is deltaf=1/npad. (Default: 255)
#' 
#' @details
#' The function calls the \code{SDF} function in the \code{sapa} package to
#' compute the cross-spectral densities. The method used is \strong{Welch's 
#' Overlapped Segment Averaging} with a normalized \strong{Hanning} window.
#' The overlap used is 50%. 
#' 
#' @return
#' an \code{idfrd} object containing the estimated frequency response
#' 
#' @references
#' Arun K. Tangirala (2015), \emph{Principles of System Identification: 
#' Theory and Practice}, CRC Press, Boca Raton. Sections 16.5 and 20.4
#' 
#' @seealso \code{\link[sapa]{SDF}}
#' 
#' @examples
#' data(frf)
#' frf <- spa(data)
#' 
#' @import sapa
#' @export
spa <- function(data,npad=255){
  temp <- cbind(data$output,data$input)
  
  # Non-parametric Estimation of Spectral Densities - 
  # WOSA and Hanning window
  gamma <- sapa::SDF(temp,method="wosa",sampling.interval = 
                       deltat(data),npad=npad)
  freq <- matrix(attributes(gamma)$frequency*2*pi)
  resp <- Conj(gamma[,2])/Mod(gamma[,3])
  
  # power-spectrum
  spec <- gamma[,2] - resp*gamma[,3]
    
  out <- idfrd(resp,freq,deltat(data),spec)
  return(out)
}

#' Estimate empirical transfer function
#' 
#' Estimates the emperical transfer function from the data by taking the 
#' ratio of the fourier transforms of the output and the input variables
#' 
#' @param data an object of class \code{idframe}
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
#' data(frf)
#' frf <- etfe(data)
#' 
#' @export
etfe <- function(data){
  temp <- cbind(data$output,data$input)
  tempfft <- mvfft(temp)/dim(temp)[1]
  freq <- seq(from=1,to=ceiling(dim(tempfft)[1]/2),
              by=1)/ceiling(dim(tempfft)[1]/2)*pi/deltat(data)
  resp <- comdiv(tempfft[,1],tempfft[,2])
  out <- idfrd(response=resp[1:ceiling(length(resp)/2)],freq=freq,
               Ts=data$Ts)
  return(out)
}

comdiv <- function(z1,z2){
  mag1 <- Mod(z1);mag2 <- Mod(z2)
  phi1 <- Arg(z1); phi2 <- Arg(z2)
  
  complex(modulus=mag1/mag2,argument=signal::unwrap(phi1-phi2))
}