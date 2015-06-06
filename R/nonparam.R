#' Estimate Impulse Response Coefficients
#' 
#' \code{impulseest} is used to estimate impulse response coefficients from 
#' the data
#' 
#' @param data an object of class \code{idframe}
#' @param M Order of the FIR Model (Default:\code{30})
#' @param K Transport delay in the estimated impulse response 
#' (Default:\code{0})
#' @param regul Parameter indicating whether regularization should be 
#' used. (Default:\code{FALSE})
#' @param lambda The value of the regularization parameter. Valid only if
#' \code{regul=TRUE}. (Default:\code{1})
#' 
#' @details
#' The IR Coefficients are estimated using linear least squares. Future 
#' Versions will provide support for multivariate data and regularized 
#' regression
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
impulseest <- function(data,M=30,K=0,regul=F,lambda=1){
  
  N <- dim(data$output)[1]
  ind <- (M+K+1):N
  
  z_reg <- function(i) data$input[(i-K):(i-M-K),]
  Z <- t(sapply(ind,z_reg))
  Y <- data$output[ind,]
  
  # Dealing with Regularization
  if(regul==F){
    lambda = 0
  }
  
  # Fit Linear Model and find standard errors
  fit <- lm(Y~Z-1)
  df <- nrow(Z)-ncol(Z);sigma2 <- sum(resid(fit)^2)/df
  vcov <- sigma2 * solve(t(Z)%*%Z)
  se <- sqrt(diag(vcov))
  
  
  out <- list(coefficients=coef(fit),residuals=resid(fit),lags=K:(M+K),
              x=colnames(data$input),y=colnames(data$output),se = se)
  class(out) <- "impulseest"
  return(out)
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
  lim <- model$se*qnorm(0.975)
  
  ylim <- c(min(coef(model)),max(coef(model)))
  
  title <- paste("Impulse Response \n From",model$x,"to",model$y)
  plot(model$lags,coef(model),type="h",xlab="Lag",ylab= model$y,
       main = title)
  abline(h=0);points(x=model$lags,y=lim,col="blue",lty=2,type="l")
  points(x=model$lags,y=-lim,col="blue",lty=2,type="l")
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
  title <- paste("Step Response \n From",model$x,"to",model$y)
  stepResp <- cumsum(coef(model))
  plot(model$lags,stepResp,type="s",xlab="Lag",ylab= model$y,
       main = title)
  abline(h=0)
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
#' @export
spa <- function(data,npad=255){
  require(sapa)
  temp <- cbind(data$output,data$input)
  
  # Non-parametric Estimation of Spectral Densities - 
  # WOSA and Hanning window
  gamma <- SDF(temp,method="wosa",sampling.interval = data$Ts,npad=npad)
  freq <- seq(from=1,to=ceiling(npad/2),by=1)/ceiling(npad/2)*pi/data$Ts
  out <- idfrd(response = gamma[,2]/gamma[,3],freq=freq,Ts= data$Ts)
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
  temp <- cbind(as.ts(data$output),as.ts(data$input))
  tempfft <- mvfft(temp)/dim(temp)[1]
  freq <- seq(from=1,to=ceiling(dim(tempfft)[1]/2),
              by=1)/ceiling(dim(tempfft)[1]/2)*pi/data$Ts
  resp <- as.complex(tempfft[,1]/tempfft[,2])
  out <- idfrd(response=resp[1:ceiling(length(resp)/2)],freq=freq,
               Ts=data$Ts)
  return(out)
}