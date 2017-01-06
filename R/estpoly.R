#' Estimated polynomial object
#' 
#' Estimated discrete-time polynomial model returned from an estimation
#' routine.
#' 
#' @param sys an \code{idpoly} object containing the estimated polynomial
#' coefficients
#' @param fitted.values 1-step ahead predictions on the training dataset
#' @param residuals 1-step ahead prediction errors
#' @param options optimization specification ser used (applicable for non-linear least
#' squares)
#' @param call the matched call
#' @param stats a list containing estimation statistics
#' @param termination termination criteria for optimization
#' @param input input signal of the training data-set
#' 
#' @details 
#' Do not use \code{estpoly} for directly specifing an input-output polynomial model.
#' \code{\link{idpoly}} is to be used instead
#' 
#' @export
estpoly <- function(sys,fitted.values,residuals,options=NULL,
                    call,stats,termination=NULL,input){
  out <- list(sys=sys,fitted.values=fitted.values,
              residuals=residuals,input=input,call=call,
              stats=stats,options=options,termination=termination)
  class(out) <- "estpoly"
  out
}


#' @export
print.estpoly <- function(x,...){
  print(summary(x),...)
}

#' @export
summary.estpoly <- function(object,...)
{
  model <- object$sys
  coefs <- params(model)
  
  se <- sqrt(diag(getcov(object)))
  params <- data.frame(Estimated=coefs,se=se)
  
  report <- list(fit=fitch(object),params=params)
  res <- list(model=model,report=report)
  class(res) <- "summary.estpoly"
  res
}

#' Fit Characteristics
#' 
#' Returns quantitative assessment of the estimated model as a list
#' 
#' @param x the estimated model
#' 
#' @return 
#' A list containing the following elements
#' 
#' \item{MSE}{Mean Square Error measure of how well the response of the model fits
#' the estimation data}
#' \item{FPE}{Final Prediction Error}
#' \item{FitPer}{Normalized root mean squared error (NRMSE) measure of how well the 
#' response of the model fits the estimation data, expressed as a percentage.}
#' \item{AIC}{Raw Akaike Information Citeria (AIC) measure of model quality}
#' \item{AICc}{Small sample-size corrected AIC}
#' \item{nAIC}{Normalized AIC}
#' \item{BIC}{Bayesian Information Criteria (BIC)}
#' 
#' @export
fitch <- function(x){
  y <- fitted(x) + resid(x)
  ek <- as.matrix(resid(x))
  N <- nrow(ek); np <- length(params(x$sys))
  
  # fit characteristics
  mse <- det(t(ek)%*%ek)/N
  fpe <- mse*(1+np/N)/(1-np/N)
  nrmse <- 1 - sqrt(sum(ek^2))/sqrt(sum((y-mean(y))^2))
  AIC <- N*log(mse) + 2*np + N*dim(matrix(y))[2]*(log(2*pi)+1)
  AICc <- AIC*2*np*(np+1)/(N-np-1)
  nAIC <- log(mse) + 2*np/N
  BIC <- N*log(mse) + N*dim(matrix(y))[2]*(log(2*pi)+1) + np*log(N)
  
  list(MSE=mse,FPE=fpe,FitPer = nrmse*100,AIC=AIC,AICc=AICc,nAIC=nAIC,BIC=BIC)
}

#' @export
print.summary.estpoly <- function(x,digits=4,...){
  print(x$model,se=x$report$params[,2],dig=digits)
  cat("\n Fit Characteristics \n")
  print(data.frame(x$report$fit),digits=digits)
}

#' @import ggplot2
#' @export
plot.estpoly <- function(x,newdata=NULL,...){
  
  if(is.null(newdata)){
    ypred <- ts(fitted(x),names="Predicted")
    yact <- ts(fitted(x) + resid(x),names="Actual")
    time <- time(x$input)
    titstr <- "Predictions of Model on Training Set"
  } else{  
    if(class(newdata)!="idframe") stop("Only idframe objects allowed")
    ypred <- predict(x,newdata)
    yact <- outputData(newdata)[,1]
    time <- time(newdata)
    titstr <- "Predictions of Model on Test Set"
  }
  df <- data.frame(Predicted=ypred,Actual=yact,Time=time)
  with(df,ggplot(df, aes(x = Actual,y=Predicted)) +  ggtitle(titstr) +
    geom_abline(intercept=0,slope=1,colour="#D55E00") +  geom_point())
}

#' Plot residual characteristics
#' 
#' Computes the 1-step ahead prediction errors (residuals) for an estimated polynomial
#' model, and plots auto-correlation of the residuals and the 
#' cross-correlation of the residuals with the input signals.
#' 
#' @param model estimated polynomial model
#' @param newdata an optional dataset on which predictions are to be computed. If
#' not supplied, predictions are computed on the training dataset.
#' @export
residplot <- function(model,newdata=NULL){
  if(is.null(newdata)){
    e <- resid(model); u <- model$input
  } else{
    if(class(newdata)!="idframe") stop("Only idframe objects allowed")
    e <- newdata$output[,1] - predict(model,newdata)[,1]
    u <- newdata$input
  }
  e <- matrix(e)
  acorr <- acf(e[,],plot = F); ccorr <- ccf(u[,1],e[,],plot = F)
  par(mfrow=c(2,1),mar=c(3,4,3,2))
  plot(acorr,ci=0.99,main="ACF of residuals")
  plot(ccorr,ci=0.99,main="CCF between the input and residuals",ylab="CCF")
}

#' Estimate ARX Models
#' 
#' Fit an ARX model of the specified order given the input-output data 
#' 
#' @param x an object of class \code{idframe}
#' @param order Specification of the orders: the three integer components 
#' (na,nb,nk) are the order of polynolnomial A, (order of polynomial B + 1) and 
#' the input-output delay
#' @param lambda Regularization parameter(Default=\code{0.1}) 
#' @param intNoise Logical variable indicating whether to add integrators in
#' the noise channel (Default=\code{FALSE})
#' @param fixed list containing fixed parameters. If supplied, only \code{NA} entries 
#' will be varied. Specified as a list of two vectors, each containing the parameters
#' of polynomials A and B respectively.
#' 
#' @details
#' SISO ARX models are of the form 
#' \deqn{
#'    y[k] + a_1 y[k-1] + \ldots + a_{na} y[k-na] = b_{nk} u[k-nk] + 
#'    \ldots + b_{nk+nb} u[k-nk-nb] + e[k] 
#' }
#' The function estimates the coefficients using linear least squares (with
#' regularization).
#' \cr
#' The data is expected to have no offsets or trends. They can be removed 
#' using the \code{\link{detrend}} function. 
#' \cr
#' To estimate finite impulse response(\code{FIR}) models, specify the first
#' order to be zero.
#' 
#' @return
#' An object of class \code{estpoly} containing the following elements:
#'  \item{sys}{an \code{idpoly} object containing the 
#'    fitted ARX coefficients}
#'  \item{fitted.values}{the predicted response}
#'  \item{residuals}{the residuals}
#'  \item{input}{the input data used}
#'  \item{call}{the matched call}
#'  \item{stats}{A list containing the following fields: \cr
#'      \code{vcov} - the covariance matrix of the fitted coefficients \cr
#'      \code{sigma} - the standard deviation of the innovations\cr
#'      \code{df} - the residual degrees of freedom}
#' 
#' 
#' @references
#' Arun K. Tangirala (2015), \emph{Principles of System Identification: 
#' Theory and Practice}, CRC Press, Boca Raton. Section 21.6.1
#' 
#' Lennart Ljung (1999), \emph{System Identification: Theory for the User}, 
#' 2nd Edition, Prentice Hall, New York. Section 10.1
#' 
#' @examples
#' data(arxsim)
#' mod_arx <- arx(arxsim,c(1,2,2))
#' mod_arx
#' plot(mod_arx) # plot the predicted and actual responses
#' 
#' @export
arx <- function(x,order=c(1,1,1),lambda=0.1,intNoise=FALSE,
                fixed=NULL){
  y <- outputData(x); u <- inputData(x)
  if(intNoise){
    y <- apply(y,2,diff)
    u <- apply(u,2,diff)
  }
  N <- dim(y)[1]
  na <- order[1];nb <- order[2]; nk <- order[3]
  nb1 <- nb+nk-1 ; n <- max(na,nb1); df <- N-na-nb
  
  yout <- apply(y,2,padZeros,n=n);
  uout <- apply(u,2,padZeros,n=n);
  
  fixedflag = is.null(fixed)
  uindex <- nk:nb1
  if(na!=0) yindex <- 1:na

  if(!fixedflag){
    # checking for correct specification of fixed parameters
    fixedA <- NULL;fixedB <- NULL
    g(fixedA,fixedB) %=% lapply(fixed,length)
    if(fixedA != na || fixedB != nb)
      stop("Number of parameters incorrectly specified in 'fixed'")
    
    fixedpars <- unlist(fixed)
    fixedpars <- fixedpars[!is.na(fixedpars)]
    df <- df + length(fixedpars)
    
    fixedpos_B <- which(!is.na(fixed[[2]]))
    uindex <- uindex[!uindex %in% (nk+fixedpos_B-1)]
    fixedpos_A <- numeric(0)
    if(na!=0){
      fixedpos_A <- which(!is.na(fixed[[1]]))
      yindex <- yindex[!yindex %in% fixedpos_A]
    }
  }
  
  reg <- function(i) {
    # regressor
    temp <- numeric(0)
    if(na!=0) temp <- c(temp,-yout[i-yindex,])
    phi <- t(c(temp,uout[i-uindex,]))
    l <- list(phi=phi)
    # fixed regressors
    if(!fixedflag){
      temp <- numeric(0)
      if(length(fixedpos_A)!=0){
        temp <- c(temp,-yout[i-fixedpos_A,])
      } 
      if(length(fixedpos_B)!=0){
        temp <- c(temp,uout[i-(nk+fixedpos_B),])
      }
      l$fixed <- t(temp)
    }
    return(l)
  }
  
  temp <- lapply(n+1:(N+n),reg)
  X <- do.call(rbind,lapply(temp, function(x) x[[1]]))
  Y <- yout[n+1:(N+n),,drop=F]
  fixedY <- matrix(rep(0,nrow(Y)))
  if(!fixedflag){
    fixedreg <- do.call(rbind,lapply(temp, function(x) x[[2]]))
    fixedY <- fixedreg%*%fixedpars
    Y <- Y - fixedY
  }
  
  # lambda <- 0.1
  inner <- t(X)%*%X + lambda*diag(dim(X)[2])
  innerinv <- solve(inner)
  pinv <- innerinv%*% t(X)
  coef <- pinv%*%Y
  
  eps <- X%*%coef
  sigma2 <- sum(eps^2)/(df+n)
  vcov <- sigma2 * innerinv
  fit <- (X%*%coef+fixedY)[1:N,,drop=F]
  if(intNoise) fit <- apply(fit,2,cumsum)
  
  if(!fixedflag){
    findex <- which(!is.na(unlist(fixed)))
    eindex <- 1:(na+nb)
    eindex <- eindex[!eindex %in% findex]
    temp <- rep(0,na+nb); temp2 <- matrix(0,nrow=na+nb,ncol=na+nb)

    temp[eindex] <- coef; temp[findex] <- fixedpars; coef <- temp
    temp2[eindex,eindex] <- vcov; vcov <- temp2
  } 
  
  if(na==0){
    A <- 1
  } else {
    A <- c(1,coef[1:na])
  }
  model <- idpoly(A = A,B = coef[na+1:nb],
               ioDelay = nk,Ts=deltat(x),noiseVar = sqrt(sigma2),
               intNoise=intNoise,unit=x$unit)
  
  estpoly(sys = model,stats=list(vcov = vcov, sigma = sqrt(sigma2),
              df = df),fitted.values=fit,residuals=eps[1:N,,drop=F],
          call=match.call(),input=u)
}

#' Estimate ARMAX Models
#' 
#' Fit an ARMAX model of the specified order given the input-output data 
#' 
#' @param x an object of class \code{idframe}
#' @param order Specification of the orders: the four integer components 
#' (na,nb,nc,nk) are the order of polynolnomial A, order of polynomial B 
#' + 1, order of the polynomial C,and the input-output delay respectively
#' @param init_sys Linear polynomial model that configures the initial parameterization.
#' Must be an ARMAX model. Overrules the \code{order} argument
#' @param intNoise Logical variable indicating whether to add integrators in
#' the noise channel (Default=\code{FALSE})
#' @param options Estimation Options, setup using \code{\link{optimOptions}}
#' 
#' @details
#' SISO ARMAX models are of the form 
#' \deqn{
#'    y[k] + a_1 y[k-1] + \ldots + a_{na} y[k-na] = b_{nk} u[k-nk] + 
#'    \ldots + b_{nk+nb} u[k-nk-nb] + c_{1} e[k-1] + \ldots c_{nc} e[k-nc]
#'    + e[k] 
#' }
#' The function estimates the coefficients using non-linear least squares 
#' (Levenberg-Marquardt Algorithm)
#' \cr
#' The data is expected to have no offsets or trends. They can be removed 
#' using the \code{\link{detrend}} function. 
#' 
#' @return
#'  An object of class \code{estpoly} containing the following elements:
#'  \item{sys}{an \code{idpoly} object containing the 
#'    fitted ARMAX coefficients}
#'  \item{fitted.values}{the predicted response}
#'  \item{residuals}{the residuals}
#'  \item{input}{the input data used}
#'  \item{call}{the matched call}
#'  \item{stats}{A list containing the following fields: \cr
#'      \code{vcov} - the covariance matrix of the fitted coefficients \cr
#'      \code{sigma} - the standard deviation of the innovations}
#'  \item{options}{Option set used for estimation. If no 
#'    custom options were configured, this is a set of default options}
#'  \item{termination}{Termination conditions for the iterative
#'     search used for prediction error minimization:
#'      \code{WhyStop} - Reason for termination \cr
#'      \code{iter} - Number of Iterations \cr
#'      \code{iter} - Number of Function Evaluations }
#' 
#' 
#' @references
#' Arun K. Tangirala (2015), \emph{Principles of System Identification: 
#' Theory and Practice}, CRC Press, Boca Raton. Sections 14.4.1, 21.6.2
#' 
#' @examples
#' data(armaxsim)
#' z <- dataSlice(armaxsim,end=1533) # training set
#' mod_armax <- armax(z,c(1,2,1,2))
#' mod_armax
#' 
#' @export
armax <- function(x,order=c(0,1,1,0),init_sys=NULL,intNoise=FALSE,
                  options=optimOptions()){
  y <- outputData(x); u <- inputData(x)
  if(intNoise){
    y <- apply(y,2,diff)
    u <- apply(u,2,diff)
  }
  N <- dim(y)[1]
  if(!is.null(init_sys)){
    checkInitSys(init_sys)
    
    # Extract orders from initial guess
    na <- length(init_sys$A) -1;nb <- length(init_sys$B); 
    nc <- length(init_sys$C) -1;nk <- init_sys$ioDelay
    order <- c(na,nb,nc,nk)
    
    # Initial guess
    theta0 <- matrix(params(init_sys))
    ivs <- matrix(predict(init_sys,x))
    e_init <- y-ivs
  } else{
    na <- order[1];nb <- order[2]; nc <- order[3]; nk <- order[4]
    
    if(nc<1) 
      stop("Error: Not an ARMAX model")
    
    # Initial Parameter Estimates
    mod_arx <- iv4(x,c(na,nb,nk)) # fitting ARX model
    eps_init <- matrix(resid(mod_arx))
    mod_ma <- arima(eps_init,order=c(0,0,nc),include.mean = F)
    e_init <- matrix(mod_ma$residuals); e_init[is.na(e_init)] <- 0 
    theta0 <- matrix(c(mod_arx$sys$A[-1],mod_arx$sys$B,mod_ma$coef))
  }
  
  nb1 <- nb+nk-1 ; n <- max(na,nb1,nc); df <- N - na - nb - nc
  l <- levbmqdt(y,u,order,e_init,obj=armaxGrad,
                theta0=theta0,N=N,opt=options)
  theta <- l$params
  e <- ts(l$residuals,start = start(y),deltat = deltat(y))
  fit <- matrix(y-e)
  if(intNoise) fit <- apply(fit,2,cumsum)
  
  model <- idpoly(A = c(1,theta[1:na]),B = theta[na+1:nb],
                  C = c(1,theta[na+nb+1:nc]),ioDelay = nk,Ts=deltat(x),
                  noiseVar = l$sigma,intNoise=intNoise,unit=x$unit)
  
  estpoly(sys = model,stats=list(vcov = l$vcov, sigma = l$sigma),
          fitted.values=fit,residuals=e,call=match.call(),input=u,
          options = options,termination = l$termination)
}

#' Estimate Output-Error Models
#' 
#' Fit an output-error model of the specified order given the input-output data 
#' 
#' @param x an object of class \code{idframe}
#' @param order Specification of the orders: the four integer components 
#' (nb,nf,nk) are order of polynomial B + 1, order of the polynomial F,
#' and the input-output delay respectively
#' @param init_sys Linear polynomial model that configures the initial parameterization.
#' Must be an OE model. Overrules the \code{order} argument
#' @param options Estimation Options, setup using 
#' \code{\link{optimOptions}}
#' 
#' @details
#' SISO OE models are of the form 
#' \deqn{
#'    y[k] + f_1 y[k-1] + \ldots + f_{nf} y[k-nf] = b_{nk} u[k-nk] + 
#'    \ldots + b_{nk+nb} u[k-nk-nb] + f_{1} e[k-1] + \ldots f_{nf} e[k-nf]
#'    + e[k] 
#' }
#' The function estimates the coefficients using non-linear least squares 
#' (Levenberg-Marquardt Algorithm)
#' \cr
#' The data is expected to have no offsets or trends. They can be removed 
#' using the \code{\link{detrend}} function. 
#' 
#' @return
#'  An object of class \code{estpoly} containing the following elements:
#'  \item{sys}{an \code{idpoly} object containing the 
#'    fitted OE coefficients}
#'  \item{fitted.values}{the predicted response}
#'  \item{residuals}{the residuals}
#'  \item{input}{the input data used}
#'  \item{call}{the matched call}
#'  \item{stats}{A list containing the following fields: \cr
#'      \code{vcov} - the covariance matrix of the fitted coefficients \cr
#'      \code{sigma} - the standard deviation of the innovations}
#'  \item{options}{Option set used for estimation. If no 
#'    custom options were configured, this is a set of default options}
#'  \item{termination}{Termination conditions for the iterative
#'     search used for prediction error minimization:
#'      \code{WhyStop} - Reason for termination \cr
#'      \code{iter} - Number of Iterations \cr
#'      \code{iter} - Number of Function Evaluations }
#' 
#' @references
#' Arun K. Tangirala (2015), \emph{Principles of System Identification: 
#' Theory and Practice}, CRC Press, Boca Raton. Sections 14.4.1, 17.5.2, 
#' 21.6.3
#' 
#' @examples
#' data(oesim)
#' z <- dataSlice(oesim,end=1533) # training set
#' mod_oe <- oe(z,c(2,1,2))
#' mod_oe 
#' plot(mod_oe) # plot the predicted and actual responses
#' 
#' @export
oe <- function(x,order=c(1,1,0),init_sys=NULL,options=optimOptions()){
  y <- outputData(x); u <- inputData(x); N <- dim(y)[1]
  if(!is.null(init_sys)){
    checkInitSys(init_sys)
    
    # Extract orders from initial guess
    nb <- length(init_sys$B); nf <- length(init_sys$F1) -1
    nk <- init_sys$ioDelay;order <- c(nb,nf,nk)
    
    # Initial guess
    theta0 <- matrix(params(init_sys))
    ivs <- matrix(predict(init_sys,x))
    e_init <- y-ivs
  } else{
    nb <- order[1];nf <- order[2]; nk <- order[3];
    nb1 <- nb+nk-1 ; n <- max(nb1,nf);
    
    if(nf<1) 
      stop("Not an OE model")
    
    # Initial Model
    mod_arx <- iv4(x,c(nf,nb,nk)) # fitting ARX model
    wk <- resid(mod_arx)
    e_init <- as.numeric(stats::filter(wk,filter=-mod_arx$sys$A[-1],
                                       method = "recursive"))
    ivs <- y-e_init
    theta0 <- matrix(c(mod_arx$sys$B,mod_arx$sys$A[-1]))
  }
  nb1 <- nb+nk-1 ; n <- max(nb1,nf);df <- N - nb - nf
  
  l <- levbmqdt(y,u,order,ivs,obj=oeGrad,theta0=theta0,N=N,
                opt=options)
  theta <- l$params
  e <- ts(l$residuals,start = start(y),deltat = deltat(y))
  
  model <- idpoly(B = theta[1:nb],F1 = c(1,theta[nb+1:nf]),
                  ioDelay = nk,Ts=deltat(x),noiseVar = l$sigma,unit=x$unit)
  
  estpoly(sys = model,stats=list(vcov = l$vcov, sigma = l$sigma),
          fitted.values=y-e,residuals=e,call=match.call(),input=u,
          options = options,termination = l$termination)
}

#' Estimate Box-Jenkins Models
#' 
#' Fit a box-jenkins model of the specified order from input-output data 
#' 
#' @param z an \code{idframe} object containing the data
#' @param order Specification of the orders: the five integer components 
#' (nb,nc,nd,nf,nk) are order of polynomial B + 1, order of the polynomial C,
#' order of the polynomial D, order of the polynomial F, and the 
#' input-output delay respectively
#' @param init_sys Linear polynomial model that configures the initial parameterization.
#' Must be a BJ model. Overrules the \code{order} argument
#' @param options Estimation Options, setup using 
#' \code{\link{optimOptions}}
#' 
#' @details
#' SISO BJ models are of the form 
#' \deqn{
#'    y[k] = \frac{B(q^{-1})}{F(q^{-1})}u[k-nk] + 
#'    \frac{C(q^{-1})}{D(q^{-1})} e[k]
#' }
#' The orders of Box-Jenkins model are defined as follows:
#' \deqn{
#'    B(q^{-1}) = b_1 + b_2q^{-1} + \ldots + b_{nb} q^{-nb+1}
#' }
#' 
#' \deqn{
#'    C(q^{-1}) = 1 + c_1q^{-1} + \ldots + c_{nc} q^{-nc}
#' }
#' 
#' \deqn{
#'    D(q^{-1}) = 1 + d_1q^{-1} + \ldots + d_{nd} q^{-nd}
#' }
#' \deqn{
#'    F(q^{-1}) = 1 + f_1q^{-1} + \ldots + f_{nf} q^{-nf}
#' }
#' 
#' The function estimates the coefficients using non-linear least squares 
#' (Levenberg-Marquardt Algorithm)
#' \cr
#' The data is expected to have no offsets or trends. They can be removed 
#' using the \code{\link{detrend}} function. 
#' 
#' @return
#'  An object of class \code{estpoly} containing the following elements:
#'  \item{sys}{an \code{idpoly} object containing the 
#'    fitted BJ coefficients}
#'  \item{fitted.values}{the predicted response}
#'  \item{residuals}{the residuals}
#'  \item{input}{the input data used}
#'  \item{call}{the matched call}
#'  \item{stats}{A list containing the following fields: \cr
#'      \code{vcov} - the covariance matrix of the fitted coefficients \cr
#'      \code{sigma} - the standard deviation of the innovations}
#'  \item{options}{Option set used for estimation. If no 
#'    custom options were configured, this is a set of default options}
#'  \item{termination}{Termination conditions for the iterative
#'     search used for prediction error minimization:
#'      \code{WhyStop} - Reason for termination \cr
#'      \code{iter} - Number of Iterations \cr
#'      \code{iter} - Number of Function Evaluations }
#' 
#' @references
#' Arun K. Tangirala (2015), \emph{Principles of System Identification: 
#' Theory and Practice}, CRC Press, Boca Raton. Sections 14.4.1, 17.5.2, 
#' 21.6.3
#' 
#' @examples
#' data(bjsim)
#' z <- dataSlice(bjsim,end=1500) # training set
#' mod_bj <- bj(z,c(2,1,1,1,2))
#' mod_bj 
#' residplot(mod_bj) # residual plots
#' 
#' @export
bj <- function(z,order=c(1,1,1,1,0),
               init_sys=NULL,options=optimOptions()){
  y <- outputData(z); u <- inputData(z); N <- dim(y)[1]
  if(!is.null(init_sys)){
    checkInitSys(init_sys)
    
    # Extract orders from initial guess
    nb <- length(init_sys$B); nf <- length(init_sys$F1) -1
    nc <- length(init_sys$C) -1;nd <- length(init_sys$d) -1
    nk <- init_sys$ioDelay;order <- c(nb,nc,nd,nf,nk)
    
    # Initial guess
    theta0 <- matrix(params(init_sys))
    ivs <- matrix(predict(init_sys,z))
    e_init <- y-ivs
  } else{
    nb <- order[1];nc <- order[2]; nd <- order[3];
    nf <- order[4]; nk <- order[5];
    
    if(nc==0 && nd==0){
      oe(z,c(nb,nf,nk))
    } else{
      
      # Initial Guess
      mod_oe <- oe(z,c(nb,nf,nk))
      v <- resid(mod_oe); zeta <- matrix(predict(mod_oe))
      mod_arma <- arima(v,order=c(nd,0,nc),include.mean = F)
      C_params <- if(nc==0) NULL else coef(mod_arma)[nd+1:nc]
      theta0 <- matrix(c(mod_oe$sys$B,C_params,
                         -coef(mod_arma)[1:nd],mod_oe$sys$F1[-1]))
      eps <- matrix(resid(mod_arma))
    }
  }
  l <- levbmqdt(y,u,order,zeta,eps,obj=bjGrad,theta0=theta0,N=N,
                opt=options)
  theta <- l$params
  e <- ts(l$residuals,start = start(y),deltat = deltat(y))
  
  C_params <- if(nc==0) NULL else theta[nb+1:nc]
  model <- idpoly(B = theta[1:nb],C=c(1,C_params),
                  D=c(1,theta[nb+nc+1:nd]),
                  F1 = c(1,theta[nb+nc+nd+1:nf]),
                  ioDelay = nk,Ts=deltat(z),noiseVar = l$sigma,unit=z$unit)
  
  estpoly(sys = model,stats=list(vcov = l$vcov, sigma = l$sigma),
          fitted.values=y-e,residuals=e,call=match.call(),input=u,
          options = options,termination = l$termination) 

}