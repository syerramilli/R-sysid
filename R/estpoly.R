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
summary.estpoly <- function(x)
{
  model <- x$sys
  coefs <- params(model)
  
  se <- sqrt(diag(getcov(x)))
  params <- data.frame(Estimated=coefs,se=se)
  
  report <- list(fit=fitch(x),params=params)
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
print.summary.estpoly <- function(x,digits=4){
  print(x$model,se=x$report$params[,2],dig=digits)
  cat("\n Fit Characteristics \n")
  print(data.frame(x$report$fit),digits=digits)
}

#' @export
plot.estpoly <- function(model,newdata=NULL){
  require(ggplot2)
  
  if(is.null(newdata)){
    ypred <- ts(fitted(model),names="Predicted")
    yact <- ts(fitted(model) + resid(model),names="Actual")
    time <- time(model$input)
    titstr <- "Predictions of Model on Training Set"
  } else{  
    if(class(newdata)!="idframe") stop("Only idframe objects allowed")
    ypred <- predict(model,newdata)
    yact <- outputData(newdata)[,1]
    time <- time(newdata)
    titstr <- "Predictions of Model on Test Set"
  }
  df <- data.frame(Predicted=ypred,Actual=yact,Time=time)
  ggplot(df, aes(x = Actual,y=Predicted)) +  ggtitle(titstr) +
    geom_abline(intercept=0,slope=1,colour="#D55E00") +  geom_point()
}

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
#' @param order: Specification of the orders: the three integer components 
#' (na,nb,nk) are the order of polynolnomial A, (order of polynomial B + 1) and 
#' the input-output delay
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
#' model <- arx(data,c(2,1,1))
#' model
#' plot(model) # plot the predicted and actual responses
#' 
#' @export
arx <- function(x,order=c(0,1,0),lambda=0.1){
  y <- outputData(x); u <- inputData(x); N <- dim(y)[1]
  na <- order[1];nb <- order[2]; nk <- order[3]
  nb1 <- nb+nk-1 ; n <- max(na,nb1); df <- N-na-nb
  
  padZeros <- function(x,n) c(rep(0,n),x,rep(0,n))
  yout <- apply(y,2,padZeros,n=n);
  uout <- apply(u,2,padZeros,n=n);
  
  reg <- function(i) {
    if(nk==0) v <- i-0:(nb-1) else v <- i-nk:nb1
    c(-yout[i-1:na,,drop=T],uout[v,,drop=T])
  }
  X <- t(sapply(n+1:(N+n),reg))
  Y <- yout[n+1:(N+n),,drop=F]
  
  # lambda <- 0.1
  inner <- t(X)%*%X + lambda*diag(dim(X)[2])
  innerinv <- solve(inner)
  pinv <- innerinv%*% t(X)
  coef <- pinv%*%Y
  
  sigma2 <- sum((Y-X%*%coef)^2)/(df+n)
  vcov <- sigma2 * innerinv
  
  model <- idpoly(A = c(1,coef[1:na]),B = coef[na+1:nb],
               ioDelay = nk,Ts=deltat(x),noiseVar = sqrt(sigma2),unit=x$unit)
  
  estpoly(sys = model,stats=list(vcov = vcov, sigma = sqrt(sigma2),
              df = df),fitted.values=(X%*%coef)[1:N,],
              residuals=(Y-X%*%coef)[1:N,],call=match.call(),input=u)
}

#' Estimate ARMAX Models
#' 
#' Fit an ARMAX model of the specified order given the input-output data 
#' 
#' @param x an object of class \code{idframe}
#' @param order: Specification of the orders: the four integer components 
#' (na,nb,nc,nk) are the order of polynolnomial A, order of polynomial B 
#' + 1, order of the polynomial C,and the input-output delay respectively
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
#' z <- dataSlice(data,end=1533) # training set
#' mod_armax <- armax(z,c(1,2,1,2))
#' mod_armax
#' 
#' @export
armax <- function(x,order=c(0,1,1,0),init_sys=NULL,options=optimOptions()){
  y <- outputData(x); u <- inputData(x); N <- dim(y)[1]
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
    mod_arx <- iv(x,c(na,nb,nk)) # fitting ARX model
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
  
  model <- idpoly(A = c(1,theta[1:na]),B = theta[na+1:nb],
                  C = c(1,theta[na+nb+1:nc]),ioDelay = nk,Ts=deltat(x),
                  noiseVar = l$sigma,unit=x$unit)
  
  estpoly(sys = model,stats=list(vcov = l$vcov, sigma = l$sigma),
          fitted.values=y-e,residuals=e,call=match.call(),input=u,
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
#' z <- dataSlice(data,end=1533) # training set
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
    mod_arx <- iv(x,c(nf,nb,nk)) # fitting ARX model
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
#' z <- dataSlice(data,end=1500) # training set
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
    ivs <- matrix(predict(init_sys,x))
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