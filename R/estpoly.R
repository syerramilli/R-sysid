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
print.estpoly <- function(est,...){
  print(summary(est),...)
}

#' @export
summary.estpoly <- function(object)
{
  model <- object$sys
  if(model$type=="arx"||model$type=="armax"){
    coefs <- c(model$A[-1],model$B)
    na <- length(model$A) - 1; nk <- model$ioDelay; 
    nb <- length(model$B)
    if(model$type=="armax"){
      coefs <- c(coefs,model$C[-1])
      nc <- length(model$C)-1
    } 
  } else if(model$type=="oe"){
    coefs <- c(model$B,model$F1[-1])
    nf <- length(model$F1) - 1; nk <- model$ioDelay; 
    nb <- length(model$B)
  }
  
  se <- sqrt(diag(getcov(object)))
  params <- data.frame(Estimated=coefs,se=se)
  
  ek <- as.matrix(resid(object))
  N <- nrow(ek); np <- nrow(params)
  mse <- t(ek)%*%ek/N
  fpe <- det(mse)*(1+np/N)/(1-np/N)
  
  report <- list(fit=list(N=N,mse=mse,fpe=fpe),params=params)
  res <- list(model=model,report=report)
  class(res) <- "summary.estpoly"
  res
}

#' @export
print.summary.estpoly <- function(object,...){
  print(object$model,se=object$report$params[,2],...)
  print(object$report$fit,...)
}

#' @export
predict.estpoly <- function(model,newdata=NULL){
  if(is.null(newdata)){
    return(fitted(model))
  } else{
    mod <- model$sys
    y <- outputData(newdata); u <- inputData(newdata)
    if(mod$type=="arx"){
      f1 <- Ma(c(rep(0,mod$ioDelay),mod$B))
      f2 <- Ma(c(0,-mod$A[-1]))
      ypred <- filter(f1,u) + filter(f2,y)
    }
    return(ypred)
  }
}

#' @export
plot.estpoly <- function(model,newdata=NULL){
  require(ggplot2)
  
  if(is.null(newdata)){
    ypred <- fitted(model)
    yact <- fitted(model) + resid(model)
    time <- time(model$input)
    titstr <- "Predictions of Model on Training Set"
  } else{  
    if(class(newdata)!="idframe") stop("Only idframe objects allowed")
    ypred <- predict(model,newdata)
    yact <- outputData(newdata)[,1]
    time <- time(newdata)
    titstr <- "Predictions of Model on Test Set"
  }
  df <- data.frame(Predicted=ypred[,1],Actual=yact[,1],Time=time)
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
  
  acorr <- acf(e,plot = F); ccorr <- ccf(u[,1],e,plot = F)
  par(mfrow=c(2,1),mar=c(3,4,3,2))
  plot(acorr,main="ACF of residuals")
  plot(ccorr,main="CCF between the input and residuals",ylab="CCF")
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
#' no regularization). Future versions may include regularization 
#' parameters as well
#' \\
#' The data is expected to have no offsets or trends. They can be removed 
#' using the \code{\link{detrend}} function. 
#' 
#' @return
#' An object of class \code{estpoly} containing the following elements:
#' 
#' \tabular{ll}{
#'    \code{coefficients} \tab an \code{idpoly} object containing the 
#'    fitted coefficients \cr
#'    \code{vcov} \tab the covariance matrix of the fitted coefficients\cr
#'    \code{sigma} \tab the standard deviation of the innovations\cr
#'    \code{df} \tab the residual degrees of freedom \cr
#'    \code{fitted.values} \tab the predicted response \cr
#'    \code{residuals} \tab the residuals  \cr
#'    \code{call} \tab the matched call \cr
#'    \code{time} \tab the time of the data used \cr
#'    \code{input} \tab the input data used
#'  }
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
#' summary(model) # obtain estimates and their covariances
#' plot(model) # plot the predicted and actual responses
#' 
#' @export
arx <- function(x,order=c(0,1,0)){
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
  
  lambda <- 0.1
  inner <- t(X)%*%X + lambda*diag(dim(X)[2])
  innerinv <- solve(inner)
  pinv <- innerinv%*% t(X)
  coef <- pinv%*%Y
  
  sigma2 <- sum((Y-X%*%coef)^2)/(df+n)
  vcov <- sigma2 * innerinv
  
  model <- idpoly(A = c(1,coef[1:na]),B = coef[na+1:nb],
               ioDelay = nk,Ts=deltat(x))
  
  estpoly(coefficients = model,vcov = vcov, sigma = sqrt(sigma2),
              df = df,fitted.values=(X%*%coef)[1:N,],
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
#' \\
#' The data is expected to have no offsets or trends. They can be removed 
#' using the \code{\link{detrend}} function. 
#' 
#' @return
#' An object of class \code{estpoly} containing the following elements:
#' 
#' \tabular{ll}{
#'    \code{coefficients} \tab an \code{idpoly} object containing the 
#'    fitted coefficients \cr
#'    \code{vcov} \tab the covariance matrix of the fitted coefficients\cr
#'    \code{sigma} \tab the standard deviation of the innovations\cr
#'    \code{df} \tab the residual degrees of freedom \cr
#'    \code{fitted.values} \tab the predicted response \cr
#'    \code{residuals} \tab the residuals  \cr
#'    \code{call} \tab the matched call \cr
#'    \code{time} \tab the time of the data used \cr
#'    \code{input} \tab the input data used
#'  }
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
#' summary(mod_armax) # obtain estimates and their covariances
#' plot(mod_armax) # plot the predicted and actual responses
#' 
#' @export
armax <- function(x,order=c(0,1,1,0)){
  require(signal)
  y <- outputData(x); u <- inputData(x); N <- dim(y)[1]
  na <- order[1];nb <- order[2]; nc <- order[3]; nk <- order[4]
  nb1 <- nb+nk-1 ; n <- max(na,nb1,nc); df <- N - na - nb - nc
  
  if(nc<1) 
    stop("Error: Not an ARMAX model")
  
  padZeros <- function(x,n) c(rep(0,n),x,rep(0,n))
  yout <- apply(y,2,padZeros,n=n)
  uout <- apply(u,2,padZeros,n=n)
  
  reg <- function(i,y,u,e) {
    if(nk==0) v <- i-0:(nb-1) else v <- i-nk:nb1
    matrix(c(-y[i-1:na,],u[v,],e[i-1:nc,]))
  }
  theta0 <- matrix(rnorm(na+nb+nc)) # current parameters
  
  l <- levbmqdt(yout,uout,order,N,obj=armaxGrad,theta0=theta0,N=N)
  theta <- l$theta
  
  model <- idpoly(A = c(1,theta[1:na]),B = theta[na+1:nb],
                  C = c(1,theta[na+nb+1:nc]),ioDelay = nk,Ts=deltat(x))
  
  estpoly(coefficients = model,vcov = l$vcov, sigma = l$sigma,df = df,
          fitted.values=y, residuals=l$e,call=match.call(),
          input=u)
}

#' Estimate Output-Error Models
#' 
#' Fit an output-error model of the specified order given the input-output data 
#' 
#' @param x an object of class \code{idframe}
#' @param order: Specification of the orders: the four integer components 
#' (nb,nf,nk) are order of polynomial B + 1, order of the polynomial F,
#' and the input-output delay respectively
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
#' \\
#' The data is expected to have no offsets or trends. They can be removed 
#' using the \code{\link{detrend}} function. 
#' 
#' @return
#' An object of class \code{estpoly} containing the following elements:
#' 
#' \tabular{ll}{
#'    \code{coefficients} \tab an \code{idpoly} object containing the 
#'    fitted coefficients \cr
#'    \code{vcov} \tab the covariance matrix of the fitted coefficients\cr
#'    \code{sigma} \tab the standard deviation of the innovations\cr
#'    \code{df} \tab the residual degrees of freedom \cr
#'    \code{fitted.values} \tab the predicted response \cr
#'    \code{residuals} \tab the residuals  \cr
#'    \code{call} \tab the matched call \cr
#'    \code{time} \tab the time of the data used \cr
#'    \code{input} \tab the input data used
#'  }
#' 
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
#' summary(mod_oe) # obtain estimates and their covariances
#' plot(mod_oe) # plot the predicted and actual responses
#' 
#' @export
oe <- function(x,order=c(1,1,0)){
  require(signal)
  y <- outputData(x); u <- inputData(x); N <- dim(y)[1]
  nb <- order[1];nf <- order[2]; nk <- order[3];
  nb1 <- nb+nk-1 ; n <- max(nb1,nf); df <- N - nb - nf
  
  if(nf<1) 
    stop("Not an OE model")
  
  leftPadZeros <- function(x,n) c(rep(0,n),x)

  reg <- function(i) {
    if(nk==0) v <- i-0:(nb-1) else v <- i-nk:nb1
    matrix(c(uout[v,],-eout[i-1:nf,]))
  }
  
  # Initialize Algorithm
  i = 0
  mod_arx <- arx(x,c(nf,nb,nk)) # fitting ARX model
  iv <- matrix(predict(mod_arx))
  e <- resid(mod_arx)
  theta <- c(coef(mod_arx)$B,coef(mod_arx)$A[-1])

  uout <- apply(u,2,leftPadZeros,n=n)
  
  tol <- 10^(-5); sumSqRatio <- 1000; lambda <- 1
  
  while (sumSqRatio > tol){
    sumsq0 <- sum(e^2)
    # Compute gradient
    eout <- apply(iv,2,leftPadZeros,n=n)
    X <- t(sapply(n+1:N,reg))
    filt1 <- Arma(b=1,a=c(1,theta[nb+1:nf]))
    grad <- apply(X,2,filter,filt=filt1)
    
    # Update Parameters
    H <- 1/N*(t(grad)%*%grad) + lambda*diag(nb+nf)
    Hinv <- solve(H)
    theta <- theta + 1/N*Hinv%*%t(grad)%*%e
    
    # Update IVs and residuals
    iv <- X%*%theta; e <- y-iv
    sumsq <- sum(e^2)
    
    sumSqRatio <- abs(sumsq0-sumsq)/sumsq0
    #     print(sumsq);print(sumSqRatio)
    i=i+1
  }
  # print(sumSqRatio)
  sigma2 <- sum(e^2)/df
  vcov <- sigma2 * Hinv
  
  model <- idpoly(B = theta[1:nb],F1 = c(1,theta[nb+1:nf]),
                  ioDelay = nk,Ts=deltat(x))
  
  estpoly(coefficients = model,vcov = vcov, sigma = sqrt(sigma2),
          df = df,fitted.values=y-e, residuals=e[,],call=match.call(),
          input=u)
}