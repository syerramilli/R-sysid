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
  require(signal)
  if(is.null(newdata)){
    return(fitted(model))
  } else{
    mod <- model$sys
    y <- outputData(newdata); u <- inputData(newdata)
    if(mod$type=="arx"){
      f1 <- Ma(c(rep(0,mod$ioDelay),mod$B))
      f2 <- Ma(c(0,-mod$A[-1]))
      ypred <- signal::filter(f1,u) + signal::filter(f2,y)
    }
    return(ypred)
  }
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
#'    \code{sys} \tab an \code{idpoly} object containing the 
#'    fitted ARX coefficients \cr
#'    \code{fitted.values} \tab the predicted response \cr
#'    \code{residuals} \tab the residuals  \cr
#'    \code{input} \tab the input data used \cr
#'    \code{call} \tab the matched call \cr
#'    \code{stats} \tab A list containing the following fields:
#'    \tabular{ll}{
#'      \code{vcov} \tab the covariance matrix of the fitted coefficients\cr
#'      \code{sigma} \tab the standard deviation of the innovations\cr
#'      \code{df} \tab the residual degrees of freedom 
#'    }
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
#' model
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
#' \\
#' The data is expected to have no offsets or trends. They can be removed 
#' using the \code{\link{detrend}} function. 
#' 
#' @return
#' An object of class \code{estpoly} containing the following elements:
#' 
#' \tabular{ll}{
#'    \code{sys} \tab an \code{idpoly} object containing the 
#'    fitted ARMAX coefficients \cr
#'    \code{fitted.values} \tab the predicted response \cr
#'    \code{residuals} \tab the residuals  \cr
#'    \code{input} \tab the input data used \cr
#'    \code{call} \tab the matched call \cr
#'    \code{stats} \tab A list containing the following fields:
#'    \tabular{ll}{
#'      \code{vcov} \tab the covariance matrix of the fitted coefficients\cr
#'      \code{sigma} \tab the standard deviation of the innovations
#'    } \cr
#'    \code{options} \tab Option set used for estimation. If no 
#'    custom options were configured, this is a set of default options. \cr
#'    \code{termination} \tab Termination conditions for the iterative
#'     search used for prediction error minimization.
#'    \tabular{ll}{
#'      \code{WhyStop} \tab Reason for termination \cr
#'      \code{iter} \tab Number of Iterations \cr
#'      \code{iter} \tab Number of Function Evaluations 
#'    }  
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
armax <- function(x,order=c(0,1,1,0),options=optimOptions()){
  require(signal)
  y <- outputData(x); u <- inputData(x); N <- dim(y)[1]
  na <- order[1];nb <- order[2]; nc <- order[3]; nk <- order[4]
  nb1 <- nb+nk-1 ; n <- max(na,nb1,nc); df <- N - na - nb - nc
  
  if(nc<1) 
    stop("Error: Not an ARMAX model")
  
  padZeros <- function(x,n) c(rep(0,n),x,rep(0,n))
  yout <- apply(y,2,padZeros,n=n)
  uout <- apply(u,2,padZeros,n=n)
  
  theta0 <- matrix(rnorm(na+nb+nc)) # current parameters
  
  l <- levbmqdt(yout,uout,order,obj=armaxGrad,theta0=theta0,N=N,
                opt=options)
  theta <- l$params
  e <- ts(l$residuals,start = start(y),deltat = deltat(y))
  
  model <- idpoly(A = c(1,theta[1:na]),B = theta[na+1:nb],
                  C = c(1,theta[na+nb+1:nc]),ioDelay = nk,Ts=deltat(x))
  
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
#' \\
#' The data is expected to have no offsets or trends. They can be removed 
#' using the \code{\link{detrend}} function. 
#' 
#' @return
#' An object of class \code{estpoly} containing the following elements:
#' 
#' \tabular{ll}{
#'    \code{sys} \tab an \code{idpoly} object containing the 
#'    fitted OE coefficients \cr
#'    \code{fitted.values} \tab the predicted response \cr
#'    \code{residuals} \tab the residuals  \cr
#'    \code{input} \tab the input data used \cr
#'    \code{call} \tab the matched call \cr
#'    \code{stats} \tab A list containing the following fields:
#'    \tabular{ll}{
#'      \code{vcov} \tab the covariance matrix of the fitted coefficients\cr
#'      \code{sigma} \tab the standard deviation of the innovations
#'    } \cr
#'    \code{options} \tab Option set used for estimation. If no 
#'    custom options were configured, this is a set of default options. \cr
#'    \code{termination} \tab Termination conditions for the iterative
#'     search used for prediction error minimization.
#'    \tabular{ll}{
#'      \code{WhyStop} \tab Reason for termination \cr
#'      \code{iter} \tab Number of Iterations \cr
#'      \code{iter} \tab Number of Function Evaluations 
#'    }  
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
#' mod_oe <- oe(z,c(2,1,2),optimOptions(tol=1e-04,LMinit=0.01))
#' mod_oe 
#' plot(mod_oe) # plot the predicted and actual responses
#' 
#' @export
oe <- function(x,order=c(1,1,0),options=optimOptions()){
  require(signal)
  y <- outputData(x); u <- inputData(x); N <- dim(y)[1]
  nb <- order[1];nf <- order[2]; nk <- order[3];
  nb1 <- nb+nk-1 ; n <- max(nb1,nf); df <- N - nb - nf
  
  if(nf<1) 
    stop("Not an OE model")
  
  leftPadZeros <- function(x,n) c(rep(0,n),x)

  # Initial Guess
  mod_arx <- arx(x,c(nf,nb,nk)) # fitting ARX model
  iv <- matrix(predict(mod_arx))
  theta0 <- matrix(c(mod_arx$sys$B,mod_arx$sys$A[-1]))
  uout <- apply(u,2,leftPadZeros,n=n)
  
  l <- levbmqdt(y,uout,order,iv,obj=oeGrad,theta0=theta0,N=N,
                opt=options)
  theta <- l$params
  e <- ts(l$residuals,start = start(y),deltat = deltat(y))
  
  model <- idpoly(B = theta[1:nb],F1 = c(1,theta[nb+1:nf]),
                  ioDelay = nk,Ts=deltat(x))
  
  estpoly(sys = model,stats=list(vcov = l$vcov, sigma = l$sigma),
          fitted.values=y-e,residuals=e,call=match.call(),input=u,
          options = options,termination = l$termination)
}