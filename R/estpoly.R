#' @export
estPoly <- function(coefficients,vcov,sigma,df,fitted.values,
                    residuals,call,input){
  out <- list(coefficients= coefficients,vcov= vcov,sigma = sigma,
              df= df,fitted.values=fitted.values, 
              residuals= residuals,call= call,input=input)
  class(out) <- "estPoly"
  out
}

#' @export
summary.estPoly <- function(object)
{
  coefs <- c(coef(object)$A[-1],coef(object)$B)
  se <- sqrt(diag(object$vcov))
  tval <- coefs / se
  TAB <- cbind(Estimate = coefs,
               StdErr = se,
               t.value = tval,
               p.value = 2*pt(-abs(tval), df=object$df))
  na <- length(coef(object)$A) - 1; nk <- coef(object)$ioDelay; 
  nb <- length(coef(object)$B) - nk
  
  rownames(TAB) <- rep("a",nrow(TAB))
  for(i in 1:na) rownames(TAB)[i] <- paste("a",i,sep="")
  for(j in (na+1):nrow(TAB)) {
    rownames(TAB)[j] <- paste("b",j-na-1+nk,sep="")
  }
  ek <- as.matrix(resid(model))
  N <- nrow(ek); np <- nrow(TAB)
  mse <- t(ek)%*%ek/N
  fpe <- det(mse)*(1+np/N)/(1-np/N)
  
  res <- list(call=object$call,coefficients=TAB,mse = mse,
              fpe=fpe,df=object$df)
  class(res) <- "summary.estPoly"
  res
}

#' @export
print.summary.estPoly <- function(object){
  cat("Discrete-time ARX model: A(q^{-1})y[k] = B(q^{-1})u[k] + e[k] \n")
  cat("Call: ");print(object$call);cat("\n\n")
  
  print(coef(object))
  cat(paste("\nMSE:",format(object$mse,digits=4),
            "\tFPE:",format(object$fpe,digits=4)))
  cat(paste("\nDoF:",object$df))
}

#' @export
predict.estPoly <- function(model,newData=NULL){
  if(is.null(newdata)){
    return(fitted(model))
  } else{
    return(sim(coef(model),newdata$input))
  }
}

#' @export
plot.estPoly <- function(model,newdata=NULL){
  require(ggplot2)
  
  if(is.null(newdata)){
    ypred <- fitted(model)
    yact <- fitted(model) + resid(model)
    time <- time(model$input)
    titstr <- "Predictions of Model on Training Set"
  } else{  
    if(class(newdata)!="idframe") stop("Only idframe objects allowed")
    ypred <- sim(coef(model),inputData(newdata))
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
    e <- newdata$output[,1] - predict(model,newdata)
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
#' An object with classes \code{estARX} and \code{estPoly}, containing 
#' the following elements:
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
  na <- order[1];nb <- order[2]-1; nk <- order[3]
  nb1 <- nb+nk ; n <- max(na,nb1); df <- N - na - nb - 1
  
  padZeros <- function(x,n) c(rep(0,n),x,rep(0,n))
  yout <- apply(y,2,padZeros,n=n);
  uout <- apply(u,2,padZeros,n=n);
  
  reg <- function(i) {
    if(nk==0) v <- i-0:(nb-1) else v <- i-nk:nb1
    cbind(-yout[i-1:na,],uout[v])
  }
  X <- t(sapply(n+1:(N+n),reg))
  Y <- yout[n+1:(N+n),,drop=F]
  
  qx <- qr(X); coef <- qr.solve(qx,Y)
  sigma2 <- sum((Y-X%*%coef)^2)/df
  
  vcov <- sigma2 * chol2inv(qx$qr)
  
  model <- idpoly(A = c(1,coef[1:na]),B = coef[na+1:(nb+1)],
               ioDelay = nk,Ts=deltat(x))
  
  estPoly(coefficients = model,vcov = vcov, sigma = sqrt(sigma2),
              df = df,fitted.values=(X%*%coef)[1:N,],
              residuals=(Y-X%*%coef)[1:N,],call=match.call(),input=u)
}

armax <- function(x,order=c(0,1,1,0)){
  y <- outputData(x); u <- inputData(x); N <- dim(y)[1]
  e <- matrix(rep(0,N),ncol=1)
  na <- order[1];nb <- order[2]-1; nc <- order[3]
  nb1 <- nb+nk ; n <- max(na,nb1,nc)
  
  if(nc<1) 
    stop("Error: Not an ARMAX model")
  
  
}