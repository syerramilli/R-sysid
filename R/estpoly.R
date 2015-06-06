#' @export
plot.estPoly <- function(model,newdata=NULL){
  require(ggplot2)
  require(reshape2)
  
  if(is.null(newdata)){
    ypred <- fitted(model)
    yact <- fitted(model) + resid(model)
    time <- model$time
  } else{  
    if(class(newdata)!="idframe") stop("Only idframe objects allowed")
    ypred <- sim(coef(model),newdata$input[,1,drop=F])
    yact <- newdata$output[,1,drop=F]
    time <- seq(from=newdata$t.start,to=newdata$t.end,by=newdata$Ts)
  }
  df <- data.frame(Predicted=ypred,Actual=yact,Time=time)
  meltdf <- melt(df,id="Time")
  ggplot(meltdf, aes(x = Time,y=value,colour=variable,group=variable)) +
    geom_line() + theme_bw()
}

#' Estimate ARX Models
#' 
#' @export
estARX <- function(data,order=c(0,1,0)){
  y <- as.matrix(data$output)
  u <- as.matrix(data$input); N <- dim(y)[1]
  na <- order[1];nb <- order[2]; nk <- order[3]
  nb1 <- nb+nk; n <- max(na,nb1); df <- N - na - nb -nk
  
  padZeros <- function(x,n) c(rep(0,n),x,rep(0,n))
  yout <- apply(y,2,padZeros,n=n);
  uout <- apply(u,2,padZeros,n=n);
  
  reg <- function(i) cbind(-yout[i-1:na,],uout[i-nk:nb1])
  X <- t(sapply(n+1:(N+n),reg))
  Y <- yout[n+1:(N+n),,drop=F]
  
  qx <- qr(X); coef <- qr.solve(qx,Y)
  sigma2 <- sum((Y-X%*%coef)^2)/df
  
  vcov <- sigma2 * chol2inv(qx$qr)
  
  model <- arx(A = c(1,coef[1:na]),B = coef[na+1:nb1],ioDelay = nk)
  time <- seq(from=data$t.start,to=data$t.end,by=data$Ts)
  
  est <- list(coefficients = model,vcov = vcov, sigma = sqrt(sigma2),
              df = df,fitted.values=(X%*%coef)[1:N,],
              residuals=(Y-X%*%coef)[1:N,],call=match.call(),
              time=time)
  class(est) <- c("estARX","estPoly")
  est
}

#' @export
predict.estARX <- function(model,newdata=NULL){
  if(is.null(newdata)){
    return(fitted(model))
  } else{
    return(sim(coef(model),newdata))
  }
}

#' @export
summary.estARX <- function(object)
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
  res <- list(call=object$call,coefficients=TAB,sigma=object$sigma,
              df=object$df)
  class(res) <- "summary.estARX"
  res
}

#' @export
print.summary.estARX <- function(object){
  cat("Discrete-time ARX model: A(q^{-1})y[k] = B(q^{-1})u[k] + e[k] \n")
  cat("Call: ");print(object$call);cat("\n\n")
  
  print(coef(object))
  cat(paste("\nsigma:",format(object$sigma,digits=4)))
  cat(paste("\nDoF:",object$df))
}