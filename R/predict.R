#' @export
predict.idpoly <- function(x,data,nahead=1){
  y <- outputData(z); u<- inputData(z)
  G <- signal::Arma(b=c(rep(0,x$ioDelay),x$B),
                    a= as.numeric(polynom::polynomial(x$A)*
                                    polynom::polynomial(x$F1)))
  if(x$type=="oe" || nahead==Inf){
    ypred <- signal::filter(G,u)
  } else{
    Hden <- as.numeric(polynom::polynomial(x$A)*polynom::polynomial(x$D))
    Hinv <- signal::Arma(b=Hden,a=x$C) 
  }
  
  return(ypred)
}

#' @export
predict.estpoly <- function(x,newdata=NULL,nahead=1){
  if(is.null(newdata)&& nahead==1){
    return(fitted(x))
  } else{
    model <- x$sys
    if(is.null(newdata)){
      y <- fitted(x)+resid(x)
      u <- x$input
      z <- idframe(y,u,Ts = deltat(y),start=start(y))
    } else{
      z <- newdata
    }
    predict(model,z,nahead)
  } 
}
