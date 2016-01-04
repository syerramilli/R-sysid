#' @export
predict.idpoly <- function(x,data,nahead=1){
  y <- outputData(x); u<- inputData(x)
  G <- signal::Arma(b=c(rep(0,x$ioDelay),x$B),
                    a= as.numeric(polynom::polynomial(x$A)*
                                    polynom::polynomial(x$F1)))
  det_sys <- signal::filter(G,u)
  if(x$type=="oe" || nahead==Inf){
    ypred <- det_sys
  } else{
    Hden <- as.numeric(polynom::polynomial(x$A)*polynom::polynomial(x$D))
    Hinv <- signal::Arma(b=Hden,a=x$C)
    filtered <- signal::filter(Hinv,y-det_sys)
    if(nahead!=1){
      
    }
    ypred <- y[k] - filtered
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

polyinv <- function(x,k){
  gamma <- 1/Re(polyroot(x))
}