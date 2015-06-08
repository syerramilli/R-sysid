#' @export
idpoly <- function(A=1,B=1,C=1,D=1,F1=1,ioDelay=0,Ts=1){
  out <- list(A= A,B=B,C=C,D=D,F1=F1,ioDelay = ioDelay,Ts=Ts)
  class(out) <- "idpoly"
  return(out)
}

#' @export
arx <- function(A,B,ioDelay=0,Ts=1){
  out <- idpoly(A=A,B=B,ioDelay = ioDelay,Ts=1)
  class(out) <- c("arx","idpoly")
  return(out)
}

#' @export
print.arx <- function(obj){
  cat("Discrete-time ARX model: A(q^{-1})y[k] = B(q^{-1})u[k] + e[k] \n\n")
  cat("A(q^{-1}) = ")
  for(i in seq_along(obj$A)){
    if(i-1==0){
      cat(obj$A[i])
    } else{
      if(obj$A[i]>0) cat(" + ") else cat("- ")
      
      if(!(abs(obj$A[i])==1)) cat(abs(obj$A[i]))
      cat("q^{-",i-1,"}",sep="")
    }
    cat("\t")
  }
  cat("\n")
  cat("B(q^{-1}) = ")
  for(i in seq_along(obj$B)){
    if(i+obj$ioDelay-1==0){
      cat(obj$B[i])
    } else{
      
      if(!((obj$ioDelay!=0) && (i==1))){
        if(obj$B[i]>0) cat(" + ") else cat("- ")
      } else{
        if(obj$B[i]<0) cat("-")
      }
      
      if(!(abs(obj$B[i])==1)) cat(abs(obj$B[i]))
      cat("q^{-",i+obj$ioDelay-1,"}",sep="")
    }
    cat("\t")
  }
}

#' @export
armax <- function(A,B,C,ioDelay=0,Ts=1){
  out <- idpoly(A=A,B=B,C=C,ioDelay = ioDelay,Ts=Ts)
  class(out) <- c("armax","idpoly")
  return(out)
}

#' @export
oe <- function(B,C,ioDelay=0,Ts=1){
  out <- idpoly(B=B,C=C,ioDelay=ioDelay,Ts=Ts)
  class(out) <- c("oe","idpoly")
  return(out)
}

#' @export
oe <- function(B,C,ioDelay=0,Ts=1){
  out <- idpoly(B=B,C=C,ioDelay=ioDelay,Ts=Ts)
  class(out) <- c("oe","idpoly")
  return(out)
}

#' @export
bj <- function(B,C,D,F1,ioDelay=0,Ts=1){
  out <- idpoly(B=B,C=C,D=D,F1=F1,ioDelay=ioDelay,Ts=Ts)
  class(out) <- c("oe","idpoly")
  return(out)
}

